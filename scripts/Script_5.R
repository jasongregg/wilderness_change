##Jason Gregg
##June 16th, 2026

###This is the final working script 

##June 2026: Part 1 loads, checks, and rasterizes spatial data and then uses a block-based approach
### and terra extract to conduct pixel counts for 1989 and 2024 at the wilderness, state, and ecoregion scales.

### Resulting CSV files for state, ecoregion, and wilderness level NLCD change are then appended so they contain names, not just codes

#It includes all steps except some pre-processing of spatial data, including clipping ecoregion and wilderness areas
#and merging and cleaning wilderness areas with multiple polygons

#July 2026: Part 2 runs a loop that clips rasters of each wilderness for 1989 and 2024, then runs another loop
#to calculate pixel transitions. This 

library(terra)
library(sf)
library(dplyr)
library(data.table)
library(readr)

setwd("bioe515_wilderness_change/")


#
##
####Step 1:Load spatial data
##
#


#contiguous US boundary (not needed in this script, this file was used to clip ecoregion and wilderness data only)
##conus <- st_read("data/conus/conus_polygon.shp")

#lower 48 state boundaries
lower48 <- vect("data/states/usa_lower48.shp")

#level 2 ecoregion boundaries, pre-clipped by contiguous USA boundary
l2ecoreg <- vect("data/ecoregions_l2/conus_clipped_l2_ecoregion/clipped_l2_ecoreg.shp")

#pre-processed wilderness area boundaries that aggregated wilderness areas with multiple polygons and clipped to contiguous USA boundary
wilderness <- vect("data/wilderness_areas/wilderness_final_aggregated_2.shp")

#pre-processed NLCD data from Script 1, process according to Travis's method for getting rid of original color template
nlcd1989 <- rast("data/NLCD1989/NLCD1989_processed.tif")
nlcd2024 <- rast("data/NLCD2024/NLCD2024_processed.tif")

#
##
####Step 2: match CRS across all data
##
#


#Match CRS of shapefiles to your NLCD datas' CRS (WGS84)
l2ecoreg <- project(l2ecoreg, crs(nlcd1989))
##conus <- st_transform(conus, crs(nlcd1989))
lower48 <- project(lower48, crs(nlcd2024))
wilderness <- project(wilderness, crs(nlcd1989))


#
##
###Step 3:Rasterize your state, ecoregion, and wilderness shapefiles
##
#


#You need to rasterize based on integers otherwise it won't work. 

#For states convert GEOID to integer directly, as its currently a character.
lower48$GEOID <- as.integer(as.character(lower48$GEOID))
# verify it worked
head(as.data.frame(lower48)$GEOID, 50)  # should show 35, 46, 6, 21, 1, 13
#For wilderness areas, verify your field WID is an integer.


# rasterize states shapefile
lower48_r <- rasterize(lower48, nlcd2024, field = "GEOID")
#write raster
terra::writeRaster(lower48_r, "data/rasterized/lower48_raster.tiff", overwrite=TRUE)

#For l2ecoreg, because NA_L2CODE is not an integer e.g. 10.2, multiply by 10 
#and put into new ID column "NA_L2CODE_int"
l2ecoreg$NA_L2CODE_int <- as.integer(as.numeric(l2ecoreg$NA_L2CODE) * 10)

#now rasterize l2ecoregion shapefile
l2ecoreg_r <- rasterize(l2ecoreg, nlcd2024, field = "NA_L2CODE_int")
#write raster
terra::writeRaster(l2ecoreg_r, "data/rasterized/l2ecoregion_raster.tiff")

#rasterize your wilderness areas based on the resolution of nlcd2024, using the WID field
#and using uses touches = true
#this means that cells the boundary touches become included in the final raster. e.g. wilderness areas with
#complex boundaries could end up with slightly more pixels
wilderness_r_touches <- rasterize(wilderness, nlcd2024, field = "WID", touches = TRUE)
#write raster
terra::writeRaster(wilderness_r_touches, "data/rasterized/wilderness_raster_touches.tiff")



###Step 4: block-based approach using terra::extract on your wilderness raster to create pixel counts in 
#1989, 2024 for wilderness areas

wilderness_r <- rast("data/rasterized/wilderness_raster_touches.tiff")

#set output placeholder
output_csv <- "nlcd_wilderness_pixel_counts_touches.csv"


#set number of blocks
bs <- blocks(nlcd1989, n = 10)  # change n for more or less blocks

message("Processing ", bs$n, " blocks...")

process_year <- function(nlcd_rast, year_label) {
  
  block_list <- vector("list", bs$n)
  
  readStart(nlcd_rast)
  readStart(wilderness_r)
  
  for (i in seq_len(bs$n)) {
    
    nlcd_b <- readValues(nlcd_rast,    row = bs$row[i], nrows = bs$nrows[i])
    wld_b  <- readValues(wilderness_r, row = bs$row[i], nrows = bs$nrows[i])
    
    # Filter to wilderness pixels inline
    wld_idx <- !is.na(wld_b) & wld_b != 0 & !is.na(nlcd_b)
    
    if (sum(wld_idx) == 0) next
    
    block_list[[i]] <- data.table(
      wilderness_id = wld_b[wld_idx],
      nlcd_class    = nlcd_b[wld_idx]
    )
    
    if (i %% 10 == 0) message("  Year ", year_label, " — block ", i, "/", bs$n)
  }
  
  readStop(nlcd_rast)
  readStop(wilderness_r)
  
  # Combine blocks and count pixels
  dt <- rbindlist(block_list, use.names = TRUE)
  counts <- dt[, .(pixel_count = .N), by = .(wilderness_id, nlcd_class)]
  counts[, year := year_label]
  
  return(counts)
}

counts_1989 <- process_year(nlcd1989, 1989)
counts_2024 <- process_year(nlcd2024, 2024)

all_counts <- rbindlist(list(counts_1989, counts_2024))

setorder(all_counts, year, wilderness_id, nlcd_class)


#write your csv
fwrite(all_counts, output_csv)
message("Done! Saved to: ", output_csv)
message("Rows: ", nrow(all_counts))
message("Wilderness areas found: ", uniqueN(all_counts$wilderness_id))





###Step 5: Block-based approach to extract pixels at the eco region level, 
###again using wilderness touches raster

#load ecoregion raster
l2ecoreg_r <- rast("data/rasterized/l2ecoregion_raster.tiff")

#placeholder output
output_csv  <- "ecoreg_wilderness_touches_pixel_counts2.csv"


#set number of blocks
bs <- blocks(nlcd1989, n = 10)  # increase n if RAM is tight

message("Processing ", bs$n, " blocks...")

process_year <- function(nlcd_rast, year_label) {
  
  block_list <- vector("list", bs$n)
  
  readStart(nlcd_rast)
  readStart(wilderness_r)
  readStart(l2ecoreg_r)
  
  for (i in seq_len(bs$n)) {
    
    nlcd_b <- readValues(nlcd_rast,   row = bs$row[i], nrows = bs$nrows[i])
    wld_b  <- readValues(wilderness_r, row = bs$row[i], nrows = bs$nrows[i])
    eco_b  <- readValues(l2ecoreg_r,   row = bs$row[i], nrows = bs$nrows[i])
    
    # Filter to wilderness pixels inline — no mask() needed
    wld_idx <- !is.na(wld_b) & wld_b != 0 & !is.na(nlcd_b) & !is.na(eco_b)
    
    if (sum(wld_idx) == 0) next
    
    block_list[[i]] <- data.table(
      ecoregion  = eco_b[wld_idx],
      nlcd_class = nlcd_b[wld_idx]
    )
    
    if (i %% 10 == 0) message("  Year ", year_label, " — block ", i, "/", bs$n)
  }
  
  readStop(nlcd_rast)
  readStop(wilderness_r)
  readStop(l2ecoreg_r)
  
  # Combine blocks and count pixels
  dt <- rbindlist(block_list, use.names = TRUE)
  counts <- dt[, .(pixel_count = .N), by = .(ecoregion, nlcd_class)]
  counts[, year := year_label]
  
  return(counts)
}

counts_1989 <- process_year(nlcd1989, 1989)
counts_2024 <- process_year(nlcd2024, 2024)

all_counts <- rbindlist(list(counts_1989, counts_2024))

setorder(all_counts, year, ecoregion, nlcd_class)



#Write csv
fwrite(all_counts, output_csv)
message("Done! Saved to: ", output_csv)
message("Rows: ", nrow(all_counts))
message("Ecoregions found: ", uniqueN(all_counts$ecoregion))

###Quick check to see whether, with the code above (terra extract) I got pixels from all wilderness areas, even the coastal ones,
#e.g. florida.


##############

# -----------------------------
# Diagnostic: NLCD x Wilderness x Ecoregion
# -----------------------------

diagnostic_counts <- data.table(
  NLCD = character(),
  Wilderness = character(),
  Ecoregion = character(),
  pixel_count = integer()
)

readStart(nlcd1989)
readStart(wilderness_r)
readStart(l2ecoreg_r)

for (i in seq_len(bs$n)) {
  
  nlcd_b <- readValues(
    nlcd1989,
    row = bs$row[i],
    nrows = bs$nrows[i]
  )
  
  wld_b <- readValues(
    wilderness_r,
    row = bs$row[i],
    nrows = bs$nrows[i]
  )
  
  eco_b <- readValues(
    l2ecoreg_r,
    row = bs$row[i],
    nrows = bs$nrows[i]
  )
  
  # Classify each pixel as Present or NA
  block_dt <- data.table(
    NLCD = ifelse(is.na(nlcd_b), "NA", "Present"),
    
    Wilderness = ifelse(
      is.na(wld_b) | wld_b == 0,
      "NA",
      "Present"
    ),
    
    Ecoregion = ifelse(
      is.na(eco_b),
      "NA",
      "Present"
    )
  )
  
  # Count combinations within this block
  block_counts <- block_dt[
    ,
    .(pixel_count = .N),
    by = .(NLCD, Wilderness, Ecoregion)
  ]
  
  diagnostic_counts <- rbind(
    diagnostic_counts,
    block_counts
  )
  
  if (i %% 10 == 0) {
    message("Processed block ", i, "/", bs$n)
  }
}

readStop(nlcd1989)
readStop(wilderness_r)
readStop(l2ecoreg_r)


# Combine counts from all blocks
diagnostic_counts <- diagnostic_counts[
  ,
  .(pixel_count = sum(pixel_count)),
  by = .(NLCD, Wilderness, Ecoregion)
]


# Sort the table
setorder(
  diagnostic_counts,
  NLCD,
  Wilderness,
  Ecoregion
)


# View ALL combinations
diagnostic_counts


#write diagnostic counts table for future reference.
write_excel_csv(
  diagnostic_counts,
  file.path("outputs/appended_csv/raster_comparison_pixel_summary.csv")
  )

#continuing the diagnostic, lets locate where these NA values are coming from
# Use the same block structure as before
bs <- blocks(nlcd1989, n = 10)

# Store the number of problematic pixels in each block
missing_eco_by_block <- data.table(
  block = seq_len(bs$n),
  row = bs$row,
  nrows = bs$nrows,
  missing_eco_pixels = 0
)

readStart(nlcd1989)
readStart(wilderness_r)
readStart(l2ecoreg_r)

for (i in seq_len(bs$n)) {
  
  nlcd_b <- readValues(
    nlcd1989,
    row = bs$row[i],
    nrows = bs$nrows[i]
  )
  
  wld_b <- readValues(
    wilderness_r,
    row = bs$row[i],
    nrows = bs$nrows[i]
  )
  
  eco_b <- readValues(
    l2ecoreg_r,
    row = bs$row[i],
    nrows = bs$nrows[i]
  )
  
  # Wilderness + NLCD present, but ecoregion missing
  problem_idx <- !is.na(nlcd_b) &
    !is.na(wld_b) & wld_b != 0 &
    is.na(eco_b)
  
  missing_eco_by_block[
    block == i,
    missing_eco_pixels := sum(problem_idx)
  ]
  
  message(
    "Block ", i, "/", bs$n,
    " — missing ecoregion pixels: ",
    sum(problem_idx)
  )
}

readStop(nlcd1989)
readStop(wilderness_r)
readStop(l2ecoreg_r)

missing_eco_by_block
###this is interesting but just shows NA values are in a different blocks.









##Step 6 #Block-based approach to analyze at the state level, using wilderness touches raster

#load raster
lower48_r <- rast("data/raster_stack/lower48_raster.tiff")

#placeholder output
output_csv <- "state_wilderness_touches_pixel_counts2.csv"

#set block number
bs <- blocks(nlcd1989, n = 10)  # increase n if RAM is tight

message("Processing ", bs$n, " blocks...")

process_year <- function(nlcd_rast, year_label) {
  
  block_list <- vector("list", bs$n)
  
  readStart(nlcd_rast)
  readStart(wilderness_r)
  readStart(lower48_r)
  
  for (i in seq_len(bs$n)) {
    
    nlcd_b   <- readValues(nlcd_rast,    row = bs$row[i], nrows = bs$nrows[i])
    wld_b    <- readValues(wilderness_r, row = bs$row[i], nrows = bs$nrows[i])
    state_b  <- readValues(lower48_r,    row = bs$row[i], nrows = bs$nrows[i])
    
    # Filter to wilderness pixels inline
    wld_idx <- !is.na(wld_b) & wld_b != 0 & !is.na(nlcd_b) & !is.na(state_b)
    
    if (sum(wld_idx) == 0) next
    
    block_list[[i]] <- data.table(
      GEOID      = state_b[wld_idx],
      nlcd_class = nlcd_b[wld_idx]
    )
    
    if (i %% 10 == 0) message("  Year ", year_label, " — block ", i, "/", bs$n)
  }
  
  readStop(nlcd_rast)
  readStop(wilderness_r)
  readStop(lower48_r)
  
  # Combine blocks and count pixels
  dt <- rbindlist(block_list, use.names = TRUE)
  counts <- dt[, .(pixel_count = .N), by = .(GEOID, nlcd_class)]
  counts[, year := year_label]
  
  return(counts)
}

counts_1989 <- process_year(nlcd1989, 1989)
counts_2024 <- process_year(nlcd2024, 2024)

all_counts <- rbindlist(list(counts_1989, counts_2024))

setorder(all_counts, year, GEOID, nlcd_class)


#write csv

fwrite(all_counts, output_csv)
message("Done! Saved to: ", output_csv)
message("Rows: ", nrow(all_counts))
message("States found: ", uniqueN(all_counts$GEOID))



# Now you have three CSV files which include 1989 and 2024 NLCD land cover pixel 
# values for the following: 1) all wilderness areas 2) and all wilderness areas within each state, and 3) all
# wilderness areas within each level 2 ecoregion



# Step 7: load csv files generated from block-based extraction so you can re-append the 
# relevant state, wilderness, nlcd class, and ecoregion names to make them easier to interpret

wilderness <- read.csv("")

ecoreg <- read.csv("outputs/ecoreg_wilderness_touches_pixel_counts2.csv")


state <- read.csv("")

#re-append propper field IDs for wilderness areas, state names, ecoregion names so the
#csvs are more useful



#Now for the ecoregion appending

#create a lookup table
lookup <- unique(
  as.data.frame(l2ecoreg)[, c("NA_L2CODE_int", "NA_L2NAME")]
)

ecoreg_append <- merge(
  ecoreg,
  lookup,
  by.x = "ecoregion",
  by.y = "NA_L2CODE_int",
  all.x = TRUE
)


#rename your 'ecoregion' field to match the original spatial data
ecoreg_append <- ecoreg_append %>%
  rename(NA_L2CODE_int = ecoregion)

#move columns around
ecoreg_append <- ecoreg_append[, c(1, ncol(ecoreg_append), 2:(ncol(ecoreg_append)-1))]


#write the appended csv for analysis and figure making.

write_csv(ecoreg_append, "outputs/appended_csv/l2ecoreg_append.csv")







###Part 2, pixel transitions
###pairs of wilderness NLCD rasters were already created and saved based on using wilderness polygons
#as a mask.

#Trying to redo this using raster on raster mask and clip wasnt working, so lets use what we have and compare difference


# Path to the parent folder containing the 700 subfolders
parent_dir <- "outputs/wilderness_raster_outputs/"

# List all subfolders
folders <- list.dirs(parent_dir, full.names = TRUE, recursive = FALSE)

for (f in folders) {
  
  # --- Identify the two rasters ---
  ras_files <- list.files(f, pattern = "\\.tif$", full.names = TRUE)
  
  if (length(ras_files) != 2) {
    message("Skipping folder ", f, ": does not contain exactly two rasters")
    next
  }
  
  # Load rasters
  r1 <- rast(ras_files[1])
  r2 <- rast(ras_files[2])
  
  # --- Stack them ---
  rs <- c(r1, r2)
  names(rs) <- c("year1", "year2")
  
  # --- Extract pixel pairs ---
  vals <- as.data.frame(terra::values(rs))
  vals <- na.omit(vals)  # remove NA pairs
  
  # --- Create pixel transition pairs ---
  vals$transition <- paste(vals$year1, vals$year2, sep = "_")
  
  # Save pixel-wise table
  write.csv(vals, file.path(f, "transition_table.csv"), row.names = FALSE)
  
  # --- Create transition matrix ---
  tm <- vals %>%
    count(year1, year2) %>%
    tidyr::pivot_wider(
      names_from = year2,
      values_from = n,
      values_fill = 0
    )
  
  # Save transition matrix
  write.csv(tm, file.path(f, "transition_matrix.csv"), row.names = FALSE)
  
  message("Processed: ", f)
}




#now create a master csv that lists %pixels that have transitioned for each wilderness



parent_dir <- "outputs/wilderness_raster_outputs/"

folders <- list.dirs(
  parent_dir,
  full.names = TRUE,
  recursive = FALSE
)

results <- list()

for (f in folders) {
  
  transition_file <- file.path(f, "transition_table.csv")
  
  if (!file.exists(transition_file)) {
    message("Skipping: ", basename(f))
    next
  }
  
  vals <- read_csv(
    transition_file,
    show_col_types = FALSE
  )
  
  # Remove any rows with missing values
  vals <- vals %>%
    filter(
      !is.na(year1),
      !is.na(year2)
    )
  
  total_pixels <- nrow(vals)
  
  pixels_stayed <- sum(
    vals$year1 == vals$year2
  )
  
  pixels_transitioned <- sum(
    vals$year1 != vals$year2
  )
  
  percent_transitioned <-
    pixels_transitioned / total_pixels * 100
  
  results[[length(results) + 1]] <- data.frame(
    wilderness_area = basename(f),
    total_pixels = total_pixels,
    pixels_stayed = pixels_stayed,
    pixels_transitioned = pixels_transitioned,
    percent_transitioned = round(
      percent_transitioned,
      2
    )
  )
}

wilderness_summary <- bind_rows(results)

write_excel_csv(
  wilderness_summary,
  file.path(
    parent_dir,
    "wilderness_transition_summary.csv"
  )
)

print(wilderness_summary)








### now another csv that includes the detailed transitions so that you can make a sanky flow chart
transition_results <- list()

for (f in folders) {
  
  transition_file <- file.path(
    f,
    "transition_table.csv"
  )
  
  if (!file.exists(transition_file)) {
    next
  }
  
  vals <- read_csv(
    transition_file,
    show_col_types = FALSE
  )
  
  vals <- vals %>%
    filter(
      !is.na(year1),
      !is.na(year2)
    )
  
  total_pixels <- nrow(vals)
  
  # Count every unique transition
  transition_counts <- vals %>%
    count(
      year1,
      year2,
      name = "pixels"
    ) %>%
    mutate(
      wilderness_area = basename(f),
      transition = paste0(
        year1,
        "_to_",
        year2
      ),
      percent_of_total =
        round(
          pixels / total_pixels * 100,
          2
        )
    ) %>%
    select(
      wilderness_area,
      from_category = year1,
      to_category = year2,
      transition,
      pixels,
      percent_of_total
    )
  
  transition_results[[length(transition_results) + 1]] <-
    transition_counts
}

all_transitions <- bind_rows(
  transition_results
)

write_excel_csv(
  all_transitions,
  file.path(
    parent_dir,
    "wilderness_transition_details.csv"
  )
)

print(all_transitions)
















###seems to work, now aggregate them

# Read all transition matrices
tm_files <- list.files(parent_dir,
                       pattern = "transition_matrix.csv$",
                       full.names = TRUE,
                       recursive = TRUE)

# Read and bind all matrices
all_tm <- purrr::map_df(tm_files, ~ read.csv(.x))

# Sum counts across all folders
global_tm <- all_tm %>%
  group_by(year1) %>%
  summarise(across(everything(), sum, na.rm = TRUE))

# Save global transition matrix
write.csv(global_tm, file.path(parent_dir, "GLOBAL_transition_matrix.csv"), row.names = FALSE)


##convert the matrix for sankey

long_tm <- global_tm %>%
  pivot_longer(
    cols = -year1,
    names_to = "year2",
    values_to = "count"
  ) %>%
  filter(count > 0)


#make nodes and links

# Node list
nodes <- data.frame(name = sort(unique(c(long_tm$year1, long_tm$year2))))

# Link list
links <- long_tm %>%
  mutate(
    source = match(year1, nodes$name) - 1,
    target = match(year2, nodes$name) - 1,
    value  = count
  )


#assign the classes
class_names <- c(
  "11" = "Open Water",
  "12" = "Perennial Ice/Snow",
  "21" = "Developed, Open Space",
  "22" = "Developed, Low Intensity",
  "23" = "Developed, Medium Intensity",
  "24" = "Developed, High Intensity",
  "31" = "Barren Land (Rock/Sand/Clay",
  "42" = "Evergreen Forest",
  "41" = "Decidous Forest",
  "43" = "Mixed Forest",
  "52" = "Shrub/Scrub",
  "71" = "Grassland/Herbaceous",
  "81" = "Pasture/Hay",
  "82" = "Cultivated Crops",
  "90" = "Woody Wetlands",
  "95" = "Emergent Herbaceous Wetlands"
  
)


nodes$name <- class_names[nodes$name]
#make the diagram
sankeyNetwork(
  Links = links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value  = "value",
  NodeID = "name",
  fontSize = 14,
  nodeWidth = 30
)



##########The above works but
##########try alluvial plot instead


matrix <- read.csv("outputs/loop_output_nov11/GLOBAL_transition_matrix.csv")

transition_long <- matrix %>%
  pivot_longer(
    cols = -1,
    names_to = "to",
    values_to = "value"
  ) %>%
  rename(from = 1) %>%
  filter(value > 0)   # remove zeros if desired


# NLCD 2016 codes and class names
nlcd_lookup <- data.frame(
  code = c(11, 12, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95),
  class = c(
    "Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity",
    "Developed, Medium Intensity", "Developed, High Intensity",
    "Barren Land", "Deciduous Forest", "Evergreen Forest",
    "Mixed Forest", "Shrub/Scrub", "Grassland/Herbaceous",
    "Pasture/Hay", "Cultivated Crops", "Woody Wetlands", "Emergent Herbaceous Wetlands"
  )
)


###get rid of the x in the to column

transition_long <- transition_long %>%
  mutate(to = as.numeric(gsub("^X", "", to)))

###
transition_long <- transition_long %>%
  left_join(nlcd_lookup, by = c("from" = "code")) %>%
  rename(from_class = class) %>%
  left_join(nlcd_lookup, by = c("to" = "code")) %>%
  rename(to_class = class)

### apply NLCD class names and colors

nlcd_lookup <- data.frame(
  code = c(11,12,21,22,23,24,31,41,42,43,52,71,81,82,90,95),
  class = c(
    "Open Water", "Perennial Ice/Snow", "Developed, Open Space", "Developed, Low Intensity",
    "Developed, Medium Intensity", "Developed, High Intensity", "Barren Land",
    "Deciduous Forest", "Evergreen Forest", "Mixed Forest", "Shrub/Scrub",
    "Grassland/Herbaceous", "Pasture/Hay", "Cultivated Crops", "Woody Wetlands",
    "Emergent Herbaceous Wetlands"
  )
)

nlcd_colors <- c(
  "Open Water" = "#466b9f",
  "Perennial Ice/Snow" = "#F5F5F5",
  "Developed, Open Space" = "#dec5c5",
  "Developed, Low Intensity" = "#d99282",
  "Developed, Medium Intensity" = "#eb0000",
  "Developed, High Intensity" = "#ab0000",
  "Barren Land" = "#b3ac9f",
  "Deciduous Forest" = "#68ab5f",
  "Evergreen Forest" = "#1c5f2c",
  "Mixed Forest" = "#b5c58f",
  "Shrub/Scrub" = "#a68c30",
  "Grassland/Herbaceous" = "#ccba7c",
  "Pasture/Hay" = "#e2e2c5",
  "Cultivated Crops" = "#d0e4af",
  "Woody Wetlands" = "#87c0cd",
  "Emergent Herbaceous Wetlands" = "#abd9e9"
)

transition_long <- transition_long %>%
  mutate(
    from_class = factor(from_class, levels = names(nlcd_colors)),
    to_class   = factor(to_class, levels = names(nlcd_colors))
  )


plot1 <- ggplot(transition_long,
                aes(axis1 = from_class, axis2 = to_class, y = value)) +
  geom_alluvium(aes(fill = from_class), width = 1/12) +
  geom_stratum(width = 1/8, fill = "grey80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  scale_x_discrete(limits = c("1989", "2024"), expand = c(.1, .1)) +
  scale_fill_manual(values = nlcd_colors) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +
  labs(
    title = "NLCD Land Cover Transitions",
    y = NULL,  # removes y-axis label
    x = ""
  )

ggsave( "plot1.png", plot = plot1, width = 16, height = 40, dpi = 300, limitsize = FALSE)




######## STEP 2

#######this worked, but lets filter the smallest classes as they are not showing anything.
# should we lump them or get rid of them entirely?

##best way to figure this out is to run pland on all wildernesses and see the percentages of the bottom lowest land classes.
# for both years.
# how big of a lump is this?


# Calculate total transitions per class (from + to)
class_totals <- transition_long %>%
  mutate(class = from_class) %>%
  group_by(class) %>%
  summarise(total_from = sum(value)) %>%
  full_join(
    transition_long %>%
      mutate(class = to_class) %>%
      group_by(class) %>%
      summarise(total_to = sum(value)),
    by = "class"
  ) %>%
  mutate(total = total_from + total_to) %>%
  arrange(desc(total))

# Select the top 10 classes
top10_classes <- class_totals$class[1:10]
top10_classes

####filter
transition_top10 <- transition_long %>%
  filter(from_class %in% top10_classes & to_class %in% top10_classes)



##plot
plot1 <- ggplot(transition_top10,
                aes(axis1 = from_class, axis2 = to_class, y = value)) +
  geom_alluvium(aes(fill = from_class), width = 1/12) +
  geom_stratum(width = 1/8, fill = "grey80", color = "black") +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 3) +
  
  # Make the year labels larger
  scale_x_discrete(limits = c("1989", "2024"), expand = c(.1, .1)) +
  
  scale_fill_manual(values = nlcd_colors) +
  
  theme_minimal(base_size = 14) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    
    # Larger year labels
    axis.text.x = element_text(size = 18, face = "bold"),
    
    # Larger, centered title
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5)
  ) +
  
  labs(
    title = "CONUS wilderness land cover transitions 
                (Top 10 Classes)",
    y = NULL,
    x = ""
  )


ggsave("plot1_top10.png", plot = plot1,
       width = 12, height = 22, dpi = 300, limitsize = FALSE)







