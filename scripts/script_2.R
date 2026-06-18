#Jason Gregg
#Feb 20, 2026

#Script_2

# 1) stacks 1989 and 2024 NLCD rasters runs a loop where it clips each 
#wilderness areas and saves a 1989 and 2024 NLCD .tif for each wilderness area.

# 2) It then calculates NLCD pixel frequencies and creates three CSV files based on these frequencies.

# 3) It then creates a pixel transition matrix for each wilderness 1989/2024 raster pair, and saves it in each
# wilderness folder.

# 4) It creates a pixel transition matrix for all aggregated pixels and saves it in outputs.

# 5) It then uses the landscape metrics to calculate landscape metrics for each 1989/2024 raster pair, and saves it
#in each wilderness's metrics folder

# 6) It runs this pipeline at the scale of each state and ecoregion.


library(terra)
library(tidyverse)
library(sf)
library(landscapemetrics)
library(mapview)
library(dplyr)
library(tidyr)


setwd("/Users/jj/bioe515_wilderness_change")

#################
#load data
#################

#contiguous US boundary
conus <- st_read("data/conus/conus.shp")

#lower 48 states boundaries
lower48 <- st_read("data/states/usa_lower48.shp")

#level 2 ecoregion boundaries (unedited)
l2ecoreg <- st_read("data/ecoregions_l2/NA_CEC_Eco_Level2.shp")

#pre-processed wilderness area boundaries from Script 1
wilderness <- st_read("data/wilderness_areas/wilderness_final_aggregated_2.shp")

#pre-processed NLCD data from Script 1
nlcd1989 <- rast("data/NLCD1989/NLCD1989_processed.tif")
nlcd2024 <- rast("data/NLCD2024/NLCD2024_processed.tif")

#match CRS of shapefiles to your NLCD CRS (WGS84)
l2ecoreg <- st_transform(l2ecoreg, crs(nlcd1989))
conus <- st_transform(conus, crs(nlcd1989))
lower48 <- st_transform(lower48, crs(nlcd1989))
wilderness <- st_transform(wilderness, crs(nlcd1989))

###################
#Running loops
###################


# Stack your rasters
nlcd_stack <- c(nlcd1989, nlcd2024)
names(nlcd_stack) <- c("1989", "2024")
years <- c(1989, 2024)

# Parent output folder
output_parent <- "outputs/output_wilderness_areas"
dir.create(output_parent, showWarnings = FALSE)

# Loop through raster layers (faster than looping polygons)
for(j in 1:nlyr(nlcd_stack)) {
  
  r <- nlcd_stack[[j]]
  year <- years[j]
  
  # Mask and crop all polygons in one go using terra::mask
  # Loop only over polygons for saving files
  for(i in seq_len(nrow(wilderness))) {
    
    poly_name <- wilderness$NAME_ABBRE[i]
    poly_geom <- wilderness[i, ]
    
    # Create folder for this polygon
    poly_folder <- file.path(output_parent, poly_name)
    dir.create(poly_folder, showWarnings = FALSE)
    
    # Crop + mask
    r_clip <- crop(r, poly_geom)
    r_clip <- mask(r_clip, poly_geom)
    
    # Save raster as int1u
    raster_filename <- file.path(poly_folder, paste0("nlcd_", year, ".tif"))
    writeRaster(r_clip, raster_filename, datatype="INT1U", overwrite=TRUE)
  }
  
  message("Finished raster layer: ", year)
}



#####Part 2
## a loop that goes into these folders and calculates terra:freq for each 
##1989/2024 pair for each wilderness area and generates a large CSV file

# Path to your output folders
output_parent <- "outputs/output_wilderness_areas"

# Get all polygon folders
poly_folders <- list.dirs(output_parent, full.names = TRUE, recursive = FALSE)

# Initialize list to store results
results_list <- list()

# Loop through polygon folders
for(poly_folder in poly_folders) {
  
  poly_name <- basename(poly_folder)
  
  # List all TIFF files in the folder
  tif_files <- list.files(poly_folder, pattern = "\\.tif$", full.names = TRUE)
  
  # Loop through TIFFs
  poly_freq <- lapply(tif_files, function(tif) {
    r <- rast(tif)
    year <- gsub("nlcd_|\\.tif", "", basename(tif))  # extract year from filename
    
    # Calculate frequency table (includes NA by default)
    f <- terra::freq(r)
    
    # Convert to data.frame
    df <- as.data.frame(f)
    
    # Replace NA value label with "NA" for column naming
    df$value <- ifelse(is.na(df$value), "NA", df$value)
    
    # Create column names with year + class/NA
    col_name <- paste0(year, "_", df$value)
    
    # Convert to named vector
    setNames(df$count, col_name)
  })
  
  # Combine frequencies for this polygon into one row
  poly_row <- do.call(c, poly_freq)
  
  # Convert to data.frame and add polygon name
  results_list[[poly_name]] <- data.frame(Wilderness = poly_name, t(poly_row), check.names = FALSE)
}

# Combine all polygons into one data.frame
results_df <- bind_rows(results_list)

# Replace missing combinations with 0
results_df[is.na(results_df)] <- 0

# Save as CSV
write.csv(results_df, "outputs/wilderness_nlcd_pixel_frequency.csv", row.names = FALSE)


###########
########### Create table 2
###########
#Using the above csv, generates a simple table that aggregates pixel counts across all 751 wilderness 
#areas and then calculates hectares lost or gained


# 1. Read your frequency CSV
df <- read_csv("outputs/wilderness_nlcd_pixel_frequency.csv")

wilderness_col <- names(df)[1]

# 2. Pivot to long format
df_long <- df %>%
  pivot_longer(
    cols = -all_of(wilderness_col),
    names_to = c("Year", "LandCover"),
    names_pattern = "(\\d{4})_(.*)",
    values_to = "Pixels"
  )

# 3. Sum pixels across all wilderness areas
df_totals <- df_long %>%
  group_by(Year, LandCover) %>%
  summarise(
    Total_Pixels = sum(Pixels, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate total pixels in each year (all classes combined)
total_pixels_1989 <- df_totals %>% filter(Year == "1989") %>% summarise(sum1989 = sum(Total_Pixels)) %>% pull(sum1989)
total_pixels_2024 <- df_totals %>% filter(Year == "2024") %>% summarise(sum2024 = sum(Total_Pixels)) %>% pull(sum2024)

# 4. Pivot wider to get 1989 and 2024 side by side
df_wide <- df_totals %>%
  pivot_wider(
    names_from = Year,
    values_from = Total_Pixels
  )

# 5. Calculate change metrics and percentages
df_summary <- df_wide %>%
  mutate(
    Pixel_Change = `2024` - `1989`,
    Abs_Change = abs(Pixel_Change),
    Percent_of_Total_1989 = (`1989` / total_pixels_1989) * 100,
    Percent_of_Total_2024 = (`2024` / total_pixels_2024) * 100
  )

# 6. Contribution to total landscape change
total_landscape_change <- sum(df_summary$Abs_Change, na.rm = TRUE)

df_summary <- df_summary %>%
  mutate(
    Contribution_Percent = (Abs_Change / total_landscape_change) * 100,
    Hectares_Change = Pixel_Change * 0.09  # 30m x 30m pixels to hectares
  )

# 7. Clean final output
df_final <- df_summary %>%
  select(
    LandCover,
    Total_1989 = `1989`,
    Percent_of_Total_1989,
    Total_2024 = `2024`,
    Percent_of_Total_2024,
    Pixel_Change,
    Contribution_Percent,
    Hectares_Change
  ) %>%
  arrange(desc(abs(Pixel_Change)))

# 8. Write simplified CSV
write_csv(df_final, "outputs/wilderness_aggregate_landcover_change.csv")




###########
########### Create table 3
###########
#Using the pixel frequency table, for each area, calculate pixel turnover between 1989 and 2024 for each NLCD classes,
#calculate total pixels changed for the wilderness area, and then rank and calculate %change.

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

#--------------------------------------------------
# 1. Read data
#--------------------------------------------------
df_original <- read_csv("wilderness_nlcd_pixel_frequency.csv")

wilderness_col <- names(df_original)[1]

#--------------------------------------------------
# 2. Pivot to long format
#--------------------------------------------------
df_long <- df_original %>%
  pivot_longer(
    cols = -all_of(wilderness_col),
    names_to = c("Year", "LandCover"),
    names_pattern = "(\\d{4})_(.*)",
    values_to = "Pixels"
  )

#--------------------------------------------------
# 3. Pivot wider so 1989 and 2024 are columns
#--------------------------------------------------
df_wide <- df_long %>%
  pivot_wider(
    names_from = Year,
    values_from = Pixels
  )

#--------------------------------------------------
# 4. Calculate class-level change
#--------------------------------------------------
df_change <- df_wide %>%
  mutate(
    Change = `2024` - `1989`,
    AbsChange = abs(Change)
  )

#--------------------------------------------------
# 5. Wilderness-level totals + contribution
#--------------------------------------------------
df_change <- df_change %>%
  group_by(across(all_of(wilderness_col))) %>%
  mutate(
    Total_Changed_Pixels = sum(AbsChange, na.rm = TRUE),
    Total_1989 = sum(`1989`, na.rm = TRUE),
    Percent_Change = (Total_Changed_Pixels / Total_1989) * 100,
    Contribution_Percent = (AbsChange / Total_Changed_Pixels) * 100
  ) %>%
  ungroup()

#--------------------------------------------------
# 6. Wilderness-level ranking summary
#--------------------------------------------------
df_summary <- df_change %>%
  select(all_of(wilderness_col),
         Total_Changed_Pixels,
         Total_1989,
         Percent_Change) %>%
  distinct() %>%
  mutate(
    Rank_Most_Change = rank(-Total_Changed_Pixels, ties.method = "min"),
    Rank_Percent_Change = rank(-Percent_Change, ties.method = "min")
  )

#--------------------------------------------------
# 7. Pivot class-level metrics back to wide format
#--------------------------------------------------
df_class_wide <- df_change %>%
  select(all_of(wilderness_col),
         LandCover,
         Change,
         Contribution_Percent) %>%
  pivot_wider(
    names_from = LandCover,
    values_from = c(Change, Contribution_Percent),
    names_sep = "_"
  )

#--------------------------------------------------
# 8. Join everything together
#--------------------------------------------------
df_final <- df_original %>%
  left_join(df_summary, by = wilderness_col) %>%
  left_join(df_class_wide, by = wilderness_col)

#--------------------------------------------------
# 9. Reorder columns (class grouped, summary at end)
#--------------------------------------------------

# Get original 1989 columns in order
cols_1989 <- grep("^1989_", names(df_original), value = TRUE)

ordered_cols <- c(wilderness_col)

for (col in cols_1989) {
  
  lc_name <- sub("^1989_", "", col)
  
  col_2024 <- paste0("2024_", lc_name)
  col_change <- paste0("Change_", lc_name)
  col_contrib <- paste0("Contribution_Percent_", lc_name)
  
  ordered_cols <- c(
    ordered_cols,
    col,
    col_2024,
    col_change,
    col_contrib
  )
}

summary_cols <- c(
  "Total_Changed_Pixels",
  "Total_1989",
  "Percent_Change",
  "Rank_Most_Change",
  "Rank_Percent_Change"
)

ordered_cols <- c(ordered_cols, summary_cols)

df_final <- df_final[, ordered_cols]

#--------------------------------------------------
# 10. Write output
#--------------------------------------------------
write_csv(df_final, "outputs/wilderness_change_ranked_change.csv")



###########

#now run a loop that calculates landscape metrics on the same 1989/2024 wilderness folders
#saves outputs in a metric folder.






















###################
###################
### Level 2 ecoregion analysis

library(sf)
library(terra)
library(stringr)

wilderness <- st_make_valid(wilderness)
l2ecoreg  <- st_make_valid(l2ecoreg)

# Intersect once
wilderness_intersect <- st_intersection(
  wilderness,
  l2ecoreg[, "NA_L2NAME"]
)

# Split into list by ecoregion name
wilderness_list <- split(
  wilderness_intersect,
  wilderness_intersect$NA_L2NAME
)

base_dir <- "outputs/output_ecoregions"
dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)

terraOptions(progress = 1, memfrac = 0.8)

for (i in seq_len(nrow(l2ecoreg))) {
  
  eco <- l2ecoreg[i, ]
  eco_name_raw <- as.character(eco$NA_L2NAME)
  
  if (is.na(eco_name_raw) || eco_name_raw == "") next
  
  eco_name <- str_replace_all(eco_name_raw, "[^[:alnum:]]", "_")
  eco_dir  <- file.path(base_dir, eco_name)
  dir.create(eco_dir, showWarnings = FALSE)
  
  # Pull pre-split wilderness
  wilderness_eco <- wilderness_list[[eco_name_raw]]
  
  if (is.null(wilderness_eco) || nrow(wilderness_eco) == 0) next
  
  # Optional but recommended: dissolve for faster masking
  wilderness_eco <- st_union(wilderness_eco)
  
  eco_vect <- terra::vect(eco)
  wilderness_vect <- terra::vect(wilderness_eco)
  
  # Crop raster to ecoregion first (major speed gain)
  nlcd89_eco <- crop(nlcd1989, eco_vect)
  nlcd24_eco <- crop(nlcd2024, eco_vect)
  
  # Mask small cropped raster
  writeRaster(
    mask(nlcd89_eco, wilderness_vect),
    filename = file.path(eco_dir, "wilderness_ecoregion_1989.tif"),
    overwrite = TRUE
  )
  
  writeRaster(
    mask(nlcd24_eco, wilderness_vect),
    filename = file.path(eco_dir, "wilderness_ecoregion_2024.tif"),
    overwrite = TRUE
  )
  
  cat("Finished:", eco_name, "\n")
}