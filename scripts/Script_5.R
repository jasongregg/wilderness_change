##Jason Gregg
##June 16th, 2026

###This is the final working script that loads, checks, and rasterizes spatial data and then uses a block-based approach
### and terra extract to conduct pixel counts for 1989 and 2024 at the wilderness, state, and ecoregion scales.

### Resulting CSV files for state, ecoregion, and wilderness level NLCD change are then appended so they contain names, not just codes

#It includes all steps except some pre-processing of spatial data, including clipping ecoregion and wilderness areas
#and merging and cleaning wilderness areas with multiple polygons

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

wilderness_r <- rast("data/raster_stack/wilderness_raster_touches.tiff")

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
l2ecoreg_r <- rast("data/raster_stack/l2ecoregion_raster.tiff")

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





