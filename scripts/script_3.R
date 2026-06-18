#Jason Gregg
#Marck 2026





library(terra)
library(sf)
library(dplyr)
library(data.table)

setwd("bioe515_wilderness_change/")


###Step 1


####Load all your spatial data


#contiguous US boundary
##conus <- st_read("data/conus/conus_polygon.shp")

#lower 48 states boundaries
lower48 <- vect("data/states/usa_lower48.shp")

#level 2 ecoregion boundaries, clipped by conus
l2ecoreg <- vect("data/ecoregions_l2/conus_clipped_l2_ecoregion/clipped_l2_ecoreg.shp")

#pre-processed wilderness area boundaries from Script 1
wilderness <- vect("data/wilderness_areas/wilderness_final_aggregated_2.shp")

#pre-processed NLCD data from Script 1
nlcd1989 <- rast("data/NLCD1989/NLCD1989_processed.tif")
nlcd2024 <- rast("data/NLCD2024/NLCD2024_processed.tif")




###Step 2

#match CRS of shapefiles to your NLCD CRS (WGS84)
l2ecoreg <- project(l2ecoreg, crs(nlcd1989))
##conus <- st_transform(conus, crs(nlcd1989))
lower48 <- project(lower48, crs(nlcd2024))
wilderness <- project(wilderness, crs(nlcd1989))

##conus <- st_union(conus)      # merges all polygons into one geometry
#conus <- st_sf(geometry = conus) 
#conus$cat <- 1

# Suppose conus_single is your dissolved SpatVector or converted sf object
#conus$ID <- 1 


##Having an issue where the non-integer ID values in the L2 ecoregion field aren't working
#so I will translate them into integers with the following:


##################################################

#turn shapefiles into rasters
#rasterize

#conus_r <- rasterize(conus, nlcd2024, field = "cat")  # use your categorical field

# rasterize, keeping the decimal places in the L2 categories

#For ecoregion, because NA_L2CODE is not an integer, add a new ID column
l2ecoreg$NA_L2CODE_int <- as.integer(as.numeric(l2ecoreg$NA_L2CODE) * 10)
table(l2ecoreg$NA_L2CODE)
table(l2ecoreg$NA_L2CODE_int)

#rasterize
l2ecoreg_r <- rasterize(l2ecoreg, nlcd2024, field = "NA_L2CODE_int")
terra::writeRaster(l2ecoreg_r, "l2ecoregion_raster.tiff")

#convert GEOID to integer, then rasterize
# convert
lower48$GEOID <- as.integer(as.character(lower48$GEOID))

# verify
head(as.data.frame(lower48)$GEOID, 50)  # should show 35, 46, 6, 21, 1, 13
# rasterize
lower48_r <- rasterize(lower48, nlcd2024, field = "GEOID")
#write raster
terra::writeRaster(lower48_r, "data/raster_stack/lower48_raster.tiff", overwrite=TRUE)
  


#rasterize wilderness using nlcd data as a template
class(wilderness$WID)
table(wilderness$WID)
wilderness_r <- rasterize(wilderness, nlcd2024, field = "WID")
terra::writeRaster(wilderness_r, "data/raster_stack/wilderness_raster.tiff")

#try another one that uses touches = true
wilderness_r_touches <- rasterize(wilderness, nlcd2024, field = "WID", touches = TRUE)
terra::writeRaster(wilderness_r_touches, "data/raster_stack/wilderness_raster_touches.tiff")


#try a combined approach that includes percentage of pixel, then mask 
# create a third raster (warning, takes a long time)
wilderness_r_50.cover <- rasterize(wilderness, nlcd2024, cover = TRUE)

 # keep only cells with >50% coverage
wilderness_mask <- wilderness_cover
wilderness_mask[wilderness_mask < 0.5] <- NA

# apply to NLCD
nlcd_masked <- mask(nlcd2024, wilderness_mask)






#####
###data checks

ext(nlcd1989)
ext(nlcd2024)
ext(wilderness_r)
ext(l2ecoreg_r)
ext(lower48_r)

###data checks

#are my rasters snapped to the same grid?
compareGeom(nlcd1989, nlcd2024, wilderness_r, l2ecoreg_r, lower48_r)


##check NA
global(wilderness_r, fun = "isNA")
global(l2ecoreg_r,   fun = "isNA")
global(lower48_r,    fun = "isNA")

# Pick a block to inspect
readStart(wilderness_r)
readStart(l2ecoreg_r)
readStart(lower48_r)

wld_b   <- readValues(wilderness_r, row = bs$row[1], nrows = bs$nrows[1])
eco_b   <- readValues(l2ecoreg_r,   row = bs$row[1], nrows = bs$nrows[1])
state_b <- readValues(lower48_r,    row = bs$row[1], nrows = bs$nrows[1])

readStop(wilderness_r)
readStop(l2ecoreg_r)
readStop(lower48_r)

# How many wilderness pixels exist?
sum(!is.na(wld_b) & wld_b != 0)

# How many survive each additional filter?
sum(!is.na(wld_b) & wld_b != 0 & !is.na(eco_b))
sum(!is.na(wld_b) & wld_b != 0 & !is.na(state_b))





#data check
##########
##you are dropping pixels when you go from one raster to the other.
##try to visualize gaps between ecoregion and NLCD data

# Create an output raster to write gaps into
# Create output raster template and open for writing
gaps_r <- rast(l2ecoreg_r)
outfile <- writeStart(gaps_r, "ecoregion_gaps.tif", overwrite = TRUE)

readStart(nlcd1989)
readStart(l2ecoreg_r)

for (i in seq_len(bs$n)) {
  nlcd_b <- readValues(nlcd1989,   row = bs$row[i], nrows = bs$nrows[i])
  eco_b  <- readValues(l2ecoreg_r, row = bs$row[i], nrows = bs$nrows[i])
  
  gap_b <- ifelse(!is.na(nlcd_b) & is.na(eco_b), 1, NA)
  
  writeValues(gaps_r, gap_b, start = bs$row[i], nrows = bs$nrows[i])
}

readStop(nlcd1989)
readStop(l2ecoreg_r)
writeStop(gaps_r)

#this plot shows areas that have values for NLCD data but are NAs in ecoregion.
#it shows large gaps along the coast, great lakes, florida keys. mostly areas with water.

#not actually that useful
plot(rast("ecoregion_gaps.tif"))





###this code should ID gaps that have values in the wilderness raster, but not on the ecoreg.
###this is more important because it shows you whats being left out of the ecoregion-scale analysis


gaps_r <- rast(l2ecoreg_r)
writeStart(gaps_r, "ecoregion_gaps_wilderness.tif", overwrite = TRUE)

readStart(wilderness_r)
readStart(l2ecoreg_r)

for (i in seq_len(bs$n)) {
  wld_b <- readValues(wilderness_r, row = bs$row[i], nrows = bs$nrows[i])
  eco_b <- readValues(l2ecoreg_r,   row = bs$row[i], nrows = bs$nrows[i])
  
  gap_b <- ifelse(!is.na(wld_b) & wld_b != 0 & is.na(eco_b), 1, NA)
  
  writeValues(gaps_r, gap_b, start = bs$row[i], nrows = bs$nrows[i])
}

readStop(wilderness_r)
readStop(l2ecoreg_r)
writeStop(gaps_r)

plot(rast("ecoregion_gaps_wilderness.tif"))


gaps_r <- rast(l2ecoreg_r)
writeStart(gaps_r, "ecoregion_gaps_wilderness.tif", overwrite = TRUE)

readStart(wilderness_r)
readStart(l2ecoreg_r)

for (i in seq_len(bs$n)) {
  wld_b <- readValues(wilderness_r, row = bs$row[i], nrows = bs$nrows[i])
  eco_b <- readValues(l2ecoreg_r,   row = bs$row[i], nrows = bs$nrows[i])
  
  gap_b <- ifelse(!is.na(wld_b) & wld_b != 0 & is.na(eco_b), 1, NA)
  
  writeValues(gaps_r, gap_b, start = bs$row[i], nrows = bs$nrows[i])
}

readStop(wilderness_r)
readStop(l2ecoreg_r)
writeStop(gaps_r)

plot(rast("ecoregion_gaps_wilderness.tif"))






#check what classes are dropped between eco regio and wilderness NLCD data

readStart(wilderness_r)
readStart(l2ecoreg_r)
readStart(nlcd1989)

dropped_classes <- c()

for (i in seq_len(bs$n)) {
  wld_b  <- readValues(wilderness_r, row = bs$row[i], nrows = bs$nrows[i])
  eco_b  <- readValues(l2ecoreg_r,   row = bs$row[i], nrows = bs$nrows[i])
  nlcd_b <- readValues(nlcd1989,     row = bs$row[i], nrows = bs$nrows[i])
  
  dropped <- nlcd_b[!is.na(wld_b) & wld_b != 0 & is.na(eco_b)]
  dropped_classes <- c(dropped_classes, dropped)
}

readStop(wilderness_r)
readStop(l2ecoreg_r)
readStop(nlcd1989)

table(dropped_classes)






##########
##########
#Block-based approach to analyze at the wilderness level, using standard wilderness raster
###########
###########


###doing this to compare with the mask + terra:freq approach of script 2

output_csv <- "nlcd_wilderness_pixel_counts.csv"


# -----------------------------------------------------------
# Block-based extraction — memory-safe for large rasters
# -----------------------------------------------------------
#set number of blocks to divide your raster into
bs <- blocks(nlcd1989, n = 10)  # increase n if RAM is tight

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

# -----------------------------------------------------------
# Save
# -----------------------------------------------------------
fwrite(all_counts, output_csv)
message("Done! Saved to: ", output_csv)
message("Rows: ", nrow(all_counts))
message("Wilderness areas found: ", uniqueN(all_counts$wilderness_id))


####



######
##########
#Block-based approach to analyze at the wilderness level, using wilderness raster touches = TRUE
###########
###########

wilderness_r_touches <- rast("data/raster_stack/wilderness_raster_touches.tiff")

###doing this to compare with the mask + terra:freq approach of script 2

output_csv <- "nlcd_wilderness_pixel_counts_touches.csv"

# -----------------------------------------------------------
# Block-based extraction — memory-safe for large rasters
# -----------------------------------------------------------
bs <- blocks(nlcd1989, n = 10)  # increase n if RAM is tight

message("Processing ", bs$n, " blocks...")

process_year <- function(nlcd_rast, year_label) {
  
  block_list <- vector("list", bs$n)
  
  readStart(nlcd_rast)
  readStart(wilderness_r_touches)
  
  for (i in seq_len(bs$n)) {
    
    nlcd_b <- readValues(nlcd_rast,    row = bs$row[i], nrows = bs$nrows[i])
    wld_b  <- readValues(wilderness_r_touches, row = bs$row[i], nrows = bs$nrows[i])
    
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
  readStop(wilderness_r_touches)
  
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

# -----------------------------------------------------------
# Save
# -----------------------------------------------------------
fwrite(all_counts, output_csv)
message("Done! Saved to: ", output_csv)
message("Rows: ", nrow(all_counts))
message("Wilderness areas found: ", uniqueN(all_counts$wilderness_id))













##########
##########
#Block-based approach to analyze at the eco region level, using touches raster
###########
###########

#load ecoregion raster
l2ecoreg_r <- rast("data/raster_stack/l2ecoregion_raster.tiff")

output_csv  <- "ecoreg_wilderness_touches_pixel_counts2.csv"

# -----------------------------------------------------------
# Block-based extraction — memory-safe for large rasters
# -----------------------------------------------------------
bs <- blocks(nlcd1989, n = 10)  # increase n if RAM is tight

message("Processing ", bs$n, " blocks...")

process_year <- function(nlcd_rast, year_label) {
  
  block_list <- vector("list", bs$n)
  
  readStart(nlcd_rast)
  readStart(wilderness_r_touches)
  readStart(l2ecoreg_r)
  
  for (i in seq_len(bs$n)) {
    
    nlcd_b <- readValues(nlcd_rast,   row = bs$row[i], nrows = bs$nrows[i])
    wld_b  <- readValues(wilderness_r_touches, row = bs$row[i], nrows = bs$nrows[i])
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
  readStop(wilderness_r_touches)
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

# -----------------------------------------------------------
# Save
# -----------------------------------------------------------
fwrite(all_counts, output_csv)
message("Done! Saved to: ", output_csv)
message("Rows: ", nrow(all_counts))
message("Ecoregions found: ", uniqueN(all_counts$ecoregion))








##########
##########
#Block-based approach to analyze at the state level, using wilderness touches raster
###########
###########

#load raster
lower48_r <- rast("data/raster_stack/lower48_raster.tiff")

output_csv <- "state_wilderness_touches_pixel_counts2.csv"
# -----------------------------------------------------------
# Block-based extraction — memory-safe for large rasters
# -----------------------------------------------------------
bs <- blocks(nlcd1989, n = 10)  # increase n if RAM is tight

message("Processing ", bs$n, " blocks...")

process_year <- function(nlcd_rast, year_label) {
  
  block_list <- vector("list", bs$n)
  
  readStart(nlcd_rast)
  readStart(wilderness_r_touches)
  readStart(lower48_r)
  
  for (i in seq_len(bs$n)) {
    
    nlcd_b   <- readValues(nlcd_rast,    row = bs$row[i], nrows = bs$nrows[i])
    wld_b    <- readValues(wilderness_r_touches, row = bs$row[i], nrows = bs$nrows[i])
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
  readStop(wilderness_r_touches)
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

# -----------------------------------------------------------
# Save
# -----------------------------------------------------------
fwrite(all_counts, output_csv)
message("Done! Saved to: ", output_csv)
message("Rows: ", nrow(all_counts))
message("States found: ", uniqueN(all_counts$GEOID))


















###########
###########
#crosstabs approach
##########
##########

# stack 1989, 2024, and wilderness areas
s <- c(nlcd1989, nlcd2024, wilderness_r, l2ecoreg_r, lower48_r)

#Now mask by wilderness_r
s_wild <- mask(s, wilderness_r)

#write this raster
writeRaster(s_wild, "stack_masked.tif", overwrite = TRUE)

#now try terra: crosstab
ct <- crosstab(s_wild)

saveRDS(ct, "raster_stack_crosstab.rds")


