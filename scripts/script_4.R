#Jason Gregg 
#March 8th 2026


library(sf)
library(terra)
library(dplyr)
library(readr)


#script_4.R appends state, wilderness, and ecoregion names back onto 
#the generated CSV files, using the previously generated csv outputs so that they can be interpreted


#load csv files generated from block-based extraction (script 3)
#these were created with the wilderness_r_touches.tiff raster

wilderness <- read.csv("outputs/nlcd_wilderness_pixel_counts_touches.csv")

ecoreg <- read.csv("ecoreg_wilderness_touches_pixel_counts2.csv")

state <- read.csv("state_wilderness_touches_pixel_counts2.csv")

#re-append propper field IDs for wilderness areas, states, ecoregions

# states
head(lower48)

# convert shapefile GEOID to integer to match csv
shp_df$GEOID <- as.integer(shp_df$GEOID)

state <- state %>% left_join(shp_df, by = "GEOID")
#write the updated csv, with overwrite
write_csv(state, "state_update.csv")

#ecoregion l2
head(l2ecoreg)
names(l2ecoreg)
names(ecoreg)


l2ecoreg$NA_L2CODE <- as.integer(l2ecoreg$NA_L2CODE)
l2ecoreg_df <- as.data.frame(l2ecoreg)

df <- ecoreg %>% left_join(l2ecoreg_df[, c("NA_L2CODE", "NA_L2NAME")], by = c("ecoregion" = "NA_L2CODE"))

#wilderness ID
head(wilderness_r_touches)
print(wilderness_r_touches)
names(wilderness)

wilderness_final_aggregated_2 <- vect("data/wilderness_areas/wilderness_final_aggregated_2.shp")
print(wilderness_og)

wilderness_df <- as.data.frame(wilderness_final_aggregated_2) %>%
  distinct(WID, .keep_all = TRUE)

wilderness <- wilderness %>% left_join(wilderness_df[, c("WID", "NAME_ABBRE", "STATE", "Acreage")], by = c("wilderness_id" = "WID"))

write_csv(wilderness, "wilderness_update.csv")

#NLCD classes for all three CSVs

print(nlcd1989)

lookup <- read_csv("wilderness.csv")  # update filename

# Check the built-in categories
tif_cats <- cats(nlcd1989)[[1]]
print(tif_cats)

# Read your CSV and append the category names
wilderness <- merge(wilderness, tif_cats, by.x = "nlcd_class", by.y = "value")

# now for state
state <- merge(state, tif_cats, by.x = "nlcd_class", by.y = "value")

# now for ecoregion 
ecoreg <- merge(ecoreg, tif_cats, by.x = "nlcd_class", by.y = "value")


