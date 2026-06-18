#Jason Gregg
#Feb 20, 2026

#Script_1 does various spatial data preperation and cleaning.


#National and state boundaries downloaded from:

#Wilderness boundaries downloaded from: https://wilderness.net/visit-wilderness/gis-gps.php

#level 2 ecoregion data downloaded from: https://www.epa.gov/eco-research/ecoregions-north-america

#1989 and 2024 NLCD data for lower 48 states downloaded from: https://www.mrlc.gov/data

library(terra)
library(raster)
library(tidyverse)
library(sf)
library(landscapemetrics)
library(mapview)
library(lubridate)
library(dplyr)
library(units)
library(rmapshaper)

setwd("/Users/jj/bioe515_wilderness_change")


##Load NLCD data
nlcd1989 <- rast("data/NLCD1989/Annual_NLCD_LndCov_1989_CU_C1V1.tif")
nlcd2024 <- rast("data/NLCD2024/Annual_NLCD_LndCov_2024_CU_C1V1.tif")


# These next steps are Travis's steps for cleaning/re-doing the NLCD class information (from lab 8)
levels(nlcd1989) <- NULL
levels(nlcd2024) <- NULL

# drop color tables from both layers
coltab(nlcd1989)  <- NULL   
coltab(nlcd2024)  <- NULL   

# create a new table called "nlcd_classes" based on the nlcd_colors()
nlcd_classes <- FedData::nlcd_colors() %>% dplyr::select(ID, Class)

# assign the levels of the NLCD data using the classes we just made
levels(nlcd1989) <- nlcd_classes
levels(nlcd2024) <- nlcd_classes

# assign the coltab (color table) using the nlcd_color() function
coltab(nlcd1989) <- with(FedData::nlcd_colors(), data.frame(value = ID, 
                                                            R = col2rgb(Color)[1,],
                                                            G = col2rgb(Color)[2,],
                                                            B = col2rgb(Color)[3,]))

coltab(nlcd2024) <- with(FedData::nlcd_colors(), data.frame(value = ID, 
                                                            R = col2rgb(Color)[1,],
                                                            G = col2rgb(Color)[2,],
                                                            B = col2rgb(Color)[3,]))

## Save these processed rasters so that you can avoid this step

writeRaster(nlcd1989,
            filename = "data/NLCD1989/NLCD1989_processed.tif",
            overwrite = TRUE)

writeRaster(nlcd2024,
            filename = "data/NLCD2024/NLCD2024_processed.tif",
            overwrite = TRUE)



#wilderness boundary
wilderness_areas <- sf::st_read("data/wilderness.areas/WildernessAreas2024.shp")

#US National boundary
usa <- sf::st_read("data/cb_2024_us_nation_5m/cb_2024_us_nation_5m.shp")

#state boundaries
states <- sf::st_read("data/cb_2024_us_state_500k/cb_2024_us_state_500k.shp")


#filter out non-lower 48 states
lower48 <- states[!states$NAME %in% c(
  "Alaska", 
  "Hawaii", 
  "Commonwealth of the Northern Mariana Islands", 
  "United States Virgin Islands", 
  "Puerto Rico", 
  "American Samoa", 
  "Guam"
), ]


#save this new shapefile.
st_write(lower48, "data/usa_lower48.shp", overwrite = TRUE)

#read in lower48 states only
lower48 <- sf::st_read("data/usa_lower48.shp")
mapview(lower48)

#clip conusa by lower 48

us_geom <- st_geometry(usa)

#Union the lower 48 states into one polygon
lower48_union <- st_union(lower48)
lower48_union <- st_as_sf(st_sfc(lower48_union, crs = st_crs(lower48)))
mapview(lower48_union)

# 4. Clip the national boundary using that union
lower48_boundary <- st_intersection(us_geom, lower48_union)

# 5 Save to a new file
st_write(lower48_boundary, "conus.shp")
conus <- sf::st_read("conus.shp")
mapview(lower48PRJ)


####### Clip the wilderness areas shapefile by your conus boundary so you only have lower 48 wilderness areas.
crs(wilderness.areas)
crs(conusPRJ)
mapview(conusPRJ)

lower48PRJ <- st_transform(lower48PRJ, crs(wilderness.areas))

wilderness_lower48 <- st_filter(wilderness.areas, lower48PRJ)
mapview(wilderness_lower48)

st_write(wilderness_lower48, "data/conus.wilderness.areas.shp")

# Do the coordinate reference systems match?
crs(conus) == crs(lower48)

# Do they match with wilderness areas shapefile?
crs(wilderness_areas) == crs(conus)
crs(wilderness_areas)
crs(conus)

# transform (project) the NWPS data so that it matches the crs of the NLCD
conusPRJ <- st_transform(conus, crs(nlcd1989))
crs(conusPRJ)

lower48PRJ <- st_transform(lower48, crs(nlcd1989))
crs(lower48PRJ)


#####################
#####################
####The following code combines polygons for any wilderness areas that have > 1 polygon (multiple mgmt agencies)

#First lets look at some examples of multi-polygon wilderness areas to see whats going on

wilderness.areas <- sf::st_read("data/wilderness.areas/conus.wilderness.areas.shp")

# remove some columns from wilderness.areas attribute table in the hopes it runs faster
wilderness.areas_edit <- wilderness.areas[, !(names(wilderness.areas) %in% c("URL", "Descriptio", "Image Path", "Creator", "Editor", "ImagePath"))]

#reproject to WGS84
wilderness.areas_edit <- st_transform(wilderness.areas_edit, crs(nlcd1989))

###Try counting names in your wilderness.areas shapefile
name_summary <- wilderness.areas_edit %>%
  st_drop_geometry() %>%            # remove geometry for faster processing
  group_by(NAME_ABBRE) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print(name_summary)

# counting how many wilderness areas were designated before (and including) 1989, and 1990-present

str(wilderness.areas_edit$Designated)
head(wilderness.areas_edit$Designated)

summary_designated <- wilderness.areas_edit %>%
  st_drop_geometry() %>%
  summarise(
    total = n(),
    after_1989 = sum(Designated > 1989, na.rm = TRUE),
    before_or_1989 = sum(Designated <= 1989, na.rm = TRUE)
  )

print(summary_designated)

#### try plotting a few of the polygons with multiple rows:

Domeland <- wilderness.areas_edit %>%
  filter(NAME_ABBRE == "Domeland")    # <-- change this to the name you want

# Plot it and save
png("outputs/Domeland.original.png", width = 6, height = 4, units = "in", res = 300)
plot(st_geometry(Domeland))
dev.off()

# Try aggregating it and seeing what it looks like
wilderness.aggregated <- aggregate(
  wilderness.areas_edit,
  by = list(wilderness.areas_edit$NAME_ABBRE),
  FUN = length
)

#This got rid of all the information in my columns. Probably dont need anyway, but going to try a second approach:

# Count how many times each NAME_ABBRE appears
name_counts <- wilderness.areas_edit %>%
  st_drop_geometry() %>%
  count(NAME_ABBRE)

# Identify duplicates (appear more than once)
dup_names <- name_counts %>% filter(n > 1) %>% pull(NAME_ABBRE)

# Split data
dupes <- wilderness.areas_edit %>% filter(NAME_ABBRE %in% dup_names)
uniques <- wilderness.areas_edit %>% filter(!NAME_ABBRE %in% dup_names)

#work on the duplicates, column by column
library(sf)
library(dplyr)

# aggregate, with rules for how each column is dealt with:
##'dupes' contains the polygons to aggregate
aggregated_dupes <- dupes %>%
  group_by(NAME_ABBRE) %>%
  summarise(
    NAME        = first(NAME),
    STATE       = first(STATE),
    Comment     = first(Comment),
    WID         = first(WID),
    dateLastMo  = first(dateLastMo),
    CreationDa  = first(CreationDa),
    EditDate    = first(EditDate),
    Agency      = paste(unique(Agency), collapse = ", "),
    joinerID    = paste(unique(joinerID), collapse = ", "),
    Designated  = paste(unique(Designated), collapse = ", "),
    GlobalID    = paste(unique(GlobalID), collapse = ", "),
    Acreage     = sum(Acreage, na.rm = TRUE),
    Shape__Are   = sum(Shape__Are, na.rm = TRUE), # optional, can recalc later
    Shape__Len   = sum(Shape__Len, na.rm = TRUE), # optional, can recalc later
    geometry    = st_union(geometry),           # combine polygons
    .groups = "drop"
  )

#recalculate the Shape__Are and Shape__Len columns based on new geometry
aggregated_dupes <- aggregated_dupes %>%
  mutate(
    Shape__Are = st_area(geometry),
    Shape__Len = st_length(geometry)
  )

#need to fix the 'Designated' column
uniques <- uniques %>%
  mutate(Designated = as.character(Designated))


# Convert the units column to plain numeric (e.g., square meters)
aggregated_dupes <- aggregated_dupes %>%
  mutate(
    Shape__Are = as.numeric(Shape__Are),
    Shape__Len = as.numeric(Shape__Len)
  )

#now recombine with your wilderness areas that just had one polygon per wilderness
wilderness_final <- bind_rows(uniques, aggregated_dupes)

# looks good, now try replotting Domeland and seeing how it compares:


Domeland_aggregated <- wilderness_final %>%
  filter(NAME_ABBRE == "Domeland")

# Plot it and save to share with Travis
png("outputs/Domeland.combined.png", width = 6, height = 4, units = "in", res = 300)
plot(st_geometry(Domeland_aggregated))
dev.off()

#save the new wilderness file with aggregated wildernesses
st_write(wilderness_final, "data/wilderness.areas/wilderness_final_aggregated.shp", delete_dsn = TRUE)

wilderness_final <- st_read("data/wilderness.areas/wilderness_final_aggregated.shp")

plot(wilderness_final)
mapview(wilderness_final)

### NEW TASK
###The aggregate function worked but there are some remnant vectors within the polygons. 
##Try cleaning those up. I'll start with a seperate object for one wilderness 
#then move on to working directly with the final wilderness file.

Domeland_aggregated <- wilderness_final %>%
  filter(NAME_ABBRE == "Domeland")
plot(Domeland_aggregated)

# Simplify polygon to remove unnecessary vertices along former boundaries
domeland_simplified <- st_simplify(Domeland_aggregated, dTolerance = 5)  # adjust tolerance if needed


# Plot
plot(domeland_simplified, border = "black")

#try something else...
Domeland_buffered <- st_buffer(Domeland_aggregated, dist = 1)
Domeland_buffered_revert <- st_buffer(Domeland_aggregated, dist = -1)

plot(Domeland_buffered, col = NA)
plot(Domeland_aggregated)
plot(Domeland_buffered_revert)

#check out the areas of your altered plots, to see whether function is messing with polygon
area_original <- st_area(Domeland_aggregated)
area_buffered <- st_area(Domeland_buffered)
area_dissolved <- st_area(Domeland_dissolved)

data.frame(
  Version = c("Original", "Buffered (dist=1)", "Disolved"),
  Area = c(area_original, area_buffered, area_dissolved))

Domeland_dissolved <- ms_dissolve(Domeland_aggregated)
plot(Domeland_dissolved, col = NA)

#It looks like the rsmapper dissolve function changed the area by 32 m sq. minor. Good option

#Now lets try and use rsmapper on all the 37 aggregated wilderness areas and then save the new shapefile.

target_ids <- c("Agua_Tibia", "Ansel_Adams", "Bighorn_Mountain", "Devils_Staircase", "Domeland", "Eldorado", "Frank_Church_River_of_No_Return", "Hells_Canyon_IDOR", "Indian_Peaks", "Inyo_Mountains", "Ireteba_Peaks", "Ishi", "Jim_McClure_Jerry_Peak", "Kanab_Creek", "Kiavah", "La_Madre_Mountain", "Lee_Metcalf", "Lower_White_River", "Machesna_Mountain", "Mount_Massive", "Mt_Charleston", "Mt_Moriah", "Muddy_Mountains", "Powderhorn", "Rainbow_Mountain", "San_Gorgonio", "Sangre_de_Cristo", "Santa_Lucia", "Santa_Rosa", "Spirit_Mountain", "Uncompahgre", "Ventana", "White_Clouds", "White_Mountains", "Wild_Rogue", "Yolla_Bolly_Middle_Eel", "Yuki")

# Loop through each target polygon and dissolve
dissolved_list <- lapply(target_ids, function(name) {
  single_poly <- wilderness_final %>% filter(NAME_ABBRE == name)
  
  # Use ms_dissolve (or st_union for single polygons with slivers)
  dissolved <- ms_dissolve(single_poly)
  
  # Return a data frame with NAME_ABBRE and geometry
  data.frame(NAME_ABBRE = name, geometry = st_geometry(dissolved))
})

dissolved_sf <- do.call(rbind, lapply(dissolved_list, function(x) st_sf(x, sf_column_name = "geometry", crs = st_crs(wilderness_final))))

mapview(dissolved_sf)

dissolve_targets <- wilderness_final %>%
  filter(NAME_ABBRE %in% target_ids)

# Plot interactively using mapview
mapview(dissolve_targets)

###overall the dissolve worked. Now I need to get the new geometries in my 'dissolved_sf' 
##back onto the original shapefile

for (i in seq_len(nrow(dissolved_sf))) {
  name <- dissolved_sf$NAME_ABBRE[i]
  wilderness_final$geometry[wilderness_final$NAME_ABBRE == name] <- st_geometry(dissolved_sf)[i]
}

# Check that the replacement worked
subset_check <- wilderness_final %>% filter(NAME_ABBRE %in% target_ids)

library(mapview)
mapview(subset_check, zcol = "NAME_ABBRE")

##seemed to work. Now resave your wilderness_final shapefile
st_write(wilderness_final, "data/wilderness.areas/wilderness_final_aggregated_2.shp", delete_dsn = TRUE)
wilderness_final_2 <- st_read("data/wilderness.areas/wilderness_final_aggregated_2.shp")

mapview(wilderness_final_2)

#looks good.








##############
##############
###Feb 23, 2026

##Below I try to get spatial data in shape for analyzing wilderness NLCD data at the ecoregion and state level



##load my files

#lower 48 states boundaries
lower48 <- st_read("data/states/usa_lower48.shp")

#level 2 ecoregion boundaries (unedited)
l2ecoreg <- st_read("data/ecoregions_l2/NA_CEC_Eco_Level2.shp")

#pre-processed wilderness area boundaries from Script 1
wilderness <- st_read("data/wilderness_areas/wilderness_final_aggregated_2.shp")

#pre-processed NLCD data from Script 1
nlcd1989 <- rast("data/NLCD1989/NLCD1989_processed.tif")
nlcd2024 <- rast("data/NLCD2024/NLCD2024_processed.tif")



## Step 1
##Turning my conus multistring shapefile into a polygon so I can use it with sf::intersect
#### load conus file

#contiguous US boundary
conus <- st_read("data/conus/conus.shp")


#match CRS of shapefiles to your NLCD CRS (WGS84)
l2ecoreg <- st_transform(l2ecoreg, crs(nlcd1989))
conus.poly <- st_transform(conus.poly, crs(nlcd1989))
lower48 <- st_transform(lower48, crs(nlcd1989))
wilderness <- st_transform(wilderness, crs(nlcd1989))

# 1. Merge all lines into a single geometry (optional but safer)
lines_merged <- st_union(conus)

# 2. Convert merged lines to polygons
polygons <- st_polygonize(lines_merged)

# 3. Turn into an sf object
polygons_sf <- st_sf(geometry = polygons)

# 4. Extract only POLYGON or MULTIPOLYGON geometries (remove GEOMETRYCOLLECTION)
polygons_sf <- st_collection_extract(polygons_sf, "POLYGON")

# 5. Ensure geometries are valid
polygons_sf <- st_make_valid(polygons_sf)

# 6. Now polygons_sf is ready for intersections or writing
# Example: write to shapefile
st_write(polygons_sf, "conus_polygon.shp")

################

###########

#####

#prepping state and ecoregion rasters using clip

conus.poly <- st_read("data/conus/conus_polygon.shp")



######## OK now try intersect tool. 

clipped <- st_intersection(l2ecoreg, conus.poly)

st_write(clipped, "clipped_polygons.shp")

conus_eco.regions <- clipped

##############

#The next step is to now try and mask/clip the wilderness areas within each ecoregion

#I'll try saving each unique ecoregion as its own polygon and working from there.

# Keep only the name field and geometry
conus_eco_clean <- conus_eco.regions[, c("NA_L2NAME")]

# Optional: check
head(conus_eco_clean)

# Get unique values in the NA_L2NAME field
unique_values <- unique(conus_eco.regions$NA_L2NAME)

# Print them
print(unique_values)

#21 ecoregions in the lower48

output_folder <- "outputs/ecoregion_split"
dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)

unique_ecos <- unique(conus_eco_clean$NA_L2NAME)

for (eco_name in unique_ecos) {
  # Clean file name
  clean_name <- gsub("[^[:alnum:]_]", "_", eco_name)
  clean_name <- substr(clean_name, 1, 50)  # truncate long names if needed
  
  # Subset polygons
  sub_shp <- conus_eco_clean[conus_eco_clean$NA_L2NAME == eco_name, ]
  
  # Write shapefile
  st_write(sub_shp, file.path(output_folder, paste0(clean_name, ".shp")), delete_dsn = TRUE)
}


