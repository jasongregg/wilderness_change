#Feb 22, 2026

#this script is to filter out specific species from the Birds of the World GDB folder
#which is located on your external 'starling' drive


library(sf)
library(terra)

# Path to your GDB
gdb_path <- "/Volumes/starling/data/BOTW_2022_all_species/BOTW.gdb"

st_layers(gdb_path)

# The species you want
BNPP <- "Otidiphaps insularis"
NBBR <- "Henicophaps foersteri"

# Read only rows where sci_name matches
species_sf <- st_read(
  dsn = gdb_path,
  layer = "All_Species",
  query = paste0(
    "SELECT * FROM All_Species WHERE sci_name = '", BNPP, "'"
  )
)