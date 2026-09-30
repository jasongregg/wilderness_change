#Jason Gregg
#July 2026

#script_figures.R is for analysis and figuremaking based on scripts 1 and 2

#it uses csv files of pixel counts for two years nlcd data: 1989 and 2024 generated in Script_5.R

#First lets load the wilderness csv file and make tables and figures
# This includes pixel counts of nlcd classes for 1989 and 2024 for all wilderness areas to detect change

library(tidyverse)
library(terra)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggnewscale)
library(forcats)
library(scales)
library(sf)
library(gt)

setwd("bioe515_wilderness_change/")

#Load CSV files

wilderness_pixcounts <- read.csv("outputs/appended_csv/wilderness_append.csv")
l2ecoreg_pixcounts <- read.csv("outputs/appended_csv/l2ecoreg_append.csv")
state_pixcounts <- read.csv("outputs/appended_csv/state_append.csv")
transitions <- read.csv("outputs/appended_csv/wilderness_transition_details.csv")

#Load shapefile data for maps
l2eco <- st_read("data/ecoregions_l2/conus_clipped_l2_ecoregion/clipped_l2_ecoreg.shp")
wilderness <- st_read("data/wilderness_areas/wilderness_final_aggregated_2.shp")
states <- st_read("data/states/usa_lower48.shp")

#Figure 1, stacked bar chart for 1989 and 2025

#This figure sums pixel counts across all wildernesses to show diference in top ten
#most common land cover classes across years in the form of bar charts.

###colors for top ten classes.

nlcd_colors <- c(
  "Shrub/Scrub" = "#a68c30",
  "Evergreen Forest" = "#1c5f2c",
  "Deciduous Forest" = "#68ab5f",
  "Grassland/Herbaceous" = "#ccba7c",
  "Mixed Forest" = "#b5c58f",
  "Woody Wetlands" = "#87c0cd",
  "Barren Land (Rock/Sand/Clay)" = "#b3ac9f",
  "Emergent Herbaceous Wetlands" = "#abd9e9",
  "Open Water" = "#466b9f"
)

# Top 10 classes based on total pixels across both years
top_classes <- wilderness_pixcounts %>%
  group_by(nlcd_class_name) %>%
  summarise(total_pixels = sum(pixel_count), .groups = "drop") %>%
  slice_max(total_pixels, n = 10) %>%
  pull(nlcd_class_name)

# Aggregate pixels first, then calculate percentages
plot_data <- wilderness_pixcounts %>%
  filter(nlcd_class_name %in% top_classes) %>%
  group_by(year, nlcd_class_name) %>%
  summarise(
    pixel_count = sum(pixel_count),
    .groups = "drop"
  ) %>%
  group_by(year) %>%
  mutate(percent = pixel_count / sum(pixel_count)) %>%
  ungroup()

# Order stack from largest (bottom) to smallest (top)
class_order <- plot_data %>%
  group_by(nlcd_class_name) %>%
  summarise(total_percent = sum(percent), .groups = "drop") %>%
  arrange(desc(total_percent)) %>%
  pull(nlcd_class_name)

plot_data <- plot_data %>%
  mutate(
    nlcd_class_name = factor(
      nlcd_class_name,
      levels = rev(class_order)
    )
  )

# Plot
ggplot(plot_data,
       aes(x = factor(year),
           y = percent,
           fill = nlcd_class_name)) +
  geom_col(
    width = 0.7,
    color = "black",
    linewidth = 0.2,
    alpha = 0.8
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = nlcd_colors) +
  labs(
    title = "Wilderness Area Land Cover Composition, 1989 vs 2024",
    x = NULL,
    y = "Percent Land Cover",
    fill = "NLCD Land Cover Class"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"
    )
  )

#### save plot to new outputs folder

ggsave(
  filename = "outputs/figures/land_cover_barplot_redo.png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)


###Table 1
####Now generate Table 1, showing each land cover class, their 1989 and 2024 pixel counts, pixel change
###and hectare lost or gained.
landcover_change_table <- wilderness_pixcounts %>%
  group_by(nlcd_class_name, year) %>%
  summarise(
    pixel_count = sum(pixel_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    area_ha = pixel_count * 0.09
  ) %>%
  select(
    nlcd_class_name,
    year,
    area_ha
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = area_ha,
    names_prefix = "ha_",
    values_fill = 0
  ) %>%
  mutate(
    `Net change (ha)` = ha_2024 - ha_1989
  ) %>%
  select(
    nlcd_class_name,
    `1989 (ha)` = ha_1989,
    `2024 (ha)` = ha_2024,
    `Net change (ha)`
  ) %>%
  arrange(`Net change (ha)`)

landcover_change_table

#write table
landcover_change_table %>%
  write_csv(
    "outputs/figures/table_wilderness_landcover_change.csv"
  )


#publication version of table.
landcover_change <- read_csv(
  "outputs/figures/table_wilderness_landcover_change.csv"
)

landcover_publication_table <- landcover_change %>%
  gt() %>%
  tab_header(
    title = "Land Cover Change within Wilderness Areas",
    subtitle = "1989–2024"
  ) %>%
  fmt_number(
    columns = c(
      `1989 (ha)`,
      `2024 (ha)`,
      `Net change (ha)`
    ),
    decimals = 1,
    use_seps = TRUE
  ) %>%
  cols_label(
    nlcd_class_name = "Land cover",
    `1989 (ha)` = "1989 (ha)",
    `2024 (ha)` = "2024 (ha)",
    `Net change (ha)` = "Net change (ha)"
  ) %>%
  cols_align(
    align = "center",
    columns = c(
      `1989 (ha)`,
      `2024 (ha)`,
      `Net change (ha)`
    )
  ) %>%
  cols_align(
    align = "left",
    columns = nlcd_class_name
  ) %>%
  tab_options(
    table.font.size = px(9),
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2)
  )

landcover_publication_table

# Save publication table

gtsave(
  landcover_publication_table,
  "outputs/figures/table_wilderness_landcover_change.docx"
)

#########Table 2
#this table will present the ecoregion data.
#we want rows of ecoregions, followed by total wilderness number in that ecoregion, hectares, and then cover types with plus and minus

# Calculate cumulative pixel count for each ecoregion
ecoregion_totals <- l2ecoreg_pixcounts %>%
  group_by(NA_L2CODE_int, NA_L2NAME) %>%
  summarise(
    cumulative_pixel_count = sum(pixel_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(cumulative_pixel_count)) %>%
  mutate(
    rank = row_number()
  )

# Calculate pixel change from 1989 to 2024 for each NLCD class
nlcd_change <- l2ecoreg_pixcounts %>%
  filter(year %in% c(1989, 2024)) %>%
  select(
    NA_L2CODE_int,
    NA_L2NAME,
    nlcd_class,
    nlcd_class_name,
    year,
    pixel_count
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = pixel_count
  ) %>%
  mutate(
    pixel_change = `2024` - `1989`
  ) %>%
  select(
    NA_L2CODE_int,
    NA_L2NAME,
    nlcd_class_name,
    pixel_change
  ) %>%
  pivot_wider(
    names_from = nlcd_class_name,
    values_from = pixel_change
  )

# Combine the ranking with the NLCD change table
final_table <- ecoregion_totals %>%
  left_join(
    nlcd_change,
    by = c("NA_L2CODE_int", "NA_L2NAME")
  ) %>%
  select(
    rank,
    NA_L2CODE_int,
    NA_L2NAME,
    cumulative_pixel_count,
    everything()
  )

# View the result
final_table


################Table 3
###########Simple table that shows each wilderness, its hectares, and calculates % forest loss and gain based on the three forest categories in hectares

wilderness_pixcounts <- read.csv("outputs/appended_csv/wilderness_append.csv")

# Three forest classes
forest_classes <- c(
  "Deciduous Forest",
  "Mixed Forest",
  "Evergreen Forest"
)

# NLCD 30 m pixel = 0.09 hectares
pixel_area_ha <- 30 * 30 / 10000

# Complete list of all wilderness areas
all_wilderness <- wilderness_pixcounts %>%
  distinct(NAME_ABBRE)

# Calculate forest area for wildernesses with forest pixels
full_forest_table <- wilderness_pixcounts %>%
  filter(nlcd_class_name %in% forest_classes) %>%
  group_by(NAME_ABBRE, year) %>%
  summarise(
    forest_pixels = sum(pixel_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    forest_ha = forest_pixels * pixel_area_ha
  ) %>%
  select(NAME_ABBRE, year, forest_ha) %>%
  pivot_wider(
    names_from = year,
    values_from = forest_ha,
    names_prefix = "forest_",
    values_fill = 0
  )

# Add forest results to ALL wilderness areas
full_forest_table <- all_wilderness %>%
  left_join(full_forest_table, by = "NAME_ABBRE") %>%
  mutate(
    forest_1989 = replace_na(forest_1989, 0),
    forest_2024 = replace_na(forest_2024, 0),
    net_change_ha = forest_2024 - forest_1989
  ) %>%
  select(
    Wilderness = NAME_ABBRE,
    `Forest 1989 (ha)` = forest_1989,
    `Forest 2024 (ha)` = forest_2024,
    `Net Change in Forest Cover (ha)` = net_change_ha
  )

# Save the analysis dataset
write_csv(
  forest_table,
  "outputs/figures/full_table_wilderness_forest_change.csv"
)



#Now create pub version of the table
# Read the already-calculated analysis table
forest_change <- read_csv(
  "outputs/figures/table_wilderness_forest_change.csv"
)

# Create publication table
publication_table <- forest_change %>%
  gt() %>%
  tab_header(
    title = "Forest Cover Change within Wilderness Areas",
    subtitle = "1989–2024"
  ) %>%
  fmt_number(
    columns = c(
      `Forest 1989 (ha)`,
      `Forest 2024 (ha)`,
      `Net Change in Forest Cover (ha)`
    ),
    decimals = 1,
    use_seps = TRUE
  ) %>%
  cols_label(
    Wilderness = "Wilderness",
    `Forest 1989 (ha)` = "1989 (ha)",
    `Forest 2024 (ha)` = "2024 (ha)",
    `Net Change in Forest Cover (ha)` = "Net change (ha)"
  ) %>%
  cols_align(
    align = "center",
    columns = c(
      `Forest 1989 (ha)`,
      `Forest 2024 (ha)`,
      `Net Change in Forest Cover (ha)`
    )
  ) %>%
  cols_align(
    align = "left",
    columns = Wilderness
  ) %>%
  tab_source_note(
    source_note = md(
      "**Note:** Forest cover represents the combined area of 
      deciduous, mixed, and evergreen forest classes. 
      Net change is calculated as 2024 forest cover minus 
      1989 forest cover. Areas are calculated from 30-m × 30-m pixels."
    )
  ) %>%
  tab_options(
    table.font.size = px(11),
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2)
  )

publication_table

#save it 
gtsave(
  publication_table,
  "outputs/figures/table_wilderness_forest_change.docx"
)

#######now do the same type of table for ecoregion and states, including the number of wildernesses in each, 
#and the amount of forest loss.
#ecoregion pix counts

#use your matched centroids csv

####states

# Three forest classes
forest_classes <- c(
  "Deciduous Forest",
  "Mixed Forest",
  "Evergreen Forest"
)

# NLCD 30 m pixel = 0.09 hectares
pixel_area_ha <- 30 * 30 / 10000


# FOREST change BY STATE
state_forest <- state_pixcounts %>%
  filter(
    year %in% c(1989, 2024),
    Class %in% forest_classes
  ) %>%
  group_by(NAME, year) %>%
  summarise(
    forest_pixels = sum(pixel_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    forest_ha = forest_pixels * pixel_area_ha
  ) %>%
  select(NAME, year, forest_ha) %>%
  pivot_wider(
    names_from = year,
    values_from = forest_ha,
    names_prefix = "forest_",
    values_fill = 0
  ) %>%
  mutate(
    net_forest_change_ha = forest_2024 - forest_1989
  )

# 2. TOTAL WILDERNESS AREA BY STATE
#    Using 2024 and ALL NLCD classes pixel counts

state_wilderness_area <- state_pixcounts %>%
  filter(year == 2024) %>%
  group_by(NAME) %>%
  summarise(
    wilderness_pixels = sum(pixel_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    wilderness_ha = wilderness_pixels * pixel_area_ha
  ) %>%
  select(NAME, wilderness_ha)


# 3. COUNT WILDERNESSES BY STATE
#    matched_centroids$STATE uses two-letter codes.
#    e.g. A wilderness with STATE = "AZ/CA" is counted once in Arizona and once in California.


state_wilderness_count <- matched_centroids %>%
  st_drop_geometry() %>%
  select(
    wilderness = NAME,
    STATE
  ) %>%
  separate_rows(STATE, sep = "/") %>%
  mutate(
    STATE = str_trim(STATE)
  ) %>%
  group_by(STATE) %>%
  summarise(
    wilderness_count = n_distinct(wilderness),
    .groups = "drop"
  ) %>%
  rename(
    state_code = STATE
  ) %>%
  left_join(
    tibble(
      state_code = state.abb,
      state = state.name
    ),
    by = "state_code"
  ) %>%
  select(
    state,
    wilderness_count
  )

# 4. CREATE LIST OF ALL 50 STATES
#    This ensures states with no wilderness forest pixels are still included.

all_states <- tibble(
  state = state.name
)


# 5. COMBINE EVERYTHING INTO FINAL STATE TABLE

state_summary <- all_states %>%
  left_join(
    state_forest %>%
      rename(state = NAME),
    by = "state"
  ) %>%
  left_join(
    state_wilderness_area %>%
      rename(state = NAME),
    by = "state"
  ) %>%
  left_join(
    state_wilderness_count,
    by = "state"
  ) %>%
  mutate(
    forest_1989 = replace_na(forest_1989, 0),
    forest_2024 = replace_na(forest_2024, 0),
    net_forest_change_ha = replace_na(net_forest_change_ha, 0),
    wilderness_ha = replace_na(wilderness_ha, 0),
    wilderness_count = replace_na(wilderness_count, 0)
  ) %>%
  select(
    state,
    wilderness_ha,
    wilderness_count,
    forest_1989,
    forest_2024,
    net_forest_change_ha
  ) %>%
  arrange(state)

#remove non-conus states, Alaska and Hawaii

state_summary

#Make table and save it

# STATE: Save analysis table as a csv

# ---------------------------------------------------------
# STATE: Save analysis table
# Ordered from largest forest loss to largest forest gain
# ---------------------------------------------------------
# ---------------------------------------------------------
# STATE: Save analysis table
# Excludes Alaska and Hawaii
# Ordered from largest forest loss to largest forest gain
# ---------------------------------------------------------

state_summary %>%
  filter(!state %in% c("Alaska", "Hawaii")) %>%
  select(
    state,
    wilderness_count,
    wilderness_ha,
    forest_1989,
    forest_2024,
    net_forest_change_ha
  ) %>%
  arrange(net_forest_change_ha) %>%
  write_csv(
    "outputs/figures/table_state_wilderness_forest_change.csv"
  )


# ---------------------------------------------------------
# STATE: Create publication version
# ---------------------------------------------------------

state_forest_change <- read_csv(
  "outputs/figures/table_state_wilderness_forest_change.csv"
)

state_publication_table <- state_forest_change %>%
  gt() %>%
  tab_header(
    title = "Forest Cover Change within Wilderness Areas by State",
    subtitle = "1989–2024"
  ) %>%
  fmt_number(
    columns = wilderness_count,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = c(
      wilderness_ha,
      forest_1989,
      forest_2024,
      net_forest_change_ha
    ),
    decimals = 1,
    use_seps = TRUE
  ) %>%
  cols_label(
    state = "State",
    wilderness_count = "# Wildernesses",
    wilderness_ha = "Wilderness area (ha)",
    forest_1989 = "1989 (ha)",
    forest_2024 = "2024 (ha)",
    net_forest_change_ha = "Net change (ha)"
  ) %>%
  cols_align(
    align = "center",
    columns = c(
      wilderness_count,
      wilderness_ha,
      forest_1989,
      forest_2024,
      net_forest_change_ha
    )
  ) %>%
  cols_align(
    align = "left",
    columns = state
  ) %>%
  tab_options(
    table.font.size = px(9),
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2)
  )

state_publication_table


# Save publication table
gtsave(
  state_publication_table,
  "outputs/figures/table_state_wilderness_forest_change.docx"
)


# ECOREGION-LEVEL TABLE

# Forest area by ecoregion and year
ecoregion_forest <- l2ecoreg_pixcounts %>%
  filter(
    year %in% c(1989, 2024),
    nlcd_class_name %in% forest_classes
  ) %>%
  group_by(NA_L2NAME, year) %>%
  summarise(
    forest_pixels = sum(pixel_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    forest_ha = forest_pixels * pixel_area_ha
  ) %>%
  select(NA_L2NAME, year, forest_ha) %>%
  pivot_wider(
    names_from = year,
    values_from = forest_ha,
    names_prefix = "forest_",
    values_fill = 0
  ) %>%
  mutate(
    net_forest_change_ha = forest_2024 - forest_1989
  )


# Total wilderness area by ecoregion
# Using 2024 and ALL NLCD classes
ecoregion_wilderness_area <- l2ecoreg_pixcounts %>%
  filter(year == 2024) %>%
  group_by(NA_L2NAME) %>%
  summarise(
    wilderness_pixels = sum(pixel_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    wilderness_ha = wilderness_pixels * pixel_area_ha
  ) %>%
  select(NA_L2NAME, wilderness_ha)


# Count wildernesses by ecoregion
ecoregion_wilderness_count <- matched_centroids %>%
  st_drop_geometry() %>%
  select(
    wilderness = NAME,
    NA_L2NAME
  ) %>%
  filter(!is.na(NA_L2NAME)) %>%
  group_by(NA_L2NAME) %>%
  summarise(
    wilderness_count = n_distinct(wilderness),
    .groups = "drop"
  )


# Final ecoregion table
ecoregion_summary <- ecoregion_forest %>%
  left_join(
    ecoregion_wilderness_area,
    by = "NA_L2NAME"
  ) %>%
  left_join(
    ecoregion_wilderness_count,
    by = "NA_L2NAME"
  ) %>%
  mutate(
    forest_1989 = replace_na(forest_1989, 0),
    forest_2024 = replace_na(forest_2024, 0),
    net_forest_change_ha = replace_na(net_forest_change_ha, 0),
    wilderness_ha = replace_na(wilderness_ha, 0),
    wilderness_count = replace_na(wilderness_count, 0)
  ) %>%
  select(
    ecoregion = NA_L2NAME,
    wilderness_ha,
    wilderness_count,
    forest_1989,
    forest_2024,
    net_forest_change_ha
  ) %>%
  arrange(ecoregion)


# View final table
ecoregion_summary

# ECOREGION: Save analysis table
# Ordered from largest forest loss to largest forest gain


ecoregion_summary %>%
  select(
    ecoregion,
    wilderness_count,
    wilderness_ha,
    forest_1989,
    forest_2024,
    net_forest_change_ha
  ) %>%
  arrange(net_forest_change_ha) %>%
  write_csv(
    "outputs/figures/table_ecoregion_wilderness_forest_change.csv"
  )

# ECOREGION forest loss table: Create publication version

ecoregion_forest_change <- read_csv(
  "outputs/figures/table_ecoregion_wilderness_forest_change.csv"
)

ecoregion_publication_table <- ecoregion_forest_change %>%
  gt() %>%
  tab_header(
    title = "Forest Cover Change within Wilderness Areas by Ecoregion",
    subtitle = "1989–2024"
  ) %>%
  fmt_number(
    columns = wilderness_count,
    decimals = 0,
    use_seps = TRUE
  ) %>%
  fmt_number(
    columns = c(
      wilderness_ha,
      forest_1989,
      forest_2024,
      net_forest_change_ha
    ),
    decimals = 1,
    use_seps = TRUE
  ) %>%
  cols_label(
    ecoregion = "Level II Ecoregion",
    wilderness_count = "# Wildernesses",
    wilderness_ha = "Wilderness area (ha)",
    forest_1989 = "1989 (ha)",
    forest_2024 = "2024 (ha)",
    net_forest_change_ha = "Net change (ha)"
  ) %>%
  cols_align(
    align = "center",
    columns = c(
      wilderness_count,
      wilderness_ha,
      forest_1989,
      forest_2024,
      net_forest_change_ha
    )
  ) %>%
  cols_align(
    align = "left",
    columns = ecoregion
  ) %>%
  tab_options(
    table.font.size = px(9),
    column_labels.font.weight = "bold",
    table.border.top.style = "solid",
    table.border.top.width = px(2),
    table.border.bottom.style = "solid",
    table.border.bottom.width = px(2)
  )

ecoregion_publication_table

# Save publication table
gtsave(
  ecoregion_publication_table,
  "outputs/figures/table_ecoregion_wilderness_forest_change.docx"
)



######Ecoregion map figure
##this figure will show CONUS with L2 ecoregion boundaries.
#For each ecoregion we'll show associated ten land categories that have changed.
  

#make centroids of the wilderness areas.
wilderness_centroids <- st_centroid(wilderness)

eco_simple <- st_simplify(
  l2eco,
  dTolerance = 1000
)



####make plot
# Join forest change data to centroids

# --------------------------------------------------
# Join forest change data to wilderness centroids
# --------------------------------------------------

wilderness_centroids_map <- wilderness_centroids %>%
  mutate(Wilderness = NAME_ABBRE) %>%
  left_join(
    full_forest_table,
    by = "Wilderness"
  ) %>%
  mutate(
    forest_change = `Net Change in Forest Cover (ha)`,
    abs_change = abs(forest_change)
  )


# --------------------------------------------------
# MAP

ggplot() +
  
  # ------------------------------------------------
# Ecoregions
# ------------------------------------------------
geom_sf(
  data = eco_simple,
  aes(fill = NA_L2CODE),
  alpha = 0.4
) +
  
  scale_fill_discrete(
    name = "Level 2 Ecoregions",
    breaks = eco_simple$NA_L2CODE,
    labels = eco_simple$NA_L2NAME,
    guide = guide_legend(
      ncol = 1,
      byrow = TRUE,
      order = 1
    )
  ) +
  
  # ------------------------------------------------
# Forest LOSS
# darker yellow -> orange -> red
# ------------------------------------------------
new_scale_fill() +
  
  geom_sf(
    data = wilderness_centroids_map %>%
      filter(forest_change < 0),
    aes(
      fill = abs(forest_change),
      size = abs_change
    ),
    shape = 21,
    color = "black",
    alpha = 1,
    stroke = 0.4
  ) +
  
  scale_fill_gradientn(
    name = "Forest loss (ha)",
    colours = c("#FFD700", "orange", "red"),
    labels = scales::label_comma(),
    guide = guide_colorbar(order = 2)
  ) +
  
  # ------------------------------------------------
# Forest GAIN
# light green -> green -> dark green
# ------------------------------------------------
new_scale_fill() +
  
  geom_sf(
    data = wilderness_centroids_map %>%
      filter(forest_change > 0),
    aes(
      fill = forest_change,
      size = abs_change
    ),
    shape = 21,
    color = "black",
    alpha = 1,
    stroke = 0.4
  ) +
  
  scale_fill_gradientn(
    name = "Forest gain (ha)",
    colours = c(
      "lightgreen",
      "green3",
      "darkgreen"
    ),
    guide = guide_colorbar(order = 3)
  ) +
  
  # ------------------------------------------------
# ZERO CHANGE / NO FOREST
# black
# ------------------------------------------------
geom_sf(
  data = wilderness_centroids_map %>%
    filter(forest_change == 0),
  aes(size = abs_change),
  shape = 21,
  fill = "black",
  color = "black",
  alpha = 1,
  stroke = 0.4
) +
  
  # ------------------------------------------------
# Point size
# ------------------------------------------------
scale_size_continuous(
  name = "Magnitude of change (ha)",
  range = c(0.8, 5),
  guide = guide_legend(order = 4)
) +
  
  # ------------------------------------------------
# Theme
# ------------------------------------------------
theme_void() +
  
  theme(
    legend.position = "right",
    legend.box = "horizontal",
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 7, face = "bold"),
    legend.key.size = unit(0.4, "cm"),
    legend.spacing.x = unit(0.4, "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0)
  )




#save
ggsave(
  filename = "outputs/figures/l2ecoregion_wilderness.centroids.png",
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)

###now make a table that lists wilderness areas per ecoregion, and forest loss.

# Assign each wilderness centroid to its ecoregion
wilderness_by_eco <- st_join(
  wilderness_centroids,
  l2eco,
  join = st_intersects,
  left = TRUE
)

# Count wilderness areas in each ecoregion
wilderness_count <- wilderness_by_eco |>
  st_drop_geometry() |>
  group_by(NA_L2NAME) |>
  summarise(
    `# Wilderness Areas` = n()
  ) |>
  arrange(NA_L2NAME)

#table
wilderness_count

#this worked but leaves me with 14 NAs
#########Check out these unassigned wilderness areas
wilderness_by_eco |>
  filter(is.na(NA_L2NAME)) |>
  st_drop_geometry() |>
  select(NAME, STATE)
##diagnose issue
problem_centroids <- wilderness_by_eco |>
  filter(is.na(NA_L2NAME))


problem_centroids |>
  st_drop_geometry() |>
  select(NAME, STATE)

#now try looking at florida, as an example:
fl_problem <- problem_centroids |>
  filter(STATE == "FL")

ggplot() +
  geom_sf(
    data = l2eco,
    fill = "grey90",
    color = "grey40"
  ) +
  geom_sf(
    data = fl_problem,
    color = "red",
    size = 3
  ) +
  geom_sf_text(
    data = fl_problem,
    aes(label = NAME),
    size = 3,
    nudge_y = 20000
  ) +
  coord_sf(
    xlim = st_bbox(fl_problem)[c("xmin", "xmax")] + c(-100000, 100000),
    ylim = st_bbox(fl_problem)[c("ymin", "ymax")] + c(-100000, 100000)
  ) +
  theme_void()

  
#michigan
mi_problem <- problem_centroids |>
  filter(STATE == "MI")

ggplot() +
  geom_sf(data = l2eco, fill = "grey90", color = "grey40") +
  geom_sf(data = mi_problem, color = "red", size = 3) +
  geom_sf_text(data = mi_problem, aes(label = NAME), size = 3, nudge_y = 20000) +
  coord_sf(
    xlim = st_bbox(mi_problem)[c("xmin", "xmax")] + c(-100000, 100000),
    ylim = st_bbox(mi_problem)[c("ymin", "ymax")] + c(-100000, 100000)
  ) +
  theme_void()

#california
ca_problem <- problem_centroids |>
  filter(STATE == "CA")

ggplot() +
  geom_sf(data = l2eco, fill = "grey90", color = "grey40") +
  geom_sf(data = ca_problem, color = "red", size = 3) +
  geom_sf_text(data = ca_problem, aes(label = NAME), size = 3, nudge_y = 20000) +
  coord_sf(
    xlim = st_bbox(ca_problem)[c("xmin", "xmax")] + c(-100000, 100000),
    ylim = st_bbox(ca_problem)[c("ymin", "ymax")] + c(-100000, 100000)
  ) +
  theme_void()

######
######
##For the states investigated, these are all coastal areas. I will assign these wilderness areas
##to the nearest L2 Ecoregion leaving me with 0 NAs.

# Find nearest ecoregion for each unmatched centroid
nearest_eco <- st_nearest_feature(
  problem_centroids,
  l2eco
)

# Add the nearest ecoregion information
problem_centroids_nearest <- problem_centroids |>
  mutate(
    NA_L2CODE = l2eco$NA_L2CODE[nearest_eco],
    NA_L2NAME = l2eco$NA_L2NAME[nearest_eco]
  )

matched_centroids <- wilderness_by_eco |>
  filter(!is.na(NA_L2NAME))

all_wilderness_eco <- bind_rows(
  matched_centroids,
  problem_centroids_nearest
)

#redo table
wilderness_count <- all_wilderness_eco |>
  st_drop_geometry() |>
  count(NA_L2NAME, name = "# Wilderness Areas") |>
  arrange(NA_L2NAME)

wilderness_count



#######
#######
####### Calculations
#######
#######

#This section is for making Results calculations based on the tables generated above.

#1) what percent of wilderness areas (by number) experienced forest loss?

#2) what percent of all wilderness forest loss was in the top ten states? How many hectares was it combined?

#3) what 
