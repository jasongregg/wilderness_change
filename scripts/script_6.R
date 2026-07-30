#Jason Gregg
#July 2026

#Script_6.R is for analysis and figuremaking based on previous scripts

#it uses csv files of pixel counts for two years nlcd data: 1989 and 2024 generated in Script_5.R

#First lets load the wilderness csv file and make tables and figures
# This includes pixel counts of nlcd classes for 1989 and 2024 for all wilderness areas to detect change

library(tidyverse)
library(terra)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)

setwd("bioe515_wilderness_change/")

#Load CSV files

wilderness_pixcounts <- read.csv("outputs/appended_csv/wilderness_append.csv")
l2ecoreg_pixcounts <- read.csv("outputs/appended_csv/l2ecoreg_append.csv")
state_pixcounts <- read.csv("outputs/appended_csv/state_append.csv")


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
    alpha = 0.9
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = nlcd_colors) +
  labs(
    x = NULL,
    y = "Percent Land Cover",
    fill = "NLCD Land Cover Classes"
  ) +
  theme_minimal()



###Table 1
####Now generate Table 1, showing each land cover class, their 1989 and 2024 pixel counts, pixel change
###and hectare lost or gained.

landcover_change_table <- wilderness_pixcounts %>%
  group_by(nlcd_class_name, year) %>%
  summarise(
    pixel_count = sum(pixel_count),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = pixel_count,
    names_prefix = "pixels_"
  ) %>%
  mutate(
    pixel_change = pixels_2024 - pixels_1989,
    hectare_change = pixel_change * 0.09
  ) %>%
  arrange(desc(pixels_1989))

landcover_change_table


#########Table 2

###Based on the wilderness_pixcounts csv, Table 2 ranks the wilderness areas 
#that have experienced the most pixel turnover and creates a simple bar chart

