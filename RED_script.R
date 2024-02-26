# ==================================== #
# Regional Economic Disprity in the EU #
# ==================================== #

# Pre-Settings -----
rm(list=ls())
wd <- "~/Documents/Research/2023-Brief-Regional Economic Disparity"
input <- paste0(wd, "/raw_data/")
output <- paste0(wd, "/new_data/")
plots <- paste0(wd, "/plots/")
tables <- paste0(wd, "/tables/")
setwd(wd)

# R-Libraries -----
library(sf)
library(giscoR)
library(ggplot2)
library(eurostat)
library(tidyverse)
library(lubridate)

# I. Section - Data =====
# 1. Subsection - GDP Datasets -----
# ----- Search for datasets with * in their title
# eurostat_datasets <- subset(eurostat::get_eurostat_toc(), grepl('GDP', title))
# Get data for GDP at all NUTS level and transform it
GDP_data <- get_eurostat("nama_10r_3gdp") %>%
  filter(unit %in% c("MIO_EUR", "EUR_HAB")) %>%
  pivot_wider(names_from = unit, values_from = values) %>%
  mutate(time = year(as.Date(time))) %>%
  arrange(geo, desc(time)) %>%
  group_by(geo) %>% filter(!is.na(MIO_EUR) & !is.na(EUR_HAB)) %>% slice(1) %>% ungroup() #latest available data

# write_csv(GDP, paste0(output, "GDP_data.csv"))
# GDP <- read_csv(paste0(output, "GDP_data.csv"), col_names = TRUE)

# 2. Subsection - Shapefiles -----
# Get shapes and filter it
NUTS_lines <- gisco_get_nuts(
  year = "2016",
  epsg = "3035", #for lines
  resolution = "10", #high resolution "03" / could be "10"
  nuts_level = "all")

NUTS_lines <- NUTS_lines %>%
  select(-c(MOUNT_TYPE, URBN_TYPE, FID, NUTS_NAME, COAST_TYPE)) %>%
  filter(!CNTR_CODE %in% c("TR", "CY")) %>% #filter out Cyprus and Turkey
  filter(!NUTS_ID %in% c("PT200", "PT300", "ES703", "ES704", "ES705", "ES706", "ES707", "ES708","ES709", "FRY10", "FRY20", "FRY30", "FRY40", "FRY50",
                         "PT20", "PT30", "ES70", "FRY1", "FRY2", "FRY3", "FRY4", "FRY5",
                         "PT2", "PT3", "ES7", "FRY")) #filter out overseas territories

# Merge data
NUTS_sf <- NUTS_lines %>% left_join(GDP_data, by = c("NUTS_ID" = "geo"))

# Group by NUTS by country and convert to lines
CNTR_lines <- NUTS_lines %>%
  filter(LEVL_CODE == 3) %>%
  group_by(CNTR_CODE) %>%
  summarise(n = n()) %>%
  st_cast("MULTILINESTRING")

# Calculate the counts for each combination of LEVL_CODE and CNTR_CODE
counted_codes <- st_drop_geometry(NUTS_lines) %>%
  group_by(LEVL_CODE, CNTR_CODE) %>%
  summarize(code_count = n(), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = LEVL_CODE, values_from = code_count, names_prefix = "n_nuts")

unique_counts <- st_drop_geometry(NUTS_sf) %>%
  group_by(LEVL_CODE, CNTR_CODE) %>%
  summarise(unique_count = n_distinct(na.omit(EUR_HAB)), .groups = "drop") %>%
  ungroup() %>%
  pivot_wider(names_from = LEVL_CODE, values_from = unique_count, names_prefix = "u_nuts")

CNTR_lines <- CNTR_lines %>%
  left_join(counted_codes, by = "CNTR_CODE") %>%
  left_join(unique_counts, by = "CNTR_CODE")

rm(counted_codes, unique_counts)

# 3. Subsection - Coordinates -----
NUTS_coord <- gisco_get_nuts(
  year = "2016",
  epsg = "4326", #for coordinates
  resolution = "10",
  nuts_level = "all")



# II. Section - Maps =====
# Palette
palettes <- list()
for (i in 1:8) {
  palettes[[i]] <- hcl.colors(i, palette = "ag_GrnYl", rev = TRUE)
}

# Labels
units <- c("EUR_HAB", "MIO_EUR") #"EUR_HAB"
title <- c("Euro per Inhabitants", "Million Euro") #"Euro per inhabitants"
countries <- as.character(CNTR_lines$CNTR_CODE)
countries <- countries[!(countries %in% c("AL", "CH", "IS", "LI", "LU", "ME", "MT", "NO", "RS", "UK"))]

# European Union -----
for (l in 1:3) for (i in 1:length(units)) {
  
  # Find a natural interval with quantile breaks
  quantile_values <- quantile(filter(NUTS_sf, LEVL_CODE == l)[[units[i]]], #FC
                              probs = seq(0, 1, by = 1/8), na.rm = TRUE)
  label <- as.character(prettyNum(round(quantile_values[-1], -2), big.mark = ","))
  
  # Now 'cuts' contains the factor with class intervals
  NUTS_sf <- NUTS_sf %>%
    mutate(values_cut = if_else(LEVL_CODE == l, #FC
                                cut(NUTS_sf[[units[i]]], quantile_values, dig.lab = 5), #FC
                                NA))
  
  # Map plot
  ggplot(filter(NUTS_sf, LEVL_CODE == l)) + #FC
    geom_sf(aes(fill = values_cut), linewidth = 0, color = NA, alpha = 0.9) +
    geom_sf(data = CNTR_lines, col = "black", linewidth = 0.1) +
    scale_fill_manual(
      name = title[i], #FC
      values = palettes[[8]],
      labels = label,
      drop = FALSE,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = 0.5,
        keywidth = 3.5,
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = .5,
        nrow = 1,
        byrow = TRUE,
        reverse = FALSE,
        label.position = "bottom")) +
    theme_void() +
    theme(
      legend.text = element_text(size = 10, color = "grey20"),
      legend.title = element_text(size = 12, color = "grey20"),
      legend.position = "bottom",
      panel.border = element_blank(),
      plot.background = element_rect(fill = "#FFF9F5", color = NA)
    )

  ggsave(paste0("EU_NUTS", l, "_", title[i], ".pdf"), plot = last_plot(), path = plots, width = 184, height = 220, units = "mm", dpi = 300)
}

# Member States ----- 
for (c in 1:length(countries)) for (l in 1:3) for (i in 1:length(units)) {
  
  #c <- 18
  #l <- 2
  #i <- 2
  
  # Find territories number on specific NUTS level in specific Country
  t <- as.integer(st_drop_geometry(CNTR_lines[which(CNTR_lines$CNTR_CODE == countries[c]), paste0("u_nuts", l)]))
  
  # If only one then skip
  if (t < 2) {
    next
  }
  
  if (t > 8) {
    t <- 8
  }

  # Find a natural interval with quantile breaks
  #quantile_values <- quantile(filter(NUTS_sf, LEVL_CODE == l, CNTR_CODE == countries[c])[[units[i]]], #FC
  #                            probs = seq(0, 1, by = 1 / t), na.rm = TRUE, names = FALSE)
  
  
  quantile_values <- classIntervals(unique(na.omit(filter(NUTS_sf, LEVL_CODE == l, CNTR_CODE == countries[c])[[units[i]]])), #FC
                                    n = t, style = "quantile", warnSmallN = FALSE)$brks
  
  # 
  quantile_values[1] <- min(na.omit(filter(NUTS_sf, LEVL_CODE == l, CNTR_CODE == countries[c])[[units[i]]])) - 1
  quantile_values[t + 1] <- max(na.omit(filter(NUTS_sf, LEVL_CODE == l, CNTR_CODE == countries[c])[[units[i]]]))
  
  label <- as.character(prettyNum(round(quantile_values[-1], -2), big.mark = ","))
  
  # Now 'cuts' contains the factor with class intervals
  NUTS_sf <- NUTS_sf %>%
    mutate(values_cut = if_else(LEVL_CODE == l & CNTR_CODE == countries[c], #FC
                                cut(NUTS_sf[[units[i]]], quantile_values, dig.lab = 5), #FC
                                NA))
  
  # Map plot
  ggplot(filter(NUTS_sf, LEVL_CODE == l, CNTR_CODE == countries[c])) + #FC
    geom_sf(aes(fill = values_cut), linewidth = 0, color = NA, alpha = 0.9) +
    geom_sf(data = filter(CNTR_lines, CNTR_CODE == countries[c]), col = "black", linewidth = 0.1) +
    scale_fill_manual(
      name = title[i], #FC
      values = palettes[[t]],
      labels = label,
      drop = FALSE,
      guide = guide_legend(
        direction = "horizontal",
        keyheight = 0.5,
        keywidth = 3.5,
        title.position = "top",
        title.hjust = 0.5,
        label.hjust = .5,
        nrow = 1,
        byrow = TRUE,
        reverse = FALSE,
        label.position = "bottom")) +
    theme_void() +
    theme(
      legend.text = element_text(size = 10, color = "grey20"),
      legend.title = element_text(size = 12, color = "grey20"),
      legend.position = "bottom",
      panel.border = element_blank(),
      plot.background = element_rect(fill = "#FFF9F5", color = NA)
    )
  
  ggsave(paste0(countries[c], "_NUTS", l, "_", title[i], ".pdf"), plot = last_plot(), path = plots, width = 184, height = 110, units = "mm", dpi = 300)
}

# III. Section - Data Analysis =====












