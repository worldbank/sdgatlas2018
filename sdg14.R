library(ggplot2)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(dplyr)
library(readr)
library(readxl)
library(tidyr)
library(stringr)
library(rgdal)
library(forcats)
source("styles.R")

fig_sdg14_catches_fishing_sector <- function(years = 1950:2014) {
  df <- read.csv("inputs/sdg14/catches_fishing_sector.csv", stringsAsFactors = FALSE)
  
  df <- df %>%
    select(date = "year", sector = "fishing_sector", value = "tonnes") %>%
    mutate(sector = fct_recode(sector, Other = "Recreational", Other = "Subsistence", Other = "Artisanal")) %>%
    group_by(date, sector) %>% 
    summarise(value = sum(value)) %>%
    ungroup()
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      ggplot(df, aes(x = date, y = value, fill = fct_reorder2(sector, date, value))) +
        geom_area() +
        geom_text(
          aes(x = date, y = cumsum(value) - value, label = sector),
          data = . %>% filter(date == max(date)),
          hjust = 1,
          vjust = 0,
          family = style$family,
          size = style$gg_text_size,
          color = style$colors$text.inverse,
          nudge_x = -1,
          nudge_y = 7 * 1e6
        ) +
        scale_x_continuous(breaks = bracketed_breaks(limits = df$date, at = 10)) +
        scale_y_continuous(labels = millions()) +
        scale_fill_manual(
          values = c(
            "Industrial" = style$colors$spot.primary,
            "Other" = style$colors$spot.secondary
          )
        ) +
        style$theme()
    },
    aspect_ratio = 1.5,
    title = "And 75 percent of fishing is industrial.",
    subtitle = wbg_name(indicator = "Global fish catch", denom = "millions of metric tons"),
    note = "Note: \"Other\" includes subsistence, recreational and artisanal sectors.",
    source = "Source: Pauly and Zeller 2016. http://doi.org/10.1038/ncomms10244"
  )
}

fig_sdg14_marine_stock_status <- function() {
  df <- read_excel("inputs/sdg14/indicator_14_4_1.xlsx")

  df <- df %>%
    subset(`Series Code` != "ER_H2O_FWTL",
           select = grep("Series Description|Series Code|Country or Area Name|^\\d", names(df))) %>%
    gather("date", "value", -c("Series Code", "Country or Area Name", "Series Description")) %>%
    rename(indicator = `Series Code`, desc = `Series Description`)
  
  df <- df %>% mutate(date = as.numeric(date))
  
  figure(
    data = df[complete.cases(df), ],
    plot = function(df, style = style_atlas_open()) {
      df <- df %>% mutate(indicator = factor(indicator,rev(c("ER_H2O_FISHNFEXP","ER_H2O_FISHFEXP","ER_H2O_FISHOVEXP"))))
      ggplot(df, aes(x = date, y = value, fill = indicator)) +
        geom_area() +
        annotate("text", label = "Not fully exploited", x = min(df$date)+1, y = 15, hjust = 0, vjust = 0.5,
                 family = style$family, size = style$gg_text_size, color = style$colors$text.inverse) +
        annotate("text", label = "Fully exploited", x = mean(range(df$date)), y = 50, hjust = 0.5, vjust = 0.5,
                 family = style$family, size = style$gg_text_size, color = style$colors$text) +
        annotate("text", label = "Overexploited", x = max(df$date)-1, y = 85, hjust = 1, vjust = 0.5,
                 family = style$family, size = style$gg_text_size, color = style$colors$text.inverse) +
        scale_x_continuous(breaks = bracketed_breaks(limits = df$date)) +
        scale_fill_manual(
          values = c(
            ER_H2O_FISHFEXP = style$colors$spot.primary.light,
            ER_H2O_FISHNFEXP = style$colors$spot.secondary,
            ER_H2O_FISHOVEXP = style$colors$spot.primary
          ),
          labels = c(
            ER_H2O_FISHFEXP = "Fully exploited",
            ER_H2O_FISHNFEXP = "Not fully exploited",
            ER_H2O_FISHOVEXP = "Overexploited"
          )
        ) +
        style$theme()
   },
   aspect_ratio = 1.5,
   title = "Fish stocks are increasingly overfished.",
   subtitle = wbg_name(indicator = "State of global fish stocks", denom = "% of total stocks"),
   source = "Source: FAO via UNSD Global SDG Indicators Database (14.4.1)."
  )
}

fig_sdg14_dead_zones <- function() {
  df <- read_xlsx("inputs/sdg14/Hypoxic-Eutrophic Updated Oct 2017.xlsx")
  
  # The column names in the spreadsheet have some weird non printing spaces...
  colnames(df) <-  iconv(colnames(df), to = "ascii", sub = "")
  
  # Cast the coordinates
  df <- df %>% mutate(Long = as.numeric(Long), Lat = as.numeric(Lat))
  
  # Can't map the unmappable
  df <- df %>% filter(!is.na(Long) & !is.na(Lat))
  
  coords_wintri <- as.data.frame(rgdal::project(as.matrix(df[,c("Long", "Lat")]), "+proj=wintri +over"))
  df <- df %>%
    select(-Long, -Lat) %>%
    cbind(coords_wintri)
  
  maps <- wbgmaps$low
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), aspect_ratio = 2) {
      p <- ggplot(data = df) +
        geom_polygon(data = maps$countries, aes(long, lat, group = group), fill = "grey80") +
        geom_polygon(data = maps$disputed, aes(long, lat, group = group, map_id = id), fill = "grey80") +
        geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white") +
        geom_path(data = maps$boundaries, aes(long, lat, group = group), color = "white", size = 0.2, lineend = maps$boundaries$lineend, linetype = maps$boundaries$linetype) +
        geom_hex(
          aes(
            x = Long, y = Lat,
            size = ,
            fill = wbggeo::supercut(..count.., c(
              "0–4" = "[0, 4]",
              "5–9" = "[5, 9]",
              "10–29" = "[10, 29]",
              "30 and over" = "[30, Inf)"
          ))),
          color = "white",
          size = 0.1,
          bins = c(60, 60)
        ) +
        scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
        scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) +
        scale_fill_manual(palette = style$colors$continuous.primary.dark) +
        coord_equal() +
        style$theme() +
        style$theme_map(aspect_ratio)
    },
    aspect_ratio = 3,
    title = "Activity on land can also damage seas. Hundreds of marine dead zones exist, with oxygen concentrations too low to support most life.",
    subtitle = wbg_name(indicator = "Marine dead zones", year = 2017, denom = "count by hexagonal area"),
    source = "Source: Diaz and Rosenberg 2008. http://doi.org/10.1126/science.1156401. Current data at http://www.vims.edu/research/topics/dead_zones"
  )
}

fig_sdg14_marine_protected_areas_by_country <- function(
  year = 2016,
  countries = c("NCL", "PLW", "USA", "AUS", "MNP", "NZL", "FRA", "GBR", "ECU", "CHL")
) {
  # Unfortunately we don't have total protected area in WDI, but we want to use
  # this as a threshold to remove very small countries. It's not really easy to
  # replicate the methodology used by PP.net as some areas overlap and we don't
  # want to go full geospatial here. So this list has been hand-validated against
  # the website.
  # https://protectedplanet.net/c/calculating-protected-area-coverage#Method used to calculate protected area coverage
  
  indicator <- "ER.MRN.PTMR.ZS"
  df <- wbgdata(
    countries,
    indicator,
    years = year,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg14_marine_protected_areas_by_country.csv"
  )
  
  df <- df %>%
    arrange(-value) %>%
    mutate(iso3c <- fct_relevel(iso3c, iso3c))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      ggplot(df, aes(x=reorder(iso3c, -value), y=value)) +
        geom_col(fill = style$colors$spot.primary) +
        scale_x_discrete(labels = setNames(str_wrap(wbgref$countries$labels, width = 10), names(wbgref$countries$labels))) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
        style$theme()
    },
    #title = "Countries varies greatly in how much ocean they protect.",
    subtitle = wbg_name(indicator, by = "top 10", denom = "% of territorial waters"),
    note = paste0("Note: Excludes countries with less than 50,000 sq. km of protected area."),
    source = "Source: UNEP-World Conservation Monitoring Centre Database on Protected Areas. WDI (ER.MRN.PTMR.ZS) and https://protectedplanet.net"
  )
}

fig_sdg14_global_sea_surface_temperature <- function(years = 1900:2017, show.uncertainty = FALSE) {
  df <- read_csv("inputs/sdg14/sea-surface-temp_fig-1.csv", skip = 6)
  
  df <- df %>%
    rename(date = Year,
           actual_f = "Annual anomaly",
           lower_f = "Lower 95% confidence interval",
           upper_f = "Upper 95% confidence interval") %>%
    filter(date %in% years)
  
  # Convert delta-farenheit to delta-celsius
  df <- df %>% mutate(
    actual_c = 5/9 * actual_f,
    lower_c = 5/9 * lower_f,
    upper_c = 5/9 * upper_f
  )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      p <- ggplot(df, aes(date, actual_c)) +
        geom_hline(yintercept = 0, color = style$colors$baseline, size = style$theme()$line$size) +
        geom_line(color = style$colors$spot.primary, size = style$linesize) +
        scale_x_continuous(breaks = bracketed_breaks(limits = df$date), limits = range(years)+c(-3,3), expand = c(0, 0)) +
        scale_y_continuous(breaks = c(-0.5,0,0.5), labels = ones(always.signed = TRUE)) +
        annotate(
          "text", x = min(years)-3, y = 0.1, label = "1971–2000 average",
          family = style$family, color = style$colors$text, size = style$gg_text_size,
          hjust = 0, vjust = 0) +
        style$theme()
      if (show.uncertainty) {
        p <- p + geom_ribbon(aes(ymin = lower_c, ymax = upper_c), fill = style$colors$neutral)
      }
      p
    },
    aspect_ratio = 3,
    #title = "Global sea surface temperature rose through the 20th century, and continues to rise.",
    subtitle = wbg_name(indicator = "Average global sea surface temperature anomaly", by = "relative to 1971–2000 average", denom = "degrees Celsius"),
    source = "Source: U.S. Environmental Protection Agency. https://www.epa.gov/climate-indicators/climate-change-indicators-sea-surface-temperature"
  )
}

fig_sdg14_barrier_reef_temperature <- function(years = 1900:2017) {
  df <- read_table("inputs/sdg14/bom_gbr_temp_anomaly_latest.txt",
                   col_names = c("ymym", "anomaly_c"))
  
  # Extract year from the weird year-month-year-month date column
  df <- df %>% mutate(date = as.integer(substr(ymym, 1, 4)))
  
  df <- df %>% filter(date %in% years)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      p <- ggplot(df, aes(date, anomaly_c)) +
        geom_hline(yintercept = 0, color = style$colors$baseline, size = style$theme()$line$size) +
        geom_line(color = style$colors$spot.primary, size = style$linesize) +
        scale_x_continuous(breaks = bracketed_breaks(limits = df$date), limits = range(years)+c(-3,3), expand = c(0, 0)) +
        scale_y_continuous(labels = ones(always.signed = TRUE)) +
        annotate(
          "text", x = 1907, y = 0.1, label = "1961–90 average",
          family = style$family, color = style$colors$text, size = style$gg_text_size,
          hjust = 0, vjust = 0) +
        style$theme()
      p
    },
    aspect_ratio = 3,
    title = "Warmer seas lead to coral bleaching or death, an outcome already observed in parts of Australia's Great Barrier Reef.",
    subtitle = wbg_name(indicator = "Average sea surface temperature anomaly, Great Barrier Reef", by = "relative to 1961–90 average",  denom = "degrees Celsius"),
    source = "Source: Australian Bureau of Meteorology. http://www.bom.gov.au/web01/ncc/www/cli_chg/timeseries/sst/0112/GBR/latest.txt"
  )
}

# make_all(path = "docs/sdg14/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg14", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg14_catches_fishing_sector(), styler, file.path(path, "fig_sdg14_catches_fishing_sector.png"), width = 2.67, height = 2)
  saver(fig_sdg14_marine_stock_status(), styler, file.path(path, "fig_sdg14_marine_stock_status.png"), width = 2.67, height = 2)

  # page 2
  saver(fig_sdg14_dead_zones(), styler, file.path(path, "fig_sdg14_dead_zones.png"), width = 5.5, height = 3.1)
  saver(fig_sdg14_marine_protected_areas_by_country(), styler, file.path(path, "fig_sdg14_marine_protected_areas_by_country.png"), width = 5.5, height = 1.9)
  
  # page 2
  saver(fig_sdg14_global_sea_surface_temperature(), styler, file.path(path, "fig_sdg14_global_sea_surface_temperature.png"), width = 5.5, height = 1.5)
  saver(fig_sdg14_barrier_reef_temperature(), styler, file.path(path, "fig_sdg14_barrier_reef_temperature.png"), width = 5.5, height = 2.6)
}
