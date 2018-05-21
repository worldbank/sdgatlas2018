library(wbgdata)
library(wbgcharts)
library(wbggeo)
library(wbgmaps)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(readxl)
library(readr)
library(proj4)
library(stringr)
source("styles.R")

fig_sdg11_urban_rural_trends_region <- function(years = 1980:2016) {
  indicators <- c(urban = "SP.URB.TOTL.IN.ZS", rural = "SP.RUR.TOTL.ZS")
  
  df <- wbgdata(
    wbgref$regions$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg11_urban_rural_trends_region.csv"
  )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      iso3c_order <- df %>%
        filter(indicatorID == "urban", date == max(years)) %>%
        arrange(iso3c != "WLD", value) %>%
        pull(iso3c)
        
      df <- df %>% mutate(iso3c = fct_relevel(iso3c, iso3c_order))
      
      facet_labeller <- as_labeller(str_wrap_lines(c(wbgref$regions$labels, WLD="World"),2,force=TRUE))
      
      ggplot(df, aes(x = as.numeric(date), y = value, group = indicatorID, color = indicatorID)) +
        geom_line(size = style$linesize) +
        facet_wrap(~ iso3c, nrow = 1, labeller = facet_labeller) +
        scale_color_manual(
          values = style$colors$urban_rural,
          labels = c(rural = "Rural", urban = "Urban")
        ) +
        geom_text(
          aes(
            y = value + 10 * ifelse(indicatorID == "rural", 1, -1),
            label = c(rural = "Rural", urban = "Urban")[indicatorID]
          ),
          data = . %>% filter(iso3c == "SAS", date == min(years)),
          family = style$family,
          size = style$gg_text_size,
          hjust = 0,
          nudge_x = 0
        ) +
        scale_y_continuous(limits = c(0, 100)) +
        scale_x_continuous(breaks = range(years), expand = c(0,5)) +
        style$theme() +
        theme(
          panel.spacing.x = unit(0.025, "npc"),
          strip.text.x = element_text(hjust = 0.5, vjust=0)
        )
    },
    aspect_ratio = 2,
    title = "Since around 2008, the majority of the world's population has lived in urban areas. Only South Asia and Sub-Saharan Africa remain more rural than urban.",
    subtitle = wbg_name(indicator = "Share of total population", denom = "%"),
    source = "Source: UN Population Division. WDI (SP.URB.TOTL.IN.ZS; SP.RUR.TOTL.ZS)."
  )
}

fig_sdg11_percent_slums_country_change <- function(years = c(2005, 2014), N_inc = 10, N_dec = 10) {
  indicator <- "EN.POP.SLUM.UR.ZS"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg11_percent_slums_country_change.csv"
  )
  
  # Calculate change over the period
  changes <- df %>%
    group_by(iso3c) %>%
    arrange(date) %>%
    summarise(change = last(value) - first(value)) %>%
    arrange(change)
  
  # Get top and bottom N
  iso3c_inc <- tail(changes$iso3c, N_inc)
  iso3c_dec <- head(changes$iso3c, N_dec)
  
  df <- df %>%
    filter(iso3c %in% c(iso3c_inc, iso3c_dec)) %>%
    mutate(facet = ifelse(iso3c %in% iso3c_inc, "Top 10 percentage-point increases","Top 10 percentage-point decreases")) %>%
    arrange(date)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = fct_reorder2(iso3c, date == max(years), ifelse(iso3c %in% iso3c_dec, value, -value)))
      p <- ggplot(df, aes(value, iso3c, color = as.factor(date), fill = as.factor(date), shape = as.factor(date))) +
        geom_other_dotplot(aes(value, iso3c, group = paste0(facet, iso3c)), arrow = TRUE, size = style$point_size, stroke = style$point_stroke) +
        scale_shape_manual(values = c(style$shapes$point, 99)) +
        scale_color_manual(values = c(style$colors$spot.primary.light, style$colors$spot.primary)) +
        scale_fill_manual(values = c(style$colors$spot.primary.light, style$colors$spot.primary)) +
        scale_y_discrete(labels = wbgref$countries$labels) +
        facet_wrap(~facet, ncol = 1, scales = "free_y") +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top")

      # Align legend relative to entire figure not just plot area
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1,
    title = "Despite increasing urbanization, many countries have reduced the share of urban dwellers living in slums.",
    subtitle = wbg_name(indicator, year = paste0(min(df$date), " and ", max(df$date))),
    source = "Source: UN-Habitat. World Development Indicators (EN.POP.SLUM.UR.ZS)."
  )
}

fig_sdg11_slum_urban_rural_pie <- function(year = 2014) {
  indicators <- c("EN.POP.SLUM.UR.ZS", "SP.URB.TOTL.IN.ZS", "SP.RUR.TOTL.ZS")
  
  df <- wbgdata(
    wbgref$regions$iso3c,
    indicators,
    years = year,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg11_slum_urban_rural_pie.csv"
  )
  
  df <- df[complete.cases(df), ] %>%
    mutate(urban_slum = ((EN.POP.SLUM.UR.ZS / 100) * SP.URB.TOTL.IN.ZS),
           urban_nonslum = SP.URB.TOTL.IN.ZS - urban_slum) %>%         
    select(-c(EN.POP.SLUM.UR.ZS, SP.URB.TOTL.IN.ZS)) %>%
    rename(rural = SP.RUR.TOTL.ZS) %>%
    gather(indicatorID, value, c(rural, urban_slum, urban_nonslum))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(x = factor(1), y = value, fill = indicatorID)) +
        geom_bar(width = 1, stat = "identity") +
        scale_fill_manual(values = c(
          rural = style$colors$spot.secondary,
          urban_slum = style$colors$spot.primary,
          urban_nonslum = style$colors$spot.primary.light
        )) +
        facet_grid(iso3c ~ ., labeller = as_labeller(str_wrap_lines(wbgref$regions$labels,3,force=TRUE))) + 
        coord_polar(theta = "y") +
        style$theme() +
        theme(
          axis.text = element_blank(),
          panel.grid  = element_blank(),
          strip.text.y = element_text(angle = 0, hjust = 0.5),
          panel.spacing = unit(0, "npc")
        )
    },
    title = "But substantial slum populations still exist.",
    subtitle = wbg_name(indicator = "Population", by = "by locale", year = year, denom = "%"),
    note = "Note: Other regions not shown due to limited country data.",
    source = paste("Source: WDI (EN.POP.SLUM.UR.ZS; SP.URB.TOTL.IN.ZS; SP.RUR.TOTL.ZS).")
  )
}

fig_sdg11_urban_services_multiple <- function(years = 2014:2016) {
  indicator <- tribble(
    ~service,           ~urban_rural, ~indicatorID,
    "Electricity",      "urban",      "EG.ELC.ACCS.UR.ZS",
    "Electricity",      "rural",      "EG.ELC.ACCS.RU.ZS",
    "Water",            "urban",      "SH.H2O.BASW.UR.ZS",
    "Water",            "rural",      "SH.H2O.BASW.RU.ZS",
    "Sanitation",       "urban",      "SH.STA.BASS.UR.ZS",
    "Sanitation",       "rural",      "SH.STA.BASS.RU.ZS",
    "Poverty",          "urban",      "SI.POV.URHC",
    "Poverty",          "rural",      "SI.POV.RUHC"
  )
  df <- wbgdata(
    c(wbgref$countries$iso3c, "WLD"),
    indicator$indicatorID,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg11_urban_services_multiple.csv"
  )
  
  df <- df %>%
    group_by(iso3c, indicatorID) %>%
    filter(date == max(date)) %>%
    ungroup() 
  
  df <- df %>% left_join(indicator)
  
  count <- df %>%
    group_by(iso3c) %>%
    filter(!grepl("safely", service)) %>%
    summarise(not_na = sum(!is.na(value)))
  
  iso3c_select <- count %>% filter(not_na == 8 | iso3c == "WLD") %>% pull(iso3c)
  iso3c_order <- iso3c_select[order(c(wbgref$countries$labels, WLD="ZZZ")[iso3c_select])]
  
  df <- df %>% filter(iso3c %in% iso3c_select)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = factor(iso3c, iso3c_order))
      df <- df %>% mutate(service = factor(service, c(
        "Sanitation",
        "Water",
        "Electricity",
        "SPACER",
        "Poverty"
      )))
      
      facet_labeller <- as_labeller(c(
        setNames(str_replace(wbgref$countries$labels, "Republic", "Rep."), names(wbgref$countries$labels)),
        WLD = "World"
      ))
      ggplot(df, aes(service, value, fill = factor(urban_rural, c("urban", "rural")))) +
        geom_col(position = "bullet", width = 0.8) +
        coord_flip() +
        scale_y_continuous(limits = c(0, 100), breaks = c(0, 50, 100), labels = c("", "", 100), expand = c(0, 0)) +
        scale_x_discrete(labels = c(
          Poverty = "Poverty",
          Water = "Water",
          Sanitation = "Sanitation",
          Electricity = "Electricity",
          SPACER = ""
        )) +
        scale_fill_manual(
          values = style$colors$urban_rural,
          labels = c(urban = "Urban", rural = "Rural"),
          guide = guide_legend(ncol = 2, reverse = TRUE),
          drop = FALSE
        ) +
        facet_wrap(~iso3c, labeller = facet_labeller, scales = "free_x", ncol = 5) +
        style$theme() +
        style$theme_barchart() +
        theme(
          panel.spacing.y = unit(0.04, "npc"),
          panel.spacing.x = unit(0.06, "npc"),
          strip.text.x = element_text(hjust = 0.5, vjust = 0, margin = margin(0, 0, 0.5, 0, "lines")),
          axis.text.y = element_text(margin = margin(0,1,0,0,"lines")),
          legend.position = c(0.97, -0.02),
          legend.justification = c(1, 0)
        )
    },
    title = "Reliable infrastructure helps cities to thrive: urban dwellers have better access to services and tend to be less poor than their rural counterparts.",
    subtitle = wbg_name(indicator = "Poverty headcount ratio at national poverty lines; and access to electricity, at least basic water and at least basic sanitation", by = "countries with all four indicators available", year = 2014, denom = "% of rural and urban populations"),
    note = "a. Poverty aggregate based on national poverty lines not available for world since these lines differ by country.",
    source = "Source: World Bank; WHO; and WHO/UNICEF JMP for Water Supply, Sanitation and Hygiene. WDI (SI.POV.URHC; SI.POV.RUHC; EG.ELC.ACCS.UR.ZS; EG.ELC.ACCS.RU.ZS; SH.H2O.BASW.UR.ZS; SH.H2O.BASW.RU.ZS; SH.STA.BASS.UR.ZS; SH.STA.BASS.RU.ZS)."
  )
}

fig_sdg11_pm25_countries <- function(year = 2016) {
  ind <- "EN.ATM.PM25.MC.M3"
  
  df <- wbgdata(
    country = wbgref$countries$iso3c,
    indicator = ind,
    years = year,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg11_pm25_countries.csv"
  )
  
  df$bins <- supercut(df$EN.ATM.PM25.MC.M3, c(
    "0–10" = "[0, 10)",
    "10–25" = "[10, 25)",
    "25–35" = "[25, 35)",
    "35 and over" = "[35, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins", aspect_ratio = 2)
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.5,
    title = "Most countries exceed safe levels of fine particulate matter (PM2.5) pollution. Industry, transport, and household use of solid fuels are among the sources.",
    subtitle = wbg_name(indicator = "Ambient air pollution, PM2.5, annual mean exposure", denom = "micrograms per cubic meter, ug/m3", year = year),
    source = "Source: van Donkelaar and others 2016. World Development Indicators (EN.ATM.PM25.MC.M3)."
  )
}

fig_sdg11_pm25_delhi_time <- function() {
  df <- read_xlsx("inputs/sdg11/site_11820180325091614.xlsx", skip=16)

  df <- df %>% transmute(
    date = as.Date(df$`From Date`, format = "%d-%m-%Y"),
    value = as.numeric(ifelse(PM2.5 == "None", NA, PM2.5))
  )
  
  df <- df %>% mutate(value = ifelse(value == 0, NA, value))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      df$month<-as.numeric(as.POSIXlt(df$date)$mon+1)
      df$monthf<-factor(df$month,levels=as.character(1:12),labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE)
      df$weekday<-((as.POSIXlt(df$date)$wday-1) %% 7) + 1
      df$week <- as.numeric(format(df$date,"%W"))
    
      df$bins <- supercut(df$value, c(
        "0–25" = "[0, 100)",
        "25–100" = "[100, 200)",
        "100–400" = "[200, 400)",
        "400 and over" = "[400, Inf)"
      ))
      
      # Now for the plot
      p <- ggplot(df, aes(weekday, week, fill = bins)) + 
        geom_tile(colour = "white") +
        facet_grid(monthf~., scales = "free_y", space = "free_y", switch = "y") +
        scale_y_reverse() +
        scale_x_continuous(
          position = "top",
          breaks = 1:7,
          labels = c("M", "Tu", "W", "Th", "F", "Sa", "Su"),
          expand = c(0, 0)
        ) +
        scale_fill_discrete(
          palette = style$colors$continuous,
          labels = wbggeo::rename_na("No data"),
          na.value = style$colors$neutral) +
        style$theme() +
        style$theme_legend("righttop") +
        theme(
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          strip.text.y = element_text(angle=180),
          panel.spacing = unit(0.1, "lines"),
          legend.key.width = unit(1,"lines")
        )
    },
    aspect_ratio = 1.5,
    title = "And even in a specific location, PM2.5 varies with seasons and weather.",
    subtitle = wbg_name(indicator = "PM2.5, daily mean", by = "DTU(a) Delhi", year = 2017, denom = "ug/m3"),
    note = "a. Sampled at Delhi Technological University (DTU).",
    source = "Source: India Central Pollution Control Board. https://app.cpcbccr.com"
  )
}

fig_sdg11_air_pollution_deaths_map <- function() {
  df <- read_csv("inputs/sdg11/AIR_5,AIR_50,AIR_41.csv")

  df <- df %>%
    filter(`GHO (CODE)` == "AIR_5") %>% # Not age standardized
    transmute(
      date = `YEAR (DISPLAY)`,
      iso3c = `COUNTRY (CODE)`,
      value = Numeric
    )
  
  df$bins <- supercut(df$value, c(
    "0–20" = "[0, 20)",
    "20–40" = "[20, 40)",
    "40 and over" = "[40, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
    },
    aspect_ratio = 1.5,
    title = "Ambient air pollution has many adverse consequences, the most serious of which is increased risk of premature death.",
    subtitle = wbg_name(indicator = "Deaths attributable to ambient air pollution", year = unique(df$date), denom = "per 100,000"),
    source = "Source: WHO Global Health Observatory (database). http://apps.who.int/gho/data/view.main.BODAMBIENTAIRDTHS"
  )
}

fig_sdg11_labor_income_loss_pollution <- function() {
  df <- read_csv("inputs/sdg11/AnnualLaborIncomeLosses_AirPollution.csv")

  df <- df %>%
    rename(iso3c = "Region") %>%
    gather(indicator, value, -iso3c)
  df <- df %>%
    filter(indicator %in% c("Total", "Ambient_PM2.5"))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      order_colors = c(
        Total = style$color$spot.secondary,
        Ambient_PM2.5 = style$colors$spot.primary
      )
      labels = c(
        Total = "Total (a)",
        Ambient_PM2.5 = "Ambient PM2.5"
      )
      ggplot(df, aes(
        x = fct_reorder2(iso3c, indicator == "Total air pollution", value),
        y = value,
        fill = factor(indicator, names(order_colors))
      )) +
        geom_col(position = "dodge") +
        geom_text(
          aes(y = 0.025, label = labels[indicator]),
          data = . %>% filter(iso3c == "SAS"),
          position = position_dodge(width = 0.9),
          vjust = 0.5,
          hjust = 0,
          family = style$family,
          size = style$gg_text_size,
          color = style$colors$text.inverse,
          angle = 90
        ) +
        scale_x_discrete(labels = str_wrap_lines(wbgref$regions$labels,2,force=TRUE)) +
        scale_fill_manual(values = order_colors) +
        style$theme()
    },
    title = "In addition to the human toll, premature deaths attributable to air pollution have an economic cost to countries.",
    subtitle = wbg_name(indicator = "Estimated annual labor income losses from deaths due to air pollution", by = "by type", year = 2015, denom = "% of GDP"),
    note ="a. Includes losses attributable to household PM2.5 air pollution and ambient ozone.",
    source = "Source: World Bank 2018. http://hdl.handle.net/10986/29001"
  )
}

# make_all(path = "docs/sdg11/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg11", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg11_urban_rural_trends_region(), styler, file.path(path, "fig_sdg11_urban_rural_trends_region.png"), width = 5.5, height = 2.5)
  saver(fig_sdg11_percent_slums_country_change(), styler, file.path(path, "fig_sdg11_percent_slums_country_change.png"), width = 3.67, height = 4.25)
  saver(fig_sdg11_slum_urban_rural_pie(), styler, file.path(path, "fig_sdg11_slum_urban_rural_pie.png"), width = 1.67, height = 4.25)

  # page 2
  saver(fig_sdg11_urban_services_multiple(), styler, file.path(path, "fig_sdg11_urban_services_multiple.png"), width = 5.5, height = 8.5, padding = margin(0,0.25,0,0,"in"))
  
  # page 3
  saver(fig_sdg11_pm25_countries(), styler, file.path(path, "fig_sdg11_pm25_countries.png"), width = 5.5, height = 3.75)
  saver(fig_sdg11_pm25_delhi_time(), styler, file.path(path, "fig_sdg11_pm25_delhi_time.png"), width=2.67, height = 4.8, padding = margin(0.125,0,0,0,"in"))

  # page 4
  saver(fig_sdg11_air_pollution_deaths_map(), styler, file.path(path, "fig_sdg11_air_pollution_deaths_map.png"), width = 5.5, height = 4.25)
  saver(fig_sdg11_labor_income_loss_pollution(), styler, file.path(path, "fig_sdg11_labor_income_loss_pollution.png"), width = 5.5, height = 4.25)
}
