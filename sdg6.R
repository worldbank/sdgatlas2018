library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(extrafont)
library(readxl)
library(forcats)
library(countrycode)
library(stringr)
source("styles.R")

fig_sdg6_global_water_ladder <- function(year = 2015) {
  df_wide <- read_xlsx(path = "inputs/sdg6/JMP_2017_WLD.xlsx", sheet = "Water",
                      skip = 2, col_names = TRUE)
  
  df_wide <- df_wide %>% select(
      area           = "COUNTRY, AREA OR TERRITORY",
      date           = "Year",
      at_least_basic = "At least basic",
      limited        = "Limited (more than 30 mins)",
      unimproved     = "Unimproved",
      surface_water  = "Surface water",
      safely_managed = "Safely managed"
  ) %>%
    filter(area == "World" & date == year) %>%
    mutate(basic = as.numeric(at_least_basic) - as.numeric(safely_managed)) %>%
    select(-at_least_basic)
  
  df <- df_wide %>%
    gather(water_access, coverage, -area, -date) %>% 
    mutate(coverage = as.numeric(coverage))
  
  figure(
    data = df,
    plot = function(data, 
                    style = style_atlas(), 
                    aspect_ratio = 3) {
      water_access_levels <- rev(c("surface_water", "unimproved", "limited", "basic", "safely_managed"))
      data <- data %>%
        mutate(water_access = factor(water_access, levels = water_access_levels)) %>%
        arrange(water_access) # As well as arranging the factor, arrange the rows so we can calc label position
      ggplot(data, aes(x=area, y=coverage, fill=water_access)) +
        geom_col(width = 1, position = position_stack(reverse=TRUE)) + # use L->R stacking - makes it easier to do text
        geom_text(aes(
          label = ones(0)(coverage),
          y = cumsum(coverage) - coverage/2),
          color = style$colors$text.inverse,
          family = style$theme()$text$family,
          size = style$gg_text_size,
          hjust = 0.5
        ) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(
          values = rev(c(style$colors$spot.secondary.dark,
                     style$colors$spot.secondary, 
                     style$colors$spot.secondary.light,
                     style$colors$spot.primary.light,
                     style$colors$spot.primary))
        ) +
        coord_flip() +
        style$theme() +
        theme(panel.grid = element_blank(),
              axis.text = element_blank())
    },
    aspect_ratio = 4,
    title = "Drinking water is essential to life, but only 71 percent of people have water that is considered safely managed.",
    subtitle = wbg_name(indicator = "Access to water at different categories", year = year, denom ="% of global population"),
    source = "Source: WHO/UNICEF JMP for Water Supply, Sanitation and Hygiene, https://washdata.org. WDI (SH.H2O.SMDW.ZS; SH.BASW.ZS)."
  )
}

fig_sdg6_safely_managed_water_components_multiples <- function(year = 2015, countries = c("NPL", "GHA")) {
  
  df_raw <- read_xlsx(path = "inputs/sdg6/JMP_2017_WLD.xlsx",
                      sheet = "Water",
                      skip = 2,
                      col_names = TRUE,
                      na = "-")
  
  df <- df_raw %>%
    select(2, 3, 24:27) %>%
    filter(Year == year, ISO3 %in% countries) %>%
    mutate(ISO3 = fct_reorder(ISO3, -`Safely managed`)) %>%
    gather(indicatorID, value, 3:6) %>%
    rename(iso3c = ISO3, date = Year)
  
  figure(data = df,
         plot = function(df, style = style_atlas_open()) {
           df$indicatorID <- factor(df$indicatorID,
                                      levels = c("Safely managed",
                                                 "Accessible on premises",
                                                 "Available when needed",
                                                 "Free from contamination"))
           
           ggplot(df, aes(x = indicatorID, y = value, fill = indicatorID)) +
             geom_col(data = . %>% filter(indicatorID != "Safely managed")) +
             geom_hline(
               aes(yintercept = value, color = indicatorID),
               data = . %>% filter(indicatorID == "Safely managed"),
               size = style$linesize) +
             scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
             scale_fill_manual(
               values = c(
                  "Safely managed" = style$colors$spot.primary,
                  "Accessible on premises" = style$colors$spot.secondary.light,
                  "Available when needed" = style$colors$spot.secondary,
                  "Free from contamination" = style$colors$spot.secondary.dark
               )
             ) +
             scale_color_manual(
               values = c(
                 "Safely managed" = style$colors$spot.primary,
                 "Accessible on premises" = style$colors$spot.secondary.light,
                 "Available when needed" = style$colors$spot.secondary,
                 "Free from contamination" = style$colors$spot.secondary.dark
               )
             ) +
             facet_wrap(~ iso3c,labeller = as_labeller(wbgref$countries$labels), strip.position = "bottom") +
             style$theme() +
             style$theme_legend("righttop") +
             theme(
               axis.text.x = element_blank(),
               strip.text = element_text(hjust = 0.5),
               panel.spacing.x = unit(0.05, "npc") 
              )
         },
         aspect_ratio = 2,
         title = "Countries may have similar rates of safely managed access for different reasons.",
         subtitle = wbg_name(indicator = "Components of safely managed water for two countries", denom ="% of population", year = year),
         source = "Source: WHO/UNICEF Joint Monitoring Programme for Water Supply, Sanitation and Hygiene, https://washdata.org. WDI (SH.H2O.SMDW.ZS)."
  )
}

fig_sdg6_safely_managed_water_by_region <- function(year = 2015) {
  indicators <- c("SH.H2O.BASW.ZS", "SH.H2O.SMDW.ZS")
  
  df <- wbgdata(
    country = c(wbgref$regions$iso3c),
    indicator = indicators,
    years = year,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg6_safely_managed_water_by_region.csv"
  )
  
  # If a country has NA for safely managed, we'll keep the at least basic column
  # otherwise we'll NA that and use the two components
  df <- df %>%
    mutate(basic = SH.H2O.BASW.ZS - SH.H2O.SMDW.ZS) %>%
    mutate(SH.H2O.BASW.ZS = ifelse(is.na(basic), SH.H2O.BASW.ZS, NA))
  
  df_long <- df %>% gather(indicator, value, SH.H2O.BASW.ZS, SH.H2O.SMDW.ZS, basic, na.rm=TRUE)
  
  figure(data = df_long,
         plot = function(data, style = style_atlas_open()) {
           region_levels <- c("SSF", "LCN", "MEA", "ECS", "NAC", "SAS", "EAS", "WLD")
           
           indicator_levels = c("SH.H2O.BASW.ZS", "basic", "SH.H2O.SMDW.ZS")
           labels <- c(WLD = "World", wbgref$regions$labels)
           agg_regions <- data %>% filter(indicator == "SH.H2O.BASW.ZS") %>% pull(iso3c)
           labels[agg_regions] <- paste(labels[agg_regions], " (a)")
           labels <- str_wrap_lines(labels, 3)
           ggplot(data, aes(x=factor(iso3c, levels = region_levels), y=value)) +
             geom_col(aes(fill = factor(indicator, levels = indicator_levels)), width = 0.75) +
             scale_fill_manual(values = c(style$colors$spot.secondary.light, style$colors$spot.primary.light, style$colors$spot.primary),
                               labels = c("At least basic", "Basic", "Safely managed")) +
             scale_x_discrete(labels = labels) +
             style$theme() +
             style$theme_legend("right")
         },
         title = "The gap between access to at least basic and safely managed water is often substantial.",
         subtitle = wbg_name(indicator = "Access to safely managed and basic water", denom = "% of population", year = year),
         note = "a. In East Asia & Pacific and South Asia, too few countries have data on safely managed water to calculate the regional aggregate.",
         source = "Source: WHO/UNICEF JMP for Water Supply, Sanitation and Hygiene, https://washdata.org. WDI (SH.H2O.SMDW.ZS; SH.BASW.ZS)."
  )
}

fig_sdg6_at_least_basic_water_map <- function(year = 2015) {
  indicator <- "SH.H2O.BASW.ZS"
  
  df <- wbgdata(
    country = wbgref$countries$iso3c, 
    indicator = indicator, 
    years = year,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg6_at_least_basic_water_map.csv"
  )
  
  df$bins <- supercut(df$SH.H2O.BASW.ZS, c(
    "Under 50" = "[0,50)",
    "50–75" = "[50,75)",
    "75–90" = "[75,90)",
    "90–100" = "[90,100]"
  ))
  
  figure(
    data = df,
    plot = function(data, style = style_atlas, quality = "high") {
      g <- wbg_choropleth(data, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.3,
    title = "At least basic water requires only an improved water source within a 30 minute round trip, but 42 percent of people in Sub-Saharan Africa lack even that.",
    subtitle = wbg_name(indicator = "People using at least basic water services", year = year, denom = "% of population"),
    source = paste("Source: WHO/UNICEF Joint Monitoring Programme for Water Supply, Sanitation and Hygiene. World Development Indicators (SH.H2O.BASW.ZS)."))
  
}

fig_sdg6_at_least_basic_water_by_urban_rural <- function(years = c(2000,2015)) {
  indicators <- c(
    urban = "SH.H2O.BASW.UR.ZS",
    rural = "SH.H2O.BASW.RU.ZS"
  )
  
  df <- wbgdata(
    country = wbgref$regions$iso3c, 
    indicator = indicators, 
    years = years, 
    indicator.wide = FALSE,
    rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg6_at_least_basic_water_by_urban_rural.csv"
  )

  figure(data = df,
         plot = function(df, style = style_atlas()) {
           df <- df %>% arrange(date) # So we overprint in the right order
           df <- df %>% filter(complete.cases(.))
           ggplot(df, aes(x = value, y = indicatorID, color = as.factor(date), fill = as.factor(date), shape = as.factor(date))) +
             geom_other_dotplot(
               aes(value, indicatorID, group = paste0(indicatorID, iso3c)),
               arrow = TRUE,
               size = style$point_size,
               stroke = style$point_stroke
             ) +
             scale_shape_manual(values = c(style$shapes$point, 99)) +
             scale_color_manual(values = c(style$colors$spot.primary.light, style$colors$spot.primary)) +
             scale_fill_manual(values = c(style$colors$spot.primary.light, style$colors$spot.primary)) +
             scale_x_continuous(limits = c(0,105)) +
             scale_y_discrete(labels = Hmisc::capitalize) +
             facet_wrap(~ iso3c, ncol = 1, labeller = as_labeller(wbgref$regions$labels)) +
             style$theme() +
             style$theme_barchart() +
             theme(
               panel.spacing.y = unit(0, "npc"),
               strip.text.x = element_text(vjust = 0)
              )
         },
         title = "Rural dwellers are less likely than their urban counterparts to have access to at least basic water.",
         subtitle = wbg_name(indicator = "People using at least basic water services", denom = "%"),
         note = "Note: Data not available for North America (rural) for 2000.",
         source = paste("Source: WDI (SH.H2O.BASW.UR.ZS; SH.H2O.BASW.RU.ZS)."))
}

fig_sdg6_piped_water_on_premises <- function(years = 2013:2016) {
  df_raw <- read_xlsx(path = "inputs/sdg6/wpd_piped_water.xlsx", col_names = TRUE)

  df <- df_raw %>%
        subset(Country != "WPD Average (14)") %>%
        mutate(iso3c = countrycode(Country, "country.name", "iso3c")) %>%
        mutate(iso3c = fct_reorder(iso3c, B40)) %>%
        gather(indicatorID, value, B40, T60)

  figure(data = df,
         plot = function(df, style = style_atlas_open()) {
           p <- ggplot(df, aes(x=value, y=iso3c, color=as.factor(indicatorID))) +
             geom_other_dotplot(aes(y=iso3c), size = style$point_size, stroke = style$point_stroke, shape = style$shapes$point) +
             scale_color_manual(
               values = c(style$colors$spot.primary.light, style$colors$spot.primary),
               labels = c(T60 = "Richest 60 percent", B40 = "Poorest 40 percent")
             ) +
             scale_y_discrete(labels = wbgref$countries$labels) +
             scale_x_continuous(limits = c(0, 100)) +
             style$theme() +
             style$theme_legend("top") +
             style$theme_barchart()
           
           # Align legend over entire figure, not just plot area
           g <- ggplotGrob(p)
           g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
           g$theme <- style$theme()
           g
         },
         aspect_ratio = 0.7,
         title = "Poorer people are less likely have the convenience and potential safety of water piped to their homes.",
         subtitle = wbg_name(indicator = "People using piped water on premises", year = "most recent value", denom = "%"),
         source = "Source: World Bank (2017). http://hdl.handle.net/10986/27831"
  )
}

fig_sdg6_global_sanitation_ladder <- function(year = 2015) {
  df_wide <- read_xlsx(path = "inputs/sdg6/JMP_2017_WLD.xlsx", sheet = "Sanitation",
                       skip = 2, col_names = TRUE, na = "-")
  
  df_wide <- df_wide %>% select(
    area            = "COUNTRY, AREA OR TERRITORY",
    date            = "Year",
    at_least_basic  = "At least basic",
    limited         = "Limited (shared)",
    unimproved      = "Unimproved",
    open_defecation = "Open defecation",
    safely_managed  = "Safely managed"
  ) %>%
    filter(area == "World" & date == year) %>%
    mutate(basic = as.numeric(at_least_basic) - as.numeric(safely_managed)) %>%
    select(-at_least_basic)
  
  df <- df_wide %>%
    gather(sanitation_access, coverage, -area, -date) %>% 
    mutate(coverage = as.numeric(coverage))

  figure(
    data = df,
    plot = function(data, 
                    style = style_atlas_open(), 
                    aspect_ratio = 3) {
      sanititation_access_levels <- rev(c("open_defecation", "unimproved", "limited", "basic", "safely_managed"))
      data <- data %>%
        mutate(sanitation_access = factor(sanitation_access, levels = sanititation_access_levels)) %>%
        arrange(sanitation_access) # As well as arranging the factor, arrange the rows so we can calc label position
      ggplot(data, aes(x=area, y=coverage, fill=sanitation_access)) +
        geom_col(width = 1, position = position_stack(reverse=TRUE)) + # use L->R stacking - makes it easier to do text
        geom_text(aes(
          label = ones(0)(coverage),
          y = cumsum(coverage) - coverage/2),
          color = style$colors$text.inverse,
          family = style$theme()$text$family,
          size = style$gg_text_size,
          hjust = 0.5
        ) +
        scale_y_continuous(expand = c(0,0)) +
        scale_fill_manual(
          values = rev(c(style$colors$spot.secondary.dark,
                         style$colors$spot.secondary, 
                         style$colors$spot.secondary.light,
                         style$colors$spot.primary.light,
                         style$colors$spot.primary))
        ) +
        coord_flip() +
        style$theme() +
        theme(panel.grid = element_blank(),
              axis.text = element_blank())
    },
    aspect_ratio = 4,
    title = "Globally, 6 in 10 people use sanitation facilities that are not safely managed and may contribute to the spread of disease.",
    subtitle = wbg_name(indicator = "Access to sanitation at different categories", year = year, denom = "% of global population"),
    source = "Source: WHO/UNICEF JMP for Water Supply, Sanitation and Hygiene, https://washdata.org. WDI (SH.STA.SMSS.ZS; SH.STA.BASS.ZS)."
  )
}

fig_sdg6_safely_managed_sanitation_by_region <- function(year = 2015) {
  indicators <- c("SH.STA.BASS.ZS", "SH.STA.SMSS.ZS")
  df_wide <- wbgdata(
    country = c(wbgref$regions$iso3c),
    indicator = indicators,
    years = year,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg6_safely_managed_sanitation_by_region.csv"
  )
  
  # If a country has NA for safely managed, we'll keep the at least basic column
  # otherwise we'll NA that and use the two components
  df_wide <- df_wide %>%
    mutate(basic = SH.STA.BASS.ZS - SH.STA.SMSS.ZS) %>%
    mutate(SH.STA.BASS.ZS = ifelse(is.na(basic), SH.STA.BASS.ZS, NA))
  
  df <- df_wide %>% gather(indicatorID, value, SH.STA.BASS.ZS, SH.STA.SMSS.ZS, basic, na.rm=TRUE)

  figure(data = df,
         plot = function(df, style = style_atlas_open()) {
           region_levels <- c("LCN", "MEA", "EAS", "ECS", "NAC", "SSF", "SAS", "WLD")

           labels <- c(WLD = "World", wbgref$regions$labels)
           agg_regions <- df %>% filter(indicatorID == "SH.STA.BASS.ZS") %>% pull(iso3c)
           labels[agg_regions] <- paste(labels[agg_regions], " (a)")
           labels <- str_wrap_lines(labels, 3)
           
           ggplot(df, aes(factor(iso3c, levels = region_levels), value)) +
             geom_col(aes(fill = factor(indicatorID, levels = c("SH.STA.BASS.ZS", "basic", "SH.STA.SMSS.ZS"))), width = 0.75) +
             scale_fill_manual(values = c(SH.STA.BASS.ZS = style$colors$spot.secondary.light,
                                          SH.STA.SMSS.ZS = style$colors$spot.primary,
                                          basic = style$colors$spot.primary.light),
                               labels = c(SH.STA.BASS.ZS = "At least basic",
                                          SH.STA.SMSS.ZS = "Safely managed",
                                          basic = "Basic")) +
             scale_x_discrete(labels = labels) +
             style$theme() +
             style$theme_legend("right")
         },
         title = "In Latin America & Caribbean, 86 percent of people have access to at least basic sanitation, but only a quarter of those have access to safely managed sanitation.",
         subtitle = wbg_name(indicator = "Access to safely managed and basic sanitation", year = 2015, denom =  "% of population"),
         note = "a. In South Asia and Sub-Saharan Africa, too few countries have data on safely managed sanitation to calculate the regional aggregate.",
         source = paste0("Source: WHO/UNICEF Joint Monitoring Programme for Water Supply, Sanitation and Hygiene. WDI (SH.STA.SMSS.ZS; SH.STA.BASS.ZS)."))
}

fig_sdg6_at_least_basic_sanitation_by_country <- function(year = 2015, N_countries = 15) {
  indicator <- "SH.STA.BASS.ZS"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator, 
    years = year,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg6_at_least_basic_sanitation_by_country.csv"
  )

  df <- df %>% top_n(-N_countries, value)

  figure(data = df,
         plot = function(df, style = style_atlas_open()) {
           ggplot(df, aes(fct_reorder(iso3c, -value), value)) +
             geom_bar(stat = "identity", fill = style$colors$spot.secondary.light) +
             scale_x_discrete(labels = wbgref$countries$labels) +
             scale_y_continuous(limits = c(0, 30.2), expand = c(0, 0)) +
             coord_flip() +
             style$theme() +
             style$theme_barchart() +
             style$theme_legend("top")
           },
         aspect_ratio = 1,
         title = "Even by the less demanding standard of at least basic sanitation, many countries, especially in Sub-Saharan Africa, have very low rates of access.",
         subtitle = "Access to at least basic sanitation, 2015 (% of population)",
         note = paste0("Note: The ",N_countries," countries with lowest access to at least basic sanitation (out of 210 countries with data)."),
         source = paste0("Source: WHO/UNICEF Joint Monitoring Programme for Water Supply, Sanitation and Hygiene. World Development Indicators (SH.STA.BASS.ZS).")
      )
}

fig_sdg6_open_defecation_by_region_time <- function(years = 2000:2015) {
  indicators <- c("SH.STA.ODFC.ZS","SP.POP.TOTL")
  
  df <- wbgdata(
    c("IND", wbgref$regions$iso3c),
    indicators,
    years = years,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg6_open_defecation_by_region_time.csv"
  )
  
  # Remove North America as it is zero in the entire period
  df <- df %>% filter(iso3c != "NAC")
  
  df <- df %>% 
    mutate(num_def = SH.STA.ODFC.ZS/100 * SP.POP.TOTL) %>% 
    select(iso3c, date, num_def) %>% 
    spread(iso3c, num_def) %>% 
    mutate(SAS = SAS - IND) %>% 
    gather(iso3c, num_def, EAS:SSF)
  
  figure(
    data = df,
    plot = function(data, style = style_atlas_open()) {
      region_levels <- data[which(data$date == max(2015)), ] %>%
        arrange(iso3c %in% c("IND", "SAS"), num_def) %>%
        pull(iso3c)
      
      ggplot(data, aes(x = date, y = num_def, fill = factor(iso3c, levels = region_levels))) +
        geom_area() +
        style$theme() + 
        style$theme_legend("right") +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_continuous(labels = millions()) +
        scale_fill_manual(
          values = c("IND" = unname(style$colors$regions.light["SAS"]), style$colors$regions),
          labels = c("IND" = "India", wbgref$regions$labels, "SAS" = "South Asia (excluding India)")
        )
      },
      aspect_ratio = 1.5,
      title = paste0("India still has the largest number of people practicing open defecation."),
      subtitle = wbg_name("SH.STA.ODFC.ZS", denom = "millions"),
      note = "Note: North America is zero over the entire period; Europe & Central Asia is zero from 2013.",
      source = paste("Source: WHO/UNICEF Joint Monitoring Programme for Water Supply, Sanitation and Hygiene. WDI (SH.STA.ODFC.ZS; SP.POP.TOTL).")
  )
}

fig_sdg6_hand_washing_by_quintile <- function(num_countries = 30) {
  df <- read_xlsx(path = "inputs/sdg6/JMP estimates wealth quintiles for WDI (from UNICEF).xlsx",
                      skip = 5, col_names = TRUE)
  
  df <- df %>% 
    select(iso3, year, inequality, service_level, service_type, value_national) %>% 
    filter(inequality %in% c("Poorest", "Richest") & service_level == "Basic service" & service_type == "Hygiene") %>% 
    na.omit %>% 
    spread(inequality, value_national) %>% 
    arrange(Poorest) %>% 
    slice(1:30) %>% 
    gather(inequality, value_national, Poorest:Richest)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      iso3_levels <- df %>%
        filter(inequality == "Poorest") %>% 
        arrange(-value_national) %>%
        pull(iso3)
      
      ggplot(df, aes(x=value_national, y=iso3, color=inequality)) +
        geom_other_dotplot(aes(y=factor(iso3, levels = iso3_levels)), size = style$point_size, stroke = style$point_stroke, shape = style$shapes$point) +
        scale_color_manual(
          values = c(Poorest = style$colors$spot.primary.light, Richest = style$colors$spot.primary),
          labels = c(Poorest = "Poorest quintile", Richest = "Richest quintile")
        ) +
        scale_x_continuous(limits = c(0,100), expand = c(0, 1)) +
        scale_y_discrete(labels = wbgref$countries$labels) +
        style$theme() +
        style$theme_barchart() +
        theme(legend.position = c(0.95,1), legend.justification = c(1,1), legend.direction = "horizontal")
    },
    aspect_ratio = 1,
    title = "Handwashing makes an important contribution to hygiene, but many households, especially amongst the poor, lack basic facilities.",
    subtitle = wbg_name(indicator = "Access to handwashing facilities with soap and water on premises", mrv=df$year, denom ="%"),
    note = paste0("Note: The ", num_countries, " countries with lowest access among the poorest wealth quintile (out of 51 countries with data)."),
    source = "Source: WHO/UNICEF Joint Monitoring Programme for Water Supply, Sanitation and Hygiene. WDI (SH.STA.HYGN.Q1.ZS; SH.STA.HYGN.Q5.ZS)."
  )
}

# make_all(path = "docs/sdg6/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg6", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg6_global_water_ladder(), styler, file.path(path, "fig_sdg6_global_water_ladder.png"), width = 5.5, height=3, padding=margin(0,0,35,0,"mm"))
  saver(fig_sdg6_safely_managed_water_components_multiples(), styler, file.path(path, "fig_sdg6_safely_managed_water_components_multiples.png"), width = 5.5, height = 1.85, padding = margin(0,0,0,1.5,"in"))
  saver(fig_sdg6_safely_managed_water_by_region(), styler, file.path(path, "fig_sdg6_safely_managed_water_by_region.png"), width = 5.5, height = 2.25)
  
  # page 2
  saver(fig_sdg6_at_least_basic_water_map(), styler, file.path(path, "fig_sdg6_at_least_basic_water_map.png"), width = 5.5, height=4)
  saver(fig_sdg6_at_least_basic_water_by_urban_rural(), styler, file.path(path, "fig_sdg6_at_least_basic_water_by_urban_rural.png"), height = 4.5, width = 2.67)
  saver(fig_sdg6_piped_water_on_premises(), styler, file.path(path, "fig_sdg6_piped_water_on_premises.png"), width = 2.67, height = 4.5)
  
  # page 3
  saver(fig_sdg6_global_sanitation_ladder(), styler, file.path(path, "fig_sdg6_global_sanitation_ladder.png"), width = 5.5, height=3, padding=margin(0,0,35,0,"mm"))
  saver(fig_sdg6_safely_managed_sanitation_by_region(), styler, file.path(path, "fig_sdg6_safely_managed_sanitation_by_region.png"), width = 5.5, height = 2.45)
  saver(fig_sdg6_at_least_basic_sanitation_by_country(), styler, file.path(path, "fig_sdg6_at_least_basic_sanitation_by_country.png"), width = 5.5, height = 3)
  
  # page 4
  saver(fig_sdg6_open_defecation_by_region_time(), styler, file.path(path, "fig_sdg6_open_defecation_by_region_time.png"), width = 5.5, height = 3.5)
  saver(fig_sdg6_hand_washing_by_quintile(), styler, file.path(path, "fig_sdg6_hand_washing_by_quintile.png"), width = 5.5, height=5)
}

