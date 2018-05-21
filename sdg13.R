library(ggplot2)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(forcats)
library(tidyr)
source("styles.R")

fig_sdg13_co2_emissions_by_income <- function(years = 1960:2014) {
  indicator <- c("EN.ATM.CO2E.KT")
  
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicator,
    years = years,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg13_co2_emissions_by_income.csv"
  )
  
  totals <- df %>%
    group_by(date) %>%
    summarise(EN.ATM.CO2E.KT = sum(EN.ATM.CO2E.KT, na.rm = TRUE))
  
  increase <- (
    totals %>% filter(date == max(date)) %>% pull(EN.ATM.CO2E.KT) /
    totals %>% filter(date == min(date)) %>% pull(EN.ATM.CO2E.KT)
  )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = factor(iso3c, rev(wbgref$incomes$iso3c)))
      ggplot(df, aes(date, EN.ATM.CO2E.KT, fill = iso3c)) +
        geom_area() +
        scale_fill_manual(
          values = style$colors$incomes,
          labels = wbgref$incomes$labels,
          guide = guide_legend(ncol = 2)) +
        scale_y_continuous(labels = millions(), position = "left", limits = c(0, 42e6)) +
        scale_x_continuous(limits = c(1960, 2018), breaks = c(1960,1980,2000,max(years)), expand = c(0, 0)) +
        style$theme()
    },
    aspect_ratio = 1,
    title = "Carbon dioxide (CO2) emissions have been growing steadily...",
    subtitle = wbg_name(indicator = "Annual CO2 emissions", by = "by income group", denom = "Gt"),
    source = "Source: Carbon Dioxide Information Analysis Center. World Development Indicators (EN.ATM.CO2E.KT)."
  )
}

fig_sdg13_co2_ppm_mauna_loa <- function() {
  df <- read_table(
    "inputs/sdg13/co2_mm_mlo.txt",
    col_names = c("year", "month", "decimal_date", "average", "interpolated", "trend", "num_days"),
    comment = "#",
    na = c("-99.99", "-1")
  )
  
  df <- df %>% mutate(date = date_decimal(decimal_date))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(x = decimal_date)) +
        geom_line(aes(y = average), color = style$colors$spot.primary, size = style$linesize) +
        geom_line(aes(y = trend), color = style$colors$spot.secondary, size = style$linesize) +
        scale_x_continuous(limits = c(1960, 2018), breaks = c(1960, 1980, 2000, 2017)) +
        scale_y_continuous(breaks = seq(320,400,20), position = "left") +
        style$theme()
    },
    aspect_ratio = 1,
    title = "...so its concentration in the atmosphere is also growing—at an accelerating rate.",
    subtitle = wbg_name(indicator = "Atmospheric CO2", by = "at Mauna Loa, Hawaii", denom = "parts per million"),    
    source = "Source: Tans, P / NOAA/ESRL & Keeling, R / Scripps Institution of Oceanography. http://www.esrl.noaa.gov/gmd/ccgg/trends"
  )
}

fig_sdg13_co2_pop_pc_vs_absolute <- function(year = 2014, population.cutoff = 5e6) {
  indicators <- c("EN.ATM.CO2E.PC","SP.POP.TOTL")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg13_co2_pop_pc_vs_absolute.csv"
  )

  df <- df %>% left_join(wbgref$countries$incomegroups)
  df <- df %>% mutate(income_iso3c = factor(income_iso3c, wbgref$incomes$iso3c))

  df <- df %>% arrange(income_iso3c, -EN.ATM.CO2E.PC)
  
  excluded_count <- df %>% filter(SP.POP.TOTL < population.cutoff) %>% nrow()
  df <- df %>% filter(SP.POP.TOTL >= population.cutoff)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      gap <- 300e6
      df$xmin <- cumsum(df$SP.POP.TOTL) - df$SP.POP.TOTL + cumsum(c(0,diff(as.numeric(df$income_iso3c)))*gap)
      df$xmax <- cumsum(df$SP.POP.TOTL)  + cumsum(c(0,diff(as.numeric(df$income_iso3c)))*gap)
      
      ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = EN.ATM.CO2E.PC, fill = income_iso3c)) +
        geom_rect(color = "white", size = 0.1) +
        scale_fill_manual(values = style$colors$incomes, labels = wbgref$incomes$labels) +
        scale_x_continuous(expand = c(0, 0)) +
        xlab(paste("Countries with a population of at least",millions(0)(population.cutoff),"million, scaled by population")) +
        style$theme() +
        style$theme_legend("bottom") +
        theme(
          axis.text.x = element_blank(),
          axis.title.x = element_text()
        )
    },
    aspect_ratio = 1.25,
    title = "Climate change is caused by this atmospheric CO2, and other greenhouse cases. Not every country, or every person, has the same carbon footprint.",
    subtitle = wbg_name(indicators[1], by = "by country and income group", year = year),
    source = "Source: Carbon Dioxide Information Analysis Center. World Development Indicators (EN.ATM.CO2E.KT; SP.POP.TOTL)."
    #note = paste("Note: Excludes",excluded_count,"countries with populations less than",millions(1)(5e6),"million, which would not be easily visible.")
  )
}

fig_sdg13_co2_rcp_scenarios <- function() {
  df <- rbind(
    read_xls("inputs/sdg13/RCP/R26_bulk.xls"),
    read_xls("inputs/sdg13/RCP/R45_bulk.xls"),
    read_xls("inputs/sdg13/RCP/R60_bulk.xls"),
    read_xls("inputs/sdg13/RCP/R85_bulk.xls")
  )
  
  df <- df %>%
    mutate(Scenario = fct_recode(Scenario,
                                 RCP2.6 = "IMAGE - RCP3-PD (2.6)",
                                 RCP4.5 = "MiniCAM - RCP 4.5",
                                 RCP6.0 = "AIM - RCP 6.0",
                                 RCP8.5 = "MESSAGE - RCP 8.5"
    ))
  
  # The WDI indicator excludes land use change, so we use the comparable series
  df <- df %>%
    filter(Variable == "CO2 emissions - Fossil fuels and Industry") %>%
    filter(Region == "World")
  
  df <- df %>%
    select(-Region, -Variable, -Unit, -Notes) %>%
    gather("date", "value",`2000`:`2100`, convert = TRUE)
  
  # These are in different units, petagrams of atomic carbon vs gigatons
  # of carbon dioxide, so we convert
  GtCO2_per_PgC <- 3.67
  df <- df %>%
    mutate(value = value * GtCO2_per_PgC)
  
  # Minimize overlap to avoid confusion
  df <- df %>% filter(
    date >= 2010
  )
  
  # Now get the WDI historical data
  df.hist <- wbgdata("WLD", "EN.ATM.CO2E.KT", years = c(1960:2014), indicator.wide = FALSE)
  
  # Put it in gTCO2 as well
  df.hist <- df.hist %>% mutate(value = value / 1e6)
  
  figure(
    data = list(historical = df.hist, scenarios = df),
    plot = function(dfs, style = style_atlas()) {
      ggplot(mapping = aes(date, value)) +
        geom_area(data = dfs$historical, fill = "black") +
        geom_line(aes(color = Scenario, group = Scenario), data = dfs$scenarios, linetype = style$linetypes$world, color = style$colors$world, size = style$linesize, lineend="round") +
        #scale_fill_manual(values = style$colors$incomes) +
        scale_x_continuous(expand = c(0, 0), limits = c(1960, 2103), breaks = c(1960,1980,2000,2014,2040,2060,2080,2100)) +
        scale_y_continuous(breaks = 0:10*10) +
        style$theme()
    },
    aspect_ratio = 1,
    title = "Further climate change is inevitable, but the degree of change depends on the path of future emissions of CO2 and other greenhouse gases.",
    subtitle = wbg_name(indicator = "Annual CO2 emissions", by = "historical and four future scenarios used in climate modelling", denom = "Gt"),
    source = "Source: RCP Database (version 2.0.5). http://tntcat.iiasa.ac.at:8787/RcpDb"
  )  
}

fig_sdg13_vulnerability_vs_readiness <- function(year = 2016) {
  df <- read_xlsx("inputs/sdg13/NDC readiness vulnerability data.xlsx", sheet="NDC readiness")
  
  iso3c_to_income_mapping <- setNames(wbgref$countries$incomegroups$income_iso3c, wbgref$countries$incomegroups$iso3c)
  df <- df %>% 
    select(iso3c = "ISO3",
           readiness = "Readiness 2016",
           vulnerability = "Vulnerability 2016"
    ) %>% 
    mutate(income_group = iso3c_to_income_mapping[iso3c])
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(x = readiness, y = vulnerability, color = factor(income_group, levels = c("LIC", "LMC", "UMC", "HIC")))) +
        geom_point(size = style$point_size, stroke = style$point_stroke, shape = style$shapes$point, alpha = 0.8) +
        scale_color_manual(values = style$colors$incomes, labels = wbgref$incomes$labels) +
        scale_x_continuous(limits = c(0,0.8)) +
        scale_y_continuous(limits = c(0.2,0.8)) +
        coord_equal() +
        labs(x = wbg_name(indicator = "Readiness to make effective use of investments for adaptation actions, score\n", denom = "0–1, higher is more ready")) +
        style$theme() +
        style$theme_scatter() +
        style$theme_legend("righttop")
    },
    aspect_ratio = 1.3,
    title = "Low-income countries tend to be more vulnerable to, and less equipped to invest against, extreme climate impacts.",
    subtitle = wbg_name(indicator = "Vulnerability to climate hazards, score", by = "by country", year = year, denom = "0–1, higher is more vulnerable"),
    source = "Source: Notre Dame Global Adaptation Initiative Country Index (database). https://gain.nd.edu/our-work/country-index"
  )
}

fig_sdg13_climate_disasters_gdp <- function() {
  df <- read_xlsx(path = "inputs/sdg13/results_risk_and_resilience.xlsx", sheet = "results", col_names = TRUE)
  
  df <- df %>% 
    select(
      iso3c  = "ISO3",
      value = "risk"
    ) %>%
    mutate(value = value * 100) %>% 
    right_join(wbgref$countries$regions)  
  
  df$bins <- supercut(df$value, c(
    "0.0–0.5" = "[0.0, 0.5)",
    "0.5–1.0" = "[0.5, 1.0)",
    "1.0 and over" = "[1.0, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
    },
    title = "The risk to well-being from natural disasters is greater than narrow measures of asset loss suggest. It falls more heavily on the poor within countries.",
    subtitle = wbg_name(indicator = "Risk to well-being", denom = "% of GDP per year"),
    note = "a. World Bank 2018. http://hdl.handle.net/10986/29461",
    source = "Source: World Bank 2017. http://hdl.handle.net/10986/25335"
  )
}

fig_sdg13_ndcs_by_sector_country <- function() {
  if (!file.exists("inputs/restricted/sdg13/Sectoral mitigation and adaptation.xlsx")) {
    warning("Figure uses restricted data and is not reproducible.")
    return(figure(data = NULL, plot = function(df, style) {grid::textGrob("Data restricted")}))
  }
  
  df <- read_xlsx("inputs/restricted/sdg13/Sectoral mitigation and adaptation.xlsx","Sectoral_updated Nov 2017")
  
  df <- df %>% left_join(wbgref$countries$iso2to3, by = c(CountryCode = "iso2c"))
  df <- df %>% left_join(wbgref$countries$incomegroups)
  
  # Three non WDI countries here:
  # CK = Cook Islands - remove
  # NU = Niue - remove
  # EU = EU - include as high income
  df <- df %>%
    filter(!(CountryCode %in% c("CK", "NU"))) %>%
    mutate(income_iso3c = ifelse(CountryCode == "EU", "HIC", income_iso3c))
  
  # Cross-cutting has multiple spelling variations
  df <- df %>%
    mutate(Sector = ifelse(grepl("cross-cutting", Sector, ignore.case=TRUE), "Cross-cutting", Sector))
  
  df <- df %>%
    mutate(Sector = fct_recode(Sector,
                               `Land use & forestry` = "LULUCF/Forestry",
                               `Coastal zone` = "Coastal Zone",
                               `Disaster risk management` = "Disaster Risk Management (DRM)",
                               `Social development` = "Social Development"
    ))
  
  unique.countries <- df %>%
    select(income_iso3c, iso3c) %>%
    unique() %>%
    group_by(income_iso3c) %>%
    summarise(total = n())
  
  df <- df %>%
    select(income_iso3c, iso3c, Sector, SectorSubSectorType) %>%
    unique() %>%
    group_by(income_iso3c, Sector, SectorSubSectorType) %>%
    summarise(countries = n()) %>%
    left_join(unique.countries) %>%
    mutate(percent = countries / total * 100) %>%
    ungroup()
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(
        SectorSubSectorType = factor(SectorSubSectorType, c("Mitigation","Adaptation")),
        combined_sector = paste0(SectorSubSectorType, "::", Sector)
      )
      df <- df %>% mutate(
        income_iso3c = factor(income_iso3c, rev(wbgref$incomes$iso3c))
      )
      facet_labeller = labeller(
        income_iso3c = function(c) {
          l <- str_wrap_lines(wbgref$incomes$labels[c], 2, force = TRUE)
          counts <- df %>% group_by(income_iso3c) %>% summarise(total = first(total))
          l.counts <- counts$total[match(c, counts$income_iso3c)]
          paste0(l, "\n(", l.counts, ")")
        }
      )
      ggplot(df, aes(fct_reorder2(combined_sector, income_iso3c == "LIC", -percent), countries)) +
        geom_col(aes(y = total), fill = style$colors$neutral, width = 0.75) +
        geom_col(fill = style$colors$spot.primary, width = 0.75) +
        geom_bartext(aes(label = countries), family = style$family, size = style$gg_text_size, color = style$colors$text.inverse) +
        scale_x_discrete(labels = function (l) {sapply(strsplit(l, "::", fixed=TRUE), last)}) +
        coord_flip() +
        facet_grid(
          SectorSubSectorType ~ income_iso3c,
          scales = "free", space = "free",
          labeller = facet_labeller,
          switch = "y"
        ) +
        style$theme() +
        style$theme_barchart() +
        theme(
          strip.placement = "outside",
          strip.text.y = element_text(angle = 180, vjust = 1),
          strip.text.x = element_text(hjust = 0.5),
          panel.spacing.y = unit(0.15, "npc"),
          axis.text.x = element_blank(),
          panel.grid = element_blank())
    },
    aspect_ratio = 1,
    title = "Under the Paris Agreement, countries make commitments to reduce emission (mitigation) and manage the adverse impacts of climate change (adaptation).",
    subtitle = wbg_name(indicator = "Number of countries with a commitment", by = "by sector and income group"),
    source = "Source: World Bank Intended Nationally Determined Contributions (database). http://indc.worldbank.org.",
    note = paste("Note: Totals shown for each income group reflect the number of countries that have submitted Intended National Determine Contributions. As the European Union is a party to the agreement in its own right, it is counted as a single high-income country. a. UNFCCC NDC Registry (interim).")
  )  
}

# make_all(path = "docs/sdg13/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg13", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg13_co2_emissions_by_income(), styler, file.path(path, "fig_sdg13_co2_emissions_by_income.png"), width = 2.4, height = 2.4, padding = margin(0,0,0,1,"mm"))
  saver(fig_sdg13_co2_ppm_mauna_loa(), styler, file.path(path, "fig_sdg13_co2_ppm_mauna_loa.png"), width = 2.94, height = 2.4)
  saver(fig_sdg13_co2_pop_pc_vs_absolute(), styler, file.path(path, "fig_sdg13_co2_pop_pc_vs_absolute.png"), width = 5.5, height = 4.25)
  
  # page 2
  saver(fig_sdg13_co2_rcp_scenarios(), styler, file.path(path, "fig_sdg13_co2_rcp_scenarios.png"), width = 5.5, height = 4.15)

  # page 3
  saver(fig_sdg13_vulnerability_vs_readiness(), styler, file.path(path, "fig_sdg13_vulnerability_vs_readiness.png"), width = 5.5, height=4.1)
  saver(fig_sdg13_climate_disasters_gdp(), styler, file.path(path, "fig_sdg13_climate_disasters_gdp.png"), width = 5.5, height=4.6)
  
  # page 4
  saver(fig_sdg13_ndcs_by_sector_country(), styler, file.path(path, "fig_sdg13_ndcs_by_sector_country.png"), width = 5.5, height = 8.5, padding = margin(0, 0,0.5,0,"in"))
}
