library(ggplot2)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(ggmosaic)
library(ggtreemap)
library(stringr)
library(dplyr)
library(tidyr)
library(readxl)
library(countrycode)
library(forcats)
source("styles.R")

fig_sdg7_people_without_electricity <- function(years = c(1990, 2016)) {
  indicator = "EG.ELC.ACCS.ZS"
  df <- wbgdata(
    country = wbgref$countries$iso3c,
    indicator = indicator,
    years = years,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg7_people_without_electricity.csv"
  )
  
  df.pop <- wbgdata(
    country = wbgref$countries$iso3c,
    indicator = "SP.POP.TOTL",
    years = years,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg7_people_without_electricity-pop.csv"
  )

  df <- df %>% inner_join(df.pop)
  
  df$no_access <- (100 - df[[indicator]])/100 * df$SP.POP.TOTL
  
  df <- df %>% left_join(wbgref$countries$regions)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), aspect_ratio = 0.75) {
      df_max <- df %>%
        group_by(iso3c) %>%
        summarise(max = max(no_access)) %>%
        filter(!is.na(max)) # Drop countries with NA in any year
      df <- df %>% inner_join(df_max)
      
      ggplot(df, aes(area = no_access, layout_area = max, fill = region_iso3c, subgroup = region_iso3c)) +
        geom_rect(aes(area = max), stat = "treemap", aspect.ratio = aspect_ratio * 2, color = NA, alpha = 0.25) +
        geom_rect(stat = "treemap", aspect.ratio = aspect_ratio * 2, color = NA) +
        geom_rect(aes(area = max), stat = "treemap", aspect.ratio = aspect_ratio * 2, color = "white", fill=NA, size = 0.35) +
        geom_text(
          aes(
            label = ifelse(max > 5e6, str_wrap(wbgref$countries$labels[iso3c],11), ""),
            size = cut(max,c(0,25,50,Inf)*1e6),
            color = region_iso3c
          ),
          stat = "treemap", aspect.ratio = aspect_ratio * 2,
          family = style$theme()$text$family,
          lineheight = 0.9
        ) +
        scale_size_manual(values = c(0.6,0.9,1.1)*style$gg_text_size, guide = "none") +
        scale_y_reverse(expand = c(0, 0)) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_fill_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
        scale_color_manual(values = contrasting_colors(style$colors$regions, c(style$colors$text, style$colors$text.inverse)), guide = "none") +
        facet_wrap(~date, nrow = 2) +
        guides(fill = guide_legend(nrow = 2, direction = "horizontal")) +
        style$theme() +
        style$theme_legend("top") +
        theme(axis.text = element_blank(), panel.grid = element_blank(),
              strip.text = element_text(size = rel(1.2), face = "bold"),
              panel.spacing.y = unit(0.1, "npc"))
    },
    aspect_ratio = 0.80,
    title = "Population growth has outpaced infrastructure development in Sub-Saharan Africa, where more people now live without electricity than in 1990.",
    subtitle = paste0("People without access to electricity, ",min(years)," and ",max(years)),
    source = "Source: World Bank. World Development Indicators (EG.ELC.ACCS.ZS; SP.POP.TOTL)."
  )
}

fig_sdg7_people_without_clean_fuels <- function(year = 2016, cutoff_population = 10e6, cutoff_access = 95) {
  indicators <- c("EG.CFT.ACCS.ZS", "SP.POP.TOTL")
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg7_people_without_clean_fuels.csv"
  )
  
  df <- df %>% mutate(people_without = SP.POP.TOTL * (100 - EG.CFT.ACCS.ZS)/100)
  df <- df %>% arrange(EG.CFT.ACCS.ZS)
  df <- df %>% left_join(wbgref$countries$regions)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "high", aspect_ratio = 1) {
      df <- df[complete.cases(df),]
      df <- df %>%
        filter(EG.CFT.ACCS.ZS <= cutoff_access) %>%
        filter(SP.POP.TOTL >= cutoff_population)
      df$xmin <- cumsum(df$SP.POP.TOTL) - df$SP.POP.TOTL
      df$xmax <- cumsum(df$SP.POP.TOTL)
      
      labels <- df %>%
        filter(people_without > 25e6 | SP.POP.TOTL > 50e6) %>%
        transmute(x = (xmin  + xmax)/2, label = wbgref$countries$labels[iso3c])
      p <- ggplot(df, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 100 - EG.CFT.ACCS.ZS, fill = region_iso3c)) +
        geom_rect(color = "white", size = 0.2) +
        scale_fill_manual(
          values = style$colors$regions,
          labels = wbgref$regions$labels, #str_wrap_lines(wbgref$regions$labels, 2),
          guide = guide_legend(ncol = 2)) +
        scale_x_reverse(breaks = labels$x, labels = labels$label, expand = c(0.01, 0)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
        ylab("People without access (% of population)") +
        xlab("Countries, scaled by total population") +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("bottom") +
        theme(axis.title.x.top = element_blank(),
              axis.title.x = element_text(margin = margin(1,0,0,0,"lines")),
              axis.title.y = element_text(angle = 90),
              axis.text.y = element_text(margin = margin(r = 0), size = rel(0.9)),
              legend.text = element_text(size = rel(0.9))
        )
      
      # Align legend relative to whole figure not just plot area
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 0.5,
    title = "Worldwide, 3 billion people lack access to clean cooking fuels, instead using fuels that create health risks.",
    subtitle = wbg_name(indicator = "People without access to clean fuels and technologies for cooking", year = year),
    source = "Source: WHO. WDI (EG.CFT.ACCS.ZS; SP.POP.TOTL).",
    note = paste0("Note: Excludes countries with a population less than ", millions()(cutoff_population), " million or an access rate above ", ones()(cutoff_access), " percent")
  )
}

fig_sdg7_growth_in_access_elec_clean_fuel <- function(years = c(2000, 2016)) {
  indicators <- c("EG.ELC.ACCS.ZS", "EG.CFT.ACCS.ZS")
  df <- wbgdata(
    wbgref$regions$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg7_growth_in_access_elec_clean_fuel.csv"
  )
  
  df <- df %>% filter(date %in% years)
  df <- df %>% arrange(iso3c, indicatorID, date)
  iso3c_order <- df %>%
    filter(date %in% min(years), indicatorID == "EG.CFT.ACCS.ZS") %>%
    arrange(value) %>%
    pull(iso3c)

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = factor(iso3c, iso3c_order))
      facet_labeller <- as_labeller(function(l) {wbgref$regions$labels[l]})
      p <- ggplot(df, aes(x = value, y = indicatorID, color = as.factor(date), fill = as.factor(date), shape = as.factor(date))) +
        geom_other_dotplot(
          aes(value, indicatorID, group = paste0(indicatorID, iso3c)),
          arrow = TRUE,
          size = style$point_size,
          stroke = style$point_stroke
        ) +
        scale_y_discrete(labels = c("Clean fuels", "Electricity")) +
        scale_x_continuous(limits = c(0, 100)) +
        scale_shape_manual(values = c(style$shapes$point, 99)) +
        scale_color_manual(values = c(style$colors$spot.primary.light, style$colors$spot.primary)) +
        scale_fill_manual(values = c(style$colors$spot.primary.light, style$colors$spot.primary)) +
        facet_wrap(~ iso3c, ncol = 1, labeller = facet_labeller) +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") +
        theme(
          strip.text = element_text(hjust = 0),
          strip.placement = "inside",
          panel.spacing.y = unit(0.03, "npc")
        )
      p
    },
    aspect_ratio = 0.33,
    title = "In South Asia and Sub-Saharan Africa, gains in access to clean fuels have not kept up with those in electricity",
    subtitle = wbg_name(indicator = "Access rates", denom = "% of population", year = paste(min(years), "and", max(years))),
    source = "Source: World Bank, WHO. WDI (EG.ELC.ACCS.ZS; EG.CFT.ACCS.ZS)."
  )
}

fig_sdg7_renew_share_TFEC <- function(year = 2015) {
  indicator <- "EG.FEC.RNEW.ZS"
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year,
    removeNA = FALSE,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg7_renew_share_TFEC.csv"
  )
  
  df$bins <- supercut(df$value, c(
    "0–10"   = "[0,  10)",
    "10–40"  = "[10, 40)",
    "40 and over"  = "[40, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.5,
    title = "Renewable energy accounts for a large share of energy consumption in Sub-Saharan Africa, but that often reflects burning of biomass in traditional ways in open fires.",
    subtitle = wbg_name(indicator, year = year),
    source = "Source: IEA, UNSD. World Development Indicators (EG.FEC.RNEW.ZS)."
  )  
}

fig_sdg7_energy_sources_by_income_group <- function(year = 2015) {
  if (!file.exists("inputs/restricted/sdg7/Modern RE by income.xlsx")) {
    warning("Figure uses restricted data and is not reproducible.")
    return(figure(data = NULL, plot = function(df, style) {grid::textGrob("Data restricted")}))
  }
  
  df.mr <- read_xlsx("inputs/restricted/sdg7/Modern RE by income.xlsx", "Modern RE")
  df.mr <- df.mr %>%
    select(-Income, -Country) %>%
    rename(iso3c = Code) %>%
    gather(date, modern_renewables_TJ, `1990`:`2015`, convert = TRUE) %>%
    mutate(modern_renewables_TJ = ifelse(modern_renewables_TJ == 0.0, NA, modern_renewables_TJ))
  
  df.tfec <- read_xlsx("inputs/restricted/sdg7/Modern RE by income.xlsx", "TFEC")
  df.tfec <- df.tfec %>%
    select(-Income, -Country) %>%
    rename(iso3c = Code) %>%
    gather(date, tfec_TJ, `1990`:`2015`, convert = TRUE) %>%
    mutate(tfec_TJ = ifelse(tfec_TJ == 0.0, NA, tfec_TJ))

  df.afec <- read_xlsx("inputs/restricted/sdg7/Modern RE by income.xlsx", "AFEC")
  df.afec <- df.afec %>%
    select(-X__1) %>%
    rename(iso3c = `Unit: TJ`) %>%
    gather(date, afec_TJ, `1990`:`2015`, convert = TRUE) %>%
    mutate(afec_TJ = ifelse(afec_TJ == 0.0, NA, afec_TJ))
  
  df <- df.mr %>% inner_join(df.tfec) %>% inner_join(df.afec)
  df <- df %>%
    filter(iso3c != "WLD") %>% # Remove world
    mutate(
      biomass_TJ = afec_TJ - modern_renewables_TJ,
      nonrenew_TJ = tfec_TJ - afec_TJ
    ) %>%
    select(-afec_TJ, -tfec_TJ) %>%
    gather(source, value, modern_renewables_TJ, nonrenew_TJ, biomass_TJ) %>%
    filter(complete.cases(.)) %>%
    left_join(wbgref$countries$incomegroups) %>%
    select(-iso3c) %>%
    group_by(income_iso3c, date, source) %>%
    summarise(value = sum(value)) %>%
    rename(iso3c = income_iso3c)

  df <- df %>% filter(date == year)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df$iso3c <- factor(df$iso3c, wbgref$incomes$iso3c)
      df$source <- factor(df$source, c("modern_renewables_TJ", "biomass_TJ", "nonrenew_TJ"))
      ggplot(df, aes(weight=value, x=product(iso3c), fill=source)) +
        geom_mosaic(na.rm = TRUE, offset = 0.005, alpha = 1) +
        scale_fill_manual(
          values = c(style$colors$spot.primary, style$colors$spot.primary.light, style$colors$spot.secondary),
          labels = c("Modern renewables", "Traditional biomass", "Nonrenewables"),
          guide = guide_legend(reverse = TRUE)  
        ) +
        scale_y_continuous(expand = c(0,0), label = function(x) (x*100)) +
        scale_x_productlist(labels = str_wrap_lines(wbgref$incomes$labels,3)) +
        coord_cartesian(xlim=c(0.05/1.1, 1.05/1.1)) +
        xlab("Scaled by total final energy consumption") +
        style$theme() +
        style$theme_x_title() + 
        style$theme_legend("righttop") +
        theme(
          #axis.text.y = element_blank(),
          panel.grid = element_blank()
        )
    },
    aspect_ratio = 1.25,
    title = "Modern renewables still make a modest contribution, across all income groups.",
    subtitle = wbg_name(indicator = "Global total final energy consumption", by = "by income group and source", denom = "% of income group total", year = year),
    source = "Source: IEA, UNSD, Global Tracking Framework. http://gtf.esmap.org/"
  )
}

fig_sdg7_modern_renewable_by_income <- function() {
  # This figure uses restricted data which we cannot redistribute
  if (!file.exists("inputs/restricted/sdg7/Modern RE by income.xlsx")) {
    warning("Figure uses restricted data and is not reproducible.")
    return(figure(data = NULL, plot = function(df, style) {grid::textGrob("Data restricted")}))
  }
  
  df.mr <- read_xlsx("inputs/restricted/sdg7/Modern RE by income.xlsx", "Modern RE")
  df.mr <- df.mr %>%
    select(-Income, -Country) %>%
    rename(iso3c = Code) %>%
    gather(date, modern_renewables_TJ, `1990`:`2015`, convert = TRUE) %>%
    mutate(modern_renewables_TJ = ifelse(modern_renewables_TJ == 0.0, NA, modern_renewables_TJ))
  
  df.tfec <- read_xlsx("inputs/restricted/sdg7/Modern RE by income.xlsx", "TFEC")
  df.tfec <- df.tfec %>%
    select(-Income, -Country) %>%
    rename(iso3c = Code) %>%
    gather(date, tfec_TJ, `1990`:`2015`, convert = TRUE) %>%
    mutate(tfec_TJ = ifelse(tfec_TJ == 0.0, NA, tfec_TJ))
  
  df <- df.mr %>% inner_join(df.tfec)
  
  df <- df %>% mutate(iso3c = ifelse(iso3c == "KSV", "XKX", iso3c))

  # Match to income groups  
  df <- df %>% left_join(wbgref$countries$incomegroups)
  
  # Remove any missing values (income group, renewable or TFEC)
  df <- df %>% filter(complete.cases(.))
  
  # Aggregate using only countries / years with values for both components
  df <- df %>%
    group_by(income_iso3c, date) %>%
    summarise(
      tfec_TJ = sum(tfec_TJ),
      modern_renewables_TJ = sum(modern_renewables_TJ)
    ) %>%
    mutate(modern_re_share_tfec = modern_renewables_TJ / tfec_TJ)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df$income_iso3c <- factor(df$income_iso3c, wbgref$incomes$iso3c)
      ggplot(df, aes(x = date, y = modern_re_share_tfec*100, color = income_iso3c)) +
        geom_line(size = style$linesize) +
        scale_color_manual(values = style$colors$incomes, labels = wbgref$incomes$labels) +
        scale_x_continuous(breaks = bracketed_breaks(limits = df$date)) +
        scale_y_continuous(limits = c(0, 15)) +
        style$theme() +
        style$theme_legend("rightbottom")
    },
    aspect_ratio = 2,
    title = "But the share of modern renewables has been growing.",
    subtitle = wbg_name(indicator = "Modern renewable energy consumption", denom = "% of total final energy consumption"),
    source = "Source: IEA, UNSD, Global Tracking Framework. http://gtf.esmap.org/"
  )
}

fig_sdg7_energy_intensity <- function(year = 2015) {
  indicator <- "EG.EGY.PRIM.PP.KD"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year,
    removeNA = FALSE,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg7_energy_intensity.csv"
  )
  
  df$bins <- supercut(df$value, c(
    "0–5"   = "[0,  5)",
    "5–10"  = "[5, 10)",
    "10 and over"  = "[10, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.5,
    title = "The amount of energy used to produce one dollar's worth of goods and services varies around the world.",
    subtitle = wbg_name(indicator = "Energy intensity of primary energy", denom = "MJ/2011 PPP$ GDP", year = year),
    source = "Source: IEA, UNSD, World Bank. World Development Indicators (EG.EGY.PRIM.PP.KD)."
  )  
}

fig_sdg7_energy_intensity_change_by_region <- function(years = c(1990, 2015)) {
  indicator = c("EG.EGY.PRIM.PP.KD")
  
  df <- wbgdata(
    wbgref$regions$iso3c,
    indicator,
    years = years,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg7_energy_intensity_change_by_region.csv"
  )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(EG.EGY.PRIM.PP.KD, iso3c,color = as.factor(date), fill = as.factor(date), shape = as.factor(date))) +
        geom_other_dotplot(
          aes(EG.EGY.PRIM.PP.KD, iso3c, group = iso3c),
          arrow = TRUE,
          flip.legend = TRUE,
          size = style$point_size,
          stroke = style$point_stroke
        ) +
        scale_y_discrete(labels = wbgref$regions$labels) +
        scale_color_manual(
          values = c(style$colors$spot.primary.light, style$colors$spot.primary),
          guide = guide_legend(reverse = TRUE)
        ) +
        scale_fill_manual(
          values = c(style$colors$spot.primary.light, style$colors$spot.primary),
          guide = guide_legend(reverse = TRUE)
        ) +
        scale_shape_manual(
          values = c(style$shapes$point, 99),
          guide = guide_legend(reverse = TRUE)
        ) +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("topleft")
    },
    aspect_ratio = 1,
    title = "Energy intensity has fallen everywhere but the Middle East & North Africa.",
    subtitle = wbg_name(indicator = "Energy intensity of primary energy", denom = "MJ/2011 PPP$ GDP"),
    source = "Source: IEA, UNSD, World Bank. WDI (EG.EGY.PRIM.PP.KD)."
  )  
}

fig_sdg7_energy_intensity_by_sector <- function(years = 1990:2014) {
  # This figure uses restricted data which we cannot redistribute
  if (!file.exists("inputs/restricted/sdg7/EI by sector 1990 - 2015.xlsx")) {
    warning("Figure uses restricted data and is not reproducible.")
    return(figure(data = NULL, plot = function(df, style) {grid::textGrob("Data restricted")}))
  }
  
  df <- read_xlsx("inputs/restricted/sdg7/EI by sector 1990 - 2015.xlsx", skip = 1)
  
  colnames(df)[1] <- "sector"
  
  df <- gather(df, "date", "value", -sector, convert = TRUE)
  df <- df %>% filter(date %in% years)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(date, value, color = sector)) +
        geom_line(size = style$linesize) +
        scale_color_manual(values = style$colors$categorical) +
        scale_y_continuous(limits = c(0, 6), breaks = c(0,2,4,6), sec.axis = dup_axis(
          breaks = df %>% filter(date == max(date)) %>% pull(value),
          labels = df %>% filter(date == max(date)) %>% pull(sector)
        )) +
        scale_x_continuous(expand = c(0,0), breaks = bracketed_breaks(df$date)) +
        style$theme()
    },
    aspect_ratio = 1,
    title = "And globally, energy intensity has fallen in all sectors.",
    subtitle = wbg_name(indicator = "Energy intensity", denom = "MJ/2011 PPP$ GDP"),
    source = "Source: IEA, UNSD, World Bank. http://gtf.esmap.org/"
  )
}

# make_all(path = "docs/sdg7/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg7", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg7_people_without_electricity(), styler, file.path(path, "fig_sdg7_people_without_electricity.png"), width = 5.5)

  # page 2
  saver(fig_sdg7_people_without_clean_fuels(), styler, file.path(path, "fig_sdg7_people_without_clean_fuels.png"), width = 2.67, height = 8.5)
  saver(fig_sdg7_growth_in_access_elec_clean_fuel(), styler, file.path(path, "fig_sdg7_growth_in_access_elec_clean_fuel.png"), width = 2.67, height = 8.5)

  # page 3
  saver(fig_sdg7_renew_share_TFEC(), styler, file.path(path, "fig_sdg7_renew_share_TFEC.png"), width = 5.5, height = 4.25)
  saver(fig_sdg7_energy_sources_by_income_group(), styler, file.path(path, "fig_sdg7_energy_sources_by_income_group.png"), width = 5.5, height = 2.25)
  saver(fig_sdg7_modern_renewable_by_income(), styler, file.path(path, "fig_sdg7_modern_renewable_by_income.png"), width = 5.5, height = 2)
  
  # page 4
  saver(fig_sdg7_energy_intensity(), styler, file.path(path, "fig_sdg7_energy_intensity.png"), width = 5.5, height = 4.25)
  saver(fig_sdg7_energy_intensity_change_by_region(), styler, file.path(path, "fig_sdg7_energy_intensity_change_by_region.png"), width = 2.67, height = 4.25)
  saver(fig_sdg7_energy_intensity_by_sector(), styler, file.path(path, "fig_sdg7_energy_intensity_by_sector.png"), width = 2.67, height = 4.25)
}
