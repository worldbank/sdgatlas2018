library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(forcats)
library(readxl)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(countrycode)
library(gtable)
library(grid)
source("styles.R")

fig_sdg9_rai_pop_no_access <- function() {
  df <- read_csv("inputs/sdg9/Rural access index by country.csv")
  
  df <- df %>% gather(indicator, value, RAI, no_access_pop)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = fct_reorder2(iso3c, indicator == "RAI", -value))
      
      facet_labeller = as_labeller(c(
        RAI = "People with access (% of rural population)",
        no_access_pop = "People without access (millions)"
      ))
      
      p.rai <- ggplot(
        df %>% filter(indicator == "RAI"),
        aes(iso3c, value)
      ) +
        geom_col(fill = style$colors$spot.primary) +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
        coord_flip() +
        facet_wrap(~ indicator, labeller = facet_labeller) +
        style$theme() +
        style$theme_barchart() +
        theme(strip.text.x = element_text(hjust = 0.5))

      p.pop <- ggplot(
        df %>% filter(indicator == "no_access_pop"),
        aes(iso3c, value)
      ) +
        geom_col(fill = style$colors$spot.secondary) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() +
        facet_wrap(~ indicator, labeller = facet_labeller) +
        style$theme() +
        style$theme_barchart() +
        theme(
          axis.text.y = element_blank(),
          strip.text.x = element_text(hjust = 0.5))
      
      pt.rai <- ggplotGrob(p.rai)
      pt.pop <- ggplotGrob(p.pop)

      chart <- gtable_row(
        "chart",
        list(pt.rai, pt.pop),
        height = unit(1, "null"),
        widths = unit(c(1,1), "null"))
      chart$theme <- style$theme()
      chart
    },
    title = "Infrastructure supports communities. Without access to an all-season road, people are cut off from crucial services and markets.",
    subtitle = wbg_name(indicator = "Access to an all-season road, within 2 km", mrv = df$date),
    note = "Note: As yet, this indicator has only been calculated for these 10 countries.",
    source = "Source: World Bank 2016. http://hdl.handle.net/10986/25187"
  )
}

fig_sdg9_sector_share_multiples <- function(years = 2000:2016) {
  indicators <- c(
    Manufacturing = "NV.IND.MANF.ZS",
    Industry = "NV.IND.TOTL.ZS", 
    Agriculture = "NV.AGR.TOTL.ZS",
    Services = "NV.SRV.TETC.ZS",
    GDPpc = "NY.GDP.PCAP.KD"
  )
  
  ldc <- read_excel("inputs/reference_data/CLASS.xls", sheet = "Groups") %>%
    subset(GroupName == "Least developed countries: UN classification") %>%
    pull(CountryCode)
  
  df <- wbgdata(
    c(ldc,"WLD"),
    indicators,
    years = years,
    rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg9_sector_share_multiples.csv"
  )
  
  df <- df %>%
    mutate(OtherIndustry = Industry - Manufacturing) %>%
    select(-Industry) %>%
    mutate(
      Agriculture = Agriculture / 100 * GDPpc,
      Manufacturing = Manufacturing / 100 * GDPpc,
      OtherIndustry = OtherIndustry / 100 * GDPpc,
      Services = Services / 100 * GDPpc
    )
  
  df <- df %>%
    mutate(has_breakdown = complete.cases(.)) %>%
    mutate(
      Agriculture = ifelse(has_breakdown, Agriculture, NA),
      Manufacturing = ifelse(has_breakdown, Manufacturing, NA),
      OtherIndustry = ifelse(has_breakdown, OtherIndustry, NA),
      Services = ifelse(has_breakdown, Services, NA)
    ) %>%
    gather("sector", "value", Agriculture, Manufacturing, OtherIndustry, Services, GDPpc)
  
  # Filter to countries with complete GDPpc at least
  iso3c_filter1 <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(sector == "GDPpc") %>%
    summarise(start = min(date), end = max(date)) %>%
    filter(start == min(years), end == max(years)) %>%
    pull(iso3c)
  
  # Secondary filter, for reasonable breakdown coverage
  iso3c_filter2 <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(has_breakdown) %>%
    summarise(start = min(date), end = max(date)) %>%
    filter(end-start+1 >= 5, end > 2005) %>%
    pull(iso3c)
  
  iso3c_order <- intersect(iso3c_filter1, iso3c_filter2)
  iso3c_order <- iso3c_order[order(wbgref$countries$labels[iso3c_order])]
  
  df <- df %>%
    filter(iso3c %in% iso3c_order) %>%
    mutate(
      iso3c = factor(iso3c, iso3c_order),
      sector = factor(sector, rev(c("GDPpc", "Manufacturing", "OtherIndustry", "Services", "Agriculture")))
    )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      p <- ggplot(df, aes(date, value)) +
        geom_area(data = . %>% filter(sector == "GDPpc"), aes(fill = sector)) +
        geom_area(data = . %>% filter(sector != "GDPpc"), aes(fill = sector)) +
        geom_text(
          aes(label = ones(0)(value)),
          . %>% filter(sector == "GDPpc") %>% group_by(iso3c) %>% filter(date == max(date)) %>% summarise(date = max(date), value = sum(value)),
          family = style$family,
          size = style$gg_text_size*0.8,
          color = "grey60",
          hjust = 0,
          vjust = 0.5,
          nudge_x = 0.5
        ) +
        facet_wrap(
          ~ iso3c,
          scales = "free",
          labeller = as_labeller(str_wrap_lines(c(wbgref$countries$labels, WLD="World"),2)),
          strip.position = "bottom",
          ncol = 5) +
        scale_x_continuous(breaks = range(years), limits = c(1999,2017)) +
        scale_fill_manual(
          values = c(
            Manufacturing = style$colors$spot.primary,
            OtherIndustry = style$colors$spot.primary.light,
            Services = style$colors$spot.secondary.light,
            Agriculture = style$colors$spot.secondary,
            GDPpc = style$colors$neutral
          ),
          labels = c(
            Manufacturing = "Manufacturing",
            OtherIndustry = "Other industry",
            Services = "Services",
            Agriculture = "Agriculture",
            GDPpc = "Total (breakdown not available)"
          ),
          guide = guide_legend(ncol = 1),
          drop = FALSE
        ) +
        style$theme() +
        theme(
          axis.text.y = element_blank(),
          panel.grid = element_blank(),
          axis.text.x = element_blank(),
          strip.text.x = element_text(hjust = 0.5, vjust = 1, margin = margin(0.2,0,0.1,0,"lines")),
          strip.placement = "inside",
          panel.spacing.y = unit(0.02, "npc"),
          panel.spacing.x = unit(0.03, "npc"),
          legend.position = c(0.97, -0.015),
          legend.justification = c(1, 0)
        )
      
      # Turn off clipping
      g <- ggplotGrob(p)
      g$layout$clip[grepl("panel",g$layout$name)] <- "off"
      g$theme <- p$theme
      g
    },
    title = "Manufacturing and other industry is a large source of employemnt. But many Least Developed Countries have a small manufacturing sector.",
    subtitle = wbg_name(indicator = "GDP per capita", by = "by sector value added", year = str_range(years, short = c(2,2)), denom = "constant 2010 US$, each country scaled independently"),
    note = "Note: Includes Least Developed Countries (UN classification) with complete GDP per capita data and at least five years of sector value-added data.",
    source = "Source: World Bank and OECD. WDI (NV.IND.MANF.ZS; NV.IND.TOTL.ZS; NV.AGR.TOTL.ZS; NV.SRV.TETC.ZS; NY.GDP.PCAP.KD)."
  )
}

fig_sdg9_hitech_map <- function(year = 2015) {
  indicators <- c("NV.MNF.TECH.ZS.UN")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg9_hitech_map.csv"
  )
  
  df <- df %>% mutate(NV.MNF.TECH.ZS.UN = NV.MNF.TECH.ZS.UN * 100)
  
  df <- df %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()
  
  df <- df %>% right_join(wbgref$countries$regions)

  df$bins <- supercut(df$NV.MNF.TECH.ZS.UN, c(
    "0–15" = "[0,15)",
    "15–30" = "[15,30)",
    "30 and over" = "[30,Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style = style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.3,
    title = "Medium- and high-tech industry allows for greater diversification and offers better opportunities for skills development and innovation.",
    subtitle = wbg_name(indicators),
    source = "Source: UNIDO. World Development Indicators (NV.MNF.TECH.ZS.UN)."
  )
}

fig_sdg9_patents <- function(years = 1960:2016, num_countries = 6) {
  indicators <- c("IP.PAT.RESD", "SP.POP.TOTL")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = years,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg9_patents.csv"
  )
  
  df <- df %>% mutate(patent_per_100k = IP.PAT.RESD / (SP.POP.TOTL / 1e5))
  
  countries <- df %>%
    filter(date == max(years)) %>% 
    top_n(num_countries, patent_per_100k) %>% 
    arrange(-patent_per_100k) %>% 
    pull(iso3c)
  
  df <- df %>% filter(iso3c %in% countries)
  
  df <- df %>% gather("indicator", "value", patent_per_100k, IP.PAT.RESD)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      facet_labeller = as_labeller(c(
        patent_per_100k = "Per 100,000 people",
        IP.PAT.RESD = "Total, millions"
      ))
      
      p.rate <- ggplot(df %>% filter(indicator == "patent_per_100k"), aes(x = date, y = value, color = factor(iso3c, levels = countries))) +
        geom_line(size = style$linesize) +
        scale_x_continuous(expand = c(0, 0), breaks = c(range(years), 1980, 2000)) +
        scale_color_manual(
          values = style$colors$categorical,
          labels = wbgref$countries$labels
        ) +
        scale_y_continuous(sec.axis = dup_axis(
          breaks = df %>% filter(indicator == "patent_per_100k", date == max(date)) %>% pull(value) %>% repel(10),
          labels = wbgref$countries$labels[df %>% filter(indicator == "patent_per_100k", date == max(date)) %>% pull(iso3c)]
        )) +
        facet_wrap(~ indicator, labeller = facet_labeller) +
        style$theme()
      
      p.abs <- ggplot(df %>% filter(indicator == "IP.PAT.RESD"), aes(x = date, y = value, color = factor(iso3c, levels = countries))) +
        geom_line(size = style$linesize) +
        scale_x_continuous(expand = c(0, 0), breaks = c(range(years), 1980, 2000)) +
        scale_y_continuous(labels = millions(), sec.axis = dup_axis(
          breaks = df %>% filter(indicator == "IP.PAT.RESD", date == max(date)) %>% pull(value) %>% repel(0.02*1e6),
          labels = wbgref$countries$labels[df %>% filter(indicator == "IP.PAT.RESD", date == max(date)) %>% pull(iso3c)]
        )) +
        scale_color_manual(
          values = style$colors$categorical,
          labels = wbgref$countries$labels
        ) +
        facet_wrap(~ indicator, labeller = facet_labeller) +
        style$theme()
      
      pt.rate <- ggplotGrob(p.rate)
      pt.abs <- ggplotGrob(p.abs)
      
      chart <- gtable_row(
        "chart",
        list(pt.rate, zeroGrob(), pt.abs),
        height = unit(1, "null"),
        widths = unit.c(unit(1, "null"), unit(0.25, "in"), unit(1, "null"))
      )
      chart$theme <- style$theme()
      chart
    },
    title = "Patents are designed to encourage innovation by incentivizing research and development.",
    subtitle = wbg_name("IP.PAT.RESD", by = "top six countries in 2016"),
    source = "Source: WIPO. World Development Indicators (IP.PAT.RESD; SP.POP.TOTL)."
  )
}

# make_all(path = "docs/sdg9/pdf", styler=style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg9", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg9_rai_pop_no_access(), styler, file.path(path, "fig_sdg9_rai_pop_no_access.png"), width = 5.5, height = 3)
  
  # page 3
  saver(fig_sdg9_sector_share_multiples(), styler, file.path(path, "fig_sdg9_sector_share_multiples.png"), width = 5.5, height = 8.5, padding = margin(0.2,0.15,0,0,"in"))

  # page 4
  saver(fig_sdg9_hitech_map(), styler, file.path(path, "fig_sdg9_hitech_map.png"), width = 5.5, height = 4.25)
  saver(fig_sdg9_patents(), styler, file.path(path, "fig_sdg9_patents.png"), width = 5.5, height = 4.25)
}
