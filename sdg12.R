library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(forcats)
library(countrycode)
library(stringr)
library(gtable)
library(readr)
source("styles.R")

fig_sdg12_mf_pc <- function(year = 2010) {
  df <- read_csv("inputs/sdg12/unstats_sdgs_12.2.1.csv")
  
  # Select what we need, tidy and make long
  df <- df %>%
    filter(`Series Description` == "Material footprint per capita") %>%
    rename(iso3c = `Country or Area Code`) %>%
    select(iso3c, matches("[0-9]{4}")) %>%
    gather("date", "value", -iso3c, convert = TRUE) %>%
    filter(date == year)
  
  # Remove area codes, which are longer than 3 letters.
  # Everything else aligns with a WDI code.
  df <- df %>% filter(nchar(iso3c) == 3)
  
  # Join region codes to make sure all countries present
  df <- df %>% right_join(wbgref$countries$regions)

  df$bins <- supercut(df$value, c(
    "0–5" = "[0, 5)",
    "5–10" = "[5, 10)",
    "10–25" = "[10, 25)",
    "25 and over" = "[25, Inf)"
  ))
  
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    title = "Material footprints measure the demand for extracted materials used by households, governments, and businesses. They are smaller in low-income countries than in richer countries, which consume more.",
    # title = "The material footprint per person is considerably smaller in low-income countries compared with wealthier countries which consume more.",
    subtitle = wbg_name(indicator = "Material footprint", denom = "metric tons per capita", year = year),
    source = "Source: UNEP (database). https://unstats.un.org/sdgs/indicators/database?indicator=12.2.1"
  )
}

fig_sdg12_mf_region <- function() {
  country_breakout = c("CHN", "USA")
  
  df <- read_csv("inputs/sdg12/unstats_sdgs_12.2.1.csv")
  
  # Select what we need, tidy and make long
  df <- df %>%
    filter(`Series Description` == "Material footprint") %>%
    rename(iso3c = `Country or Area Code`) %>%
    select(iso3c, matches("[0-9]{4}")) %>%
    gather("date", "value", -iso3c, convert = TRUE)
  
  # Remove area codes, which are longer than 3 letters.
  # Everything else aligns with a WDI code.
  df <- df %>% filter(nchar(iso3c) == 3)
  
  # Join region codes and break out countries, otherwise aggregate to regions
  df <- df %>% left_join(wbgref$countries$regions)
  df <- df %>%
    mutate(iso3c = ifelse(iso3c %in% country_breakout, iso3c, region_iso3c)) %>%
    group_by(region_iso3c, iso3c, date) %>%
    summarise(value = sum(value, na.rm = TRUE))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df.region <- df %>%
        group_by(region_iso3c, date) %>%
        summarise(region_total = sum(value))
      
      df <- df %>% left_join(df.region)
      iso3c_order <- df %>%
        filter(date == max(date)) %>%
        arrange(region_total, value) %>%
        pull(iso3c)
      
      ggplot(df, aes(date, value, fill = factor(iso3c, iso3c_order))) +
        geom_area(stat = "identity") +
        scale_y_continuous(labels = billions()) +
        scale_x_continuous(breaks = seq(min(df$date), max(df$date), 2),
                           expand = c(0,0)) +
        scale_fill_manual(
          values = c(
            "USA" = unname(style$colors$regions.light["NAC"]), 
            "CHN" = unname(style$colors$regions.light["EAS"]),
            style$colors$regions
          ),
          labels = c(
            wbgref$all_geo$labels,
            "NAC" = "North America (excluding United States)", 
            "EAS" = "East Asia & Pacific (excluding China)"
          )
        ) +
        style$theme() + 
        style$theme_legend("right")
      
    },
    title = "China's material footprint increased three-fold between 2000 and 2010, overtaking that of the United States in 2003.",
    subtitle = wbg_name(indicator = "Material footprint", denom = "metric tons, billions"),
    source = "Source: UNEP (database). https://unstats.un.org/sdgs/indicators/database?indicator=12.2.1"
  )
}

fig_sdg12_ans_flow <- function(countries = c("GHA", "CHL"), year = 2015) {
  indicators <- c(
    "NY.ADJ.ICTR.GN.ZS", # gross savings (% of GNI)
    "NY.ADJ.DKAP.GN.ZS", # consumption of fixed capital (% of GNI)
    "NY.ADJ.AEDU.GN.ZS", # education expenditure (% of GNI)
    "NY.ADJ.DFOR.GN.ZS", # net forest depletion (% of GNI)
    "NY.ADJ.DNGY.GN.ZS", # energy depletion (% of GNI)
    "NY.ADJ.DMIN.GN.ZS", # mineral depletion (% of GNI)
    "NY.ADJ.DCO2.GN.ZS", # carbon dioxide damage (% of GNI)
    "NY.ADJ.DPEM.GN.ZS"  # particulate emission damage (% of GNI)
  )
  
  df <- wbgdata(
    countries,
    indicators,
    years = year,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg12_ans_flow.csv"
  )
  
  df <- df %>%
    mutate(gns = NY.ADJ.ICTR.GN.ZS) %>%
    mutate(nns = gns - NY.ADJ.DKAP.GN.ZS) %>% 
    mutate(nns_plus_education = nns + NY.ADJ.AEDU.GN.ZS) %>% 
    mutate(das = nns_plus_education - NY.ADJ.DNGY.GN.ZS - NY.ADJ.DMIN.GN.ZS - NY.ADJ.DFOR.GN.ZS) %>% 
    mutate(ans = das - NY.ADJ.DPEM.GN.ZS - NY.ADJ.DCO2.GN.ZS) %>% 
    gather(indicator, value, c(gns, nns, nns_plus_education, das, ans)) %>% 
    mutate(indicator = factor(indicator, levels = c("gns", "nns", "nns_plus_education", "das", "ans")))
  
  figure(
    data = df,
    plot = function(data, style = style_atlas()) {
      labels = c(
        gns = "Gross national saving",
        nns = "Net national saving (NNS)",
        nns_plus_education = "NNS + Education",
        das = "Depletion-adjusted saving",
        ans = "Adjusted net saving (ANS)"
      )
    
      ggplot(data , aes(x=indicator, y=value)) +
        geom_col(fill = style$colors$spot.primary) +
        geom_text(
          aes(x=indicator, y=0, label=str_wrap(labels[indicator], 14),
          vjust=ifelse(sign(value)>0, 1.25, -0.25)),
          position = position_dodge(),
          size = style$gg_text_size,
          family = style$family
        ) +
        geom_hline(yintercept = 0, color = style$colors$baseline, size = style$theme()$line$size)+
        facet_wrap(~iso3c, ncol = 1, labeller = as_labeller(wbgref$countries$labels)) +
        style$theme() +
        style$theme_legend("right") +
        theme(
          axis.text.x = element_blank(),
          plot.margin = margin(10,25,0,0,"mm"),
          panel.spacing.y = unit(5, "mm")
        )
    },
    title="Adjusted net saving is a measure of economic sustainability. It monitors whether the depreciation and depletion of physical and natural capital, and pollution damages, are compensated for by savings and investment.",
    subtitle = wbg_name(indicator = "Share of gross national income", year = year, denom="%"),
    source = "Source: World Bank and OECD. WDI (NY.ADJ.ICTR.GN.ZS; NY.ADJ.DKAP.GN.ZS; NY.ADJ.AEDU.GN.ZS; NY.ADJ.DFOR.GN.ZS; NY.ADJ.DNGY.GN.ZS; NY.ADJ.DMIN.GN.ZS; NY.ADJ.DCO2.GN.ZS; NY.ADJ.DPEM.GN.ZS)."
  )
}

fig_sdg12_ans_rents_gni <- function(years = 2010:2016, labelling=FALSE) {
  indicators <- c(
    gni_pc = "NY.GNP.PCAP.CD", 
    ans = "NY.ADJ.SVNG.GN.ZS", 
    resource_rent = "NY.GDP.TOTL.RT.ZS"
  )
  
  df_ans_rent_raw <- wbgdata(
    wbgref$countries$iso3c,
    indicator = c(indicators['ans'], indicators['resource_rent']),
    years = years,
    removeNA = FALSE,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg12_ans_rents_gni-ans.csv"
  )
  
  df_ans_rent <- df_ans_rent_raw %>% 
    group_by(iso3c) %>% 
    summarise(NY.ADJ.SVNG.GN.ZS = mean(NY.ADJ.SVNG.GN.ZS), NY.GDP.TOTL.RT.ZS = mean(NY.GDP.TOTL.RT.ZS)) %>% 
    gather(indicatorID, value, c(NY.ADJ.SVNG.GN.ZS, NY.GDP.TOTL.RT.ZS))
  
  df_gni_raw <- wbgdata(
    wbgref$countries$iso3c,
    indicator = indicators['gni_pc'],
    years = max(years),
    removeNA = FALSE,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg12_ans_rents_gni-gni.csv"
  )

  df_gni <- df_gni_raw %>% 
    select(-date)
  
  df <- rbind(df_ans_rent, df_gni) %>% 
    spread(indicatorID, value) %>% 
    na.omit()
  
  df$resource_rent_bins <- supercut(df$NY.GDP.TOTL.RT.ZS, c(
    "0–1" = "[0,1)",
    "1–5" = "[1,5)",
    "5–10" = "[5,10)",
    "10–20" = "[10,20)",
    "20 and over" = "[20, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      income_range = range(df$NY.GNP.PCAP.CD, na.rm = TRUE)
      income_group_breaks = c(LIC = income_range[1], LMC = 1005, UMC = 3955, HIC = 12235, income_range[2])
      labels <- data.frame(
        x = sqrt(income_group_breaks * lead(income_group_breaks)),
        label = names(income_group_breaks),
        stringsAsFactors = FALSE
      )
    
      ggplot(df, aes(x = NY.GNP.PCAP.CD, y = NY.ADJ.SVNG.GN.ZS, color = resource_rent_bins)) +
        geom_hline(yintercept = 0, color = style$colors$reference, size = style$theme()$line$size)+
        geom_point() +
        scale_x_continuous(
          limits = c(280, NA), 
          trans = "log2", 
          breaks = income_group_breaks[2:4], 
          labels = ones(0)) +
          scale_y_continuous(limits = c(-65,50), breaks = seq(-50, 50, 25)
        ) +
        {
          if (labelling) 
            geom_text(
              data=df %>% filter(NY.ADJ.SVNG.GN.ZS < 0),
              aes(x=NY.GNP.PCAP.CD, y=NY.ADJ.SVNG.GN.ZS, label=wbgref$countries$labels[iso3c]),
              size=style$gg_text_size,
              color=style$colors$text
            )
        } +
        geom_text(
          data = labels,
          aes(x = x, label = str_wrap_lines(wbgref$incomes$labels, force=TRUE)[label]),
          y = -55,
          hjust = 0.5,
          vjust = 1,
          family = style$family,
          size = style$gg_text_size,
          color = style$colors$text
        ) +
        scale_color_manual(name="Total natural\nresource rents\n(% of GDP)", values = style$colors$continuous(nlevels(df$resource_rent_bins))) +
        labs(x = wbg_name("NY.GNP.PCAP.CD", by = "log scale", year=2016 )) +
        style$theme() +
        style$theme_scatter() +
        style$theme_legend("righttop") +
        theme(legend.title = element_text(lineheight = 1))
    },
    aspect_ratio = 1.3,
    title="Transforming natural resources into other forms of wealth is a major challenge. Many resource-rich low-income countries have negative adjusted net saving.",
    subtitle = wbg_name(indicator = "Adjusted net saving", year = "average, 2010-16", denom="% of GNI"),
    source = "Source: World Bank. World Development Indicators (NY.GNP.PCAP.CD; NY.ADJ.SVNG.GN.ZS; NY.GDP.TOTL.RT.ZS)."
  )
}

fig_sdg12_food_loss_map <- function(year = 2013) {
  df_raw <- read_csv("inputs/sdg12/food_loss.csv")
  
  df <- df_raw %>% 
    select(
      iso3c  = "code",
      value = "2013"
    ) %>%
    right_join(data.frame(iso3c = wbgref$countries$iso3c, stringsAsFactors = FALSE))  
  
  df$bins <- supercut(df$value, c(
    "Under 100" = "[0, 100)",
    "100–300" = "[100, 300)",
    "300 and over" = "[300, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    title="One-third of food produced for human consumption is lost or wasted. This is also a waste of the resources used to produce, manage, and transport it.",
    subtitle = wbg_name(indicator = "Food loss", year = year, denom="kilocalories per person per day"),
    note="a. FAO 2011 http://www.fao.org/docrep/014/mb060e/mb060e00.htm",
    source = "Source: FAO Food Balance Sheets (database). http://www.fao.org/faostat/en/#data/FBS"
  )
}

fig_sdg12_waste_stacked_bar <- function(num_countries = 10) {
  df <- read_excel("inputs/sdg12/Municipal waste treatment.xlsx", range = "B16:N139", na = "...")
  
  df <- df %>%
    filter(!is.na(Country)) %>%
    transmute(
      country = Country,
      date = as.numeric(`latest year available`),
      total = as.numeric(`Municipal waste collected`) * 1000, # in 1000 tonnes in file
      landfilled = as.numeric(`Municipal waste landfilled`),
      incinerated = as.numeric(`Municipal waste incinerated`),
      recycled = as.numeric(`Municipal waste recycled`),
      composted = as.numeric(`Municipal waste composted`)
    ) %>%
    mutate(disposal_not_specified = 100 - replace_na(landfilled, 0) - replace_na(incinerated,0) - replace_na(recycled,0) - replace_na(composted,0)) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c"))
  
  df <- df %>% 
    top_n(num_countries, total) %>% 
    gather(indicator, value, -date, -country, -iso3c)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      iso3c_levels <- df %>% filter(indicator == "total") %>% arrange(value) %>% pull(iso3c)
      
      p.shares <- ggplot(
        df %>% filter(indicator != "total") %>% mutate(panel="Waste treatment method\n(% of total)"), 
        aes(
          x = factor(iso3c, levels = iso3c_levels), 
          y = value, 
          fill = factor(indicator, levels = c("landfilled", "incinerated", "recycled", "composted", "disposal_not_specified")))
        ) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(
          values=replace(style$colors$categorical, c(4, 5), style$colors$categorical[c(5, 4)]),
          labels = c(
            "landfilled" = "Landfill",
            "incinerated"  = "Incinerate",
            "recycled" = "Recycle",
            "composted" = "Compost",
            "disposal_not_specified" = "Unspecified"
          )
        ) +
        coord_flip() +
        facet_grid(~ panel) +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") + 
        theme(
          axis.text.y = element_blank(),
          strip.text.x = element_text(hjust = 0.5),
          axis.title.x=element_blank(),
          panel.grid.major.x = element_blank()
        )
      
      p.totals <- ggplot(
        df %>% filter(indicator=="total") %>% mutate(panel="Total waste collected\n(metric tons, millions)"), 
        aes(x = factor(iso3c, levels = iso3c_levels), y = value, fill=indicator)
      ) +
        geom_col() +
        scale_fill_manual(values=style$colors$spot.secondary.light) +
        scale_y_continuous(labels=millions(),breaks=c(0,1e8,2e8)) +
        scale_x_discrete(labels = wbgref$countries$labels) +
        coord_flip() +
        facet_grid(~ panel) +
        style$theme() + 
        style$theme_barchart() +
        theme(
          strip.text.x = element_text(hjust = 0.5),
          plot.margin=margin(7.5,0,5,0,"mm")
        )

      # Combine both plots side by side
      pt.shares <- ggplotGrob(p.shares)
      pt.totals <- ggplotGrob(p.totals)
      
      chart <- gtable_row("chart", list(pt.totals, pt.shares), height = unit(1, "null"), widths = unit(c(1.5,2), "null"))
      chart$theme <- style$theme()
      chart
    },
    subtitle = wbg_name(indicator = "Municipal waste", by = "top 10 countries with data by total waste collected", mrv = df$date),
    title="The United States and China collect the most municipal waste, the majority of which makes its way to landfills.",
    source = "Source: UNEP, UNSD (database). https://unstats.un.org/unsd/envstats/qindicators.cshtml"
  )
}

fig_sdg12_landfill_map <- function() {
  df <- read_excel("inputs/sdg12/Municipal waste treatment.xlsx", range = "B16:N139", na = "...")
  
  df <- df %>%
    filter(!is.na(Country)) %>%
    transmute(
      country = Country,
      date = as.numeric(`latest year available`),
      value = as.numeric(`Municipal waste landfilled`)
    ) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
    filter(!is.na(value))
  
  # Make sure all countries represented
  df <- df %>% right_join(data.frame(iso3c = wbgref$countries$iso3c, stringsAsFactors = FALSE))
  
  df$bins <- supercut(df$value, c(
    "0–25" = "[0, 25)",
    "25–50" = "[25, 50)",
    "50–75" = "[50, 75)",
    "75–100" = "[75, 100]"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
    },
    title = "In two-thirds of countries with data, over 50 percent of municipal waste goes to landfill. These statistics are still being developed by many countries.",
    subtitle = wbg_name(indicator = "Share of municipal waste that is sent to landfill", mrv=df$date, denom = "%"),
    source = "Source: UNEP, UNSD (database). https://unstats.un.org/unsd/envstats/qindicators.cshtml"
  )
}

fig_sdg12_recycle_compost_map <- function() {
  df <- read_excel("inputs/sdg12/Municipal waste treatment.xlsx", range = "B16:N139", na = "...")
  
  df <- df %>%
    filter(!is.na(Country)) %>%
    transmute(
      country = Country,
      date = as.numeric(`latest year available`),
      recycled = as.numeric(`Municipal waste recycled`),
      composted = as.numeric(`Municipal waste composted`),
      recycled_or_composted = ifelse(
        is.na(recycled) & is.na(composted),
        NA,
        replace_na(recycled, 0) + replace_na(composted, 0)
      )
    ) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c"))
  
  # Make sure all countries represented
  df <- df %>% right_join(data.frame(iso3c = wbgref$countries$iso3c, stringsAsFactors = FALSE))

  df$bins <- supercut(df$recycled_or_composted, c(
    "0–1" = "[0, 1)",
    "1–25" = "[1, 25)",
    "25–50" = "[25, 50)",
    "Over 50" = "[50, 100]"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
    },
    title = "Only 1 in 10 countries with available data recycles or composts more than 50 percent of municipal waste.",
    subtitle = wbg_name(indicator = "Share of municipal waste that is recycled or composted",mrv=df$date, denom = "%"),
    source = "Source: UNEP, UNSD (database). https://unstats.un.org/unsd/envstats/qindicators.cshtml"
  )
}

# make_all(path = "docs/sdg12/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg12", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg12_mf_pc(), styler, file.path(path, "fig_sdg12_mf_pc.png"), width = 5.5, height=4.5)
  saver(fig_sdg12_mf_region(), styler, file.path(path, "fig_sdg12_mf_region.png"), width = 5.6, height = 2.6)
 
  # page 2
  saver(fig_sdg12_ans_flow(), styler, file.path(path, "fig_sdg12_ans_flow.png"), width = 5.5, height = 5) 
  saver(fig_sdg12_ans_rents_gni(), styler, file.path(path, "fig_sdg12_ans_rents_gni.png"), width = 5.6, height=3.5)
  saver(fig_sdg12_ans_rents_gni(labelling=TRUE), styler, file.path(path, "fig_sdg12_ans_rents_gni-LABELLED.png"), width = 5.6, height=3.5)
  
  # page 3
  saver(fig_sdg12_food_loss_map(), styler, file.path(path, "fig_sdg12_food_loss_map.png"), width = 5.5, height=4.5)
  saver(fig_sdg12_waste_stacked_bar(), styler, file.path(path, "fig_sdg12_waste_stacked_bar.png"), width = 5.5, height=4, padding=margin(0,2,0,0,"mm"))

  # page 4
  saver(fig_sdg12_landfill_map(), styler, file.path(path, "fig_sdg12_landfill_map.png"), width = 5.5, height = 4.25)
  saver(fig_sdg12_recycle_compost_map(), styler, file.path(path, "fig_sdg12_recycle_compost_map.png"), width = 5.5, height = 4.25)
}

