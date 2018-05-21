library(ggplot2)
library(dplyr)
library(tidyr)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(readr)
library(readxl)
library(forcats)
library(countrycode)
library(stringr)
source("styles.R")

fig_sdg16_homicides_dotplot <- function(start_years = 1996:2005, end_years = 2006:2015, num_countries = 5, pop_cutoff = 1) {
  indicators <- c("VC.IHR.PSRC.P5", "SP.POP.TOTL")
  
  df_all <- wbgdata(
    country = wbgref$countries$iso3c, 
    indicator = indicators, 
    years = c(start_years, end_years), 
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg16_homicides_dotplot.csv"
  )
  
  iso3c_bigpop <- df_all %>%
    filter(date == head(start_years, n=1) & SP.POP.TOTL > pop_cutoff) %>%
    pull(iso3c)
  df_pop <- df_all %>% filter(iso3c %in% iso3c_bigpop) %>% select(-SP.POP.TOTL)
  
  df.start <- df_pop %>%
    filter(date %in% start_years) %>%
    group_by(iso3c) %>%
    summarise(avg_start = mean(VC.IHR.PSRC.P5, na.rm = TRUE)) %>%
    na.omit()
  df.end <- df_pop %>%
    filter(date %in% end_years) %>%
    group_by(iso3c) %>%
    summarise(avg_end = mean(VC.IHR.PSRC.P5, na.rm = TRUE)) %>%
    na.omit()
  df.change <- df.start %>% 
    merge(df.end) %>% 
    mutate(change = avg_end - avg_start) %>% 
    arrange(change)
  
  df.top.decreases <- df.change %>% 
    head(num_countries) %>% 
    mutate(direction = "Largest decreases")

  df <-df.top.decreases %>% select(-change) %>% gather(indicator, value, c(avg_start, avg_end))
  
  indicator <- c("VC.IHR.PSRC.P5")
  df.world <- wbgdata(
    country = "WLD", indicator = indicator,
    years = tail(end_years, n=1),
    removeNA = FALSE
  )
  
  figure(
    data = list(countries = df, world = df.world),
    plot = function(data, style = style_atlas_open()) {
      data$countries <- data$countries %>% 
        spread(indicator, value) %>% 
        arrange(avg_end) %>% 
        gather(indicator, value, c(avg_start, avg_end)) %>% 
        mutate(indicator = factor(indicator, levels = c("avg_start", "avg_end"), ordered = TRUE))
      
      ggplot(data$countries, aes(x=value, y=iso3c, color = indicator, fill = indicator, shape = indicator)) +
        geom_other_dotplot(
          aes(x=value, y=fct_reorder2(iso3c, indicator == "avg_end", -value),group = paste0(direction, iso3c)),
          arrow = TRUE, size = style$point_size, stroke = style$point_stroke, flip.legend = TRUE
        ) +
        geom_vline(
          aes(xintercept = VC.IHR.PSRC.P5),
          data = data$world,
          color = style$color$reference, linetype = style$linetypes$reference
        ) +
        scale_colour_manual(
          values = c(avg_start = style$colors$spot.primary.light, avg_end = style$colors$spot.primary),
          labels = c(avg_start = paste("Average", str_range(start_years, shorten=TRUE)), avg_end = paste("Average", str_range(end_years, shorten=TRUE)))
        ) +
        scale_fill_manual(
          values = c(avg_start = style$colors$spot.primary.light, avg_end = style$colors$spot.primary),
          labels = c(avg_start = paste("Average", str_range(start_years, shorten=TRUE)), avg_end = paste("Average", str_range(end_years, shorten=TRUE)))
        ) +
        scale_shape_manual(
          values = c(avg_start = style$shapes$point, avg_end = 99),
          labels = c(avg_start = paste("Average", str_range(start_years, shorten=TRUE)), avg_end = paste("Average", str_range(end_years, shorten=TRUE)))
        ) +
        scale_x_continuous(
          expand = c(0, 0),
          limits = c(0, 62),
          sec.axis = dup_axis(breaks = df.world$VC.IHR.PSRC.P5, labels = "World, 2015")
        ) +
        scale_y_discrete(labels = wbgref$countries$labels) +
        style$theme() +
        style$theme_barchart() +
        theme(legend.position = c(0.95, 0.05), legend.justification = c(1, 0), legend.direction = "vertical")
    },
    title = "Homicide rates have declined dramatically in some countries.",
    subtitle = wbg_name(indicators[1], by = "five countries with largest reductions in rate"),
    source = "Source: UNODC. WDI (VC.IHR.PSRC.P5; SP.POP.TOTL)."
  )
}

fig_sdg16_battle_deaths <- function(years = 2001:2016, breakout_threshold_recent = 5e3, breakout_threshold_ever = 20e3) {
  indicator <- "VC.BTL.DETH"
  
  # Get data for countries
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = years,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg16_battle_deaths.csv"
  )
  
  # Get high countries, ever
  top_iso3c_ever <- df %>%
    group_by(iso3c) %>%
    summarise(max = max(VC.BTL.DETH, na.rm = T)) %>%
    arrange(-max) %>%
    filter(max >= breakout_threshold_ever) %>%
    pull(iso3c)

  # Get high countries, recently
  top_iso3_recent <- df %>%
    filter(date > max(years) - 5) %>%
    group_by(iso3c) %>%
    summarise(max = max(VC.BTL.DETH, na.rm = T)) %>%
    arrange(-max) %>%
    filter(max >= breakout_threshold_recent) %>%
    pull(iso3c)
  
  breakout_iso3c <- union(top_iso3c_ever, top_iso3_recent)
  
  # Aggregate other countries
  df <- df %>% 
    mutate(iso3c = ifelse(iso3c %in% breakout_iso3c, iso3c, "ZZZ")) %>%
    group_by(date, iso3c) %>%
    summarise(VC.BTL.DETH = sum(VC.BTL.DETH))

  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      iso3c_order <- df %>%
        group_by(iso3c) %>%
        summarise(max = max(VC.BTL.DETH)) %>%
        arrange(iso3c == "ZZZ", -max) %>%
        pull(iso3c)
      df <- df %>%
        mutate(iso3c = factor(iso3c, iso3c_order))
      ggplot(df, aes(date, VC.BTL.DETH, fill = iso3c)) +
        geom_col(position = position_stack(reverse=TRUE)) +
        scale_x_continuous(breaks = seq(2001, 2016, 5)) +
        scale_y_continuous(labels = thousands(), position = "right") +
        scale_fill_manual(
          values = c(style$colors$spot.primary, style$colors$spot.primary.light, style$colors$spot.secondary, style$colors$spot.primary.dark, style$colors$spot.secondary.light),
          labels = c(wbgref$countries$labels, ZZZ = "Other"),
          guide = guide_legend(ncol = 1, reverse = TRUE)) +
        style$theme() +
        theme(legend.position = c(0,1), legend.justification = c(0,1))
    },
    aspect_ratio = 2,
    title = "But battle-related deaths remain high due to the continuing Syrian conflict.",
    subtitle = wbg_name(indicator, denom = "thousands of people"),
    source = "Source: Uppsala Conflict Data Program. WDI (VC.BTL.DETH)."
  )
}

fig_sdg16_fcas <- function() {
  df <- read_xls("inputs/reference_data/CLASS.xls", "Groups")

  fcas_iso3c <- df %>%
    filter(GroupCode == "FCS") %>%
    pull(CountryCode)
  
  df <- data.frame(
    iso3c = wbgref$countries$iso3c,
    is_fcas = wbgref$countries$iso3c %in% fcas_iso3c
  )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      df <- df %>% mutate(is_fcas = ifelse(is_fcas, "Fragile or conflict-affected situation", NA))
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "is_fcas", na.in.legend = FALSE)
    },
    aspect_ratio = 1.5,
    title = "The World Bank currently identifies 36 fragile situations globally.",
    source = paste("Source: World Bank. http://www.worldbank.org/en/topic/fragilityconflictviolence/brief/harmonized-list-of-fragile-situations")
  )
}

fig_sdg16_refugees <- function(cutoff = 50000) {
  df <- read_xlsx("inputs/sdg16/17-MYSR-tab_v3.xlsx", sheet = "Tab 1.3", skip = 5, na = "*")

  df <- df %>% select(
    origin  = "Origin",
    destination = "Country/territory of asylum/residence",
    type = "Type of population",
    value = "Population__1" # mid 2017
  )
  df <- df %>%
    filter(type == "Total Refugee and people in refugee-like situations") %>%
    select(-type)
  
  # Remap countries to WDI compatible codes, if not exact fit to Other/ZZZ
  custom_match <- c(
    "Central African Rep." = "CAF",
    "various/unknown" = "ZZZ",
    "Stateless" = "ZZZ",
    "Tibetan" = "ZZZ",
    "Serbia and Kosovo: S/RES/1244 (1999)" = "ZZZ",
    "Western Sahara" = "ZZZ",
    "Palestinian" = "ZZZ",
    "Various" = "ZZZ"
  )
  
  # Remap origins and destinations to WDI compatible codes
  df$dest_iso3c <- countrycode::countrycode(
    iconv(df$destination, to="ASCII//TRANSLIT"),
    "country.name", "iso3c",
    custom_match = custom_match)
  df$origin_iso3c <- countrycode::countrycode(
    iconv(df$origin, to="ASCII//TRANSLIT"),
    "country.name", "iso3c",
    custom_match = custom_match
  )
  df <- df %>% select(-origin, -destination)
  
  # Generate a list of origins & destinations we'll actually break out
  origins <- df %>%
    group_by(origin_iso3c) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    filter(value > cutoff) %>%
    filter(origin_iso3c %in% wbgref$countries$iso3c) %>% # exclude non WDI countries
    pull(origin_iso3c) %>%
    unique()
  
  dests <- df %>%
    group_by(dest_iso3c) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    filter(value > cutoff) %>%
    filter(dest_iso3c %in% wbgref$countries$iso3c) %>% # exclude non WDI countries
    pull(dest_iso3c) %>%
    unique
  
  # Everything else gets grouped up into an "other" group
  df <- df %>% mutate(
    origin_iso3c = ifelse(origin_iso3c %in% origins, origin_iso3c, "ZZZ"),
    dest_iso3c = ifelse(dest_iso3c %in% dests, dest_iso3c, "ZZZ")
  )
  
  df <- df %>%
    group_by(origin_iso3c, dest_iso3c) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup
  
  # Generate subtotals by origin and destination, a grand total, and add to main dataset
  origin.totals <- df %>%
    group_by(origin_iso3c) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(dest_iso3c = "ZZY") %>%
    select(origin_iso3c, dest_iso3c, value)
  
  dest.totals <- df %>%
    group_by(dest_iso3c) %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(origin_iso3c = "ZZY") %>%
    select(origin_iso3c, dest_iso3c, value)
  
  grand.total <- df %>%
    summarise(value = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(origin_iso3c = "ZZY", dest_iso3c = "ZZY") %>%
    select(origin_iso3c, dest_iso3c, value)
  
  df <- df %>%
    rbind(origin.totals) %>%
    rbind(dest.totals) %>%
    rbind(grand.total) %>%
    left_join(wbgref$countries$regions, by = c("origin_iso3c" = "iso3c"))
  
  # Draw the figure
  figure(
    data =  df,
    plot = function(df, style = style_atlas()) {
      iso3c_levels_origin <- c("SPACER2", "ZZY", "ZZZ", "SPACER1", wbgref$countries$iso3c[order(wbgref$countries$labels)])
      iso3c_levels_dest <- c("ZZZ", "SPACER1", rev(wbgref$countries$iso3c[order(wbgref$countries$labels)]), "SPACER2", "ZZY")
      df <- df %>% mutate(
        dest_iso3c = factor(dest_iso3c, iso3c_levels_dest),
        origin_iso3c = factor(origin_iso3c, iso3c_levels_origin),
        is_origin_total = (origin_iso3c == "ZZY"),
        is_dest_total = (dest_iso3c == "ZZY")
      )
      df <- df %>% arrange(value)
      
      # This works around a known bug that scale_*_continous(expand = ) doesn't work if
      # the plot panel only has one value - it's ugly but it gets the job done
      # https://github.com/tidyverse/ggplot2/issues/2281
      df <- rbind(df, tribble(
        ~origin_iso3c, ~dest_iso3c, ~value, ~region_iso3c, ~is_origin_total, ~is_dest_total,
        "SPACER1", "ZZZ", NA, NA, FALSE, FALSE,
        "SPACER2", "ZZZ", NA, NA, TRUE, FALSE,
        "ZZZ", "SPACER1", NA, NA, FALSE, FALSE,
        "ZZZ", "SPACER2", NA, NA, FALSE, TRUE
      ))
      big_origins <- c("SYR")
      allpanels <- expand.grid(is_origin_total = c(TRUE, FALSE), is_dest_total = c(TRUE, FALSE))
      country_labels <- c(wbgref$countries$labels, ZZZ = "Other countries", SPACER1 = "", SPACER2 = "")
      country_labels_origin <- c(country_labels, ZZY = "")
      country_labels_destination <- c(country_labels, ZZY = "")
      df <- df %>% arrange(-value)
      p <- ggplot(df, aes(origin_iso3c, dest_iso3c, size = value, color = origin_iso3c %in% big_origins)) +
        geom_point(alpha = 0.85) +
        scale_size_area(max_size = 27) +
        scale_x_discrete(expand = c(0, 1), position = "top", labels = country_labels_origin) +
        scale_y_discrete(expand = c(0, 1), labels = country_labels_destination, position = "right") +
        scale_color_manual(values = c(style$colors$spot.secondary, style$colors$spot.primary)) +
        xlab("Country of origin") +
        ylab("Country of asylum/residence") +
        facet_grid(!is_dest_total ~ is_origin_total, scales = "free", space = "free") +
        style$theme() +
        theme(
          axis.title.x = element_text(),
          axis.title.y = element_text(angle = 90),
          axis.text.x = element_text(angle = -60, hjust = 1), 
          panel.grid.major.y = element_blank(),
          legend.position = "none",
          strip.text = element_blank(),
          plot.margin = margin(1,3,25,5, unit = "mm")
        )

      # Disable all the clipping, needed b/c everything is so crowded.
      g <- ggplotGrob(p)
      g$layout$clip[g$layout$name=="panel-1-1"] <- "off"
      g$layout$clip[g$layout$name=="panel-1-2"] <- "off"
      g$layout$clip[g$layout$name=="panel-2-1"] <- "off"
      g$layout$clip[g$layout$name=="panel-2-2"] <- "off"
      g$theme <- p$theme
      g
    },
    aspect_ratio = 0.5,
    title = "People often cross borders to seek refuge from conflict and fragility, but most remain in directly neighboring countries. Only a minority travel farther afield.",
    subtitle = wbg_name(indicator = "Refugees", by = "by country of origin and country of asylum/residence", year = "mid-2017"),
    note = paste0("Note: \"Other countries\" includes countries and territories of origin or asylum/residence with a total refugee population of less than ",ones()(cutoff),". Population is people reported by UNHCR to be refugees or in a refugee-like situation."),
    source = "Source: UNHCR Population Statistics, mid-year 2017, version 3 (database). http://popstats.unhcr.org"
  )
}

fig_sdg16_birth_reg <- function(years = 2010:2017, num_countries = 40) {
  indicators <- c(poorest20 = 'SP.REG.BRTH.Q1.ZS', richest20 = 'SP.REG.BRTH.Q5.ZS')
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg16_birth_reg.csv"
  )
  
  df <- df %>% 
    spread(indicatorID, value) %>% 
    na.omit() %>% 
    group_by(iso3c) %>% 
    slice(which.max(date)) %>% 
    ungroup() %>% 
    top_n(num_countries, -SP.REG.BRTH.Q1.ZS) %>% 
    gather(indicatorID, value, c(SP.REG.BRTH.Q1.ZS, SP.REG.BRTH.Q5.ZS))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      iso3c_levels <- df %>% 
        spread(indicatorID, value) %>% 
        arrange(-SP.REG.BRTH.Q1.ZS) %>% 
        pull(iso3c)

      ggplot(df, aes(x=value, y=iso3c)) +
        geom_other_dotplot(
          aes(
            y = factor(iso3c, levels = iso3c_levels), 
            color = factor(indicatorID, levels = c("SP.REG.BRTH.Q1.ZS", "SP.REG.BRTH.Q5.ZS"))
          ),
          size = style$point_size,
          shape = style$shapes$point,
          stroke = style$point_stroke
        ) +
        geom_text(
          aes(label = wbgref$countries$labels[iso3c]),
          data = . %>% group_by(iso3c) %>% mutate(value = min(value)),
          hjust = 1,
          family = style$family,
          size = style$gg_text_size,
          color = style$colors$text,
          nudge_x = -1.5
        ) +
        scale_colour_manual(
          values = c("SP.REG.BRTH.Q1.ZS" = style$colors$spot.primary.light,
                     "SP.REG.BRTH.Q5.ZS" = style$colors$spot.primary),
          labels = c("SP.REG.BRTH.Q1.ZS" = "Poorest quintile", 
                     "SP.REG.BRTH.Q5.ZS" = "Richest quintile")
        ) +
        scale_x_continuous(limits = c(-10, 102), expand = c(0, 0)) +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") +
        theme(axis.text.y = element_blank())
    },
    title = "A legal identity ensures basic human rights and allows participation in the formal economy. But registration at birth is often unavailable to the poor.",
    subtitle = wbg_name(indicator = "Completeness of birth registration", by = "40 countries with lowest registration in poorest quintile", mrv = df$date, denom = "%"),
    source = paste("Source: UNICEF. Health Nutrition and Population Statistics by Wealth Quintile (SP.REG.BRTH.Q1.ZS; SP.REG.BRTH.Q5.ZS).")
  )
}


fig_sdg16_bribery_region <- function(year = 2016) {
  indicators <- c(
    bribery = "IC.FRM.BRIB.ZS",
    informal_payment = "IC.FRM.CORR.ZS",
    gift = "IC.TAX.GIFT.ZS"
  )
  
  df <- wbgdata(
    c(wbgref$regions$iso3c, "WLD"),
    indicators,
    years = year,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg16_bribery_region.csv"
  )
  
  df <- df %>% 
    na.omit() %>% 
    mutate(iso3c = fct_reorder2(iso3c, indicatorID == "IC.FRM.BRIB.ZS", -value))

  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
    labels = c(
      IC.FRM.BRIB.ZS = "Firms experiencing at least one bribe payment request (a)",
      IC.FRM.CORR.ZS = "Firms expected to give gifts to public officials",
      IC.TAX.GIFT.ZS = "Firms expected to give gifts in meetings with tax officials"
    )
    facet_labeller <- as_labeller(setNames(str_wrap_lines(labels, 3, force=TRUE), names(labels)))
    
    ggplot(df, aes(x = iso3c, y = value, fill = (iso3c == "WLD"))) +
      geom_col() +
      scale_x_discrete(labels = wbgref$all_geo$labels) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_fill_manual(values = c(`FALSE` = style$colors$spot.primary, `TRUE` = style$colors$spot.secondary)) +
      facet_wrap(~ indicatorID, ncol = 3, labeller = facet_labeller) +
      coord_flip() +
      style$theme() +
      style$theme_barchart() +
      theme(panel.spacing = unit(2, "lines"), strip.text.x = element_text(hjust = 0.5))
    },
    aspect_ratio = 2,
    title = "Corrupt public officials may make it harder for citizens and businesses to access government services.",
    subtitle = wbg_name(indicator = "Bribery and gifts (informal payments)", year = year, denom = "% of firms experiencing"),
    note = "Note: Excludes data for most high-income countries. a. During six transactions dealing with utilities access, permits, licenses, and taxes.",
    source = "Source: World Bank Enterprise Surveys. WDI (IC.FRM.BRIB.ZS; IC.FRM.CORR.ZS; IC.TAX.GIFT.ZS)."
  )
}


fig_sdg16_regulatorygov_vs_gni <- function(income_years = 2015:2016) {
  indicator <- "NY.GNP.PCAP.CD"

  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = income_years,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg16_regulatorygov_vs_gni.csv"
  )
  
  df <- df %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()
  
  df <- df %>% left_join(wbgref$countries$incomegroups)
  
  df_score <- read_xlsx(
    path = "inputs/sdg16/Scores-Global-Indicators-of-Regulatory-Governance-2016.xlsx",
    col_names = TRUE,
  )
  df_score <- df_score %>% 
    filter(Economy != "European Union") %>%
    mutate(iso3c = countrycode(Economy, "country.name", "iso3c", custom_match = c(Kosovo = "XKX"))) %>% 
    select(iso3c = "iso3c", reg_score = "Consolidated regulatory governance score")
  
  df <- df %>% full_join(df_score, by = "iso3c",)
  
  df <- df %>% filter(complete.cases(.)) # Explicitly remove to silence ggplot warnings
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open()) {
      income_range = range(df$NY.GNP.PCAP.CD, na.rm = TRUE)
      income_group_breaks = c(LIC = income_range[1], LMC = 1005, UMC = 3955, HIC = 12235, income_range[2])
      labels <- data.frame(x = sqrt(income_group_breaks * lead(income_group_breaks)), label = names(income_group_breaks), stringsAsFactors = FALSE)
      labels <- labels[complete.cases(labels),]
      ggplot(df) +
        geom_point(
          aes(x = NY.GNP.PCAP.CD, y = reg_score, color = income_iso3c, fill = income_iso3c), 
          size = style$point_size, stroke = style$point_stroke, shape = style$shapes$point
        ) +
        geom_text(
          data = labels,
          aes(x = x, label = str_wrap_lines(wbgref$incomes$labels, force=TRUE)[label]),
          y = -0.5, #min(df$reg_score, na.rm = TRUE) + 0.6,
          hjust = 0.5,
          vjust = 1,
          family = style$family,
          size = style$gg_text_size,
          color = style$colors$text
        ) +
        scale_color_manual(values = style$colors$incomes) +
        scale_fill_manual(values = style$colors$incomes) +
        scale_shape_manual(values = style$shapes$incomes) +
        scale_x_continuous(limits = c(150, NA), trans = "log2", breaks = income_group_breaks[2:4], labels = ones(0)) +
        scale_y_continuous(limits = c(-1, NA), breaks = seq(0,6,2)) +
        labs(x = wbg_name(indicator, by = "log scale", mrv = income_years)) +
        style$theme() +
        style$theme_scatter()
    },
    aspect_ratio = 1.3,
    title = "Public consultation in rulemaking protects the rule of law and provides a buffer against corruption.",
    subtitle = wbg_name(indicator = "Consolidated regulatory governance score", by = "by country", year = 2016),
    source = paste("Source: World Bank Global Indicators of Regulatory Governance. World Development Indicators (NY.GNP.PCAP.CD).")
  )
}

fig_sdg16_public_spending <- function() {
  df <- read_xlsx("inputs/sdg16/PEFA SDG List 2018.xlsx")
  
  # Code countries
  df$iso3c <- countrycode(df$Country, "country.name", "iso3c", custom_match = c("Kosovo" = "XKX"))  
  
  # Decode scores
  df$bins = fct_recode(df$Scores,
    "0–5" = "A",
    "5–10" = "B",
    "10–15"  = "C",
    "15 and over" = "D",
    "NULL" = "NR"
  )
  
  # Extract dates
  df$date <- as.numeric(format(df$`Date on the cover of the report`, "%Y"))
  
  # Join with all countries
  df <- df %>% right_join(wbgref$countries$regions)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    title = "Accountability also means setting, and sticking to, budgets for public expenditure.",
    subtitle = wbg_name(indicator = "Variation from the original approved budget expenditure", denom="% above/below", mrv = df$date),
    source = "Source: Public Expenditure and Financial Accountability (database). https://pefa.org"
  )
}

# make_all(path = "docs/sdg16/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg16", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg16_homicides_dotplot(), styler, file.path(path, "fig_sdg16_homicides_dotplot.png"), width = 2.67, height = 2.75)
  saver(fig_sdg16_battle_deaths(), styler, file.path(path, "fig_sdg16_battle_deaths.png"), width = 2.67, height = 2.75)
  saver(fig_sdg16_fcas(), styler, file.path(path, "fig_sdg16_fcas.png"), width = 5.5, height = 4.25)

  # page 2  
  saver(fig_sdg16_refugees(), styler, file.path(path, "fig_sdg16_refugees.png"), width = 5.5, height = 8.5)

  # page 3
  saver(fig_sdg16_birth_reg(), styler, file.path(path, "fig_sdg16_birth_reg.png"), width = 5.5, height = 6)
  saver(fig_sdg16_bribery_region(), styler, file.path(path, "fig_sdg16_bribery_region.png"), width = 5.5, height = 2.5)
  
  # page 4
  saver(fig_sdg16_regulatorygov_vs_gni(), styler, file.path(path, "fig_sdg16_regulatorygov_vs_gni.png"), width = 5.5, height = 4.4)
  saver(fig_sdg16_public_spending(), styler, file.path(path, "fig_sdg16_public_spending.png"), width = 5.5, height = 4.2)
}
