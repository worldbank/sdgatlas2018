library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(gtable)
source("styles.R")

fig_sdg3_pyramids_by_income <- function(years = c(1965, 2016)) {
  # Set up age groups to generate indicator names
  agegroups <- data.frame(startage = 0:15*5) %>%
    mutate(endage = startage + 4) %>%
    mutate(startage_str = sprintf("%.2d",startage),endage_str = sprintf("%.2d",endage)) %>%
    rbind(data.frame(startage = 80, endage = Inf, startage_str = "80", endage_str = "UP",stringsAsFactors = FALSE))
  agegroups <- agegroups %>%
    crossing(gender = factor(c("MA", "FE"))) %>%
    mutate(indicatorID = paste0("SP.POP.",startage_str,endage_str,".",gender,".5Y"))

  # Grab data for all indicators, join with age groups & tidy
  df <- wbgdata(
    country = wbgref$incomes3$iso3c,
    indicator = agegroups$indicatorID,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_pyramids_by_income.csv"
  )
  
  df <- df %>% left_join(agegroups)
  df <- df %>% mutate(gender = ifelse(gender == "MA", "male", "female"))
  df <- df %>%
    select(iso3c, date, startage, endage, gender, value) %>%
    group_by(iso3c, date, startage, endage, gender) %>%
    summarise(value = sum(value)) %>%
    ungroup

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      col_labeller <- as_labeller(wbgref$incomes3$labels)
      df <- df %>% mutate(iso3c = factor(iso3c, rev(wbgref$incomes3$iso3c)))
      p <- ggplot(df, aes(xmin = startage, xmax = startage+5, fill = gender)) +
        geom_rect(aes(ymin = -value, ymax = 0),df %>% filter(gender == "male"), color = "white", size = 0.15) +
        geom_rect(aes(ymin = 0, ymax = value),df %>% filter(gender == "female"), color = "white", size = 0.15) +
        coord_flip() + 
        facet_grid(date ~ iso3c, labeller = labeller(.cols = col_labeller), switch="y") +
        geom_text(
          aes(x, y, label = date),
          data.frame(x = 95, y = -19, iso3c = "LIC", date = c(1965, 2016)),
          inherit.aes = FALSE,
          hjust = 0, vjust = 1,
          size = style$gg_text_size,
          family = style$family,
          color = style$colors$text
        ) +
        geom_text(
          aes(x = startage, y = ifelse(gender == "male", -value, value), label = Hmisc::capitalize(gender), color = gender),
          df %>% filter(startage == 0, iso3c == "LIC", date == 1965),
          inherit.aes = FALSE,
          hjust = c(1, 0), vjust = 1,
          nudge_x = -3,
          size = style$gg_text_size,
          family = style$family
        ) +
        scale_y_continuous(breaks = c(-15,-10, -5,0,5,10,15), labels = abs) +
        scale_x_continuous(breaks = 20*(0:4), sec.axis = dup_axis(), expand = c(0, 5)) +
        scale_fill_manual(values = style$colors$gender) +
        scale_color_manual(values = style$colors$gender) +
        ylab("Distribution of population in five-year age bands by sex (%)") +
        style$theme() +
        style$theme_x_title() +
        theme(
          legend.position = "none",
          axis.text.y.right = element_text(hjust = 0),
          strip.text.y = element_blank(), #element_text(angle = 180, vjust = 0.5), # HACK to fix flipped title ggplot2 issue #2356
          strip.text.x = element_text(hjust = 0.5),
          strip.placement = "outside",
          panel.spacing.x = unit(0.025, "npc"),
          panel.spacing.y = unit(0.07, "npc"),
          plot.margin = margin(0,1,3,0,"mm")
        ) 
      
        g <- ggplotGrob(p)
        g$layout$clip[g$layout$name=="panel-1-1"] <- "off"
        g$theme <- p$theme
        g
      },
    aspect_ratio = 1.4,
    title = "Low-income countries have younger populations than high-income countries do. As countries become richer, fertility rates fall and life expectancy rises.",
    note = "Note: Ages 80 and older are combined into a single group.",
    source = paste("Source: World Bank and UN Population Division. World Development Indicators (SP.POP.0004.MA.5Y and other five-year bands by sex).")
  )
}

fig_sdg3_life_expectancy <- function(years = 1960:2016, highlight_HIV_threshold = 10) {
  indicator <- "SP.DYN.LE00.IN"

  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = years,
    removeNA = FALSE,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_life_expectancy.csv"
  )

  df <- df %>%
    rename("SP.DYN.LE00.IN"=value) %>%
    left_join(wbgref$countries$regions)

  df_hiv <- wbgdata(country = wbgref$countries$iso3c, indicator = "SH.DYN.AIDS.ZS", years = years)
  highlight_countries <- df_hiv %>%
    group_by(iso3c) %>%
    summarise(max = max(SH.DYN.AIDS.ZS, na.rm=TRUE)) %>%
    filter(max > highlight_HIV_threshold) %>%
    pull(iso3c)

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      # Some ugliness to calculate the breaks that we'll label
      breaks <- (0:10)*10
      startbreaks <- df %>% filter(date == min(date)) %>% pull(SP.DYN.LE00.IN) %>% range(na.rm = T)
      startbreaks <- c(floor(startbreaks[1]), ceiling(startbreaks[2]))
      endbreaks <- df %>% filter(date == max(date)) %>% pull(SP.DYN.LE00.IN) %>% range(na.rm = T)
      endbreaks <- c(floor(endbreaks[1]), ceiling(endbreaks[2]))
      startbreaks <- c(startbreaks, breaks[breaks > startbreaks[1] + 2 & breaks < startbreaks[2] - 2])
      endbreaks <- c(endbreaks, breaks[breaks > endbreaks[1] + 2 & breaks < endbreaks[2] - 2])
      
      df <- df %>% mutate(region_iso3c = factor(ifelse(region_iso3c == "SSF", "SSF", "ZZZ")))

      df <- df %>% arrange(desc(region_iso3c))
      
      ggplot(df, aes(date, SP.DYN.LE00.IN, group = iso3c, color = region_iso3c)) +
        geom_line(alpha = 0.25, size = 0.3) +
        geom_line(
          data = df %>% filter(iso3c %in% highlight_countries),
          alpha = 1, size = 0.4
        ) +
        scale_x_continuous(expand = c(0,0), breaks = bracketed_breaks(closeness_threshold = 0.05)) +
        scale_y_continuous(limits = c(10, NA), breaks = startbreaks, sec.axis = dup_axis(breaks = endbreaks)) +
        scale_color_manual(
          values = c(style$colors$regions, "ZZZ" = "gray70"),
          labels = c(wbgref$regions$labels, "ZZZ" = "Other regions")
        ) +
        style$theme() +
        theme(
          legend.position = c(0, 1),
          legend.direction = "horizontal",
          legend.justification = c(0, 1),
          panel.grid = element_blank(),
          legend.background = element_blank())
    },
    aspect_ratio = 1.3,
    title = "Demography is closely related to health outcomes: while life expectancy has generally risen, HIV/AIDS caused sharp declines in many countries in the 1990s.",
    subtitle = wbg_name(indicator="Life expectancy at birth", by = "by country", denom="years"),
    note = paste0("Note: The countries highlighted with heavier lines are those where all-time peak HIV prevalance exceeded ",highlight_HIV_threshold,"percent."),
    source = "Source: UN Population Division and other sources. World Development Indicators (SP.DYN.LE00.IN)."
  )
}

fig_sdg3_death_pyramids_by_income <- function(period = "2010-2015") {
  # Set up age groups to generate indicator names
  agegroups <- data.frame(startage = 0:15*5) %>%
    mutate(endage = startage + 4) %>%
    mutate(un_band = paste0(startage,"-",endage)) %>%
    rbind(data.frame(
      startage = c(80), endage = c(Inf),
      un_band = c("80-84", "85-89", "90-94", "95+"),
      stringsAsFactors = FALSE
    ))  
  
  male <- readxl::read_excel("inputs/sdg3/WPP2017_MORT_F04_2_DEATHS_BY_AGE_MALE.xlsx", "ESTIMATES", skip = 16)
  female <- readxl::read_excel("inputs/sdg3/WPP2017_MORT_F04_3_DEATHS_BY_AGE_FEMALE.xlsx", "ESTIMATES", skip = 16)
  df <- rbind(
    male %>% mutate(gender = "MA"),
    female %>% mutate(gender = "FE")
  )
  wbg_regions_un_codes <- tribble(
    ~iso3c, ~un,
    "LIC", 1500,
    "MIC", 1517,
    "HIC", 1503
  )
  df <- df %>% right_join(wbg_regions_un_codes, by = c("Country code" = "un"))
  df <- df %>%
    filter(Period == period) %>%
    select(`0-4`:`95+`, gender, iso3c) %>%
    gather("un_band", "value", `0-4`:`95+`) %>%
    group_by(iso3c, gender) %>%
    mutate(value = value / sum(value) * 100) %>%
    ungroup
  
  df <- df %>% left_join(agegroups)
  df <- df %>% mutate(gender = ifelse(gender == "MA", "male", "female"))
  df <- df %>%
    select(iso3c, startage, endage, gender, value) %>%
    group_by(iso3c, startage, endage, gender) %>%
    summarise(value = sum(value)) %>%
    ungroup
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      col_labeller <- as_labeller(wbgref$incomes3$labels)
      df <- df %>% mutate(iso3c = factor(iso3c, rev(wbgref$incomes3$iso3c)))
      p <- ggplot(df, aes(xmin = startage, xmax = startage+5, fill = gender)) +
        geom_rect(aes(ymin = -value, ymax = 0),df %>% filter(gender == "male"), color = "white", size = 0.15) +
        geom_rect(aes(ymin = 0, ymax = value),df %>% filter(gender == "female"), color = "white", size = 0.15) +
        coord_flip() + 
        facet_grid( ~ iso3c, labeller = labeller(.cols = col_labeller), switch="y") +
        geom_text(
          aes(x = startage, y = ifelse(gender == "male", -value, value), label = Hmisc::capitalize(gender), color = gender),
          df %>% filter(startage == 0, iso3c == "LIC"),
          inherit.aes = FALSE,
          hjust = c(0, 1), vjust = 0.5,
          nudge_x = 5, nudge_y = c(5, -5),
          size = style$gg_text_size,
          family = style$theme()$text$family
        ) +
        scale_y_continuous(limits = c(-65, 65), breaks = c(-50, -25,0,25,50), labels = abs) +
        scale_x_continuous(limits = c(0, 85), breaks = 20*(0:4), sec.axis = dup_axis(), expand = c(0, 5)) +
        scale_fill_manual(values = style$colors$gender) +
        scale_color_manual(values = style$colors$gender) +
        ylab("Distribution of deaths among population in five-year age bands by sex (%)") +
        style$theme() +
        style$theme_x_title() +
        theme(
          legend.position = "none",
          axis.text.y.right = element_text(hjust = 0),
          strip.text.y = element_blank(), #element_text(angle = 180, vjust = 0.5), # HACK to fix flipped title ggplot2 issue #2356
          strip.text.x = element_text(hjust = 0.5),
          strip.placement = "outside",
          panel.spacing.x = unit(0.025, "npc"),
          panel.spacing.y = unit(0.07, "npc")
        ) 
      
      # Switch off clipping
      g <- ggplotGrob(p)
      g$layout$clip[g$layout$name=="panel-1-1"] <- "off"
      g$theme <- p$theme
      g
    },
    aspect_ratio = 6/3,
    title = "In high-income countries the majority of people who die are old. But in low-income countries, children under 5 account for one in three deaths.",
    subtitle = wbg_name(indicator = "Deaths by sex and age group", year = "2010-15"),
    note = "Note: Ages 80 and older are combined into a single group.",
    source = paste("Source: UN Population Division, World Population Prospects 2017.")
  )
}

fig_sdg3_child_maternal_mortality_attendance <- function(year_child = 2016, year_maternal = 2015, year_attended = 2013) {
  child_indicators <- c("SH.DYN.NMRT","SP.DYN.IMRT.IN","SH.DYN.MORT")
  maternal_indicator <- "SH.STA.MMRT"
  attended_indicator <- "SH.STA.BRTC.ZS"
  
  # Child mortality
  child <- wbgdata(
    wbgref$regions$iso3c,
    child_indicators,
    years = year_child,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_child_maternal_mortality_attendance-child.csv"
  )
  child <- child %>% mutate(
    neo_mort = SH.DYN.NMRT,
    infant_mort_excess = SP.DYN.IMRT.IN - SH.DYN.NMRT,
    child_mort_excess = SH.DYN.MORT - SP.DYN.IMRT.IN
  )
  iso3c_order <- child %>% arrange(SH.DYN.MORT) %>% pull(iso3c)
  child <- child %>% select(iso3c, date, child_mort_excess, infant_mort_excess, neo_mort)
  child <- gather(child, "class", "value", child_mort_excess, infant_mort_excess, neo_mort)
  child <- child %>% mutate(
    iso3c = factor(iso3c, iso3c_order),
    class = factor(class, c("child_mort_excess", "infant_mort_excess", "neo_mort"))
  )
  
  # Maternal mortality
  maternal <- wbgdata(
    wbgref$regions$iso3c,
    maternal_indicator,
    years = year_maternal,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_child_maternal_mortality_attendance-maternal.csv"
  )
  maternal <- maternal %>% mutate(iso3c = factor(iso3c, iso3c_order))
  
  # Births attended
  attended <- wbgdata(
    wbgref$regions$iso3c,
    attended_indicator,
    years = year_attended,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_child_maternal_mortality_attendance-births.csv"
  )
  attended <- attended %>% mutate(iso3c = factor(iso3c, iso3c_order))
  
  figure(
    data = list(child = child, maternal = maternal, attended = attended),
    plot = function(data, style = style_atlas(), aspect_ratio = 3) {
      p.child <- ggplot(data$child %>% mutate(panel = paste0("Child mortality, ", year_child,"\n(per 1,000 live births)")), aes(x = iso3c, y = value, fill = class)) +
        geom_col() +
        annotate("text", x = 7, y = 15, label = "phantom() <= plain(\"28 days\")", vjust = 0.5, hjust = 0.5, size = style$gg_text_size*0.9, color = style$colors$text.inverse, family = style$family, parse = TRUE) +
        annotate("text", x = 7, y = 40, label = "phantom() <= plain(\"1 year\")", vjust = 0.5, hjust = 0.5, size = style$gg_text_size*0.9, color = style$colors$text.inverse, family = style$family, parse = TRUE) +
        annotate("text", x = 7, y = 65, label = "phantom() <= plain(\"5 years\")", vjust = 0.5, hjust = 0.5, size = style$gg_text_size*0.9, color = style$colors$text.inverse, family = style$family, parse = TRUE) +
        scale_x_discrete(labels = wbgref$regions$labels) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_fill_manual(values = c(style$colors$spot.secondary.light, style$colors$spot.secondary, style$colors$spot.primary, style$colors$spot.primary.light)) +
        coord_flip() +
        facet_grid(~ panel) +
        style$theme() + 
        style$theme_barchart() +
        theme(strip.text.x = element_text(hjust = 0.5))
      
      p.maternal <- ggplot(data$maternal %>% mutate(panel = paste0("Maternal mortality, ", year_maternal, "\n(per 100,000 live births)")), aes(x = iso3c, y = value)) +
        geom_col() +
        scale_x_discrete(labels = wbgref$regions$labels) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() +
        facet_grid(~ panel) +
        style$theme() + 
        style$theme_barchart() +
        theme(axis.text.y = element_blank()) +
        theme(strip.text.x = element_text(hjust = 0.5))
      
      p.attended <- ggplot(
        data$attended %>% mutate(panel = paste0("Births attended, ", year_attended, "\n(% of total)")),
        aes(x = iso3c, y = value)
      ) +
        geom_col() +
        geom_percent_col(aes(y = value), fill = style$colors$spot.secondary, fill.bg = style$colors$neutral) +
        scale_x_discrete(labels = wbgref$regions$labels) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,104)) +
        coord_flip() +
        facet_grid(~ panel) +
        style$theme() + 
        style$theme_barchart() +
        theme(axis.text.y = element_blank()) +
        theme(strip.text.x = element_text(hjust = 0.5))
      
      pt.child <- ggplotGrob(p.child)
      pt.maternal <- ggplotGrob(p.maternal)
      pt.attended <- ggplotGrob(p.attended)
      
      chart <- gtable_row("chart", list(pt.child, pt.maternal, pt.attended), height = unit(1, "null"), widths = unit(c(2,1,1), "null"))
      chart$theme <- style$theme()
      chart
    },
    aspect_ratio = 6/2.8,
    title = "Children are at greatest risk in the first 28 days of life. Birth attendance by skilled health staff helps reduce maternal & neonatal mortality.",
    source = paste("Source: UN Inter-agency Group for Child Mortality Estimation, WHO, UNICEF, UNFPA, World Bank and UN Population Division. World Development Indicators (SH.DYN.NMRT; SP.DYN.IMRT.IN; SH.DYN.MORT; SH.STA.MMRT; SH.STA.BRTC.ZS).")
  )
}  

fig_sdg3_road_mortality <- function(year = 2015) {
  indicator <- "SH.STA.TRAF.P5"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_road_mortality.csv"
  )

  df$bins <- supercut(df$SH.STA.TRAF.P5, c(
    "0–10" = "[0,10)",
    "10–20" = "[10,20)",
    "20–30" = "[20,30)",
    "30 and over" = "[30,Inf)"
  ))
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins",aspect_ratio = "wide")
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.2,
    title= "Globally, 1 in 11 deaths is due to injury, and traffic accidents account for over a quarter of these. Over 1.25 million people died from road traffic injuries in 2015.",
    subtitle = wbg_name(indicator, year = year),
    source = paste("Source:", "WHO. World Development Indicators (SH.STA.TRAF.P5).")
  )
}

fig_sdg3_health_workforce <- function (startdate = 2010, enddate = 2015) {
  indicators <- c(physicians = "SH.MED.PHYS.ZS", nurses = "SH.MED.NUMW.P3")

  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    startdate = startdate,
    enddate = enddate,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_health_workforce.csv"
  )

  df <- df %>%
    mutate(workforce = df$SH.MED.PHYS.ZS + df$SH.MED.NUMW.P3) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    select(iso3c,workforce) %>%
    na.omit %>%
    ungroup()

  df <- df %>% left_join(wbgref$countries$incomegroups)
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(income_iso3c = factor(income_iso3c, rev(wbgref$incomes$iso3c) ))
      labels <- c(workforce = "Workforce")
      ggplot(df, aes(income_iso3c, workforce)) +
        geom_hline(yintercept = 4.45, color = style$colors$reference, linetype=style$linetypes$reference) +
        geom_point(color = style$colors$spot.primary, alpha = 0.7, size = style$point_size, stroke = style$point_stroke, shape = style$shapes$point) +
        scale_x_discrete(labels = wbgref$incomes$labels) +
        scale_y_continuous(limits = c(0,25), expand = c(0.01, 0)) +
        coord_flip() +
        style$theme() +
        style$theme_barchart()
    },
    aspect_ratio = 2.5,
    title = "Not every country has enough health workers to meet the needs of its population. High-income countries have 15 times as many physicians as low-income countries do.",
    subtitle = wbg_name(indicator = "Physicians, nurses, and midwives", by = "by country", mrv = startdate:enddate, denom = "per 1,000 people"),
    source = paste("Source:", "WHO, OECD, and other sources. World Development Indicators (SH.MED.PHYS.ZS; SH.MED.NUMW.P3).")
  )
}

fig_sdg3_surgical_workforce <- function(years = 2011:2016) {
  indicator <- "SH.MED.SAOP.P5"
  
  # Up to date data wasn't yet available in API, so we use an offline file
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = years,
    removeNA = FALSE,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_surgical_workforce.csv"
  )
  
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()

  df <- df %>% left_join(wbgref$countries$incomegroups)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(income_iso3c = factor(income_iso3c, rev(wbgref$incomes$iso3c) ))
      ggplot(df, aes(income_iso3c, value)) +
        geom_point(color = style$colors$spot.primary, alpha = 0.7, size = style$point_size, stroke = style$point_stroke, shape = style$shapes$point) +
        geom_hline(yintercept = 20, color = style$colors$reference, linetype=style$linetypes$reference) +
        scale_x_discrete(labels = wbgref$incomes$labels) +
        scale_y_continuous(limits = c(0,200), expand = c(0.01, 0)) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() + 
        theme(legend.position = "top")
      
    },
    title = "Low-income countries have a severe shortage of specialist surgical workers. All low- and most lower-middle-income countries have fewer than the target number. ",
    subtitle = wbg_name(indicator, by = "by country", mrv = years, denom = "per 100,000 people"),
    source = paste("Source:", "The Lancet Commission on Global Surgery. World Development Indicators (SH.MED.SAOP.P5).")
  )
}

fig_sdg3_surgeons_vs_life_expectancy <- function(surgeons.years = 2011:2016, lifeexp.year = 2016) {
  indicators <- c(surgeons = "SH.MED.SAOP.P5", lifeexp = "SP.DYN.LE00.IN")
  
  # Surgeons
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator = indicators["surgeons"],
    years = surgeons.years,
    removeNA = FALSE,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_surgeons_vs_life_expectancy-surgeons.csv"
  )
  
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    rename(date.surgeons = date)
  
  # Life expectancy
  df.lifeexp <- wbgdata(
    wbgref$countries$iso3c,
    indicator = indicators["lifeexp"],
    years = lifeexp.year,
    removeNA = FALSE,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_surgeons_vs_life_expectancy-life.csv"
  ) %>%
    rename(date.lifeexp = date)
  
  # Join both
  df <- df %>% inner_join(df.lifeexp)
  
  df <- df %>%
    left_join(wbgref$countries$incomegroups) %>%
    mutate(income_iso3c = factor(income_iso3c, wbgref$incomes$iso3c))

  figure(
    data = df,
    plot = function(df, style = style_atlas(), aspect_ratio = 3) {
      ggplot(df, aes(SH.MED.SAOP.P5, SP.DYN.LE00.IN, color = income_iso3c)) +
        geom_point(size = style$point_size, 
                   shape = style$shapes$point, 
                   stroke = style$point_stroke) +
        geom_vline(xintercept = 20, color = style$colors$reference, linetype=style$linetypes$reference) +
        scale_color_manual(values=style$colors$incomes, labels=wbgref$incomes$labels) +
        scale_fill_manual(values=style$colors$incomes, labels=wbgref$incomes$labels) +
        scale_shape_manual(values=style$shapes$incomes, labels=wbgref$incomes$labels) +
        scale_y_continuous(limits = c(40, 90)) +
        xlab(wbg_name(indicators["surgeons"], mrv = surgeons.years)) +
        style$theme() +
        style$theme_scatter() +
        theme(legend.position = c(0.8, 0.2))
    },
    aspect_ratio = 1,
    title = "Better staffed health systems can lead to improved health outcomes. For example, life expectancies are higher where there are more surgical workers per person.",
    subtitle = wbg_name(indicator="Life expectancy at birth",by = "by country", year = lifeexp.year, denom ="years"),
    source = paste("Source:", "The Lancet Commission on Global Surgery and UN Population Division. WDI (SH.MED.SAOP.P5; SP.DYN.LE00.IN).")
  )
}  

fig_sdg3_uhc_service_index <- function(year = 2015) {
  indicator <- "SH.UHC.SRVS.CV.XD"

  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    year=year, 
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg3_uhc_service_index.csv"
  )
  
  df <- df %>% right_join(data.frame(iso3c = wbgref$countries$iso3c, stringsAsFactors=FALSE))
  df$bins <- supercut(df$value, c(
    "Under 50" = "[0,50)",
    "50–60" = "[50,60)",
    "60–70" = "[60,70)",
    "70 and over" = "[70,Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
    },
    aspect_ratio = 1.5,
    title = "Universal health coverage is about all people having access to the care they need without financial hardship. Service coverage varies widely across countries.",
    subtitle = wbg_name(indicator="Universal Health Coverage (UHC) service index", year = year),
    source = "Source: Hogan and others. Universal Health Coverage (SH.UHC.SV.COV.IND)."
  )
}

fig_sdg3_proportion_10pc <- function(years = c(2000,2005,2010)){
  indicator = "SH.UHC.OOPC.10.ZS"
  
  # This is now available in the API as the indicator code above.

  wbg_uhc_regions<- tribble(
    ~code, ~iso3c,
    "WB_NAR", "NAC",
    "EAP", "EAS",
    "ECA", "ECS",
    "Global", "WLD",
    "LAC", "LCN",
    "MNA", "MEA",
    "SAR", "SAS",
    "SSA", "SSF"
  )
  
  df <- readxl::read_excel("inputs/sdg3/UHCEXCEL.xlsx", "Data")
  
  df <- df %>% filter(`Indicator Code` %in% c("SH.UHC.OOPC.10.ZS"))
  df <- df %>% filter(`Country Code` %in% wbg_uhc_regions$code)
  df <- df %>%
    right_join(wbg_uhc_regions, by = c("Country Code" = "code")) %>%
    subset(select = -c(`Country Code`,`Country Name`, `Indicator Name`)) %>%
    subset(select =c(`iso3c`,1,`2000`,`2005`,`2010`)) %>%
    gather("year", "value", 3:5) %>%
    mutate(year = as.numeric(year))
  
  df <- df %>% filter(iso3c != "WLD")
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(x=year,y=value,color=iso3c,linetype=iso3c)) +
        geom_line(size = style$linesize) +
        scale_x_continuous(breaks = bracketed_breaks(at = 5)) +
        scale_y_continuous(limits = c(0, 18)) +
        scale_linetype_manual(values = style$linetypes$regions) +
        scale_color_manual(values = style$colors$regions) +
        style$theme()  
    },
    #title = "Catastrophic spending—if more than 10% of a household's budget is spent on health care—varies substantially by region",
    subtitle = "Proportion of population spending more than 10 percent of household consumption or income on out-of-pocket healthcare expenditure (%)",
    source = paste("Source:"," Wagstaff and others. WDI (SH.UHC.OOPC.10.ZS).")
  )
}

fig_sdg3_number_10pc <- function(years = c(2000,2005,2010)){
  indicator = "SH.UHC.OOPC.10.TO"
  
  #TODO - update this data section when correct aggregates are available via API

  wbg_uhc_regions<- tribble(
    ~code, ~iso3c,
    "WB_NAR", "NAC",
    "EAP", "EAS",
    "ECA", "ECS",
    "Global", "WLD",
    "LAC", "LCN",
    "MNA", "MEA",
    "SAR", "SAS",
    "SSA", "SSF"
  )
  
  df <- readxl::read_excel("inputs/sdg3/UHCEXCEL.xlsx", "Data")
  
  df <- df %>% filter(`Indicator Code` %in% c("SH.UHC.OOPC.10.TO"))
  df <- df %>% filter(`Country Code` %in% wbg_uhc_regions$code)
  df <- df %>%
    right_join(wbg_uhc_regions, by = c("Country Code" = "code")) %>%
    subset(select = -c(`Country Code`,`Country Name`, `Indicator Name`)) %>%
    subset(select =c(`iso3c`,1,`2000`,`2005`,`2010`)) %>%
    gather("year", "value", 3:5)
  
  figure(
    data = df, 
    
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = fct_reorder2(iso3c, year, -value))
      ggplot(df %>% filter(!iso3c=="WLD"), aes(x = year, y = value, fill = iso3c)) +
        geom_col() +
        scale_x_discrete() +
        scale_y_continuous(labels = millions()) +
        scale_fill_manual(
          values = c(style$colors$regions, style$colors$world),
          labels = wbgref$all_geo$labels
        ) +
        style$theme()
    },
    subtitle = wbg_name(indicator="Number of people spending more than 10 percent of household consumption or income on out-of-pocket healthcare expenditure", denom = "millions"),
    source = paste("Source: Wagstaff and others. WDI (SH.UHC.OOPC.10.TO).")
  )
}

fig_sdg3_impoverished_190_310 <- function(years = c(2000,2005,2010)){
  
  indicators = c("SH.UHC.NOP1.TO", "SH.UHC.NOP2.TO")
  #TODO - update this data section when correct aggregates are available via API

  wbg_uhc_regions<- tribble(
    ~code, ~iso3c,
    "WB_NAR", "NAC",
    "EAP", "EAS",
    "ECA", "ECS",
    "Global", "WLD",
    "LAC", "LCN",
    "MNA", "MEA",
    "SAR", "SAS",
    "SSA", "SSF"
  )
  
  df <- readxl::read_excel("inputs/sdg3/UHCEXCEL.xlsx", "Data")
  
  df <- df %>% filter(`Indicator Code` %in% c("SH.UHC.NOP1.TO","SH.UHC.NOP2.TO"))
  df <- df %>% filter(`Country Code` %in% wbg_uhc_regions$code)
  df <- df %>%
    right_join(wbg_uhc_regions, by = c("Country Code" = "code")) %>%
    subset(select = -c(`Country Code`,`Country Name`, `Indicator Name`)) %>%
    subset(select =c(`iso3c`,1,`2000`,`2005`,`2010`)) %>%
    gather("year", "value", 3:5) 

  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      labels <- c( "SH.UHC.NOP1.TO" = "People pushed below $1.90 a day poverty line", "SH.UHC.NOP2.TO" = "People pushed below $3.10 a day poverty line")
      df <- df %>% mutate(iso3c = fct_reorder2(iso3c, year, -value))
      ggplot(df %>% filter(!iso3c=="WLD"), aes(x = year, y = value, fill = iso3c)) +
        geom_col() +
        scale_y_continuous(labels = millions()) +
        scale_fill_manual(
          values = c(style$colors$regions, style$colors$world),
          labels = wbgref$all_geo$labels,
          guide = guide_legend(reverse = TRUE)
        ) +
        facet_wrap(~`Indicator Code`, labeller=labeller(`Indicator Code` = labels)) +
        style$theme() +
        style$theme_legend("top") +
        theme(strip.text.x = element_text(hjust=0.5))
      
    },
    title = "In 2010, 800 million people spent over 10 percent of their household budget on healthcare, and 97 million were pushed into extreme poverty by health spending.",
    subtitle = wbg_name(indicator = "Number of people pushed into poverty by out-of-pocket healthcare expenditures", denom = "millions"),
    source = paste("Source: Wagstaff and others. WDI (SH.UHC.NOP1.TO; SH.UHC.NOP2.TO; SH.UHC.OOPC.10.TO; SH.UHC.OOPC.10.ZS).")
  )
}

#make_all(path = "docs/sdg3/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg3", styler = style_atlas, saver = figure_save_draft_png) {
  #Page 1
  saver(fig_sdg3_pyramids_by_income(), styler, file.path(path, "fig_sdg3_pyramids_by_income.png"), width = 5.5, height = 3.6)
  saver(fig_sdg3_life_expectancy(), styler, file.path(path, "fig_sdg3_life_expectancy.png"), width = 5.5, height = 3.4)

  #Page 2
  saver(fig_sdg3_death_pyramids_by_income(), styler, file.path(path, "fig_sdg3_death_pyramids_by_income.png"), width = 5.5, height = 2.65)
  saver(fig_sdg3_child_maternal_mortality_attendance(), styler, file.path(path, "fig_sdg3_child_maternal_mortality_attendance.png"), width = 5.5, height=2.6, padding=margin(0,0,0,0,"mm"))
  saver(fig_sdg3_road_mortality(), styler, file.path(path, "fig_sdg3_road_mortality.png"), width = 5.5, height=3.5)

  #Page 3
  saver(fig_sdg3_health_workforce(), styler, file.path(path, "fig_sdg3_health_workforce.png"), width = 5.65, height = 2.2)
  saver(fig_sdg3_surgical_workforce(), styler, file.path(path, "fig_sdg3_surgical_workforce.png"), width = 5.5, height=2.2)
  saver(fig_sdg3_surgeons_vs_life_expectancy(), styler, file.path(path, "fig_sdg3_surgeons_vs_life_expectancy.png"), width = 5.5, height=4)

  #Page 4
  saver(fig_sdg3_uhc_service_index(), styler, file.path(path, "fig_sdg3_uhc_service_index.png"), width = 5.5, height = 4.1)
  saver(fig_sdg3_proportion_10pc(), styler, file.path(path, "fig_sdg3_proportion_10pc.png"), width = 2.67, height=2)
  saver(fig_sdg3_number_10pc(), styler, file.path(path, "fig_sdg3_number_10pc.png"), width = 2.67, height=2)
  saver(fig_sdg3_impoverished_190_310(), styler, file.path(path, "fig_sdg3_impoverished_190_310.png"), width = 5.5,height=2.7)
}
