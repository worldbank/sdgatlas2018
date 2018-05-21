library(ggplot2)
library(png)
library(dplyr)
library(forcats)
library(readxl)
library(curl)
library(tidyr)
library(wbgdata)
library(wbgcharts)
library(wbggeo)
library(wbgmaps)
source("styles.R")
  
fig_sdg8_gdp_pc_ldcs <- function(years = 2007:2016) {
  # Get the codes of LDC countries
  groups <- read_excel("inputs/reference_data/CLASS.xls", sheet = "Groups")

  ldc <- groups %>%
    subset(GroupName == "Least developed countries: UN classification") %>%
    pull(CountryCode)
  
  # Get GDP ####################
  indicator <- "NY.GDP.MKTP.KD"
  df <- wbgdata(
    ldc,
    indicator,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_gdp_pc_ldcs.csv"
  )
  
  # This ugly code captures the minimum and maximum values and years wide-style
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    summarize(min_date = min(date), max_date = max(date)) %>%
    merge(df, by.x = c("iso3c", "min_date"), by.y = c("iso3c", "date"), all.x = TRUE) %>%
    merge(df, by.x = c("iso3c", "max_date"), by.y = c("iso3c", "date"), all.x = TRUE) %>%
    select(subset = -c(indicatorID.y)) %>%
    rename(indicatorID = indicatorID.x, min_date_value = value.x, max_date_value = value.y)
  
  # Calculate annualised growth rate
  df <- df %>%
    mutate(growth_ratio = (max_date_value / min_date_value)) %>%
    mutate(rate = growth_ratio ^ (1 / (max_date - min_date))) %>%
    mutate(value = (rate - 1) * 100)
  
  # Get GDP per capita ########
  indicator.percap <- "NY.GDP.PCAP.KD"
  df_pc <- wbgdata(
    ldc,
    indicator.percap,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_gdp_pc_ldcs-percap.csv"
  )
  
  # This ugly code captures the minimum and maximum values and years wide-style
  df_pc <- df_pc %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    summarize(min_date = min(date), max_date = max(date)) %>%
    merge(df_pc, by.x = c("iso3c", "min_date"), by.y = c("iso3c", "date"), all.x = TRUE) %>%
    merge(df_pc, by.x = c("iso3c", "max_date"), by.y = c("iso3c", "date"), all.x = TRUE) %>%
    select(subset = -c(indicatorID.y)) %>%
    rename(indicatorID = indicatorID.x, min_date_value = value.x, max_date_value = value.y)
  
  # Calculate annualised growth rate
  df_pc <- df_pc %>%
    mutate(growth_ratio = (max_date_value / min_date_value)) %>%
    mutate(rate = growth_ratio ^ (1 / (max_date - min_date))) %>%
    mutate(value = (rate - 1) * 100)
  
  # Select only what we need and combine into one data frame
  df <- df %>% select(iso3c, indicatorID, value)
  df_pc <- df_pc %>% select(iso3c, indicatorID, value)
  df <- rbind(df, df_pc)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% filter(!iso3c %in% c("SDN","ERI","SSD"))
      
      ggplot(df, aes(fct_reorder(iso3c, value), value)) +
        geom_col(aes(fill = indicatorID), data = .%>% filter(indicatorID == "NY.GDP.MKTP.KD")) +
        geom_hline(yintercept = 7,linetype=style$linetypes$reference, size = style$linesize_reference, color = "grey80") +
        geom_point(
          aes(iso3c, value, color = indicatorID), 
          data= df %>% filter(indicatorID == "NY.GDP.PCAP.KD"),
          size=style$point_size, shape=style$shapes$point, stroke=style$point_stroke
        ) +
        scale_y_continuous(expand = c(0,0), limits=c(-10,14), breaks=c(-10,-5,0,5,10))+
        scale_fill_manual(values = c("NY.GDP.MKTP.KD" = style$colors$spot.secondary.light, "NY.GDP.PCAP.KD" = style$colors$spot.primary), labels = c("NY.GDP.MKTP.KD" = "GDP growth", "NY.GDP.PCAP.KD" = "GDP per capita growth")) +
        scale_color_manual(values = c("NY.GDP.MKTP.KD" = style$colors$spot.secondary.light, "NY.GDP.PCAP.KD" = style$colors$spot.primary), labels = c("NY.GDP.MKTP.KD" = "GDP growth", "NY.GDP.PCAP.KD" = "GDP per capita growth")) +
        geom_text(
          data=.%>% group_by(iso3c) %>% summarise(value = max(value, 0)),
          mapping = aes(label=unlist(wbgref$countries$labels)[iso3c]),
          size = style$gg_text_size,
          family = style$family,
          color = style$colors$text,
          hjust = 0,
          nudge_y=0.3
        ) +
        coord_flip() +
        style$theme() +
        style$theme_barchart()+
        theme(
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.spacing.y = unit(0, "lines"),
          legend.margin=margin(c(0,0,0,0)),
          legend.position = c(0.95, 0.05), 
          legend.justification = c(1,0)
        )
    },
    aspect_ratio = 0.8,
    title = "Many Least Developed Countries have seen economic growth in the last decade, but few have achieved the SDG target of 7 percent a year.",
    subtitle = wbg_name(indicator="Average annual GDP and GDP per capita growth", mrv=years, denom="%"),
    note="Note: Data are not available for Djibouti, Eritrea, Niger, Somalia, South Sudan and Sudan.",
    source = "Source: World Bank national accounts data and OECD National Accounts data files. WDI (NY.GDP.MKTP.KD; NY.GDP.PCAP.KD)."
  )
}

fig_sdg8_sector_map <- function(year = 2016) {
  indicators <- c(
    Agriculture = "SL.AGR.EMPL.ZS",
    Industry = "SL.IND.EMPL.ZS",
    Services = "SL.SRV.EMPL.ZS"
  )
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    rename.indicators = TRUE,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_sector_map.csv"
  )
  
  df$bins <- supercut(df$value, c(
    "0-25" = "[0, 25)",
    "25-50" = "[25, 50)",
    "50-75" = "[50, 75)",
    "75-100" = "[75, 100]"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      maps <- wbgmaps::wbgmaps[[quality]]
      p <- ggplot() + 
        geom_map(data = df, aes(map_id = iso3c, fill = bins), map = maps$countries) + 
        geom_polygon(data = maps$disputed, aes(long, lat, group = group, map_id = id), fill = "grey80") + 
        geom_polygon(data = maps$lakes, aes(long, lat, group = group), fill = "white") + 
        # geom_path(data = maps$boundaries,
        #           aes(long, lat, group = group),
        #           color = "white",
        #           size = 0.1,
        #           lineend = maps$boundaries$lineend) +
        #          linetype = maps$boundaries$linetype) +
        scale_x_continuous(expand = c(0, 0), limits = standard_crop_wintri()$xlim) +
        scale_y_continuous(expand = c(0, 0), limits = standard_crop_wintri()$ylim) +
        scale_fill_manual(palette = style$colors$continuous,
                          na.value = "grey80",
                          labels = rename_na("No data"),
                          drop = FALSE) +
        coord_equal() + 
        facet_wrap( ~ indicatorID) +
        style$theme()+ 
        style$theme_map(1) + 
        theme(strip.text = element_text(hjust = 0.5))
      
      pg <- wbg_color_disputed(p)
      pg$theme <- style$theme()
      pg
    },
    aspect_ratio = 1.2,
    title = "Agriculture dominates employment in South Asia and Sub-Saharan Africa, while most people in Europe & Central Asia, Latin America & Caribbean and North America work in the service sector.",
    subtitle = "Employment by sector, 2016 (% of total employment)",
    source = paste0("Source: ILO. World Development Indicators (SL.AGR.EMPL.ZS; SL.IND.EMPL.ZS; SL.SRV.EMPL.ZS).")
  )
}

fig_sdg8_emp_sector_panel <- function(years = 1990:2016) {
  indicators <- c("SL.AGR.EMPL.ZS", "SL.IND.EMPL.ZS", "SL.SRV.EMPL.ZS")
  
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_emp_sector_panel.csv"
  )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = factor(iso3c, rev(wbgref$incomes$iso3c)))
      iso3c_labeller <- as_labeller(function(l) wbgref$incomes$labels[l])
      ggplot(df, aes(date, value, group = indicatorID, color = indicatorID)) +
        geom_line(size=style$linesize) +
        scale_y_continuous(expand=c(0,0), limits = c(0, 80)) +
        scale_x_continuous(expand=c(0,0), limits=c(1990,2020), breaks = c(1990,2016)) +
        scale_color_manual(
          values = c(
            SL.AGR.EMPL.ZS = style$colors$spot.secondary,
            SL.IND.EMPL.ZS = style$colors$spot.secondary.light,
            SL.SRV.EMPL.ZS = style$colors$spot.primary
          ),
          labels = c(
            SL.AGR.EMPL.ZS = "Agriculture",
            SL.IND.EMPL.ZS = "Industry",
            SL.SRV.EMPL.ZS = "Services"
          )
        ) +
        facet_wrap(~iso3c, nrow=1, labeller = iso3c_labeller) +
        style$theme() +
        style$theme_legend("top") +
        theme(panel.spacing.x = unit(0.03, "npc"))
    },
    aspect_ratio = 1.2,
    title = "In the early 2000s the service sector overtook agriculture to become the world's largest employer. Globally, services account for 50 percent of employment, agriculture 30 percent and industry 20 percent.",
    subtitle = paste0("Employment by sector (% of total employment)"),
    source = paste0("Source: ILO. World Development Indicators (SL.AGR.EMPL.ZS; SL.IND.EMPL.ZS; SL.SRV.EMPL.ZS).")
  )
}

fig_sdg8_emp_gap_number_panel <- function(years = 1990:2016) {
  indicators <- c(
    "SP.POP.TOTL",
    "SP.POP.1564.TO.ZS",
    "SP.POP.65UP.TO.ZS", 
    "SL.EMP.TOTL.SP.ZS"
  )
  
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = years,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_emp_gap_number_panel.csv"
  )
  
  df <- df %>% 
    mutate(
      count_pop  = (SP.POP.1564.TO.ZS/100 * SP.POP.TOTL) + (SP.POP.65UP.TO.ZS/100 * SP.POP.TOTL),
      count_employ = SL.EMP.TOTL.SP.ZS/100 * count_pop
    ) %>%
    gather(indicatorID, value, c(count_pop, count_employ)) %>%
    select(iso3c, indicatorID, date, value)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(
        iso3c = factor(iso3c, rev(wbgref$incomes$iso3c))
      )
      iso3c_labeller <- as_labeller(function(l) wbgref$incomes$labels[l])
      
      ggplot(df, aes(date, value, group = indicatorID, color = indicatorID)) +
        geom_line(size=style$linesize) +
        scale_y_continuous(expand=c(0,0), limits=c(0,2.2e09),labels = billions()) +
        scale_x_continuous(expand=c(0,0), limits=c(1990,2020), breaks = c(1990,2016)) +
        scale_color_manual(
          values = c(
            count_pop = style$colors$spot.primary.light,
            count_employ = style$colors$spot.primary
          ),
          labels = c(
            count_pop = "Population ages 15 and older",
            count_employ = "Employed ages 15 and older"
          )
        ) +
        facet_wrap(~iso3c, nrow=1, labeller = iso3c_labeller) +
        style$theme() +
        style$theme_legend("top") + 
        theme(panel.spacing.x = unit(0.03, "npc"))
    },
    aspect_ratio = 1.2,
    title = "Not everyone of working age can find employment, especially young people. And as populations age, the share of the population that is working falls.",
    subtitle = paste0("People (billions)"),
    source = paste0("Source: ILO. WDI (SP.POP.TOTL; SP.POP.1564.TO.ZS; SP.POP.65UP.TO.ZS; SL.EMP.TOTL.SP.ZS).")
  )
}

fig_sdg8_labor_stat_GDP_PC <- function(year = 2016) {
  indicators <- c(
    "SL.EMP.TOTL.SP.FE.ZS", # Employment to population ratio, 15+, female
    "SL.EMP.TOTL.SP.MA.ZS", # Employment to population ratio, 15+, male
    "SL.UEM.TOTL.FE.ZS",    # Unemployment as % labor force, female
    "SL.UEM.TOTL.MA.ZS",    # Unemployment as % labor force, male
    "SL.TLF.CACT.FE.ZS",    # Labor force participation rate, 15+, female
    "SL.TLF.CACT.MA.ZS"     # Labor force participation rate, 15+, male
  )
  
  # Get data for both genders for all income groups
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = year,
    indicator.wide=TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_labor_stat_GDP_PC.csv"
  )
  
  # Female gender analysis - we want to calculate everything over a denom of 15+, gender
  df_female <- df %>% 
    na.omit() %>% 
    mutate(
      employed = SL.EMP.TOTL.SP.FE.ZS,                             # Employment is already correctly denominated
      unemployed = (SL.UEM.TOTL.FE.ZS * SL.TLF.CACT.FE.ZS  / 100), # Unemployment we need to scale by labor force participation
      out_of_labor_force = 100 - employed - unemployed,            # Out of labor force is the residual
      gender = "Female"
    ) %>% 
    select(iso3c, employed, unemployed, out_of_labor_force, gender) %>% 
    gather(employment_category, value, c(employed, unemployed, out_of_labor_force))

  # Male gender analysis
  df_male <- df %>% 
    na.omit() %>% 
    mutate(
      employed = SL.EMP.TOTL.SP.MA.ZS,
      unemployed = (SL.UEM.TOTL.MA.ZS * SL.TLF.CACT.MA.ZS / 100),
      out_of_labor_force = 100 - employed - unemployed,
      gender = "Male"
    ) %>% 
    select(iso3c, employed, unemployed, out_of_labor_force, gender) %>% 
    gather(employment_category, value, c(employed, unemployed, out_of_labor_force))
  
  # Recombine genders
  df <- rbind(df_female, df_male) %>% mutate(gender=fct_rev(gender))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(gender, value, fill = factor(employment_category, levels = c("employed", "unemployed", "out_of_labor_force")))) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(
          values = c(
            employed = style$colors$spot.primary,
            unemployed = style$colors$spot.primary.light,
            out_of_labor_force = style$colors$spot.secondary
          ),
          labels = c(
            c(
              employed = "Employed",
              unemployed = "Unemployed",
              out_of_labor_force = "Out of labor force"
            )
          )
        ) +
        facet_wrap(~ factor(iso3c, levels = c("LIC", "LMC", "UMC", "HIC")), 
                   ncol = 7, 
                   labeller = as_labeller(wbgref$incomes$labels)
        ) +
        style$theme() +
        style$theme_legend("top")  +
        theme(strip.text.x = element_text(hjust=0.5))
    },
    aspect_ratio = 1,
    title = "Globally, women are less likely to be employed than men, but the gap is most pronounced in lower-middle-income countries.",
    subtitle = wbg_name(indicator = "Share of people by employment status",year = year, denom = "% of population ages 15 and older"),
    source = paste("Source: ILO. World Development Indicators (SL.UEM.TOTL.FE.ZS; SL.UEM.TOTL.MA.ZS; SL.TLF.CACT.FE.ZS; SL.TLF.CACT.MA.ZS; SL.EMP.TOTL.SP.FE.ZS; SL.EMP.TOTL.SP.MA.ZS; SP.POP.1564.FE.ZS; SP.POP.65UP.FE.ZS; SP.POP.1564.MA.ZS; SP.POP.65UP.MA.ZS).")
  )
}

fig_sdg8_wage_gender <- function(years = 2016) {
  indicators <- c(
    # Employers
    "SL.EMP.MPYR.FE.ZS",
    "SL.EMP.MPYR.MA.ZS",

    # Wage & salaried
    "SL.EMP.WORK.FE.ZS",
    "SL.EMP.WORK.MA.ZS",
    
    # Own account
    "SL.EMP.OWAC.FE.ZS",
    "SL.EMP.OWAC.MA.ZS",
    
    # Family
    "SL.FAM.WORK.FE.ZS",
    "SL.FAM.WORK.MA.ZS"
  )
  
  df <- wbgdata(
    wbgref$regions$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_wage_gender.csv"
  )

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      
      df <- df %>%
        mutate(
          ind_gen = substr(indicatorID, 1, 11),
          ind_sex = substr(indicatorID, 13, 14)
        ) %>%
        mutate(
          ind_gen = fct_reorder(ind_gen, -value),
          ind_sex = fct_reorder(ind_sex, -value),
          iso3c   = fct_reorder2(iso3c, ind_gen == "SL.EMP.WORK", -value)
        )
      
      p <- ggplot(df, aes(ind_sex, value, fill = ind_gen)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_x_discrete(labels = c(FE = "Female", MA = "Male")) +
        scale_y_continuous(expand=c(0,0), limits=c(0,103)) +
        scale_fill_manual(
          values = style$colors$categorical,
          labels = c(
            c(SL.EMP.WORK = "Wage and salaried workers",
              SL.EMP.OWAC = "Own-account workers",
              SL.FAM.WORK = "Contributing family workers",
              SL.EMP.MPYR = "Employers")
          )
        ) +
        facet_wrap(~ iso3c,
                   ncol = 1,
                   strip.position = "left",
                   labeller = as_labeller(wbgref$regions$labels)
        ) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") + 
        theme(strip.placement = "outside",
              strip.text.y = element_text(angle = 180))
      
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 3
      g$theme <- p$theme
      g
    },
    aspect_ratio = 1,
    title = paste0("Many people in South Asia and Sub-Saharan Africa work for themselves or their family. They are more likely to lack social safety nets, and they face a greater risk from economic shocks than salaried workers do."),
    subtitle = wbg_name(indicator = "Employment type", year=years, denom ="% of total employment"),
    source = paste("Source: ILO. World Development Indicators (SL.EMP.MPYR.FE.ZS; SL.EMP.MPYR.MA.ZS; SL.EMP.WORK.FE.ZS; SL.EMP.WORK.MA.ZS; SL.EMP.OWAC.FE.ZS; SL.EMP.OWAC.MA.ZS; SL.FAM.WORK.FE.ZS; SL.FAM.WORK.MA.ZS).")
  )
}

fig_sdg8_account_map <- function(year = 2017) {
  indicator <- "FX.OWN.TOTL.ZS"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_account_map.csv"
  )
  
  df$bins <- supercut(df$value, c(
    "0–20" = "[0,20)",
    "20–40" = "[20,40)",
    "40–65" = "[40,65)",
    "65–90" = "[65,90)",
    "90–100" = "[90,100]"
  ))
 
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      wbg_choropleth(df, wbgmaps[[quality]], style = style, variable = "bins", legend.nrow = 1)
    },
    aspect_ratio = 1.3,
    title = "Access to financial services benefits individuals and societies. Globally, 69 percent of adults have an account with a financial institution or mobile money provider.",
    subtitle = wbg_name(indicator = "Account ownership", year = year, denom = "% of population ages 15 and older"),
    source = paste("Source: Global Findex Database. World Development Indicators (FX.OWN.TOTL.ZS).")
  )
}

fig_sdg8_findex_panel_dimensions <- function(year = 2017) {
  indicators <- tribble(
    ~indicatorID,        ~group,      ~category,
    "FX.OWN.TOTL.OL.ZS", "Age",       "Ages 25 and above",
    "FX.OWN.TOTL.YG.ZS", "Age",       "Ages 15–24",
    "FX.OWN.TOTL.SO.ZS", "Education", "Secondary or more",
    "FX.OWN.TOTL.PL.ZS", "Education", "Primary or less", 
    "FX.OWN.TOTL.MA.ZS", "Gender",    "Male",
    "FX.OWN.TOTL.FE.ZS", "Gender",    "Female",
    "FX.OWN.TOTL.60.ZS", "Income",    "Richest 60 percent",
    "FX.OWN.TOTL.40.ZS", "Income",    "Poorest 40 percent"
  )
  
  df <- wbgdata(
    c("WLD", wbgref$regions$iso3c),
    indicators$indicatorID,
    years = year,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg8_findex_panel_dimensions.csv"
  )
  
  df <- df %>% left_join(indicators)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(iso3c = fct_reorder2(iso3c, category=="Ages 15–24", value)) %>%
        mutate(category = factor(category, levels = indicators$category))
      ggplot(df, aes(value, iso3c ,color=category)) +
        geom_other_dotplot(
          aes(value,iso3c, group=paste0(group,category)),
          size = style$point_size,
          stroke=style$point_stroke,
          shape = style$shapes$point
        ) +
        scale_color_manual(values = c(
          "Male"               = style$colors$spot.primary,
          "Female"             = style$colors$spot.primary.light,
          "Richest 60 percent" = style$colors$spot.primary,
          "Poorest 40 percent" = style$colors$spot.primary.light,
          "Ages 25 and above"  = style$colors$spot.primary,
          "Ages 15–24"         = style$colors$spot.primary.light,
          "Secondary or more"  = style$colors$spot.primary,
          "Primary or less"    = style$colors$spot.primary.light
        )) +
        scale_x_continuous(limits = c(0, 100)) +
        scale_y_discrete(labels = wbgref$all_geo$labels) +
        facet_wrap(~group,nrow=1)+
        style$theme() +
        style$theme_legend("top") +
        style$theme_barchart() + 
        theme(
          strip.text.x=element_text(hjust=0.5),
          panel.spacing.x = unit(0.05, "npc")
        )
    },
    aspect_ratio = 1,
    note = "Note: Data refer to the richest 60 percent and poorest 40 percent within individual economies rather than the region as a whole.",
    title = "Financial account ownership is lower among younger adults, those with less education, women, and poorer adults.",
    subtitle = wbg_name(indicator = "Account ownership", year = 2017, denom = "% of population ages 15 and older"),
    source = "Source: Global Findex Database. World Development Indicators (FX.OWN.TOTL.MA.ZS; FX.OWN.TOTL.FE.ZS; FX.OWN.TOTL.YG.ZS; FX.OWN.TOTL.OL.ZS; FX.OWN.TOTL.PL.ZS; FX.OWN.TOTL.SO.ZS; FX.OWN.TOTL.40.ZS; FX.OWN.TOTL.60.ZS)."
  )
}
  
#make_all(path = "docs/sdg8/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg8", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg8_gdp_pc_ldcs(), styler, file.path(path, "fig_sdg8_gdp_pc_ldcs.png"), width = 5.5, height=7.2)
  
  # page 2
  saver(fig_sdg8_sector_map(), styler, file.path(path, "fig_sdg8_sector_map.png"), width = 5.5, height = 2.65)
  saver(fig_sdg8_emp_sector_panel(), styler, file.path(path, "fig_sdg8_emp_sector_panel.png"), width = 5.5, height=2.9)
  saver(fig_sdg8_emp_gap_number_panel(), styler, file.path(path, "fig_sdg8_emp_gap_number_panel.png"), width = 5.5, height=2.9)
 
  # page 3
  saver(fig_sdg8_labor_stat_GDP_PC(), styler, file.path(path, "fig_sdg8_labor_stat_GDP_PC.png"), width = 5.5, height = 3)
  saver(fig_sdg8_wage_gender(), styler, file.path(path, "fig_sdg8_wage_gender.png"), width=5.5, height=5.5)
  
  # page 4
  saver(fig_sdg8_account_map(), styler, file.path(path, "fig_sdg8_account_map2.png"), width = 5.5, height=4.5)
  saver(fig_sdg8_findex_panel_dimensions(), styler, file.path(path, "fig_sdg8_findex_panel_dimensions2.png"), width = 5.5, height=4, padding=margin(0,5,0,0))
}
