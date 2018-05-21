library(wbgdata)
library(wbgcharts)
library(wbggeo)
library(wbgmaps)
library(ggplot2)
library(dplyr)
library(forcats)
library(tidyr)
library(gtable)
library(stringr)
source("styles.R")

fig_sdg2_stunted_number <- function(years = 1989:2016) {
  indicators <- c("SH.STA.STNT.ZS", "SP.POP.0004.FE", "SP.POP.0004.MA")
  
  df <- wbgdata(
    wbgref$regions$iso3c,
    indicators,
    years = years,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_stunted_number.csv"
  )

  df <- df %>%
    mutate(value = (SH.STA.STNT.ZS/100) * (SP.POP.0004.FE + SP.POP.0004.MA)) %>%
    select(iso3c, date, value) %>%
    filter(complete.cases(.))

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(x = date, y = value, group = iso3c, color = iso3c)) +
        geom_line(size = style$linesize) +
        scale_y_continuous(
          labels = millions(),
          position="right",
          sec.axis = dup_axis(
            breaks = df %>% filter(date == min(date)) %>% pull(value) %>% repel(5e6),
            label = wbgref$regions$labels[df %>% filter(date == min(date)) %>% pull(iso3c)]
          )
        ) +
        scale_x_continuous(breaks = bracketed_breaks(df$date), 
                           expand=c(0,0),
                           limits=range(df$date)) +
        scale_color_manual(
          values = style$colors$regions,
          labels = wbgref$regions$labels
        ) +
        scale_linetype_manual(values = style$linetypes$regions) +
        style$theme()
    },
    title = "Young children and infants are most vulnerable to the effects of malnutrition. Globally, over 95 million fewer children were stunted in 2016 than in 1990.",
    note = "Note: Estimates not available for Europe & Central Asia due to poor data coverage.",
    subtitle = "Number of children under age 5 that are stunted, height for age (millions)",
    source = "Source: UNICEF, WHO and World Bank. WDI (SH.STA.STNT.ZS); Health Nutrition and Population Statistics (SP.POP.0004.FE; SP.POP.0004.MA)."
  )
}

fig_sdg2_malnutrition_dimensions <- function(year = 2016) {
  indicators = c(
    "SH.STA.STNT.ZS",   # stunting
    "SH.STA.WAST.ZS",   # wasting
    "SH.SVR.WAST.ZS",   # severe wasting
    "SH.STA.OWGH.ZS"    # overweight 
  )

  df.incomes <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_malnutrition_dimensions-incomes.csv"
  )
  df.incomes <- df.incomes %>% mutate(group="By income") 
  
  df.regions <- wbgdata(
    wbgref$regions$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_malnutrition_dimensions-regions.csv"
  )
  df.regions <- df.regions %>% mutate(group="By region")
  
  df <- rbind(df.regions, df.incomes)
  
  df <- df %>% gather("indicator", "value", -group,-iso3c,-date) 
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      labels <- c(wbgref$regions$labels,wbgref$incomes$labels)
      df <- df %>% mutate(
        iso3c = fct_reorder2(iso3c, indicator == "SH.STA.STNT.ZS", -value),
        indicator = factor(indicator, levels=c("SH.STA.STNT.ZS","SH.STA.WAST.ZS","SH.SVR.WAST.ZS","SH.STA.OWGH.ZS")),
        group = factor(group, levels=c("By region", "By income"))
      )
      
      ggplot(df, aes(iso3c, value )) +
        geom_col(aes(y = value), fill=style$colors$spot.primary) +
        facet_grid(group ~ indicator, scales = "free" , space = "free", 
                   labeller = as_labeller(c(
                              SH.STA.STNT.ZS = "Stunting",
                              SH.STA.WAST.ZS = "Wasting",
                              SH.SVR.WAST.ZS = "Severe wasting",
                              SH.STA.OWGH.ZS = "Overweight"
                    ))
        ) +
        coord_flip() +
        scale_x_discrete(labels = labels) +
        scale_y_continuous(limits=c(0,50), expand = c(0, 0)) +
        style$theme() +
        style$theme_barchart() +
        theme (
          plot.margin = margin(15,3,5,0,"mm"),
          panel.spacing.x = unit(0.05,"npc"),
          strip.text.y = element_blank()
        )
    },
    aspect_ratio = 1.25,
    title = "Malnutrition is manifested in multiple ways. In lower-middle-income countries, 12 percent of children suffer from wasting, while 5 percent are overweight.",
    subtitle = paste0("Prevalence of different types of malnutrition, children under age 5, 2016 (%)"),
    note= "Note: Regional aggregates for Europe & Central Asia are not available.",
    source = paste("Source: UNICEF, WHO and World Bank. WDI (SH.STA.STNT.ZS; SH.STA.WAST.ZS; SH.SVR.WAST.ZS; SH.STA.OWGH.ZS).")
  )
}
  
fig_sdg2_stunted_quintile <- function(years = 2014:2017) {
  indicators <- c("lowest" = "SH.STA.STNT.Q1.ZS", "highest" = "SH.STA.STNT.Q5.ZS")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_stunted_quintile.csv"
  )
  
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c, indicatorID) %>%
    filter(date == max(date)) %>%
    slice(which.max(date)) %>%
    ungroup()

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%  mutate(iso3c = fct_reorder2(iso3c, indicatorID == "lowest", -value)) 
      
      p <- ggplot(df, aes(x=value, y=iso3c, color=as.factor(indicatorID))) +
        geom_other_dotplot(aes(y=iso3c), size = style$point_size, shape = style$shapes$point, stroke = style$point_stroke) +
        scale_y_discrete(expand=c(0,1), labels = wbgref$countries$labels) +
        scale_x_continuous(expand = c(0,-1), limits = c(-2,70)) +
        scale_color_manual(
          values = c(
            lowest = style$colors$spot.primary.light, 
            highest = style$colors$spot.primary
            ),
          labels = c(
            highest = "Richest wealth quintile", 
            lowest = "Poorest wealth quintile"
            )
          ) +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") 
      
      # Align legend over entire figure not just plot area
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
   aspect_ratio = 0.9,
   title = "There are large differences in stunting rates between rich & poor households...",
   title = "Childhood stunting is seen more in poor households than rich...",
   subtitle = wbg_name(indicator ="Prevalence of stunting, children under 5", mrv = df$date, denom = "%"),
   source = "Source: UNICEF and The DHS Program. Health Nutrition and Population Statistics by Wealth Quintile (SH.STA.STNT.Q1.ZS; SH.STA.STNT.Q5.ZS)"
  )
}

fig_sdg2_stunted_sex <- function(years = 2012:2017) {
  indicators <- c(female = "SH.STA.STNT.FE.ZS", male = "SH.STA.STNT.MA.ZS")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_stunted_sex.csv"
  )

  df <- df %>% 
    spread(indicatorID,value ) %>% 
    filter(complete.cases(.)) %>% 
    gather(indicatorID,value,female,male) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup() 
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% 
        mutate(iso3c = fct_reorder2(iso3c, indicatorID == "male", -value)) %>%
        filter(iso3c != "EGY")
      
      p <- ggplot(df, aes(value, iso3c, color=as.factor(indicatorID))) +
        geom_other_dotplot(aes(y=iso3c), size = style$point_size, shape = style$shapes$point, stroke = style$point_stroke) +
        scale_x_continuous(expand = c(0,0), limits = c(-1,70)) +
        scale_y_discrete(labels = wbgref$countries$labels) +
        scale_color_manual(
          values = style$colors$gender,
          labels = c(male = "Male", female = "Female")
          ) +
        style$theme() +
        style$theme_legend("top") +
        style$theme_barchart() 
      
      # Align legend over entire figure not just plot area
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    title = "…and in many countries, boys are more likely to be stunted than girls.",
    subtitle = wbg_name(indicator ="Prevalence of stunting, children under 5", mrv = df$date, denom = "%"),
    source = "Source: WHO. World Development Indicators (SH.STA.STNT.FE.ZS; SH.STA.STNT.MA.ZS)."
  )
}

fig_sdg2_wasting_panel <- function(years = 2005:2015, top_N=10) {
  indicators <- c(
    "SH.STA.WAST.MA.ZS",      # wasting - M
    "SH.SVR.WAST.MA.ZS",      # severe wasting - M
    "SH.STA.WAST.FE.ZS",      # wasting - F
    "SH.SVR.WAST.FE.ZS"       # severe wasting - F
  )
  average <- "SH.STA.WAST.ZS" # average to do top N
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    year=years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_wasting_panel.csv"
  )
  
  df.average <- wbgdata(
    wbgref$countries$iso3c,
    average,
    year=years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_wasting_panel-average.csv"
  )
  
  #assign country values to F/M
  df <- df[complete.cases(df),] %>%
    group_by(iso3c, indicatorID) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(sex = ifelse(grepl('FE',indicatorID), "Female", "Male" ))
  
  #get means for doing "Top N" by income group
  df.average <- df.average[complete.cases(df),] %>%
    group_by(iso3c, indicatorID) %>%
    filter(date == max(date)) %>%
    #Hack Tonga doesn't have M & F data, should refactor code to do this top N stuff smarter.
    filter(iso3c != "TON") %>%
    rename(mean = value) %>%
    left_join(wbgref$countries$incomegroups) %>%
    group_by(income_iso3c) %>% 
    top_n(n=top_N, mean) %>%
    mutate(topn = "yes") %>%
    ungroup() %>%
    select(iso3c,mean,topn)
   
  df <- df %>% left_join(wbgref$countries$incomegroups)
  df <- df %>% left_join(df.average)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "high", aspect_ratio = 0.25) {
      df <- df %>%
        mutate(income_iso3c = factor(income_iso3c, rev(wbgref$incomes$iso3c))) %>%
        mutate(iso3c = reorder(iso3c, mean)) %>%
        filter(topn == "yes")
      
      df <- df %>% 
      
          mutate(indicatorID = factor(indicatorID, levels=c("SH.SVR.WAST.MA.ZS","SH.STA.WAST.MA.ZS", "SH.SVR.WAST.FE.ZS","SH.STA.WAST.FE.ZS")),
               sex = factor(sex, levels=c("Male", "Female")))
      
        p <- ggplot(df, aes(x = iso3c, y = value, fill = indicatorID)) +
        geom_col(position = "bullet") +
        facet_grid(income_iso3c ~ sex,
                   scales = "free",
                   space = "free",
                   switch = "y",
                   labeller = as_labeller(c(wbgref$incomes$labels,"Male"="Male","Female"="Female"))
                   ) +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(limits = c(0,27), breaks=c(0,5,10,15,20,25), expand=c(0,0)) +
        scale_fill_manual(breaks= c("SH.SVR.WAST.MA.ZS","SH.STA.WAST.MA.ZS"),
                          values = c("SH.SVR.WAST.MA.ZS" = style$colors$spot.primary,
                                     "SH.STA.WAST.MA.ZS" = style$colors$spot.primary.light, 
                                     "SH.SVR.WAST.FE.ZS" = style$colors$spot.primary,
                                     "SH.STA.WAST.FE.ZS" = style$colors$spot.primary.light),
                          labels = c("SH.SVR.WAST.MA.ZS" = "Severe wasting",
                                     "SH.STA.WAST.MA.ZS" = "Wasting")
                          ) + 
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") +
        theme(
            strip.placement = "outside",
            strip.text.y = element_text(angle = 180,vjust=1),
            panel.spacing.x = unit(0.08, "npc")
          )
        
        # Align legend over entire figure not just plot area
        g <- ggplotGrob(p)
        g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
        g$theme <- style$theme()
        g
    },
    title = "Wasting affects 1 in 13 children globally. These 50 million children weigh less than expected for their height. Half of them live in South Asia, and a quarter live in Sub-Saharan Africa. Boys are more often affected than girls.",
    subtitle = wbg_name(indicator="Prevalence of wasting, children under age 5", mrv=years,denom="%"),
    note = paste0("Note: For each income group, up to ",top_N," countries with the highest average wasting rate, and data available for both sexes are shown."),
    source = paste("Source:", wbg_source(indicators, "UNICEF, WHO and World Bank."))
  )
  }

fig_sdg2_undernourish_map <- function(year = 2015) {
  indicator <- "SN.ITK.DEFC.ZS"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_undernourish_map.csv"
  )
  
  df$bins <- supercut(df$SN.ITK.DEFC.ZS, c(
    "0–5" = "[0,5)",
    "5–15" = "[5,15)",
    "15 and over" = "[15,Inf)"
  ))

  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.2,
    title = "Globally, 1 in 10 people are undernourished and do not have enough food to meet their dietary needs. Undernourishment is most widespread in Sub-Saharan Africa, South Asia, and East Asia & Pacific.",
    subtitle = wbg_name(indicatorID = indicator, year = year),
    source = "Source: Food and Agriculture Organization. World Development Indicators (SN.ITK.DEFC.ZS)."
  )
}

fig_sdg2_undernourish_time <- function(years = 1991:2016) {
  indicator <- "SN.ITK.DEFC.ZS"
  
  df <- wbgdata(
    c("WLD", wbgref$regions$iso3c),
    indicator,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_undernourish_time.csv"
  )

  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), aspect_ratio = 3){
      ggplot(df, aes(x = date, y = value, group = iso3c, color = iso3c, linetype = iso3c)) +
        geom_line(size = style$linesize) +
        scale_x_continuous(breaks = bracketed_breaks(at = 5)) +
        scale_y_continuous(limits = c(0, 30)) +
        scale_colour_manual(
          labels = wbgref$all_geo$labels,
          values = c(style$colors$regions, style$colors$world)
          ) +
        scale_linetype_manual(
          labels = wbgref$all_geo$labels,
          values = c(style$linetypes$regions, style$linetypes$world)
          ) +
        style$theme() +
        style$theme_legend("top")
      },
    title = "Undernourishment declining in almost every region, remains highest in Sub-Saharan Africa and South Asia.",
    subtitle = wbg_name(indicator),
    source = paste("Source:", wbg_source(ind))
  )
}

fig_sdg2_fooddeficit_time <- function(years = 1990:2016) {
  indicator <- "SN.ITK.DFCT"

  df <- wbgdata(
    wbgref$regions$iso3c,
    indicator,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg2_undernourish_time.csv"
  )

  figure(
    data = df,
    plot = function(df, style = style_atlas()){
      ggplot(df, aes(x = date, y = value, group = iso3c, color = iso3c, linetype = iso3c)) +
        geom_line(size = style$linesize) +
        scale_x_continuous(breaks=bracketed_breaks(limits = df$date), expand=c(0,0)) +
        scale_y_continuous(
          limits = c(0, 250),
          position="right",
          sec.axis = dup_axis(
            breaks = df %>% subset(date==min(date)) %>% pull(value),
            labels = wbgref$regions$labels[df %>% subset(date==min(date)) %>% pull(iso3c)]
          )) +
        scale_colour_manual(
          labels = wbgref$regions$labels,
          values = c(style$colors$regions)
        ) +
        scale_linetype_manual(
          labels = wbgref$all_geo$labels,
          values = c(style$linetypes$regions, style$linetypes$world)
        ) +
        style$theme() 
    },
    title = "The food deficit measures, on average, how much food people need to stop them from being considered undernourished. They have generally been declining, but food deficits remain at levels where many people lack sufficient calories.",
    subtitle = wbg_name(indicator),
    source = paste("Source: Food and Agriculture Organization. World Development Indicators (SN.ITK.DFCT).")
  )
}

#make_all(path = "docs/sdg2/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg2", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg2_stunted_number(), styler, file.path(path, "fig_sdg2_stunted_number.png"), width = 5.5, height = 3.5)
  saver(fig_sdg2_malnutrition_dimensions(), styler, file.path(path, "fig_sdg2_malnutrition_dimensions.png"), width = 5.5, height = 3.5)

  # page 2
  saver(fig_sdg2_stunted_quintile(), styler, file.path(path, "fig_sdg2_stunted_quintile.png"), width = 2.7, height = 8.5)
  saver(fig_sdg2_stunted_sex(), styler, file.path(path, "fig_sdg2_stunted_sex.png"), width = 2.7, height = 8.5)
 
  # page 3
  saver(fig_sdg2_wasting_panel(), styler, file.path(path, "fig_sdg2_wasting_panel.png"), width = 5.5, height = 8.5)
  
  # page 4
  saver(fig_sdg2_undernourish_map(), styler, file.path(path, "fig_sdg2_undernourish_map.png"), width = 5.5, height = 4.5) 
  saver(fig_sdg2_fooddeficit_time(), styler, file.path(path, "fig_sdg2_fooddeficit_time.png"), width = 5.5, height = 4.0)
}

