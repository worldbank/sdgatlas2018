library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(readxl)
library(countrycode)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(gtable)
source("styles.R")

fig_sdg5_law_gender_hiring <- function(year = 2017) {
  indicator <- "SG.LAW.NODC.HR"
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_law_gender_hiring.csv"
  )
  
  df <- df %>% 
      select(iso3c, "SG.LAW.NODC.HR") %>%
      right_join(wbgref$countries$regions)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      df$bins = factor(ifelse(df$SG.LAW.NODC.HR == 1, "Yes", "No"), levels = c("Yes", "No"))
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, 
                          variable = "bins", 
                          aspect_ratio = 2,
                          fill.values = c(
                            "No" = style$colors$spot.primary.light,
                            "Yes" = style$colors$spot.primary)) 
    },
    title = "Laws are a first step in helping women and girls achieve gender equality. Around half of all countries have laws against gender-based discrimination in hiring.",
    subtitle = "Does the law mandate nondiscrimination based on gender in hiring? 2017",
    note = "a. World Bank Women, Business and the Law 2016",
    source = "Source: World Bank Women, Business and the Law 2018. World Development Indicators (SG.LAW.NODC.HR; SL.EMP.TOTL.SP.FE.ZS)."
  )
}

fig_sdg5_domesticviolence <- function() {
  year <- 2017
  df <- read_excel(path = "inputs/sdg5/WBLRAWDATA2010201829March2018.xlsx", sheet = "WBL2018", col_names = TRUE)
  
  df <- df %>% 
    select(
      iso3c = `Economy code`,
      indicator = `Are there clear criminal penalties for domestic violence?`
    ) %>%
    right_join(wbgref$countries$regions)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      df <- df %>% mutate(indicator = factor(indicator, c("Yes", "No")))
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, 
                          variable = "indicator",
                          aspect_ratio = "wide",
                          fill.values = c(
                            "No" = style$colors$spot.primary.light,
                            "Yes" = style$colors$spot.primary))
      g$theme <- style$theme()
      g
    },
    title = "Laws may help protect women from violence, but two out of five countries have no clear penalties for domestic violence.",
    subtitle="Are there clear criminal penalties for domestic violence? 2017",
    #note = "a. World Development Indicators (SG.VAW.1549.ZS).",
    source = "Source: World Bank Women, Business and the Law 2018. http://wbl.worldbank.org"
  )
}

fig_sdg5_child_marriage_15_18 <- function(years = 2008:2016) {
  indicators <- c(mar_by_18 = "SP.M18.2024.FE.ZS", mar_by_15 = "SP.M15.2024.FE.ZS")
  
  df <- wbgdata(
    wbgref$countries$iso3c, 
    indicators,
    years = years,
    indicator.wide = TRUE,
    rename.indicators = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_child_marriage_15_18.csv"
  )

  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    slice(which.max(date)) %>%
    ungroup()
  
  df <- df %>%
    mutate(btwn_15_18 = mar_by_18 - mar_by_15) %>%
    gather(indicatorID, value, mar_by_15, mar_by_18, btwn_15_18)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(iso3c = fct_reorder2(iso3c, indicatorID == "mar_by_18", -value)) %>%
        filter(indicatorID !="mar_by_18") %>%
        mutate(indicatorID = fct_relevel(indicatorID, "mar_by_15", "btwn_15_18"))
      
      p <- ggplot(df, aes(iso3c, value, fill = indicatorID)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_x_discrete(labels = wbgref$countries$labels) + 
        scale_y_continuous(limit = c(0, 104), expand = c(0, 0)) +
        scale_fill_manual(
          values = c(btwn_15_18 = style$colors$spot.primary, 
                     mar_by_15 = style$colors$spot.secondary),
          labels = c(mar_by_15 = "15 or younger",
                     btwn_15_18 = "Between 15 and 18")
        ) +
        coord_flip() +
        style$theme() +
        style$theme_legend("top") + 
        style$theme_barchart()
      
      # Align legend over entire figure not just plot area
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    title = "Although the legal age of marriage is 18 in most countries, a large share of women are married at an earlier age.",
    subtitle = wbg_name(indicator = "Age at first marriage", mrv = years, denom = "% of women ages 20–24"),
    source = paste0("Source: Household surveys (DHS) and World Bank Women, Business and the Law. World Development Indicators (SP.M18.2024.FE.ZS; SP.M15.2024.FE.ZS).")
  )
}

fig_sdg5_teenage_mothers_rich_poor_average <- function(years = 2008:2017) {
  indicators <- c(quintile_1 = "SP.MTR.1519.Q1.ZS", quintile_5 = "SP.MTR.1519.Q5.ZS")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_teenage_mothers_rich_poor_average.csv"
  )
  
  df <- df[complete.cases(df), ] %>%
    group_by(iso3c, indicatorID) %>%
    slice(which.max(date)) %>%
    ungroup()
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = fct_reorder2(iso3c, indicatorID == "quintile_1", -value))
     
      p <- ggplot(df, aes(x=value, y=iso3c, color=as.factor(indicatorID))) +
        geom_other_dotplot(aes(y=iso3c), size = style$point_size, shape = style$shapes$point, stroke = style$point_stroke) +
        scale_y_discrete(expand=c(0,1), labels = wbgref$countries$labels) +
        scale_x_continuous(limits = c(-1, 84), expand = c(0, 1)) +
        scale_color_manual(
          values = c(quintile_1 = style$colors$spot.primary.light, quintile_5 = style$colors$spot.primary),
          labels = c(quintile_1 = "Poorest quintile", quintile_5 = "Richest quintile"),
          guide = guide_legend(reverse = TRUE)
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
    title = "Girls from poorer households are more likely to become teenage mothers than girls from wealthier households are.",
    subtitle = wbg_name(indicator = "Had a child or is currently pregnant", mrv = df$date, denom = "% of women ages 15-19"),
    source = paste0("Source: Household surveys (DHS, MICS). Health Nutrition and Population Statistics by Wealth Quintile (SP.MTR.1519.Q1.ZS; SP.MTR.1519.Q5.ZS).")
  )
}

fig_sdg5_female_manager_owner <- function(years = 2010:2017) {
  indicator <-"IC.FRM.FEMO.ZS"
  
  df.country <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_female_manager_owner-country.csv"
  )
  df.country <- df.country %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()
  df.country <- df.country %>% left_join(wbgref$countries$regions)
  
  df.region <- wbgdata(
    wbgref$regions$iso3c,
    indicator,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_female_manager_owner-region.csv"
  )
  df.region <- df.region %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(iso3c = fct_reorder(iso3c, -value))
  
  # Make sure both datasets have the same level ordering
  df.country <- df.country %>% mutate(region_iso3c = factor(region_iso3c, levels(df.region$iso3c)))
  
  figure(
    data = list(country = df.country, region = df.region),
    plot = function(dfs, style = style_atlas()) {
      ggplot(dfs$region, aes(iso3c, value)) +
        geom_point(data = dfs$country, aes(region_iso3c,value),  
                   stroke = style$point_stroke, 
                   shape = style$shapes$point,
                   color = style$colors$spot.secondary.light, 
                   size = style$point_size,
                   alpha = 0.5) +
        geom_point(color=style$colors$spot.primary, 
                   stroke = style$point_stroke, 
                   shape = style$shapes$point, 
                   size = style$point_size) +
        scale_x_discrete(labels = wbgref$regions$labels) +
        scale_y_continuous(limits = c(0,100)) +
        coord_flip() +
        style$theme() +
        style$theme_barchart()
    },
    aspect_ratio = 2.5,
    title = "Women lag behind men in business ownership. In every region, on average fewer than half of firms are even partially owned by women.",
    subtitle = wbg_name(indicator="Firms with female participation in ownership, by country and regional median", mrv=years, denom = "%"),
    note = "Note: Aggregates are based mostly on low- and middle-income countries.",
    source = paste("Source: World Bank Enterprise Surveys. World Development Indicators (IC.FRM.FEMO.ZS).")
  )
}

fig_sdg5_female_minister_parliamentarians <- function(years = 2017) {
  indicator <-"SG.GEN.PARL.ZS"
  
  df.country <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_female_minister_parliamentarians-country.csv"
  )
  df.country <- df.country %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()
  df.country <- df.country %>% left_join(wbgref$countries$regions)
  
  df.region <- wbgdata(
    wbgref$regions$iso3c,
    indicator,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_female_minister_parliamentarians-region.csv"
  )
  df.region <- df.region %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(iso3c = fct_reorder(iso3c, -value))
  
  # Make sure both datasets have the same level ordering
  df.country <- df.country %>% mutate(region_iso3c = factor(region_iso3c, levels(df.region$iso3c)))
  
  figure(
    data = list(country = df.country, region = df.region),
    plot = function(dfs, style = style_atlas()) {
      ggplot(dfs$region, aes(iso3c, value)) +
        geom_point(data = dfs$country, aes(region_iso3c,value),  
                   stroke = style$point_stroke, 
                   shape = style$shapes$point, size = style$point_size, 
                   color = style$colors$spot.secondary.light, 
                   alpha = 0.5) +
        geom_point(color=style$colors$spot.primary, 
                   stroke = style$point_stroke, 
                   shape = style$shapes$point, 
                   size = style$point_size) +
        scale_x_discrete(labels = wbgref$regions$labels) +
        scale_y_continuous(limits = c(0,100)) +
        coord_flip() +
        style$theme() +
        style$theme_barchart()
    },
    aspect_ratio = 2.5,
    title = "In political life, men are overrepresented. Across regions, women on average occupy less than a quarter of parliamentary seats.",
    subtitle = wbg_name(indicator="Proportion of seats held by women in national parliaments, by country and regional median", year=years, denom = "%"),
    source = paste("Source: Inter-Parliamentary Union. World Development Indicators (SG.GEN.PARL.ZS).")
  )
}

fig_sdg5_unpaid_work <- function(years = 2007:2015) {
  indicators <- c("SG.TIM.UWRK.MA", "SG.TIM.UWRK.FE")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = years, 
    indicator.wide = FALSE, 
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_unpaid_work.csv"
  )
  df <- df %>%
    group_by(iso3c, indicatorID) %>%
    filter(date == max(date)) %>%
    ungroup()
  df <- df %>% left_join(wbgref$countries$regions)
  df <- df %>% spread(indicatorID, value)
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas(), quality = "low") {
      p<- ggplot(df, aes(SG.TIM.UWRK.MA, SG.TIM.UWRK.FE, color = region_iso3c)) +
        geom_point(alpha = 1.0, 
                   size = style$point_size, 
                   stroke = style$point_stroke,
                   shape = style$shapes$point) +
        geom_abline(slope = 1, intercept = 0, 
                    color = style$colors$reference, 
                    linetype=style$linetypes$reference) +
        scale_color_manual(values = style$colors$regions, 
                           labels = wbgref$regions$labels) +
        scale_x_continuous(limits = c(0, 30), 
                           breaks = (0:3)*10) +
        scale_y_continuous(limits = c(0, 30), breaks = (0:3)*10) +
        coord_equal() +
        style$theme() +
        style$theme_scatter() +
        xlab("Men\n") +
        ylab("Women\n") +
        theme(panel.grid.major.x = NULL, 
              legend.position = c(1.3, 0.8),
              plot.margin = margin(0,50,0,0,"mm"),
              axis.title.y = element_text(angle = 90)
        )
      
      # Align legend over entire figure not just plot area
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    
    aspect_ratio = 0.9,
    title = paste0("Women average 2.6 times as much time on unpaid care & domestic work as men do."),
    subtitle = wbg_name(indicator = "Proportion of time spent on unpaid domestic and care work", mrv = df$date, denom = "% of 24 hour day"),
    note = paste0("Note: 2.6 times estimate from UN Women (2018) http://www.unwomen.org/en/digital-library/sdg-report. Data may not be strictly comparable across countries as the method and sampling used for data collection may differ."),
    source = paste("Source: UN Statistics Division. World Development Indicators (SG.TIM.UWRK.MA; SG.TIM.UWRK.FE).")
  )
}

fig_sdg5_women_reproductive_decisionmaking <- function(years = 2007:2015, top_N=35) {
  indicator <- "SG.DMK.SRCR.FN.ZS"
  df <- wbgdata(
    wbgref$countries$iso3c, 
    indicator,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_women_reproductive_decisionmaking.csv"
  )
  
  df <- df %>%
    filter(complete.cases(df)) %>%
    group_by(iso3c) %>%
    slice(which.max(date)) %>%
    ungroup()
  
  df <- df %>%
    left_join(wbgref$countries$regions) %>%
    filter(region_iso3c=="SSF") %>%
    top_n(n=top_N, -value)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df$iso3c <- as.factor(df$iso3c)
      df <- df %>% mutate(iso3c=fct_reorder(iso3c,-value))
      
      ggplot(df, aes(iso3c, value)) +
        geom_segment(aes(y = 0, yend = value,
                           x = iso3c, xend = iso3c),
                     color=style$colors$reference,
                     size=style$linesize_reference) +
        geom_point(color=style$colors$spot.primary,
                   size = style$point_size, 
                   stroke = style$point_stroke, 
                   shape = style$shapes$point) +
        scale_x_discrete(expand=c(0,0.5),labels = wbgref$countries$labels) +
        scale_y_continuous(expand=c(0,0),limits = c(0, 84)) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() 
    },
    aspect_ratio = 0.7,
    title = "Many women in Sub-Saharan Africa are not free to make their own decisions about reproductive health and sexual relations.",
    subtitle = wbg_name(indicator="Women making their own informed decisions regarding sexual relations, contraceptive use, and reproductive healthcare",mrv=years, denom="% of women ages 15–49"),
    note = paste0("Note: Countries in Sub-Saharan Africa with available data shown."),
    source = "Source: Household surveys (DHS) compiled by United Nations Population Fund. WDI (SG.DMK.SRCR.FN.ZS)."
  )
 
}

fig_sdg5_decision_vs_contraceptive_vs_TFR <- function(years = 2007:2015) {
  # Demographic and Health Surveys. World Development Indicators (SG.DMK.SRCR.FN.ZS; SP.DYN.TFRT.IN).  
  indicators <- c("SG.DMK.SRCR.FN.ZS", "SP.DYN.TFRT.IN","SP.DYN.CONM.ZS")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = years,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg5_decision_vs_contraceptive_vs_TFR.csv"
  )
  
  df <- df %>%
    filter(complete.cases(df)) %>%
    group_by(iso3c) %>%
    slice(which.max(date)) %>%
    ungroup() %>%
    left_join(wbgref$countries$regions) %>%
    filter(region_iso3c=="SSF")
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      p.tfr <- ggplot(df %>% mutate(panel=paste0("Total fertility rate (births per woman)")), aes(SG.DMK.SRCR.FN.ZS, SP.DYN.TFRT.IN)) +
        geom_point(alpha = 1, 
                   size = style$point_size, 
                   stroke = style$point_stroke, 
                   shape = style$shapes$point,
                   color=style$colors$spot.primary) +
        scale_x_continuous(limits=c(0,80)) +
        facet_grid(~ panel) +
        xlab(str_wrap_lines("Can make their own decisions regarding sexual and reproductive health (% of women ages 15-49)",indent=5,exdent=5)) +
        style$theme() +
        style$theme_scatter()
      
      p.contraception <- ggplot(df %>% mutate(panel=paste0("Use of modern contraception (% of women ages 15-49)")), aes(SG.DMK.SRCR.FN.ZS, SP.DYN.CONM.ZS)) +
        geom_point(alpha = 1, 
                   size = style$point_size, 
                   stroke = style$point_stroke, 
                   shape = style$shapes$point,
                   color=style$colors$spot.primary) +
        scale_x_continuous(limits=c(0,80)) +
        facet_grid(~ panel) +
        xlab(str_wrap_lines("Can make their own decisions regarding sexual and reproductive health (% of women ages 15-49)",indent=5,exdent=5)) +
        style$theme() +
        style$theme_scatter()
      
      pt.tfr <- ggplotGrob(p.tfr)
      pt.contraception <- ggplotGrob(p.contraception)
      
      chart <- gtable_row("chart", list(pt.contraception,pt.tfr), height = unit(1, "null"), widths = unit(c(1,1),"null"))
      chart$theme <- style$theme()
      chart   
    },
    aspect_ratio = 0.75,
    title = "Women with greater decisionmaking power are more likely to use modern contraceptive methods and to have fewer children.",
    subtitle = paste0("Most recent values in ", str_range(years, shorten = TRUE)),
    note="Note: All countries plotted are in Sub-Saharan Africa.",
    source = "Source: Household surveys (DHS, MICS) and UN Population Division. WDI (SP.DYN.CONM.ZS; SG.DMK.SRCR.FN.ZS; SP.DYN.TFRT.IN)."
  )
}

#make_all(path = "docs/sdg5/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg5", styler = style_atlas, saver = figure_save_draft_png) {
  #page 1
  saver(fig_sdg5_law_gender_hiring(), styler, file.path(path, "fig_sdg5_law_gender_hiring.png"), width = 5.5, height=3.75)
  saver(fig_sdg5_domesticviolence(), styler, file.path(path, "fig_sdg5_domesticviolence.png"), width = 5.5, height=3.65)
  
  #page 2
  saver(fig_sdg5_child_marriage_15_18(), styler, file.path(path, "fig_sdg5_child_marriage_15_18.png"), width = 2.7, height=8.5)
  saver(fig_sdg5_teenage_mothers_rich_poor_average(), styler, file.path(path, "fig_sdg5_teenage_mothers_rich_poor_average.png"), width = 2.7, height=8.5)
  
  #page 3
  saver(fig_sdg5_female_manager_owner(), styler, file.path(path, "fig_sdg5_female_manager_owner.png"), width = 5.55, height=2.25)
  saver(fig_sdg5_female_minister_parliamentarians(), styler, file.path(path, "fig_sdg5_female_minister_parliamentarians.png"), width = 5.5, height=2.25)
  saver(fig_sdg5_unpaid_work(), styler, file.path(path, "fig_sdg5_unpaid_work.png"), width = 5.55, height=4.3)
  
  #page 4
  saver(fig_sdg5_women_reproductive_decisionmaking(), styler, file.path(path, "fig_sdg5_women_reproductive_decisionmaking.png"), width = 5.5, height=4.7)
  saver(fig_sdg5_decision_vs_contraceptive_vs_TFR(), styler, file.path(path, "fig_sdg5_decision_vs_contraceptive_vs_TFR.png"), width = 5.5, height=3.8)
}
