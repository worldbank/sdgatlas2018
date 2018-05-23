library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(gtable)
library(readxl)
library(readr)
library(tibble)
library(stringr)
source("styles.R")

fig_sdg10_average_living_standards <- function(years = 1990:2016) {
  indicator <- "NY.GDP.PCAP.PP.KD"
  
  df <- wbgdata(
    c(wbgref$regions$iso3c, "WLD"),
    indicator,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg10_average_living_standards.csv"
  )
  
  df <- df %>%
    spread(iso3c, value) %>% 
    mutate_at(vars(EAS, ECS, LCN, MEA, NAC, SAS, SSF),
              funs(. / WLD)) %>% 
    select(-WLD) %>% 
    gather(iso3c, value, -c(indicatorID, date))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(x = date, y = value, color = iso3c)) +
        geom_line(size = style$linesize) +
        geom_hline(
          yintercept = 1,
          linetype = style$linetypes$reference,
          size = style$linesize_reference,
          color = style$colors$reference
        ) + 
        scale_x_continuous(breaks = bracketed_breaks(df$date), expand=c(0,0)) +
        scale_y_continuous(
          position="right",
          breaks = 0:4,
          labels = c("0","1x","2x","3x","4x"),
          sec.axis = dup_axis(
            breaks = df %>% filter(date == min(date)) %>% pull(value) %>% repel(5, gap=0.15),
            label = wbgref$regions$labels[df %>% filter(date == min(date)) %>% pull(iso3c)]
          )) +
        scale_color_manual(values = style$colors$regions) +
        style$theme()
    },
    aspect_ratio = 2,
    title = "There is great inequality across countries and regions. North America is 3.5 times richer than the world average, but its relative income per capita has been falling. By contrast, incomes are rising in South Asia and East Asia & Pacific.",
    subtitle = "Relative GDP per capita (1x = world average)",
    source = "Source: World Bank, International Comparison Program database. WDI (NY.GDP.PCAP.PP.KD)."
  )
}

fig_sdg10_50pc_median_vs_gini_gnipc <- function() {
  df <- read_xlsx("inputs/sdg10/50pctmedian_scatter_data.xlsx")
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      p.gini <- ggplot(
        df %>% mutate(panel=paste0("Gini index (0-100, 0 is perfect equality)")), 
        aes(y = gini, x = share_50pctmedian)
      ) +
        geom_point(color=style$colors$spot.primary, alpha=0.7, size=style$point_size, shape = style$shapes$point, stroke = style$point_stroke) +
        scale_y_continuous(limits = c(25,65)) +
        facet_grid(~ panel) +
        labs(x = "Share living below 50 percent of median income (%)") +
        style$theme() +
        style$theme_scatter() +
        theme(
          panel.grid.major.x = NULL,
          legend.position = c(1, 1.25),
          legend.direction="horizontal",
          legend.box = "horizontal",
          plot.margin=margin(0,2,3,0,"mm"),
          axis.title = NULL,
          axis.title.y = element_blank()
        ) 

      p.gnipc <- ggplot(
        df %>% mutate(panel=paste0("GNI per capita (current US$, thousands, log scale)")), 
        aes(y = gnipc_k*265, x = share_50pctmedian)
      ) +
        geom_point(color=style$colors$spot.secondary, alpha=0.7, size=style$point_size, shape = style$shapes$point, stroke = style$point_stroke) +
        scale_y_continuous(trans = "log10", breaks=c(1000,10000,50000),labels=c("1","10","50")) +
        facet_grid(~ panel) +
        labs(x = "Share living below 50 percent of median income (%)") +
        style$theme() +
        style$theme_scatter() +
        theme(
          plot.margin=margin(0,0,3,2,"mm"),
          axis.title = NULL,
          axis.title.y = element_blank()
        )
      
      # Display plots side by side
      pt.gini <- ggplotGrob(p.gini)
      pt.gnipc <- ggplotGrob(p.gnipc)
      
      chart <- gtable_row("chart", list(pt.gini,pt.gnipc), height = unit(1, "null"), widths = unit(c(1,1),"null"))
      chart$theme <- style$theme()
      chart  
    },
    aspect_ratio = 1.3,
    title="One simple way to measure inequality within a country is to consider the share of people living below 50 percent of its median income.",
    source = "Source: World Bank PovcalNet (database). WDI (SI.POV.GINI; NY.GNP.PCAP.CD)."
  )
}

fig_sdg10_b40_explainer <- function(country = "PER", years = c(2011,2016)){
  # The data for this explainer was revised, so exact replication is not possible.
  
  # SI.SPR.PCAP
  indicators <- c(
    inc_pc = "SI.SPR.PCAP",
    q1="SI.DST.FRST.20",
    q2="SI.DST.02ND.20",
    q3="SI.DST.03RD.20",
    q4="SI.DST.04TH.20",
    q5="SI.DST.05TH.20"
  )
  df <- wbgdata(
    country,
    indicators,
    years = years,
    indicator.wide = TRUE,
    rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg10_b40_explainer.csv"
  )
  
  df <- df %>%
    mutate(
      q1 = q1 / 20 * inc_pc,
      q2 = q2 / 20 * inc_pc,
      q3 = q3 / 20 * inc_pc,
      q4 = q4 / 20 * inc_pc,
      q5 = q5 / 20 * inc_pc
    ) %>%
    mutate(
      b40 = (q1 + q2)/2
    ) %>%
    rename(all = inc_pc)
  
  df <- df %>%
    gather(key = "quintile", value = "value", q1:q5, all, b40) %>%
    spread(date, value)
  
  # Calculate growth rates
  df$growth = ((df[[as.character(max(years))]] / df[[as.character(min(years))]])^(1/(max(years)-min(years))) - 1) * 100
  
  figure(
    data = df,
    plot = function(data, style = style_atlas()) {
      ggplot(df %>% filter(quintile %in% c("q1", "q2", "q3", "q4", "q5")), aes(quintile,growth)) +
        geom_segment(
          aes(y = 0, yend = growth, x = quintile, xend = quintile),
          color=style$colors$reference,
          size=style$linesize_reference
        ) +
        geom_point(color=style$colors$spot.secondary.dark) +
        geom_hline(yintercept = df$growth[df$quintile == "all"], color = style$colors$spot.primary, linetype=style$linetypes$reference) +
        geom_hline(yintercept = df$growth[df$quintile == "b40"], color = style$colors$spot.primary.light, linetype=style$linetypes$reference) +
        scale_x_discrete(labels = c(
          q1 = "Quintile 1", q2 = "Quintile 2", q3 = "Quintile 3", q4 = "Quintile 4", q5 = "Quintile 5"
        )) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        theme(plot.margin=margin(16,0,3,0,"mm"))
    },
    aspect_ratio = 0.9,
    title = "Changes in inequality can be measured by the relative income growth of the poorest 40 percent of people.",
    subtitle = wbg_name(indicator="Annualized growth rate, Peru", denom="%", year = str_range(years, shorten = c(2,2))),
    source = paste0("Source: World Bank Global Database of Shared Prosperity. WDI (SP.SPR.PCAP.ZG; SP.SPR.PC40.ZG; SI.SPR.PCAP; SI.DST.FRST.20; SI.DST.02ND.20; SI.DST.03RD.20; SI.DST.04TH.20; SI.DST.05TH.20).")
  )
}

fig_sdg10_b40_growth_faster <- function(years=2009:2016) {
  indicators <- c(Bottom40 = "SI.SPR.PC40.ZG", Average = "SI.SPR.PCAP.ZG")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    year=years,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg10_b40_growth_faster.csv"
  )
  
  #adding a "shared prosperity premium" variable which is TRUE if bottom 40 growth higher than average growth"
  df <- df %>%
    mutate(sppremium = (SI.SPR.PC40.ZG - SI.SPR.PCAP.ZG) > 0) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    gather("indicator", "value", -sppremium,-date,-iso3c)
  
  figure(
    data = df,
    plot = function(data, style = style_atlas()) {
      #order by average growth and filter for sppremium is TRUE
      df <- df %>% 
        mutate(iso3c = fct_reorder2(iso3c, indicator, -value)) %>%
        filter(sppremium == TRUE)
      
      ggplot(df, aes(value, iso3c)) +
        geom_other_dotplot(
          aes(
            y = as.factor(iso3c), 
            color = as.factor(indicator)
          ),
          shape = style$shapes$point,
          size = style$point_size,
          stroke = style$point_stroke) +
        geom_text(
          aes(label = wbgref$countries$labels[as.character(iso3c)]),
          data = . %>% group_by(iso3c) %>% mutate(value = min(value)),
          hjust = 1,
          vjust = 0.25,
          family = style$family,
          size = style$gg_text_size,
          color = style$colors$text,
          nudge_x = -0.7
        ) +
        scale_x_continuous(limits = c(-11,10), breaks=c(-10,-5,0,5,10)) +
        style$theme() +
        scale_color_manual(
          values = c(style$colors$spot.primary.light, style$colors$spot.primary),
          labels = c("Poorest 40 percent", "Average")
        ) +
        style$theme_barchart() +
        style$theme_legend("top") +
        theme(axis.text.y = element_blank())
    },
    aspect_ratio = 0.9,
    title = "In 61 countries income growth among the poorest is faster than average.",
    subtitle = wbg_name(indicator="Annualized growth rate, circa 2009–14", denom="%"),
    note = "Note: Growth rates refer to real survey mean consumption or income.",
    source = paste0("Source: World Bank Global Database of Shared Prosperity. WDI (SI.SPR.PC40.ZG; SI.SPR.PCAP.ZG).")
  )
}

fig_sdg10_b40_growth_slower <- function(years=2008:2016) {
  indicators <- c(
    Bottom40 = "SI.SPR.PC40.ZG",
    Average = "SI.SPR.PCAP.ZG"
  )
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,year=years,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg10_b40_growth_slower.csv"
  )
  
  #adding a "shared prosperity premium" variable which is FALSE if bottom 40 growth lower than average growth"
  df <- df %>%
    mutate(sppremium = (SI.SPR.PC40.ZG - SI.SPR.PCAP.ZG) > 0) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    gather("indicator", "value", -sppremium,-date,-iso3c)
  
  figure(
    data = df,
    plot = function(data, style = style_atlas_open()) {
      
      #order by average growth and filter for sppremium is TRUE
      df <- df %>% 
        mutate(iso3c = fct_reorder2(iso3c, indicator, -value)) %>%
        filter(sppremium == FALSE)
      
      ggplot(df, aes(value, iso3c)) +
        geom_other_dotplot(
          aes(
            y = as.factor(iso3c), 
            color = as.factor(indicator)
          ),
          shape = style$shapes$point,
          size = style$point_size,
          stroke = style$point_stroke) +
        geom_text(
          aes(label = wbgref$countries$labels[as.character(iso3c)]),
          data = . %>% group_by(iso3c) %>% mutate(value = max(value)),
          hjust = 0,
          vjust = 0.25,
          family = style$family,
          size = style$gg_text_size,
          color = style$colors$text,
          nudge_x = 0.7
        ) +
        scale_x_continuous(limits = c(-14,11), breaks=c(-10,-5,0,5)) +
        style$theme() +
        scale_color_manual(
          values = c(style$colors$spot.primary.light, style$colors$spot.primary),
          labels = c("Poorest 40 percent", "Average")
        ) +
        style$theme_barchart() +
        style$theme_legend("top") +
        theme(axis.text.y = element_blank())
    },
    aspect_ratio = 0.9,
    title = "In 34 countries income growth among the poorest is slower than average.",
    subtitle = wbg_name(indicator="Annualized growth rate, circa 2009–14",denom="%"),
    note = "Note: Growth rates refer to real survey mean consumption or income.",
    source = paste0("Source: World Bank Global Database of Shared Prosperity. WDI (SI.SPR.PC40.ZG; SI.SPR.PCAP.ZG).")
  )
}

fig_sdg10_remittance_cost_by_receiving <- function(year = 2017) {
  
  df_raw <- read_excel("inputs/sdg10/remittances_receiving.xlsx",
                       range = "A1:F113", na = "..")
  
  df <- df_raw %>%
    select(
      "iso3c" = `Receiving Countries  Code`,
      "value" = `2017Q1 [YR2017Q1]`) %>%
    # filter(complete.cases(.)) %>%
    right_join(data.frame(iso3c = wbgref$countries$iso3c, stringsAsFactors = FALSE))  
  
  df$bins <- supercut(df$value, c(
    "0-3" = "[0, 3)",
    "3-5" = "[3, 5)",
    "5-10" = "[5, 10)",
    "10 and above" = "[10, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
    },
    title = "Personal remittances are an important source of income for people in low- and middle-income countries. But the average cost of sending this money remains high.",
    subtitle = "Average cost of sending remittances to a country, Q1 2017 (% of transaction)",
    source = "Source: World Bank, Remittances Prices Worldwide (database) https://remittanceprices.worldbank.org/"
  )

}

fig_sdg10_remittance_cost_by_sending <- function() {
  df <- read_excel("inputs/sdg10/remittances_sending.xlsx", range = "A1:F49", na = "..")
  
  df <- df %>%
    select(
      "iso3c" = `Country Code`,
      "value" = `2017Q4 [YR2017Q4]`) %>%
    right_join(data.frame(iso3c = wbgref$countries$iso3c, stringsAsFactors = FALSE))  
  
  df$bins <- supercut(df$value, c(
    "0-3" = "[0, 3)",
    "3-5" = "[3, 5)",
    "5-10" = "[5, 10)",
    "10 and above" = "[10, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
    },
    title = "The cost of sending remittances also varies by which country they are sent from.",
    subtitle = "Average cost of sending remittances from a country, Q1 2017 (% of transaction)",
    source = "Source: World Bank, Remittances Prices Worldwide (database) https://remittanceprices.worldbank.org/"
  )
}

fig_sdg10_remittance_cost_time <- function() {
  df <- read_csv("inputs/sdg10/remittances_regional_time.csv")
  
  quarter_levels <- df %>% select(-iso3c) %>% names()
  
  df <- df %>% 
    gather(quarter, value, -iso3c) %>% 
    mutate(quarter = factor(quarter, levels = quarter_levels))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(x = quarter, y = value, group = iso3c, color = iso3c, linetype = iso3c)) +
        geom_line(size = style$linesize) +
        geom_hline(yintercept = 3, color = style$colors$reference, linetype=style$linetypes$reference) +
        scale_color_manual(
          values = c(style$colors$regions, style$colors$world),
          labels = wbgref$all_geo$labels
        ) +
        scale_linetype_manual(
          values = c(style$linetypes$regions, style$linetypes$world),
          labels = wbgref$all_geo$labels
        ) +
        scale_x_discrete(expand=c(0,0),breaks=quarter_levels[c(1,5,9,13,17,20)],
                         labels=c("Q1-2013","2014","2015","2016","2017","Q4-2017"))+
        scale_y_continuous(position="right",
                           breaks=c(0,2,4,6,8,10,12), 
                           limits=c(2,13),
                           sec.axis = dup_axis(
                             breaks = df %>% filter(quarter == "Q1 2013") %>% pull(value) %>% repel(10, gap=0.4),
                             label = wbgref$all_geo$labels[df %>% filter(quarter == "Q1 2013") %>% pull(iso3c)]
                           ))+
        style$theme()
    },
    title = "Globally, the average cost to send remittances fell from 9.1 to 7.1 percent over the last four years, but it remains above the SDG target of 3 percent.",
    subtitle = wbg_name(indicator = "Average cost of remittance services, by receiving region", denom = "% of transaction"),
    source = "Source: Remittance Prices Worldwide, World Bank, Issue 24. https://remittanceprices.worldbank.org/"
    
  )
}

fig_sdg10_remittance_corridor_heatmap_smart <- function() {
  df <- read_excel("inputs/sdg10/remittances_smart_corridors.xlsx", 
                       range = "A3:N17", 
                       na = "")
  
  df <- df %>%
    gather(receive, value, -`Sending Countries Name`) %>%
    mutate(receive = gsub(".*\\[|\\]", "", receive)) %>%
    rename("send" = `Sending Countries Name`) %>%
    mutate(send = str_replace(send, "United States of America", "United States"))
  
  # We'll suppress the negative values which are confusing
  df <- df %>% mutate(value = ifelse(value < 0, NA, value))
  
  df$bins <- supercut(df$value, c(
    "0-3" = "[0, 3)",
    "3-5" = "[3, 5)",
    "5-10" = "[5, 10)",
    "10 and above" = "[10, Inf)")
  )
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      receive_levels <- df %>%
        filter(complete.cases(.)) %>%
        group_by(receive) %>%
        summarize(num = n()) %>%
        arrange(-num) %>%
        pull(receive)
      
      send_levels <- df %>%
        filter(complete.cases(.)) %>%
        group_by(send) %>%
        summarize(num = n()) %>%
        arrange(-num) %>%
        pull(send)
      
      df <- df %>%
        mutate(
          receive = factor(receive, levels = receive_levels),
          send = fct_rev(factor(send, levels = send_levels))) %>%
        filter(complete.cases(send))
      
      df$bins <- as.character(df$bins)
      df$bins[is.na(df$bins)] <- "No data"
      df$bins <- as.factor(df$bins)
      df$bins <- factor(df$bins, levels=c("0-3","3-5","5-10","10 and above","No data"))
      
      ggplot(df, aes(receive, send, fill = bins, label = round(value, 1))) + 
        geom_tile(color="white") +
        geom_text(aes(color=bins), size = style$gg_text_size,show.legend = FALSE) +
        scale_x_discrete(label = wbgref$countries$labels,position="top") + 
        scale_y_discrete(label = wbgref$countries$labels) + 
        scale_color_manual(values = contrasting_colors(c(style$colors$continuous(4),"grey90"),c(style$colors$text,style$colors$text.inverse))) +
        scale_fill_manual(
          values = c(style$colors$continuous(4),"grey90")) +
        style$theme() +
        style$theme_legend("bottom") +
        ylab("Sending from this country\n") +
        xlab("To this country") +
        theme(
          axis.title.x = element_text(margin = margin(0,0,0,0,unit = "mm")), 
          axis.title.y = element_text(angle=90),
          axis.text.x = element_text(angle =35,hjust=0),
          panel.grid = element_blank(),
          plot.margin=margin(0,25,5,0,"mm")
        )     
    },
    title = "Remittance costs vary between sending and receiving country corridors. The SDG target aims to bring all corridor costs to below 5 percent of the amount remitted.",
    subtitle = wbg_name(indicator = "Average cost of sending remittances between countries", denom = "% of transaction"),
    source = "Source: World Bank, Remittances Prices Worldwide (database) https://remittanceprices.worldbank.org/",
    note="Note: The costs shown use the Smart Remitter Target methodology, which averages the three cheapest services for remitting money. Remittance corridors with the largest flows of money are shown. The dataset does not cover corridors where remittance flows are relatively small."
  )
}

# make_all(path = "docs/sdg10/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg10", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg10_average_living_standards(), styler, file.path(path, "fig_sdg10_average_living_standards.png"), width = 5.5, height = 4)
  saver(fig_sdg10_50pc_median_vs_gini_gnipc(), styler, file.path(path, "fig_sdg10_50pc_median_vs_gini_gnipc.png"), width = 5.5, height = 3.2)

  # page 2  
  saver(fig_sdg10_b40_explainer(), styler, file.path(path, "fig_sdg10_b40_explainer.png"), width = 2.7, height = 3)
  saver(fig_sdg10_b40_growth_faster(), styler, file.path(path, "fig_sdg10_b40_growth_faster.png"), width = 2.7, height = 8.5)
  saver(fig_sdg10_b40_growth_slower(), styler, file.path(path, "fig_sdg10_b40_growth_slower.png"), width = 2.7, height = 5.5)
  
  # page 3
  saver(fig_sdg10_remittance_cost_by_receiving(), styler, file.path(path, "fig_sdg10_remittance_cost_by_receiving.png"), width = 5.5, height = 4.25)
  saver(fig_sdg10_remittance_cost_by_sending(), styler, file.path(path, "fig_sdg10_remittance_cost_by_sending.png"), width = 5.5, height = 4.25)
  
  # page 4
  saver(fig_sdg10_remittance_cost_time(), styler, file.path(path, "fig_sdg10_remittance_cost_time.png"), width = 5.5, height = 4)
  saver(fig_sdg10_remittance_corridor_heatmap_smart(), styler, file.path(path, "fig_sdg10_remittance_corridor_heatmap_smart.png"), width = 5.5, height = 4.5)
}
