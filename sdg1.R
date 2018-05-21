library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(forcats)
library(gtable)
library(countrycode)
library(extrafont)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
source("styles.R")

fig_sdg1_income_histogram_poverty <- function(years = c(1990, 2013), region = "WLD") {
  df <- read_excel("inputs/sdg1/SDG1_f1_global_dist_histogram.xlsx")
  
  df <- df %>%
    filter(regioncode == region) %>%
    select(year, povertyline, people, p_label) %>%
    filter(year %in% years) %>%
    mutate(people = people * 1e6) %>%         # people cols are in millions
    mutate(p_label = recode(p_label,
      ".5-1"  = "0.5-1",
      "1-1.9" = "1-1.90",
      "1.9-4" = "1.90-4"
    ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(p_label = endashify(p_label)) %>%  # use en-dashes for display
        mutate(p_label = fct_reorder(p_label, as.numeric(povertyline)))
      
      ggplot(df, aes(p_label, people, fill = (povertyline <= 1.90))) +
        geom_col() +
        scale_y_continuous(labels = billions()) +
        scale_fill_manual(
          values = c(
            `FALSE` = style$colors$spot.secondary.light,    
            `TRUE` = style$colors$spot.primary
          )
        ) +
        xlab("Income or consumption per day (2011 PPP $)") +
        facet_wrap(~ year, ncol = 1) +
        style$theme() +
        style$theme_x_title()
    },
    title = "Ending extreme poverty is at the heart of the SDG agenda. Between 1990 and 2013 the number of people living below $1.90 a day fell by over 1 billion.",
    subtitle = wbg_name(indicator = "People", denom = "billions"),
    source = "Source: World Bank PovcalNet (database). http://iresearch.worldbank.org/PovcalNet/home.aspx"
  )
}

fig_sdg1_poor_population_area_chart <- function(years = c(1990, 2013)) {
  df <- read_excel("inputs/sdg1/SDG1_f2_global_dist.xls")
  
  df <- df %>%
    filter(year %in% years, regioncid != "WLD") %>%
    mutate(population = population * 1e6) %>%  # population is in millions
    mutate(poverty = hc, not_poverty = (1 - hc)) %>%
    select(year, regioncid, population, poverty, not_poverty) %>%
    rename(iso3c = regioncid)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      # Arrange data ascending in poor % in first year
      df <- df %>%
        mutate(iso3c = fct_reorder2(iso3c, year == min(years), poverty)) %>%
        arrange(year, iso3c)
      
      # Generate rectangle bounds for this marimekko-style plot
      df <- df %>%
        group_by(year) %>%
        mutate(x_max = cumsum(population)) %>%
        mutate(x_min = x_max - population) %>%
        ungroup()
      df <- df %>%
        gather(indicatorID, value, c(poverty, not_poverty)) %>%
        group_by(iso3c, year) %>%
        mutate(y_max = cumsum(value)) %>%
        mutate(y_min = y_max - value) %>%
        ungroup()
      
      df <- df %>%
        mutate(indicatorID = factor(indicatorID, levels = c("poverty", "not_poverty")))
      
      df <- df %>% left_join(wbgref$regions_excl_high_income$regions) %>%
        mutate(region_iso3c = ifelse(iso3c == "OHI", "OHI", region_iso3c))
      
      p <- ggplot(df, aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max, fill = indicatorID)) +
        geom_rect(color = "white", size = 0.1) +
        geom_text( # numeric labels
          data = . %>% filter(indicatorID=="poverty"),
          aes(x = x_min + (x_max - x_min) / 2, 
              y = y_max + 0.03,
              label = round(value*100,1)
          ),
          position = position_dodge(0.9),
          hjust = 0,
          size = style$gg_text_size*0.8,
          family = style$family
        ) +
        geom_text( # region labels
          data = . %>% filter(year == 2013, indicatorID == "not_poverty"),
          aes(
            x = x_min + (x_max - x_min) / 2, 
            y = y_max + 0.03,
            label = c(wbgref$regions$labels,"OHI" = "Other high income")[region_iso3c]
          ),
          position = position_dodge(),
          hjust = 0,
          size = style$gg_text_size,
          family = style$family
        ) +
        scale_fill_manual(
          values = c(poverty = style$colors$spot.primary, not_poverty = style$colors$neutral),
          labels = c(poverty = "Poor", not_poverty = "Not Poor")
        ) +
        scale_y_continuous(
          name="Share of population (%)", 
          breaks=c(0,0.25,0.50,0.75,1.00),
          labels=c("0","25","50","75","100")
        ) +
        coord_flip() +
        facet_grid(. ~ year, scales = "free_x", space = "free_x") +
        style$theme() +
        theme(
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x= element_text(margin = margin(1,0,0,0,"char")),
          panel.grid.major = element_blank(),
          legend.position = c(0.13, 0.9),
          legend.justification = c(0, 1),
          legend.direction = "horizontal",
          panel.spacing.x = unit(0.05, "npc"),
          plot.margin=margin(0,7,0.5,0,"char")
        )
      
      # Turn off clipping so right labels are visible
      g <- ggplotGrob(p)
      g$layout$clip[g$layout$name=="panel-2-1"] <- "off"
      g$theme <- style$theme()
      g
    },
    title = "The world's population has grown and the regional distribution of poverty has changed. Compared with 1990, there are now more poor people in Sub-Saharan Africa and fewer in South Asia and East Asia & Pacific.",
    note = "Note: Poor refers to people living on less than $1.90 a day (2011 PPP). Regional aggregates exclude certain high-income countries.",
    source = "Source: World Bank PovcalNet (database). http://iresearch.worldbank.org/PovcalNet/home.aspx"
  )
}

fig_sdg1_poor_number_map <- function(years = 2010:2013) {
  indicators = c("SI.POV.DDAY", "SP.POP.TOTL")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    year=years,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg1_poor_number_map.csv"
  )
  
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup
  
  df$count <- df$SI.POV.DDAY/100 * df$SP.POP.TOTL
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      breaks = c(10e+06, 50e+06,200e+06)
      wbg_bubble_map(df, wbgmaps[[quality]], style, "count", 
                     breaks, 
                     max_size = 0.8, 
                     labels = millions())
      
    }, 
    aspect_ratio = 1.5, 
    title = "Populous countries such as China, India, Indonesia, and Bangladesh are home to a significant share of the total number of people living in extreme poverty.", 
    subtitle = wbg_name(indicator="Number of people living on less than $1.90 a day (2011 PPP)",mrv=years,denom="millions"), 
    source_url = "Source: World Bank PovcalNet. World Development Indicators (SI.POV.DDAY; SP.POP.TOTL)"
  )
}

fig_sdg1_pov_national_ur <- function(years = 2010:2015){
  indicators <- c(
    "SI.POV.RUHC",
    "SI.POV.URHC",
    "SI.POV.DDAY",
    "SI.POV.NAHC"
  )
  
  df <- wbgdata(
    wbgref$countries$iso3c, 
    indicators, years = years,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg1_pov_national_ur.csv"
  )
  
  df <- df %>%
    filter(complete.cases(.)) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()
  df <- df %>% left_join(wbgref$countries$regions)
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      p.national <- ggplot(
        df %>% mutate(panel=paste0("Poverty rate at national poverty lines")),
        aes(SI.POV.DDAY,SI.POV.NAHC, region_iso3c)
      ) +
        geom_abline(intercept = 0, color = style$colors$reference, linetype=style$linetypes$reference) +
        geom_point(alpha = 0.9, aes(color = region_iso3c), size = style$point_size, shape = style$shapes$point, stroke = style$point_stroke)+
        scale_color_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
        scale_y_continuous(limits = c(0, 80)) +
        scale_x_continuous(limits = c(0, 80)) +
        facet_grid(~ panel) +
        xlab("Poverty rate at $1.90 a day (2011 PPP)") +
        style$theme() +
        theme(
          panel.grid.major.x = NULL,
          legend.position = c(1, 1.23),
          legend.direction="horizontal",
          legend.box = "horizontal",
          plot.margin=margin(10,2,3,0,"mm"),
          axis.title = NULL,
          axis.title.y = element_blank()
        ) +
        style$theme_scatter()
      
      p.ur <- ggplot(
        df %>% mutate(panel=paste0("Rural poverty rate at national poverty lines")),
        aes(SI.POV.URHC,SI.POV.RUHC, region_iso3c)
      ) +
        geom_abline(intercept = 0, color = style$colors$reference, linetype=style$linetypes$reference) +
        geom_point(alpha = 0.9, aes(color = region_iso3c), size = style$point_size, shape = style$shapes$point, stroke = style$point_stroke)+
        scale_color_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
        scale_y_continuous(limits = c(0, 80)) +
        scale_x_continuous(limits = c(0, 80)) +
        facet_grid(~ panel) +
        xlab("Urban poverty rate at national poverty lines") +
        style$theme() +
        theme(
          panel.grid.major.x = NULL,
          plot.margin=margin(10,0,3,2,"mm"),
          axis.title = NULL,
          axis.title.y = element_blank()
        ) +
        style$theme_scatter()
      
      pt.national <- ggplotGrob(p.national)
      pt.ur <- ggplotGrob(p.ur)
      
      chart <- gtable_row("chart", list(pt.national,pt.ur), height = unit(1, "null"), widths = unit(c(1,1),"null"))
      chart$theme <- style$theme()
      chart  
      
    },
    aspect_ratio = 1,
    title = "Poverty rates at national poverty lines are generally higher than at the international $1.90 a day line, and they are higher in rural areas than in urban areas.",
    subtitle = wbg_name(indicator="Poverty headcount ratio", mrv=years, denom="% of population"),
    source = paste("Source: World Bank PovcalNet. World Development Indicators (SI.POV.DDAY; SI.POV.NAHC; SI.POV.RUHC; SI.POV.URHC).")
  )
  
}

fig_sdg1_sp_quintile_targeting <- function() {
  df <- read_excel(
    "inputs/sdg1/income group poorest quintile and total social protection.xlsx",
    range = "A1:D6"
  )
  
  df <- df %>%
    gather(indicatorID, value, -c(countrycode, Group)) %>%
    rename("iso3c" = countrycode) %>%
    select(-Group)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(
          indicatorID = fct_reorder(indicatorID, -value),
          iso3c = factor(iso3c, levels = c("LIC", "LMC", "UMC", "HIC", "WLD"))
        )
      
      ggplot(df, aes(x = iso3c, y = value, fill = indicatorID)) +
        geom_col(position = "bullet", width=.8) +
        scale_fill_manual(
          values=c(
            `Poorest quintile` = style$colors$spot.primary,
            `Total population` = style$colors$spot.secondary.light
          )
        ) +
        scale_x_discrete(labels = wbgref$all_geo$labels) +
        scale_y_continuous(expand=c(0,0),limits=c(0,102))+
        coord_flip()+
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") 
    },
    title = "Richer countries have more comprehensive social protection programs. Within countries, the poorest are more likely to be covered by such programs, but identifying and targeting support towards the poor remains challenging.",
    subtitle = wbg_name(indicator = "Share of population covered by any social protection and labor program, most recent survey in 2008-16", denom = "%"),
    note = "Note: Calculated using simple averages of country-level coverage rates across income groups. Actual coverage may be higher as not all programs are captured by household surveys in some countries. Poorest quintile is calculated using pre-transfer welfare (income or consumption) per capita.",
    source = "Source: World Bank ASPIRE: Atlas of Social Protection Indicators of Resilience and Equity  2018. http://hdl.handle.net/10986/29115"
  )
}

fig_sdg1_sp_program_spending <- function() {
  df <- read_excel("inputs/sdg1/regions by type of instrument share.xlsx", range = "A1:F7")
  
  df <- df %>%
    gather(indicatorID, value, -c(regioncode, Region)) %>%
    rename("iso3c" = regioncode) %>%
    select(-Region)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(
          indicatorID = fct_reorder(indicatorID, -value),
          iso3c = fct_reorder2(iso3c, indicatorID == "Cash (UCT, CCT, Social pensions)", -value),
          value = value * 100
        ) %>%
        mutate(
          indicatorID = recode(
            indicatorID, 
            "Cash (UCT, CCT, Social pensions)" = "Cash-based",
            "In-kind (school feeding, fee waivers, in-kind transfers)" = "In-kind"
          )
        )
      
      ggplot(df, aes(x = iso3c, y = value, fill = indicatorID)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(values = style$colors$categorical) +
        scale_x_discrete(labels = wbgref$regions$labels) +
        scale_y_continuous(expand=c(0,0), limits=c(0,102)) +
        coord_flip() +
        style$theme() +
        style$theme_legend("top") +
        style$theme_barchart()
      
    },
    title = "The most common social protection programs in every region are cash-based.",
    subtitle = wbg_name(indicator = "Share of spending on the social safety net, by program", denom = "%"),
    note = "Note: Based on administrative data. Cash-based programs include universal cash transfers, conditional cash transfers, and social pensions. In-kind programs include school feeding, fee waivers and other in-kind transfers.",
    source = "Source: World Bank ASPIRE: Atlas of Social Protection Indicators of Resilience and Equity  2018. http://hdl.handle.net/10986/29115"
  )
}

fig_sdg1_sp_cash_transfer <- function() {
  df <- read_excel("inputs/sdg1/instrument by quintile share.xlsx", range = "A1:F9")
  
  df <- df %>%
    gather(quintile, value, -Program) %>%
    rename("indicatorID" = Program)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(
        indicatorID = fct_reorder2(indicatorID, quintile == "Q1", -value)
      )
      
      ggplot(df, aes(x = indicatorID, y = value, fill = quintile)) +
        geom_col(position = position_stack(reverse = TRUE)) +
        scale_fill_manual(
          palette = style$colors$continuous,
          labels = c("Q1" = "Q1 (Poorest)", "Q5" = "Q5 (Richest)")
        ) +
        scale_y_continuous(expand=c(0,0), limits=c(0,102)) +
        coord_flip() +
        style$theme() +
        style$theme_legend("top") +
        style$theme_barchart()
      
    },
    title = "Cash transfer programs are the most likely to be directed toward the poor.",
    subtitle = wbg_name(indicator = "Share of social security programs benefitting each population quintile, most recent survey in 2008-16", denom = "%"),
    note = "Note: Calculated using simple averages of country-level coverage rates across regions. Poorest quintile is calculated using pre-transfer welfare (income or consumption) per capita.",
    source = "Source: World Bank ASPIRE: Atlas of Social Protection Indicators of Resilience and Equity  2018. http://hdl.handle.net/10986/29115"
  )
}

fig_sdg1_map_cadaster_etc <- function(year = 2017) {
  geographic <- read_excel("./inputs/sdg1/registering_property.xlsx", sheet = "geographic_coverage") 
  geographic <- geographic %>% rename("country" = "Economy")
  
  infrastructure <- read_excel("./inputs/sdg1/registering_property.xlsx", sheet = "reliability_infrastructure")
  infrastructure <- infrastructure %>% rename("country" = "Economy")
  
  df <- geographic %>%
    full_join(infrastructure, by = "country") %>%
    select(country, 
           immovable = "Are all privately held land plots in the largest business city formally registered at the immovable property registry?",
           mapped = "Are all privately held land plots in the largest business city mapped?",
           format = `In what format are the majority of maps of land plots kept in the largest business city—in a paper format or in a computerized format (scanned or fully digital)?`,
           database = `Is the information recorded by the immovable property registration agency and the cadastral or mapping agency kept in a single database, in different but linked databases or in separate databases?`
    ) %>%
    mutate(format = ifelse(format == "Computer/Fully digital", "Yes", "No"),
           database = ifelse(database == "Separate databases", "No", "Yes")) %>%
    gather(indicatorID, value, c(immovable, mapped, format, database)) %>%
    mutate(coded = ifelse(value == "Yes", 1, 0))  %>%
    group_by(country) %>%
    summarize(total = sum(coded)) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c", custom_match = c("Kosovo" = "XKX"))) %>%
    merge(data.frame("iso3c" = wbgref$countries$iso3c), by = "iso3c", all.y = TRUE) %>%
    group_by(iso3c) %>%
    summarize(value = max(total))
  
  df$bins <- as.character(as.integer(df$value))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins",legend.nrow = 1) 
      g$theme <- style$theme()
      g
    },
    title="Land rights provide security of tenure and are important for reducing poverty. But many countries lack comprehensive land registries that record ownership.",
    subtitle = wbg_name(indicator = "Number of components related to property registration from Doing Business Index", denom = "0-4, higher is better"),
    source = paste0("Source: ", "World Bank Doing Business (database). http://www.doingbusiness.org")
  )
}

fig_sdg1_tenure_vs_rights <- function(years = 2010:2015) {
  df <- read_excel("inputs/sdg1/tenure_vs_rights.xlsx",range = "A2:C11")
  
  df <- df %>%
    rename("tenure_insecurity" = `Share of households who perceived tenure insecurity (%), 2010 - 2015`,
           "formal_document" = `Share of households that own formally documented agricultural land (%), 2010 - 2015`) %>%
    mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
    gather(indicatorID, value, 2:3)
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(
          iso3c = fct_reorder2(iso3c, indicatorID == "format_document", -value),
          indicatorID = factor(indicatorID, levels = c("tenure_insecurity", "formal_document"))
        )
      
      p <- ggplot(df, aes(x = iso3c, y = value, fill = indicatorID, order = indicatorID)) +
        geom_col(position = "bullet") +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(expand=c(0,0), limits = c(0, 102)) +
        scale_fill_manual(
          values = c(
            tenure_insecurity = style$colors$spot.primary,
            formal_document = style$colors$spot.secondary.light
          ),
          labels = c(
            tenure_insecurity = "Perceived tenure insecurity",
            formal_document = "Own formally documented agricultural land"
          )
        ) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("topleft") +
        theme(legend.direction = "vertical")
      
      # Hard left-align legend
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    title="People with documented ownership of land and property feel more secure.",
    note="Note: Data from a study covering selected countries.",
    subtitle = wbg_name(indicator = "Share of households", mrv = years, denom = "%"),
    source = "Source: G. Carletto, K. Deininger, T. Hilhorst, and W. Zakout (2018)."
  )
}

fig_sdg1_females_land_titles <- function(years = 2001:2015) {
  df <- read_excel("./inputs/sdg1/female_land_title.xlsx", range = "A1:D19")
  
  df <- df %>%
    rename("country" = `2001 -`) %>%
    gather(indicatorID, value, 2:4) %>%
    mutate(value = as.numeric(gsub("%", "", value)) * 100,
           iso3c = countrycode(country, "country.name", "iso3c"))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(
        iso3c = fct_reorder2(iso3c, indicatorID == "Formal document with female included on title", value),
        indicatorID = factor(indicatorID, levels = c(
          "No formal document",
          "Formal document with no female on title",
          "Formal document with female included on title"
        ))
      )
      
      p <- ggplot(df, aes(x = iso3c, y = value, fill = indicatorID)) +
        geom_col() +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(expand=c(0,0),limits=c(0,102))+
        scale_fill_manual(
          values = c(
            style$colors$spot.secondary.light,
            style$colors$spot.secondary,
            style$colors$spot.primary
          ),
          labels= c(
            "No formal land title",
            "No female on land title",
            "Female included on land title"
          ),
          guide = guide_legend(reverse = TRUE)
        ) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top") +
        theme(legend.direction = "vertical")
      
      # Hard-left align legend
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    title = "In some countries, few women are documented on formal land titles.",
    note = "Note: Data from a study covering selected countries.",
    subtitle = wbg_name(indicator = "Share of households that own agricultural land or houses", mrv = years, denom = "%"),
    source = paste0("Source: G. Carletto, K. Deininger, T. Hilhorst, and W. Zakout (2018).")
  )
}

#make_all(path = "docs/sdg1/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg1", styler = style_atlas, saver = figure_save_draft_png) {
  #page 1
  saver(fig_sdg1_income_histogram_poverty(), styler, file.path(path, "fig_sdg1_income_histogram_poverty.png"), width = 5.5, height = 3.5)
  saver(fig_sdg1_poor_population_area_chart(), styler, file.path(path, "fig_sdg1_poor_population_area_chart.png"), width = 5.5, height = 3.5)

  #page 2
  saver(fig_sdg1_poor_number_map(), styler, file.path(path, "fig_sdg1_poor_number_map.png"), width = 5.5, height = 4.5)
  saver(fig_sdg1_pov_national_ur(), styler, file.path(path, "fig_sdg1_pov_national_ur.png"), width = 5.5, height = 4)

  #page 3
  saver(fig_sdg1_sp_quintile_targeting(), styler, file.path(path, "fig_sdg1_sp_quintile_targeting.png"), width = 5.5, height = 2.6)
  saver(fig_sdg1_sp_program_spending(), styler, file.path(path, "fig_sdg1_sp_program_spending.png"), width = 5.5, height = 2.75)
  saver(fig_sdg1_sp_cash_transfer(), styler, file.path(path, "fig_sdg1_sp_cash_transfer.png"), width = 5.5, height = 3.25)

  #page 4
  saver(fig_sdg1_map_cadaster_etc(), styler, file.path(path, "fig_sdg1_map_cadaster_etc.png"), width = 5.5, height = 4.5)
  saver(fig_sdg1_tenure_vs_rights(), styler, file.path(path, "fig_sdg1_tenure_vs_rights.png"), width = 2.8, height = 4)
  saver(fig_sdg1_females_land_titles(), styler, file.path(path, "fig_sdg1_females_land_titles.png"), width = 2.7, height = 4)
}
