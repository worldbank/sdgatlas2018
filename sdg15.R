library(wbgdata)
library(wbgcharts)
library(wbggeo)
library(wbgmaps)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggtreemap) # devtools::install_github("econandrew/ggtreemap")
library(stringr)
library(readr)
library(countrycode)
library(forcats)
library(pdftools)
source("styles.R")

fig_sdg15_forest_area_share <- function(year = 2015, cum_cover = 2/3) {
  indicator <- c("AG.LND.FRST.K2")

  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = year,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg15_forest_area_share.csv"
  )
  
  df <- df %>% filter(complete.cases(.))
  
  label_countries <- df %>%
    arrange(-AG.LND.FRST.K2) %>%
    mutate(prop_cumul = cumsum(AG.LND.FRST.K2) / sum(AG.LND.FRST.K2)) %>%
    filter(lag(prop_cumul, default = 0) < cum_cover) %>%
    pull(iso3c)
  
  df <- df %>% left_join(wbgref$countries$regions, by = "iso3c")
  
  df <- df %>%
    mutate(iso3c = ifelse(iso3c %in% label_countries, iso3c, "WLD")) %>%
    mutate(region_iso3c = ifelse(iso3c %in% label_countries, region_iso3c, "WLD"))
    
  df <- df %>%
    group_by(date, region_iso3c, iso3c) %>%
    summarise(AG.LND.FRST.K2 = sum(AG.LND.FRST.K2))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      labeller <- c(wbgref$countries$labels, WLD = "Rest of the world")
      colors <- c(WLD = style$colors$spot.secondary.light, style$colors$regions)
      aspect.ratio = 1
      ggplot(df, aes(area = AG.LND.FRST.K2, subgroup = (region_iso3c == "WLD"), fill = region_iso3c)) +
        geom_rect(stat = "treemap", color = "white", aspect.ratio = aspect.ratio) +
        geom_text(
          aes(
            label = str_wrap_lines(labeller[iso3c],3,force=TRUE),
            size = cut(AG.LND.FRST.K2, c(0, 1, Inf) * 1e6),
            color = region_iso3c
          ),
          stat = "treemap", 
          aspect.ratio = aspect.ratio,
          lineheight = 0.9,
          show.legend = FALSE
        ) +
        scale_size_manual(values = style$gg_text_size * c(0.8, 1.0)) +
        scale_fill_manual(values = colors, labels = labeller) +
        scale_color_manual(values = contrasting_colors(
          colors,
          textcolors = c(style$colors$text, style$colors$text.inverse),
          biases = c(0, 2.5)
        )) +
        scale_x_continuous(expand = c(0, 0)) +
        scale_y_reverse(expand = c(0, 0)) +
        style$theme() +
        theme(axis.text = element_blank(), panel.grid = element_blank())
    },
    aspect_ratio = 0.9,
    title = "Just ten countries account for two-thirds of global forest cover.",
    subtitle = wbg_name(indicator, by = "by region with top 10 countries", denom = NULL, year = year),
    source = "Source: FAO. WDI (AG.LND.FRST.K2)."
  )
}

fig_sdg15_forest_area_change <- function(years = c(1990,2015), cum_cover = 2/3) {
  indicators <- c("AG.LND.FRST.ZS", "AG.LND.FRST.K2")
  
  df <- wbgdata(
    c(wbgref$countries$iso3c, "WLD"),
    indicators,
    years = years,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg15_forest_area_change.csv"
  )

  top_N_iso3c <- df %>%
    filter(date == max(years), iso3c != "WLD") %>%
    arrange(-AG.LND.FRST.K2) %>%
    mutate(prop_cumul = cumsum(AG.LND.FRST.K2) / sum(AG.LND.FRST.K2)) %>%
    filter(lag(prop_cumul, default = 0) < cum_cover) %>%
    pull(iso3c)
  
  df <- df %>% filter(iso3c %in% c(top_N_iso3c, "WLD"))
  
  df <- df %>%
    left_join(wbgref$countries$regions) %>%
    mutate(region_iso3c = ifelse(iso3c == "WLD", "WLD", region_iso3c))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      p <- ggplot(df, aes(date, AG.LND.FRST.ZS, group = iso3c, color = region_iso3c, linetype = region_iso3c)) +
        geom_line(size = style$linesize) +
        scale_color_manual(values = c(style$colors$regions, style$colors$world)) +
        scale_linetype_manual(values = c(style$linetypes$regions, style$linetypes$world)) +
        scale_x_continuous(breaks = years, expand = c(0, 0)) +
        scale_y_continuous(sec.axis = dup_axis(
          breaks = df %>% filter(date == max(date)) %>% pull(AG.LND.FRST.ZS) %>% repel(3),
          labels = wbgref$all_geo$labels[df %>% filter(date == max(date)) %>% pull(iso3c)]
        )) +
        style$theme() 
      
      # Switch off clipping for labels
      g <- ggplotGrob(p)
      g$layout$clip[g$layout$name == "panel"] <- "off"
      g$theme <- p$theme
      g
    },
    aspect_ratio = 0.9,
    title = "Of these, only China's cover has been growing substantially.",
    subtitle = wbg_name(indicators[1], year = paste0(min(years)," & ",max(years))),
    source = "Source: FAO. WDI (AG.LND.FRST.ZS)."
  )
}

fig_sdg15_protected_map <- function(year = 2016) {
  indicators <- c("ER.LND.PTLD.ZS","AG.LND.TOTL.K2")
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg15_protected_map.csv"
  )

  df <- df %>% right_join(wbgref$countries$regions)

  # Calculate largest land area protected
  #df <- df %>% mutate(total = ER.LND.PTLD.ZS / 100 * AG.LND.TOTL.K2)
  #print(df %>% arrange(-total) %>% head(5))
  
  # Calculate total land area protected
  #print(sum(df$total, na.rm = TRUE))
    
  df$bins <- supercut(df$ER.LND.PTLD.ZS, c(
    "0-5"   = "[0,  5)",
    "5-15"  = "[5, 15)",
    "15 or over"  = "[15, Inf)"
  ))

  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    title = "Globally, around 14 percent of land is protected as national park, wildlife preserve, or a similar designations.",
    subtitle = wbg_name(indicators[1], year = year),
    source = "Source: UNEP, World Conservation Monitoring Centre, WRI. WDI (ER.LND.PTLD.ZS).",
    aspect_ratio = 1.3
  ) 
}

# TODO: ideally we should get this from the API rather than parsing PDFs!
read_redlist_pdf <- function(filename) {
  raw <- pdf_text(filename)
  skips <- c(7, 0, 0, 0, 0, 0)
  country_col <- c()
  dflist <- mapply(function(page, skip) {
    lines <- str_split(page, "\n")[[1]]
    if (skip > 0) lines <- lines[-(1:skip)]
    lines <- lines[nchar(lines) > 0]
    data_start <- min(unlist(lapply(str_locate_all(lines, "[0-9]"), function(l) {if (nrow(l) > 0) l[1,1] else Inf})))
    country_col <- str_trim(str_sub(lines, 1, data_start-1))
    data_cols <- str_sub(lines, data_start)
    data_string <- paste0(data_cols, collapse="\n")
    df <- read_table(data_string, col_types = c("nnnnnnnnnnnn"), col_names =
                       c("EX","EW","Subtotal_EX_EW","CR","EN","VU","Subtotal_CR_EN_VU","NT","LR/cd","DD","LC","Total")
    )
    df$country <- country_col
    df <- df %>% filter(complete.cases(.))
    df
  }, raw, skips, SIMPLIFY = FALSE, USE.NAMES = FALSE)
  df <- do.call(rbind, dflist)
  
  # Basic check for silent parse errors
  stopifnot(df$Subtotal_EX_EW == df$EX + df$EW)
  stopifnot(df$Subtotal_CR_EN_VU == df$CR + df$EN + df$VU)
  stopifnot(df$Subtotal_EX_EW + df$Subtotal_CR_EN_VU + df$NT + df$`LR/cd` + df$DD + df$LC == df$Total)
  df
}

fig_sdg15_threatened_plants <- function() {
  df <- read_redlist_pdf("inputs/sdg15/2017_3_RL_Stats_Table_6b.pdf")
  
  df <- df %>%
    mutate(
      iso3c = countrycode(
        country, "country.name", "iso3c",
        custom_match = c("Disputed Territory" = NA)
      ),
      pc_threatened = ifelse (Total > 0, Subtotal_CR_EN_VU / (Total - Subtotal_EX_EW - DD) * 100.0, 0) # No plants in Antarctica
    )
  
  df <- df %>% right_join(wbgref$countries$regions)
  
  df$bin <- supercut(df$pc_threatened, c(
    "0–2" = "[0, 2)",
    "2–8" = "[2, 8)",
    "8–18" = "[8, 18)",
    "18 and over" = "[18, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bin")
      g$theme <- style$theme()
      g
    },
    title = paste0("Over half of assessed plant species and one-quarter of assessed animal species are threatened."),
    subtitle = wbg_name(indicator = "Threatened plant species", denom = "% of all extant assessed plant species", year = "2017")
  )
}

fig_sdg15_threatened_animals <- function() {
  df <- read_redlist_pdf("inputs/sdg15/2017_3_RL_Stats_Table_6a.pdf")
  
  df <- df %>%
    mutate(
      iso3c = countrycode(
        country, "country.name", "iso3c",
        custom_match = c("Disputed Territory" = NA)
      ),
      pc_threatened = Subtotal_CR_EN_VU / (Total - Subtotal_EX_EW - DD) * 100.0
    )
  
  df <- df %>% right_join(wbgref$countries$regions)
  
  df$bin <- supercut(df$pc_threatened, c(
    "0–5" = "[0, 5)",
    "5–7" = "[5, 7)",
    "7–9" = "[7, 9)",
    "9 and over" = "[9, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas_open(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bin")
      g$theme <- style$theme()
      g
    },
    subtitle = wbg_name(indicator = "Threatened animal species", denom = "% of all extant assessed animal species", year = "2017"),
    note = "Note: Assumes data-deficient species are threatened in equal proportion to data-sufficent species. The proportion of threatened species can be larger for the world than for any country as threatened species, on average, exist in a smaller number of countries than non-threatened species. a. Royal Botanic Gardens Kew 2016, https://stateoftheworldsplants.com. b. Mora, C. and others 2011. https://doi.org/10.1371/journal.pbio.1001127",
    source = "Source: IUCN Red List of Threatened Species. http://http://www.iucnredlist.org"
  )
}

fig_sdg15_IWT_commit_map <- function() {
  df <- read_csv("inputs/sdg15/iwt_by_country_2010_2016.csv")
  
  df <- df %>%
    mutate(
      iso3c = countrycode(
        country, "country.name", "iso3c",
        custom_match = c("Global" = "ZZZ", "Regional/Multi-country" = "ZZZ")
      ),
      commitment = commitment * 1000 # in thousands in file
    ) %>%
    select(-country) %>%
    group_by(iso3c) %>%
    summarise(commitment = sum(commitment))

  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      # Edit the country list for presentation purposes
      df <- df %>%
        full_join(wbgref$countries$regions) %>%
        filter(is.na(region_iso3c) | region_iso3c %in% c("SSF", "MEA", "SAS", "EAS", "ECS")) %>%
        filter(!(iso3c %in% c("GRL", "ISL")))
      
      breaks <- c(5, 25, 100) * 1e6
      
      maps <- wbgmaps[[quality]]
      
      # Float other in north Pacific
      maps$country_centroids <- rbind(
        maps$country_centroids,
        data.frame(id = "ZZZ", long = 14000000, lat = 7500000)
      )
      
      p <- wbg_bubble_map(df, maps, style, "commitment", breaks, max_size = 1.5, labels = millions(), all_countries = FALSE)
      p +
        scale_x_continuous(expand = c(0, 0), limits = c(-4500000, standard_crop_wintri()$right)) +
        scale_y_continuous(expand = c(0, 0), limits = c(standard_crop_wintri()$ylim))
    },
    title = "For some species, poaching is an existential threat. Commitments to tackling illegal wildlife trade in Africa and Asia totaled $1.3 billion between 2010 and 2016.",
    subtitle = wbg_name(indicator = "International donor commitments for combatting illegal wildlife trade", denom = "US$ millions", year = str_range(c(2010,2016), shorten = c(2,2))),
    source = "Source: World Bank 2016. http://hdl.handle.net/10986/25340"
  )
}

fig_sdg15_IWT_commit_country_category <- function() {
  df <- read_csv("inputs/sdg15/iwt_by_country_and_category_2010_2016.csv")
  
  df <- df %>%
    rename("Promoting sustainable use" = "Promoting sustainable use and alternative livelihoods") %>%
    gather(category, value, -country)
  df <- df %>% mutate(
      iso3c = countrycode(country, "country.name", "iso3c"),
      value = value * 1000 # in thousands
    ) %>%
    select(-country)
    
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(
        x = fct_reorder(iso3c, value, sum),
        y = value,
        fill = fct_reorder(category, value, sum),
      )) +
        geom_col() +
        scale_fill_manual(
          values = rev(style$colors$categorical),
          guide = guide_legend(reverse = TRUE, byrow = TRUE)) +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 115e6), labels = millions()) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        theme(legend.position = c(0.95, 0), legend.justification = c(1, 0))
        #style$theme_legend("top")
    },
    title = "The largest category of funding for most countries is for the management of protected areas, to prevent poaching.",
    subtitle = wbg_name(indicator = "International donor commitments for combatting illegal wildlife trade", by = "top 19 recipient countries in Africa and Asia", denom = "US$ millions", year = str_range(2010:2016, shorten = c(2,2))),
    source = paste("Source: World Bank 2016. http://hdl.handle.net/10986/25340")
  )
}

# make_all(path = "docs/sdg15/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg15", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg15_forest_area_share(), styler, file.path(path, "fig_sdg15_forest_area_share.png"), width = 3.15, height = 3)
  saver(fig_sdg15_forest_area_change(), styler, file.path(path, "fig_sdg15_forest_area_change.png"), width = 2.15, height = 3)

  # page 2
  saver(fig_sdg15_protected_map(), styler, file.path(path, "fig_sdg15_protected_map.png"), width = 5.5, height = 4.25)

  # page 3
  saver(fig_sdg15_threatened_plants(), styler, file.path(path, "fig_sdg15_threatened_plants.png"), width = 5.5, height = 4.35)
  saver(fig_sdg15_threatened_animals(), styler, file.path(path, "fig_sdg15_threatened_animals.png"), width = 5.5, height = 4.35)
  
  # page 4
  saver(fig_sdg15_IWT_commit_map(), styler, file.path(path, "fig_sdg15_IWT_commit_map.png"), width = 5.5, height = 4.75)
  saver(fig_sdg15_IWT_commit_country_category(), styler, file.path(path, "fig_sdg15_IWT_commit_country_category.png"), width = 5.5, height=3.75)
}
