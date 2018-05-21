library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(extrafont)
library(forcats)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
source("styles.R")

abouttheatlas_example <- function(startdate = 2011, enddate = 2016) {
  indicator <- "SE.PRE.ENRR"
  df <- wbgdata(wbgref$countries$iso3c, indicator, startdate = startdate, enddate = enddate, removeNA = TRUE)
  df <- df %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()
  df <- df %>% right_join(data.frame(iso3c = wbgref$countries$iso3c, stringsAsFactors=FALSE))
  df$bins <- supercut(df$SE.PRE.ENRR, c(
    "0–25" = "[0,25)",
    "25–50" = "[25,50)",
    "50–75" = "[50,75)",
    "75 and over" = "[75,Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 1.5,
    # title = "Despite evidence of the importance of pre-primary education, enrollment is still far from universal",
    title = "Example: Despite its importance, enrollment in pre-primary education is not universal.",
    note = "Note: Explanations about data selection, calculations and definitions appear in notes. a. Footnotes appear like this.",
    subtitle = wbg_name(indicator = "Gross pre-primary enrollment ratio", mrv = df$date, denom = "%"),
    source = paste("Source:", "UNESCO Institute for Statistics. World Development Indicators (SE.PRE.ENRR).")
  )
}

make_all("docs/abouttheatlas/pdf", style_atlas_cmyk, figure_save_final_pdf)
make_all <- function(path = "docs/abouttheatlas", styler = style_atlas, saver = figure_save_draft_png) {
  #page 1
  saver(abouttheatlas_example(), styler, file.path(path, "abouttheatlas_example.png"), width = 5.7, height = 4)
}
