library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
source("styles.R")

fig_sdg4_school_enrollment_funnel <- function(year = 2015) {
  indicators <- c(
    Primary = "SE.PRM.ENRR",
    Secondary = "SE.SEC.ENRR",
    Tertiary = "SE.TER.ENRR"
  )
  
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = year,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_school_enrollment_funnel.csv"
  )
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      # Re-order factors for plotting
      df <- df %>%
        mutate(iso3c = factor(iso3c, rev(wbgref$incomes$iso3c))) %>%
        mutate(indicatorID = factor(indicatorID, rev(indicators)))

      ggplot(df, aes(indicatorID, value)) +
        geom_col(fill = style$colors$spot.primary) +
        scale_x_discrete(labels = setNames(names(indicators), indicators)) +
        scale_y_continuous(limits = c(0, 110), breaks = (0:4)*25) +
        facet_wrap(~ iso3c, nrow = 2, ncol = 2, labeller = as_labeller(wbgref$incomes$labels)) +
        style$theme() +
        style$theme_barchart() +
        coord_flip() +
        theme(legend.position = "none", strip.text = element_text(hjust = 0), panel.spacing.y = unit(0.26, "npc"))
    },
    aspect_ratio = 1,
    title="While most children are enrolled in primary education, fewer enroll at the secondary and tertiary levels.",
    subtitle = wbg_name(indicator = "Gross enrollment ratio", year = year, denom = "%"),
    source = paste("Source: UNESCO Institute for Statistics. WDI (SE.PRM.ENRR; SE.SEC.ENRR; SE.TER.ENRR).")
  )
}

fig_sdg4_enrollment_measures_by_income <- function(year = 2015) {
  indicators <- c("SE.PRM.ENRR", "SE.PRM.NENR", "SE.PRM.TENR")

  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = year,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_enrollment_measures_by_income.csv"
  )
  
  df <- df %>% transmute(
    iso3c,
    date,
    net = SE.PRM.NENR,
    in_secondary = SE.PRM.TENR - SE.PRM.NENR,
    wrong_age = SE.PRM.ENRR - SE.PRM.TENR
  )
  
  df <- df %>% gather(key = "category", value = "value", net, in_secondary, wrong_age)

  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      # Re-order factors for plotting
      df <- df %>%
        mutate(category = factor(category, c("net", "in_secondary", "wrong_age"))) %>%
        mutate(iso3c = factor(iso3c, df %>% filter(category == "net") %>% arrange(-value) %>% pull(iso3c)))

      #editorial decision to exclude the "in secondary" component of this - it's small and we can tell the story without it.
      p<-ggplot(df %>% filter(category !="in_secondary"), aes(iso3c, value, fill = category)) +
        geom_col(position = position_stack(reverse = TRUE), width = 0.75) +
        coord_flip() +
        scale_x_discrete(labels = setNames(str_wrap(wbgref$incomes$labels, width = 8), names(wbgref$incomes$labels))) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_fill_manual(values = c(style$colors$spot.primary,style$colors$spot.secondary),
                          labels=c("Correct age for school year", "Older or younger")) +
        style$theme() +
        style$theme_barchart() +
        theme(legend.position = "top",
              plot.margin = margin(0,3,5,0,"mm"))
      
      g <- ggplotGrob(p)
      g$layout$l[g$layout$name == "guide-box"] <- g$layout$l[g$layout$name == "guide-box"] - 1
      g$theme <- style$theme()
      g
    },
    aspect_ratio = 2/3.75,
    title= "Not all children attend school at the right age, and so gross enrolment rates can exceed 100 percent.",
    subtitle = wbg_name(indicator = "Gross primary enrollment ratio", year = year, denom = "%"),
    source = paste("Source: ", "UNESCO Institute for Statistics. WDI (SE.PRM.ENRR; SE.PRM.NENR).")
  )
}

fig_sdg4_gross_pre_primary_enrollment <- function(startdate = 2011, enddate = 2016) {
  indicator <- "SE.PRE.ENRR"

  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    startdate = startdate,
    enddate = enddate,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_gross_pre_primary_enrollment.csv"
  )

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
      wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
    },
    aspect_ratio = 1.5,
    title = "Despite its importance, enrollment in pre-primary education is not universal.",
    subtitle = wbg_name(indicator = "Gross pre-primary enrollment ratio", mrv = df$date, denom = "%"),
    source = paste("Source:", "UNESCO Institute for Statistics. World Development Indicators (SE.PRE.ENRR).")
  )
}

# fig_sdg4_school_facilities <- function(years = 2010:2014) {
# 
#   indicators <- tribble(
#     ~facility,       ~status,      ~indicatorID,
#     "Electricity",   "Yes",        "UIS.AFR.SCHBSP.1.PU.WELEC",
#     "Single-sex toilets",       "Yes", "UIS.AFR.SCHBSP.1.PU.WSTOIL", # single sex
#     "Potable water", "Yes",        "UIS.AFR.SCHBSP.1.PU.WPOWAT",
#     "Trained teachers", "Yes", "SE.PRM.TCAQ.ZS"
#   )
# 
#   df <- wbgdata(
#     wbgref$countries$iso3c,
#     indicators$indicatorID,
#     years = years,
#     indicator.wide = FALSE,
#     removeNA = TRUE,
#     # Comment the next two lines to use live API data
#     offline = "only",
#     offline.file = "inputs/cached_api_data/fig_sdg4_school_facilities.csv"
#   )
# 
#   # most recent value
#   df <- df %>%
#     group_by(iso3c, indicatorID) %>%
#     filter(date == max(date)) %>%
#     ungroup()
# 
#   df <- df %>% left_join(indicators)
# 
#   # keep only countries with all three facilities
#   df <- df %>%
#     group_by(iso3c) %>%
#     filter(n() == 4) %>%
#     ungroup()
# 
#   figure(
#     data = df,
#     plot = function(df, style = style_atlas()) {
#       iso3c_order <- df %>%
#         filter(facility == "Potable water") %>%
#         arrange(value) %>%
#         pull(iso3c)
#       df <- df %>% mutate(
#         iso3c = factor(iso3c, iso3c_order),
#         facility = factor(facility, c("Potable water", "Electricity", "Single-sex toilets", "Trained teachers"))
#       )
#       ggplot(df, aes(iso3c, value )) +
#         geom_col(aes(y = 100), fill=style$colors$neutral) +
#         geom_col(aes(y = value, fill= facility)) +
#         facet_wrap(~facility, ncol=4) +
#         coord_flip() +
#         scale_x_discrete(labels = wbgref$countries$labels) +
#         scale_fill_manual(values = c(style$colors$spot.primary,
#                                      style$colors$spot.primary,
#                                      style$colors$spot.primary,
#                                      style$colors$spot.secondary)) +
#         style$theme() +
#         style$theme_barchart() +
#         theme(
#           legend.position = "none",
#           strip.text = element_text(hjust=0.45),
#           panel.spacing.x = unit(0.05, "npc"),
#           panel.spacing.y = unit(0.1, "npc")
#         )
#     },
#     aspect_ratio = 1.25,
#     note = "Note: Dataset limited to Sub-Saharan Africa. Only countries with data on all four dimensions shown.",
#     title = "Many primary schools in Sub-Saharan Africa lack access to basic facilities that support learning, and many children are taught by teachers without qualifications.",
#     subtitle = wbg_name(indicator = "Primary schools with access to facilities, and trained teachers", mrv = years, denom = "%"),
#     source = paste("Source: UNESCO Institute for Statistics, EdStats and World Development Indicators. (UIS.AFR.SCHBSP.1.PU.WELEC; UIS.AFR.SCHBSP.1.PU.WSTOIL; UIS.AFR.SCHBSP.1.PU.WPOWAT; SE.PRM.TCAQ.ZS).")
#   )
# }

fig_sdg4_govt_education_spending <- function(years = 2011:2016, region.years = 2008:2016) {
  indicator <- "SE.XPD.TOTL.GD.ZS"
  
  df.country <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_govt_education_spending-country.csv"
  )
  
  df.country <- df.country %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()
  
  df.country <- df.country %>% left_join(wbgref$countries$regions)
  
  df.region <- wbgdata(
    wbgref$regions$iso3c,
    indicator,
    years = region.years,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_govt_education_spending-region.csv"
  )
  
  df.region <- df.region %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup() %>%
    mutate(iso3c = fct_reorder(iso3c, -value))
  
  figure(
    data = list(country = df.country, region = df.region),
    plot = function(dfs, style = style_atlas()) {
      #exclude micronesia from the plot and get it to wort properly 
      dfs$country <- dfs$country %>% filter(iso3c != "FSM") 
      levels(dfs$country$region_iso3c) <- levels(dfs$region$iso3c)
      
      ggplot(dfs$region, aes(iso3c, value)) +
        geom_point(color="white") +
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
        scale_y_continuous(expand = c(0, 0), limit = c(0, 9), breaks = c(0,2,4,5,6,8,10)) +
        coord_flip() +
        style$theme() +
        style$theme_barchart()
    },
    aspect_ratio = 2.5,
    title = "Education is an investment. All governments bear some responsibility for funding education; median spending on education worldwide is 5 percent of GDP.",
    note = "Note: Excludes Micronesia which is an outlier. Middle East & North Africa median value is from 2008.",
    subtitle = wbg_name(indicator = "Government spending on education, by country and regional median", mrv = years, denom =  "% of GDP"),
    source = paste("Source:", "UNESCO Institute for Statistics. World Development Indicators (SE.XPD.TOTL.GD.ZS).")
  )
}

fig_sdg4_pupil_teacher_ratios <- function(year = 2015){
  indicators <- c(
    "Primary" = "SE.PRM.ENRL.TC.ZS",
    "Lower secondary" = "SE.SEC.ENRL.LO.TC.ZS", 
    "Upper secondary" = "SE.SEC.ENRL.UP.TC.ZS"
  )
  
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators,
    years = year,
    indicator.wide =  FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_pupil_teacher_ratios.csv"
  )
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      df <- df %>%
        mutate(iso3c = factor(iso3c, wbgref$incomes$iso3c)) %>%
        mutate(indicatorID = fct_rev(indicatorID))
      
      ggplot(df, aes(indicatorID, value)) +
        geom_linerange(data =.%>% group_by(indicatorID) %>% mutate (min=min(value),max=max(value)),
                       aes(x = indicatorID, ymin = min, ymax = max), 
                       color=style$colors$reference, size=style$linesize_reference) +
        geom_point(aes(color=iso3c),stroke = style$point_stroke, shape = style$shapes$point, size = style$point_size) +
        scale_x_discrete(labels = setNames(names(indicators), indicators)) +
        scale_y_continuous(expand = c(0,0), limits=c(0,43)) +
        scale_color_manual(values = style$colors$incomes, labels = wbgref$incomes$labels) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        style$theme_legend("top")
    },
    aspect_ratio = 2.5,
    title = "Large class sizes are common in low- and lower-middle-income countries.",
    subtitle = wbg_name(indicator = "Average number of pupils per teacher",year = year),
    source = paste("Source: UNESCO Institute for Statistics. World Development Indicators (SE.PRM.ENRL.TC.ZS; SE.SEC.ENRL.LO.TC.ZS; SE.SEC.ENRL.UP.TC.ZS).")
  )
}

fig_sdg4_completion_by_gender_and_income <- function(years = 1990:2015) {
  indicators <- data.frame(
    indicatorID = c("SE.PRM.CMPT.MA.ZS", "SE.PRM.CMPT.FE.ZS", "SE.SEC.CMPT.LO.MA.ZS", "SE.SEC.CMPT.LO.FE.ZS"),
    level = factor(c(rep("Primary", 2), rep("Lower secondary", 2)), levels = c("Primary", "Lower secondary")),
    gender = rep(c("male", "female"),2),
    stringsAsFactors = FALSE
  )
  
  df <- wbgdata(
    wbgref$incomes$iso3c,
    indicators$indicatorID, 
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_completion_by_gender_and_income.csv"
  )
  
  df <- df %>% left_join(indicators)
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(
        iso3c = factor(iso3c, rev(wbgref$incomes$iso3c)),
        gender = factor(gender, levels = c("male", "female"))
      )
      iso3c_labeller <- as_labeller(function(l) wbgref$incomes$labels[l])
      
      ggplot(df, aes(date, value, color = gender)) +
        facet_grid(level ~ iso3c, switch = "y", labeller = labeller(iso3c = iso3c_labeller)) +
        geom_line(size = style$linesize) +
        scale_y_continuous(limits = c(0, NA), breaks = c(0,25,50,75,100)) +
        scale_x_continuous(breaks = range(years), expand = c(0, 1)) +
        scale_color_manual(values = style$colors$gender) +
        geom_text(
          aes(label = Hmisc::upFirst(as.character(gender)), y = mean(value)+3*(value-mean(value))),
          data = df %>% filter(date == min(years), iso3c == "LIC", level == "Primary"),
          size = style$gg_text_size, family = style$family,
          hjust = 0
        ) +
        style$theme() +
        theme(
          legend.position = "none",
          strip.text.x = element_text(hjust = 0.5),
          panel.spacing.x = unit(0.05, "npc"),
          panel.spacing.y = unit(0.1, "npc")
        )
    },
    aspect_ratio = 1.5,
    title = "Gender gaps in early education completion have closed, except in low-income countries, where completion rates are around 5 percentage points higher for boys.",
    subtitle = wbg_name(indicator = "Completion rate", denom = "% of relevant age group"),
    source = paste("Source: UNESCO Institute for Statistics. WDI (SE.PRM.CMPT.MA.ZS; SE.PRM.CMPT.FE.ZS; SE.SEC.CMPT.LO.MA.ZS; SE.SEC.CMPT.LO.FE.ZS).")
  )
}

fig_sdg4_gender_parity_by_level <- function(year = 2015) {
  indicators <- c(
    Primary = "SE.ENR.PRIM.FM.ZS",
    Secondary = "SE.ENR.SECO.FM.ZS",
    Tertiary = "SE.ENR.TERT.FM.ZS"
  )
  
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = year,
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_gender_parity_by_level.csv"
  )
  
  df <- df %>% left_join(wbgref$countries$incomegroups)

   figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      df <- df %>% 
        filter(iso3c != "QAT") %>%
        mutate(income_iso3c = fct_rev(factor(income_iso3c, wbgref$incomes$iso3c)))
      
      ggplot(df, aes(factor(indicatorID, rev(indicators)), value)) +
        geom_point(alpha = 0.5, color = style$colors$spot.primary, size = style$point_size, stroke = style$point_stroke, shape = style$shapes$point) +
        scale_y_continuous(
          trans = "log2",
          position = "top",
          limits = c(0.33, 3),
          breaks = c(1/2, 1, 2)
        ) +
        scale_x_discrete(labels = setNames(names(indicators), indicators)) +
        coord_flip() +
        style$theme() +
        style$theme_barchart() +
        facet_wrap(~income_iso3c, ncol = 1, labeller = as_labeller(wbgref$incomes$labels)) +
        theme(
          axis.title = element_blank(),
          strip.placement = "inside"
        )
    },
    aspect_ratio = 2,
    title = "The relative share of male and female students enrolled in education varies substantially between countries, especially at the tertiary level.",
    subtitle = wbg_name(indicator = "Gender parity index (GPI) in gross school enrollment", by = "by country",year = year),
    note = "Note: Qatar's tertiary GPI of 6.95 is excluded as an outlier because of the large share of men in the general population.",
    source = paste("Source: UNESCO Institute for Statistics. World Development Indicators (SE.ENR.PRIM.FM.ZS; SE.ENR.SECO.FM.ZS; SE.ENR.TERT.FM.ZS).")
  )
}

fig_sdg4_enrollment_vs_fertility_panel <- function(years = c(1990,2014)) {
  indicators <- c("SE.SEC.ENRR.FE","SP.ADO.TFRT")

  df <- wbgdata(
    wbgref$countries$iso3c,
    indicators,
    years = years,
    indicator.wide = TRUE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_enrollment_vs_fertility_panel.csv"
  )

  df <- df %>% left_join(wbgref$countries$regions)
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(SE.SEC.ENRR.FE, SP.ADO.TFRT, region_iso3c)) +
        geom_point(alpha = 0.8, aes(color = region_iso3c), 
                   size = style$point_size, 
                   stroke = style$point_stroke, 
                   shape = style$shapes$point) +
        scale_color_manual(
          values = style$colors$regions,
          labels = wbgref$regions$labels,
          guide = guide_legend(override.aes = aes(alpha = 1.0))) +
        scale_x_continuous() +
        xlab("Gross secondary school enrollment, female (%)") +
        facet_wrap(~date) +
        style$theme() +
        theme(
          panel.grid.major.x = NULL,
          legend.position="top",
          legend.direction="horizontal",
          legend.box = "horizontal",
          axis.title = NULL,
          axis.title.y = element_blank(),
          panel.spacing.x = unit(0.08, "npc")
        ) +
        style$theme_scatter()
        
    },
    aspect_ratio = 0.75,
    title = "Girls enrolled in school are less likely to become pregnant as teenagers. Between 1990 and 2014, every region has seen an increase in the share of girls enrolled in secondary school, and a decline in adolescent fertility rates.",
    subtitle = wbg_name(indicator = "Adolescent fertility rate by country", denom = "births per 1,000 women ages 15–19")
    # source = paste("Source: United Nations Population Division, UNESCO Institute for Statistics. WDI (SE.SEC.ENRR.FE; SP.ADO.TFRT).")
  )
}

fig_sdg4_enrollment_vs_fertility <- function(years = 1990:2014) {
  indicators <- c("SE.SEC.ENRR.FE","SP.ADO.TFRT")
  
  df <- wbgdata(
    wbgref$regions$iso3c,
    indicators,
    years = years,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg4_enrollment_vs_fertility.csv"
  )
  
  figure(
    data = df, 
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(SE.SEC.ENRR.FE, SP.ADO.TFRT)) +
        geom_point( aes(color = iso3c, alpha=date), size = style$point_size, stroke = style$point_stroke, shape = style$shapes$point) +
        scale_color_manual(values = style$colors$regions, labels = wbgref$regions$labels) +
        scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
        xlab("Gross secondary school enrollment, female (%)") +
        theme(
          panel.grid.major.x = NULL,
          axis.title = NULL, axis.title.y = element_blank()
        ) +
        style$theme() +
        style$theme_scatter()
    },
    # title = "Every region has seen an in increase in the share of girls enrolled in secondary school, and a decline in fertility rates over the last 25 years.",
    subtitle = wbg_name(indicator = "Adolescent fertility rate by region, 1990-2014", denom = "births per 1,000 women ages 15–19"),
    source = paste("Source: UN Population Division, UNESCO Institute for Statistics. World Development Indicators (SE.SEC.ENRR.FE; SP.ADO.TFRT).")
  )
}

# make_all("docs/sdg4/pdf", style_atlas_cmyk, figure_save_final_pdf)
make_all <- function(path = "docs/sdg4", styler = style_atlas, saver = figure_save_draft_png) {
  #page 1
  saver(fig_sdg4_school_enrollment_funnel(), styler, file.path(path, "fig_sdg4_school_enrollment_funnel.png"), width = 2.85, height = 3.4)
  saver(fig_sdg4_enrollment_measures_by_income(), styler, file.path(path, "fig_sdg4_enrollment_measures_by_income.png"), width = 2.55, height = 3.4)
  saver(fig_sdg4_gross_pre_primary_enrollment(), styler, file.path(path, "fig_sdg4_gross_pre_primary_enrollment.png"), width = 5.5, height = 4)
  
  #page 2
  saver(fig_sdg4_govt_education_spending(), styler, file.path(path, "fig_sdg4_govt_education_spending.png"), width = 5.55)
  #saver(fig_sdg4_school_facilities(), styler, file.path(path, "fig_sdg4_school_facilities.png"), width = 5.5)
  saver(fig_sdg4_pupil_teacher_ratios(), styler, file.path(path, "fig_sdg4_pupil_teacher_ratios.png"), width = 5.5, height=1.9)

  #page 3
  saver(fig_sdg4_completion_by_gender_and_income(), styler, file.path(path, "fig_sdg4_completion_by_gender_and_income.png"), width = 5.5, height = 3.8,  padding=margin(0,2,0,0,"mm"))
  saver(fig_sdg4_gender_parity_by_level(), styler, file.path(path, "fig_sdg4_gender_parity_by_level.png"), width = 5.5, height=4.5)
  
  #page 4
  saver(fig_sdg4_enrollment_vs_fertility_panel(), styler, file.path(path, "fig_sdg4_enrollment_vs_fertility_panel.png"), width = 5.5, height = 4)
  saver(fig_sdg4_enrollment_vs_fertility(), styler, file.path(path, "fig_sdg4_enrollment_vs_fertility.png"), width = 5.5, height=4.7)
}
