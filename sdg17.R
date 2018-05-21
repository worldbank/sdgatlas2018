library(ggplot2)
library(wbgdata)
library(wbgcharts)
library(wbgmaps)
library(wbggeo)
library(dplyr)
library(ggalluvial)
library(countrycode)
library(tidyr)
library(forcats)
library(readr)
library(readstata13)
library(readxl)
library(gtable)
source('styles.R')

fig_sdg17_aid_panel <- function(year = 2016) {
  # Get OECD data
  TRANSACTYPE_codes <- c(
    oda = "1010",
    oda_pc_gni = "2",
    refugee = "1820"
  )
  
  df <- OECD::get_dataset("TABLE1", filter = list(NULL, NULL, TRANSACTYPE_codes, 1140, "A"), start_time = year, end_time = year)
  meta <- OECD::get_data_structure("TABLE1")
  df <- df %>% inner_join(
    meta$DAC_DONOR  %>%
      filter(!grepl("20[0-9]{3}", id) & id != "918") %>%  # id >= 20000 is aggregates, 918 is EU institutions
      transmute(DAC_DONOR = id, donor_country = label)
  )
  
  df <- df %>% mutate(iso3c = countrycode(donor_country, "country.name", "iso3c"))
  
  # Load DAC members, except EU
  dac_members <- read_csv("inputs/sdg17/dac_members.csv")
  dac_members <- dac_members %>% filter(iso3c != "EUE")
  
  df <- df %>% filter(iso3c %in% dac_members$iso3c)
  
  df <- df %>% transmute(
    iso3c = iso3c,
    indicator = setNames(names(TRANSACTYPE_codes), TRANSACTYPE_codes)[TRANSACTYPE],
    date = obsTime,
    value = obsValue * (10 ** as.numeric(POWERCODE))
  )
  
  df_wide <- df %>%
    spread(indicator, value) %>%
    mutate(refugee_pc_oda = refugee / oda * 100) %>%
    select(-refugee)
  
  df <- df_wide %>% gather(key = "indicator", value = "value", oda_pc_gni, oda, refugee_pc_oda)

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = fct_reorder2(iso3c, indicator == "oda_pc_gni", -replace_na(value, 0)))
      facet_labeller <- as_labeller(c(
        oda_pc_gni = "ODA (% of GNI)\n",
        oda = "ODA (US$ billions)\n",
        refugee_pc_oda ="In-donor refugee\ncosts (% of ODA)"
      ))
      
      p.oda_pc_gni <- ggplot(df %>% filter(indicator == "oda_pc_gni"), aes(iso3c, value)) +
        geom_hline(
          yintercept = 0.7,
          linetype = style$linetypes$reference,
          size = style$linesize_reference,
          color = style$colors$reference
        ) +
        geom_col(position = "dodge", fill = style$colors$spot.primary) +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(expand = c(0, 0)) +
        coord_flip() +
        facet_wrap(~ indicator, labeller = facet_labeller) +
        style$theme() +
        style$theme_barchart()

      p.oda <- ggplot(df %>% filter(indicator == "oda"), aes(iso3c, value)) +
        geom_col(position = "dodge", fill = style$colors$spot.secondary) +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(labels = billions(), expand = c(0, 0)) +
        coord_flip() +
        facet_wrap(~ indicator, labeller = facet_labeller) +
        style$theme() +
        style$theme_barchart() +
        theme(axis.text.y = element_blank())

      p.refugee_pc_oda <- ggplot(df %>% filter(indicator == "refugee_pc_oda"), aes(iso3c, value)) +
        geom_percent_col(position = "dodge", fill = style$colors$spot.primary, fill.bg = style$colors$neutral) +
        scale_x_discrete(labels = wbgref$countries$labels) +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 115), breaks = c(0,50,100)) +
        coord_flip() +
        facet_wrap(~ indicator, labeller = facet_labeller) +
        style$theme() +
        style$theme_barchart() +
        theme(axis.text.y = element_blank())
      
      pt.oda_pc_gni <- ggplotGrob(p.oda_pc_gni)
      pt.oda <- ggplotGrob(p.oda)
      pt.refugee_pc_oda <- ggplotGrob(p.refugee_pc_oda)
      
      chart <- gtable_row(
        "chart",
        list(pt.oda_pc_gni, zeroGrob(), pt.oda, zeroGrob(), pt.refugee_pc_oda),
        height = unit(1, "null"),
        widths = unit(c(30,2,20,2,10), "null"))
      chart$theme <- style$theme()
      chart      
    },
    title = "Official development assistance totaled $144 billion in 2016, but only six countries met the longstanding commitment to contribute 0.7 percent of GNI.",
    subtitle = wbg_name(indicator = "Official development assistance (ODA) from members of OECD's Development Assistance Committee", year = 2016),
    source = "Source: OECD International Development Statistics (database). http://dx.doi.org/10.1787/dev-data-en",
    aspect_ratio = 0.5
  )
}

fig_sdg17_regional_disbursements <- function() {
  df<-read_xlsx("inputs/sdg17/bilateral_flows.xlsx") %>%
    rename(from_iso3c = from, to_iso3c = to, value = disbursements)
  
  df$to_iso3c <- recode(
    df$to_iso3c,
    "EAP" = "EAS", "ECA" = "ECS", "LAC" = "LCN", "MENA" = "MEA","SAS"="SAS","SSA"="SSF"
  )
  
  df <- df %>% mutate(value = value * 1e3) # Was in thousands, convert back to units
  
  figure(data = df, plot = function(df, style = style_atlas()) {
    stratum_fill <- c(
      rev(c("EAS", "ECS", "LCN", "MEA", "NAC", "SAS")),
      rev(c("EAS", "ECS", "LCN", "MEA", "SAS", "SSF"))
    )
    
    ggplot(df, aes(weight=value,axis1=from_iso3c,axis2=to_iso3c)) +
      geom_alluvium(aes(fill=to_iso3c),width=1/10,alpha=0.75) +
      geom_stratum(fill = style$colors$regions[stratum_fill], width=1/10, color = NA) +
      geom_text(
        aes(
          label=as.numeric(billions(1)(value)),
          alpha=(value > 2e9)
        ),
        stat = "stratum",
        hjust = 0.5,
        nudge_x = 0,
        family = style$theme()$text$family,
        size = style$gg_text_size,
        lineheight = 0.75,
        color = style$colors$text.inverse
      ) +
      scale_alpha_manual(values = c(`TRUE` = 1.0,`FALSE` = 0.0)) +
      geom_text(
        aes(label=str_wrap_lines(wbgref$regions$labels[from_iso3c], 2)), 
        stat="stratum",
        hjust = 1,
        nudge_x = -0.07,
        family = style$theme()$text$family,
        size = style$gg_text_size*0.8,
        lineheight = 0.75
      ) +
      geom_text(
        aes(label=str_wrap_lines(wbgref$regions$labels[to_iso3c], 2)),
        stat="stratum",
        hjust = 0,
        nudge_x = 0.07,
        family = style$theme()$text$family,
        size = style$gg_text_size*0.8,
        lineheight = 0.75
      ) +
      annotate("text", x = 0.93, y = 56.5e9, label = "Creditors,\nby region", hjust = 1, family = style$family, size = style$gg_text_size, lineheight = 0.9) +
      annotate("text", x = 2.07, y = 56.5e9, label = "Borrowers,\nby region (a)", hjust = 0, family = style$family, size = style$gg_text_size, lineheight = 0.9) +
      scale_fill_manual(values = style$colors$regions) +
      scale_x_continuous(expand = c(0.15, 0)) +
      style$theme() +
      theme(axis.text=element_blank(), panel.grid = element_blank())
  },
  title = "Loan disbursements from bilateral creditors (governments and their agencies) to low- and middle-income countries reached $54 billion in 2016, an all-time high.",
  subtitle = wbg_name(indicator = "Public and publicly guaranteed external debt, bilateral disbursements", denom = "US$ billions", year = 2016),
  note = "Note: Represents drawings by the borrower on bilateral debt, including loans from governments and their agencies (including central banks), loans from autonomous bodies, and direct loans from official export credit agencies. a. Excludes high-income countries.",
  source = "Source: World Bank Debtor Reporting System. Aggregates by borrower available in World Development Indicators (DT.DIS.BLAT.CD).",
  aspect_ratio = 1.3
  )
}

fig_sdg17_fdi_remittances_by_region <- function(years = 2007:2016) {
  indicators = c(
    "BX.KLT.DINV.CD.WD",
    "BX.TRF.PWKR.CD.DT"
  )
  
  df <- wbgdata(
    wbgref$regions_excl_high_income$iso3c,
    indicators,
    years = years,
    indicator.wide = FALSE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg17_fdi_remittances_by_region.csv"
  )
  
  df <- df %>% left_join(wbgref$regions_excl_high_income$regions)

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(date, value, color = indicatorID)) +
        geom_line(size = style$linesize) +
        scale_color_manual(
          values = c(
            BX.TRF.PWKR.CD.DT = style$colors$spot.primary,
            BX.KLT.DINV.CD.WD = style$colors$spot.secondary
          ),
          labels = c(
            BX.TRF.PWKR.CD.DT = wbg_name("BX.TRF.PWKR.CD.DT", denom = NULL),
            BX.KLT.DINV.CD.WD = wbg_name("BX.KLT.DINV.CD.WD", denom = NULL)
          )) +
        scale_x_continuous(breaks = bracketed_breaks(df$date, closeness_threshold = 0.2)) +
        scale_y_continuous(labels = billions()) +
        facet_wrap(~region_iso3c, labeller = as_labeller(wbgref$regions$labels)) +
        style$theme() +
        style$theme_legend("top") +
        theme(
          panel.spacing = unit("0.05", "npc"),
          strip.text.x = element_text(hjust = 0.5)
        )
    },
    title = "Foreign direct investment and remittances to low- and middle-income countries totaled around $1 trillion in 2016.",
    subtitle = wbg_name(indicator = "Foreign direct investment, net inflows, and personal remittances, received", denom = "US$ billions"),
    note = "Note: Excludes high-income countries.",
    source = "Source: World Bank, IMF, and UNCTAD. WDI (BX.KLT.DINV.CD.WD; BX.TRF.PWKR.CD.DT).",
    aspect_ratio = 2.5
  )
}

# Check: It seems that Kosovo is returned with a 2 letter code but not a 3 letter code, so we remove it - seems like an error in backend to report
fig_sdg17_enterprise_exports <- function(years = 2006:2017) {
  indicator <- "IC.FRM.TRD.TR5"
  df <- wbgdata(
    wbgref$countries$iso3c,
    indicator,
    years = years, 
    indicator.wide = FALSE,
    removeNA = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg17_enterprise_exports.csv"
  )
  
  df <- df %>% 
    group_by(iso3c, indicatorID) %>%
    filter(date == max(date)) %>%
    ungroup()

  df <- df %>% right_join(wbgref$countries$regions)
  
  df$bins <- supercut(df$value, c(
    "0–3"   = "[0,  3)",
    "3–6"  = "[3, 6)",
    "6 or over"  = "[6, Inf)"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    title = "Exports can promote economic growth, but in many countries in Sub-Saharan Africa, firms tend to export little.",
    subtitle = wbg_name(indicator, by = "manufacturing firms", mrv = df$date),
    source = paste("Source: World Bank. Enterprise Surveys (IC.FRM.TRD.TR5)."),
    aspect_ratio = 1.3
  ) 
}

fig_sdg17_trade_dtf <- function(year = 2017) {
  df <- read_xlsx("inputs/sdg17/DB18-Historical-data-complete-data-with-DTFs.xlsx", skip = 1)
  
  # Doing Business is a tricky as they do multiple cities for some countries and
  # use non-standard codes to record them.
  
  # First use the special mappings. Otherwise, fall back on standard iso3c codes
  iso3c_mapping <- read_csv("inputs/sdg17/db_main_city_iso3c_mapping.csv")
  iso3c_mapping$iso3c[is.na(iso3c_mapping$iso3c)] <- "ZZZ" # a kind of sentinel
  df <- df %>% left_join(iso3c_mapping, by = c(`Country code`= "db_country_code"))
  df <- df %>% mutate(iso3c = ifelse(is.na(iso3c), `Country code`, iso3c))
  
  # Now remove the sentinel rows and make sure everything left is WDI friendly
  df <- df %>% filter(iso3c != "ZZZ")
  unmatched <- df$iso3c[!(df$iso3c %in% wbgref$countries$iso3c)]
  stopifnot(length(unique(unmatched)) == 1) # TWN will be unmatched
  
  df <- df %>%
    transmute(
      iso3c = iso3c,
      date = `DB Year` - 1, # DB year is publication year and usually has a reference of the previous year
      value = `DTF - Trading across borders\r\n(DB16-18 methodology)`
    ) %>%
    filter(date == year)
  
  df <- df %>% right_join(wbgref$countries$regions)
  
  df$bins <- supercut(df$value, c(
    "0–25"   = "[0,  25)",
    "25–50"  = "[25, 50)",
    "50–75"  = "[50, 75)",
    "75–100" = "[75, 100]"
  ))
  
  figure(
    data = df,
    plot = function(df, style = style_atlas(), quality = "low") {
      g <- wbg_choropleth(df, wbgmaps[[quality]], style, variable = "bins")
      g$theme <- style$theme()
      g
    },
    title = "Engaging in international trade involves more barriers in low- and middle-income countries.",
    subtitle = wbg_name(indicator = "Ease of trading across borders, composite distance to frontier score", denom = paste0(str_range(c(0,100)),", higher is better"), year = 2017),
    source = "Source: World Bank Doing Business 2018 (database). http://www.doingbusiness.org",
    aspect_ratio = 1.3
  ) 
}

fig_sdg17_PPP_investment_time <- function(years = 2000:2016) {
  ppp <- read.dta13("inputs/sdg17/PPI DB_090118.dta", nonint.factors = TRUE) # we don't use these fields but this shuts up a warning 
  
  ppp <- ppp %>%
    filter(!(investment %in% c(-8888, -9999))) %>% # missing values
    filter(type != "Divestiture") %>%
    filter(stype != "Merchant") %>%
    filter(sector != "ICT")
  
  ppp <- ppp %>%
    filter(IY %in% years)
  
  ppp.totals <- ppp %>%
    group_by(IY, income) %>%
    summarise(investment = sum(investment)) %>%
    ungroup()
  
  ppp.totals <- ppp.totals %>%
    transmute(
      iso3c = unname(inv(wbgref$incomes$labels)[as.character(income)]), # as.char as factor is defined differently to ours
      date = IY,
      investment = investment * 1e6 # is in millions
    )
  
  indicator = "NY.GDP.MKTP.CD"
  gdp <- wbgdata(ppp.totals$iso3c, indicator, years = years)
  
  ppp.totals <- ppp.totals %>% left_join(gdp)
  
  ppp.totals <- ppp.totals %>%
    mutate(ppp_pc_gdp = investment / NY.GDP.MKTP.CD * 100)
  
  figure(
    data = ppp.totals,
    plot = function(ppp.totals, style = style_atlas()) {
      ggplot(ppp.totals, aes(date, ppp_pc_gdp, color = fct_reorder2(iso3c, date, ppp_pc_gdp))) +
        geom_line(size = style$linesize) +
        scale_color_manual(values = style$colors$incomes, labels = wbgref$incomes$labels) +
        scale_x_continuous(
          breaks = bracketed_breaks(ppp.totals$date),
          expand = c(0, 0),
          limits = c(1999, NA)) +
        scale_y_continuous(sec.axis = dup_axis(
          breaks = ppp.totals %>% filter(date == max(date)) %>% pull(ppp_pc_gdp),
          labels = wbgref$incomes$labels[ppp.totals %>% filter(date == max(date)) %>% pull(iso3c)]
        )) +
        style$theme()
    },
    title = "Public private partnership investment, as a proportion of GDP, has declined in recent years.",
    subtitle = wbg_name(indicator = "Investment commitments in public private partnerships", denom =  "% of GDP", by = "by target income group"),
    note = "Note: Excludes information, communications & technology projects.",
    source = "Source: World Bank Private Participation in Infrastructure (database). https://ppi.worldbank.org",
    aspect_ratio = 2
  )
}

fig_sdg17_internet_by_income <- function(years = 1995:2016) {
  indicator <- c("IT.NET.USER.ZS")
  
  df <- wbgdata(
    wbgref$incomes$iso3c, indicator,
    years = years,
    indicator.wide = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg17_internet_by_income.csv"
  )
  
  reference_LIC_latest <- df %>% filter(iso3c == "LIC", date == max(date)) %>% pull(IT.NET.USER.ZS)
  
  comparable_years <- df %>%
    filter(IT.NET.USER.ZS <= reference_LIC_latest) %>%
    group_by(iso3c) %>%
    filter(date == max(date)) %>%
    ungroup()

  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      df <- df %>% mutate(iso3c = fct_relevel(iso3c, rev(wbgref$incomes$iso3c)))
      comparable_years <- comparable_years %>% mutate(iso3c = fct_relevel(iso3c, rev(wbgref$incomes$iso3c)))
      ggplot(df, aes(date, IT.NET.USER.ZS)) +
        geom_line(size = style$linesize, color = style$colors$spot.primary) +
        geom_hline(yintercept = reference_LIC_latest, color = style$colors$reference, size = style$linesize_reference, linetype = style$linetypes$reference) +
        geom_point(data = comparable_years, color = style$colors$spot.primary, size = style$point_size, shape = style$shapes$point) +
        geom_text(data = comparable_years %>% filter(iso3c != "LIC"), aes(label = date), hjust = 0, vjust = 1, family = style$family, size = style$gg_text_size, color = style$colors$text, nudge_x = +1, nudge_y = -3) +
        #scale_color_manual(values = style$colors$incomes) +
        scale_y_continuous(limits = c(0, 100), breaks = c(0,25,50,75,100,reference_LIC_latest), labels = ones(0)) +
        scale_x_continuous(breaks = range(df$date), expand = c(0.2,0)) +
        facet_wrap(~iso3c, nrow = 1, labeller = as_labeller(str_wrap_lines(wbgref$incomes$labels,2,force=TRUE))) +
        style$theme() +
        theme(
          strip.text.x = element_text(hjust =0.5, vjust = 0),
        )
    },
    title = "Technology enables human development. In low-income countries only 12 percent of people use the Internet, but growth in access is beginning to accelerate.",
    subtitle = wbg_name(indicator),
    source = "Source: ITU. World Development Indicators (IT.NET.USER.ZS).",
    aspect_ratio = 1
  )
}

fig_sdg17_fixed_broadband_and_telephones <- function(broadband_years = 2000:2016, telephone_years = 1990:2016) {
  broadband_indicators <- c(Fixed = "IT.NET.BBND.P2")
  telephone_indicators <- c(Fixed = "IT.MLT.MAIN.P2", Mobile = "IT.CEL.SETS.P2")
  
  df.broadband <- wbgdata(
    c("SSF", "WLD"), broadband_indicators, years = broadband_years,
    indicator.wide = FALSE, rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg17_fixed_broadband_and_telephones-broadband.csv"
  )
  df.telephone <- wbgdata(
    c("SSF", "WLD"), telephone_indicators, years = telephone_years,
    indicator.wide = FALSE, rename.indicators = TRUE,
    # Comment the next two lines to use live API data
    offline = "only",
    offline.file = "inputs/cached_api_data/fig_sdg17_fixed_broadband_and_telephones-telephone.csv"
  )
  
  df.broadband <- df.broadband %>% mutate(facet = "Fixed broadband")
  df.telephone <- df.telephone %>% mutate(facet = "Telephone")
  
  df <- rbind(df.broadband, df.telephone)
  df <- df %>% mutate(facet = fct_relevel(facet, c("Telephone", "Fixed broadband")))
  figure(
    data = df,
    plot = function(df, style = style_atlas()) {
      ggplot(df, aes(date, value, color = iso3c, linetype = indicatorID)) +
        geom_line(size = style$linesize) +
        facet_grid(. ~ facet, scales = "free_x", space = "free_x") +
        scale_x_continuous(breaks = c(range(broadband_years), range(telephone_years))) +
        scale_y_continuous(limits = c(0, 100)) +
        scale_color_manual(
          values = c(style$colors$regions, style$colors$world),
          labels = str_wrap_lines(wbgref$all_geo$labels, 2, force = TRUE),
          guide = guide_legend(reverse = TRUE)) +
        scale_linetype_discrete(guide = "none") +
        style$theme() +
        style$theme_legend("rightbottom") +
        theme(legend.key.size = unit(1, "line"))
        #theme(legend.position = c(0.01, 1), legend.justification = c(0, 1))
    },
    title = "Fixed broadband Internet uptake is still negligible in Sub-Saharan Africa, but as mobile technology improves, this may not matter.",
    subtitle = wbg_name(indicator = "Subscriptions", denom = "per 100 people"),
    source = "Source: ITU. World Development Indicators (IT.NET.BBND.P2). (a) GSMA 2017. https://www.gsma.com/mobileeconomy/sub-saharan-africa-2017/",
    aspect_ratio = 1
  )
}

# make_all("docs/sdg17/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf)
make_all <- function(path = "docs/sdg17", styler = style_atlas, saver = figure_save_draft_png) {
  # page 1
  saver(fig_sdg17_aid_panel(), styler, file.path(path, "fig_sdg17_aid_panel.png"), width = 5.5, height = 6.75)

  # page 2
  saver(fig_sdg17_regional_disbursements(), styler, file.path(path, "fig_sdg17_regional_disbursements.png"), width = 5.5, height = 4.3)
  saver(fig_sdg17_fdi_remittances_by_region(), styler, file.path(path, "fig_sdg17_fdi_remittances.png"), width = 5.5, height = 4.2)
  
  # page 3
  saver(fig_sdg17_enterprise_exports(), styler, file.path(path, "fig_sdg17_enterprise_exports.png"), width = 5.5, height = 4.25)
  saver(fig_sdg17_trade_dtf(), styler, file.path(path, "fig_sdg17_trade_dtf.png"), width = 5.5, height = 4.4)

  # page 4
  saver(fig_sdg17_PPP_investment_time(), styler, file.path(path, "fig_sdg17_PPP_investment_time.png"), width = 5.5, height = 3)
  saver(fig_sdg17_internet_by_income(), styler, file.path(path, "fig_sdg17_internet_by_income.png"), width = 5.5, height = 3)
  saver(fig_sdg17_fixed_broadband_and_telephones(), styler, file.path(path, "fig_sdg17_fixed_broadband_and_telephones.png"), width = 5.5, height = 2.6)
}
