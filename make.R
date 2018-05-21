library(wbgcharts)
library(plyr)
source("styles.R")

# Remake all: make_all(path_suffix = "/pdf", styler = style_atlas_cmyk, saver = figure_save_final_pdf); make_all()
make_all <- function(path = "docs", path_suffix = "", styler = style_atlas, saver = figure_save_draft_png, goals = 1:17) {
  for (goal in goals) {
    e <- new.env()
    source(paste0("sdg", goal, ".R"), local = e)
    
    sdg_path <- paste0(path, "/sdg", goal, path_suffix)
    if (!dir.exists(sdg_path)) {
      dir.create(sdg_path)
    }
    e$make_all(path = sdg_path, styler = styler, save = saver)
  }
}

check_all <- function(original_path = "docs_pre", new_path = "docs", diff_path = "docs_diff", goals = 1:17, on_fail = stop) {
  for (goal in goals) {
    orig_sdg_path <- paste0(original_path, "/sdg", goal)
    new_sdg_path <- paste0(new_path, "/sdg", goal)
    
    for (file in dir(orig_sdg_path, "*.png")) {
      orig <- paste0(orig_sdg_path, "/", file)
      new <- paste0(new_sdg_path, "/", file)
      diff <- paste0(diff_path, "/", file)
      retval <- suppressWarnings(system2("magick", c("compare", "-metric", "PSNR", orig, new, diff), stderr = TRUE))
      if (substr(retval, 1, 3) != "inf") {
          on_fail(paste0(retval, " @ ", new_sdg_path, "/", file))
      }
    }
  }
}

extract_github_references <- function(
  baseurl = "https://github.com/worldbank/sdgatlas2018/blob/master/",
  baseurl_doc="https://worldbank.github.io/sdgatlas2018/",
  goals = 1:17
) {
  all_refs <- ldply(goals, function(goal) {
    # Source the file into a clean env
    e <- new.env()
    filename <- paste0("sdg", goal, ".R")
    source(filename, local = e)
    
    # Get the functions defined, filter to figures
    fns <- names(e)
    fns <- fns[grepl("^fig_", fns)]
    
    ldply(fns, function(fn) {
      placeholder <- paste0(fn, "_URL")
      lineref <- c(attributes(e[[fn]])$srcref)[c(1,3)]
      url <- paste0(baseurl, filename, "#L", lineref[1], "-L", lineref[2])
      imageurl <- paste0(baseurl_doc, "sdg", goal, "/", fn, ".png")
      data.frame(placeholder = placeholder, url = url, imageurl = imageurl)
    })
  })
  all_refs
}

save_github_references <- function() {
  baseurl <- "https://github.com/worldbank/sdgatlas2018/blob/master/"
  df <- extract_github_references(baseurl = baseurl)
  write.csv(df, "figure_function_urls.csv", row.names = TRUE)
}

