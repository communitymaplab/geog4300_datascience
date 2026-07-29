#!/usr/bin/env Rscript
# rmd_to_qmd.R
# Converts R Markdown (.Rmd) files to Quarto (.qmd) format
#
# Usage (interactive):  source("rmd_to_qmd.R")
# Usage (CLI):          Rscript rmd_to_qmd.R input.Rmd [output.qmd]
#                       Rscript rmd_to_qmd.R *.Rmd          # batch via shell glob
#                       Rscript rmd_to_qmd.R --dir ./docs   # convert entire directory

suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))


# ── YAML conversion ──────────────────────────────────────────────────────────

convert_yaml <- function(yaml_text) {

  # output: html_document  →  format: html
  # output: pdf_document   →  format: pdf
  # output: word_document  →  format: docx
  # output: flexdashboard::flex_dashboard  →  format: dashboard
  output_map <- c(
    "html_document"                        = "html",
    "html_document2"                       = "html",
    "pdf_document"                         = "pdf",
    "word_document"                        = "docx",
    "github_document"                      = "gfm",
    "md_document"                          = "commonmark",
    "beamer_presentation"                  = "beamer",
    "ioslides_presentation"                = "revealjs",
    "slidy_presentation"                   = "revealjs",
    "powerpoint_presentation"              = "pptx",
    "flexdashboard::flex_dashboard"        = "dashboard",
    "bookdown::html_document2"             = "html",
    "bookdown::pdf_document2"              = "pdf",
    "bookdown::word_document2"             = "docx",
    "rmarkdown::html_vignette"             = "html",
    "distill::distill_article"             = "html"
  )

  # Replace output: <format> with format: <qmd_format>
  for (rmd_fmt in names(output_map)) {
    qmd_fmt   <- output_map[[rmd_fmt]]
    # handles both bare and indented YAML (e.g. "output: html_document" or
    # "output:\n  html_document:\n    ...")
    yaml_text <- str_replace(
      yaml_text,
      regex(paste0("(^|\\n)(output:\\s*)", re_escape(rmd_fmt), "(\\s*:)?"),
            ignore_case = FALSE),
      paste0("\\1format: ", qmd_fmt)
    )
  }

  # Rename common sub-keys that differ between Rmd and Quarto
  rename_map <- c(
    "fig_width"    = "fig-width",
    "fig_height"   = "fig-height",
    "fig_caption"  = "fig-cap-location",
    "toc_depth"    = "toc-depth",
    "toc_float"    = "toc-float",
    "number_sections" = "number-sections",
    "code_folding" = "code-fold",
    "df_print"     = "df-print",
    "highlight"    = "highlight-style",
    "keep_md"      = "keep-md",
    "self_contained" = "embed-resources"
  )

  for (old_key in names(rename_map)) {
    new_key   <- rename_map[[old_key]]
    yaml_text <- str_replace_all(
      yaml_text,
      regex(paste0("(^|\\n)(\\s*)", re_escape(old_key), ":"), multiline = TRUE),
      paste0("\\1\\2", new_key, ":")
    )
  }

  yaml_text
}

# ── Chunk option conversion ──────────────────────────────────────────────────

convert_chunk_options <- function(chunk_header) {
  # Convert ```{r label, opt=val} style to Quarto hash-pipe comments
  # Returns a vector: c(new_fence_line, hash_pipe_lines_or_NULL)

  # Extract language and everything after
  m <- str_match(chunk_header, "^```\\{([a-zA-Z0-9_]+)(.*)\\}\\s*$")
  if (is.na(m[1])) return(list(fence = chunk_header, opts = NULL))

  lang      <- m[2]
  opts_raw  <- str_trim(m[3])

  if (nchar(opts_raw) == 0) {
    return(list(fence = paste0("```{", lang, "}"), opts = NULL))
  }

  # Split on commas, respecting quoted strings
  parts <- split_chunk_opts(opts_raw)

  label_line <- NULL
  opt_lines  <- character(0)

  for (i in seq_along(parts)) {
    part <- str_trim(parts[[i]])
    if (nchar(part) == 0) next

    if (!str_detect(part, "=")) {
      # First bare token is the chunk label
      if (i == 1 && !str_detect(part, "=")) {
        label_line <- paste0("#| label: ", part)
      }
      next
    }

    kv    <- str_split_fixed(part, "=", 2)
    key   <- str_trim(kv[1])
    value <- str_trim(kv[2])

    # Key rename map (knitr → Quarto)
    key <- recode_chunk_key(key)

    # Value transforms
    value <- recode_chunk_value(key, value)

    opt_lines <- c(opt_lines, paste0("#| ", key, ": ", value))
  }

  hash_lines <- c(label_line, opt_lines)
  list(fence = paste0("```{", lang, "}"),
       opts  = if (length(hash_lines) > 0) hash_lines else NULL)
}

recode_chunk_key <- function(key) {
  map <- c(
    "echo"           = "echo",
    "eval"           = "eval",
    "include"        = "include",
    "warning"        = "warning",
    "message"        = "message",
    "error"          = "error",
    "fig.width"      = "fig-width",
    "fig.height"     = "fig-height",
    "fig.cap"        = "fig-cap",
    "fig.align"      = "fig-align",
    "fig.show"       = "fig-show",
    "out.width"      = "out-width",
    "out.height"     = "out-height",
    "cache"          = "cache",
    "cache.path"     = "cache-path",
    "results"        = "output",
    "collapse"       = "collapse",
    "tidy"           = "tidy",
    "comment"        = "comment",
    "class.output"   = "class-output",
    "class.source"   = "class-source"
  )
  if (key %in% names(map)) map[[key]] else key
}

recode_chunk_value <- function(key, value) {
  # results='asis'  →  output: asis
  if (key == "output" && value %in% c("'asis'", '"asis"', "asis")) {
    return("asis")
  }
  # Strip unnecessary outer quotes from TRUE/FALSE/numeric
  if (value %in% c("TRUE", "FALSE", "true", "false")) return(tolower(value))
  value
}

split_chunk_opts <- function(s) {
  # Naive CSV-aware split: split on commas outside of quotes
  parts  <- character(0)
  buf    <- ""
  in_q   <- FALSE
  qchar  <- ""
  for (ch in strsplit(s, "")[[1]]) {
    if (!in_q && ch %in% c('"', "'")) { in_q <- TRUE; qchar <- ch; buf <- paste0(buf, ch) }
    else if (in_q && ch == qchar)     { in_q <- FALSE; qchar <- ""; buf <- paste0(buf, ch) }
    else if (!in_q && ch == ",")      { parts <- c(parts, buf); buf <- "" }
    else                              { buf <- paste0(buf, ch) }
  }
  if (nchar(buf) > 0) parts <- c(parts, buf)
  parts
}

# ── Body conversion ──────────────────────────────────────────────────────────

convert_body <- function(body_lines) {
  out        <- character(0)
  in_chunk   <- FALSE

  for (line in body_lines) {

    # Detect opening chunk fence
    if (!in_chunk && str_detect(line, "^```\\{[a-zA-Z]")) {
      in_chunk <- TRUE
      result   <- convert_chunk_options(line)
      out      <- c(out, result$fence)
      if (!is.null(result$opts)) out <- c(out, result$opts)
      next
    }

    # Detect closing chunk fence
    if (in_chunk && str_detect(line, "^```\\s*$")) {
      in_chunk <- FALSE
      out      <- c(out, line)
      next
    }

    # Outside chunks: convert inline R Markdown syntax
    if (!in_chunk) {
      # ::: {.class} callout divs — already Quarto-compatible, leave alone
      # Convert Rmd tab syntax {.tabset} → Quarto doesn't need it (tabs via ##)
      line <- str_replace(line, "\\{[.]tabset[^}]*\\}", "")

      # \@ref(fig:label) → @fig-label
      line <- str_replace_all(line, "\\\\@ref\\(fig:([^)]+)\\)", "@fig-\\1")
      # \@ref(tab:label) → @tbl-label
      line <- str_replace_all(line, "\\\\@ref\\(tab:([^)]+)\\)", "@tbl-\\1")
      # \@ref(eq:label)  → @eq-label
      line <- str_replace_all(line, "\\\\@ref\\(eq:([^)]+)\\)", "@eq-\\1")
      # \@ref(sec:label) → @sec-label
      line <- str_replace_all(line, "\\\\@ref\\(([^)]+)\\)", "@\\1")

      # bookdown-style figure captions: (ref:label) pattern — warn only
      if (str_detect(line, "^\\(ref:")) {
        line <- paste0(line, "  <!-- TODO: convert (ref:) caption to fig-cap chunk option -->")
      }
    }

    out <- c(out, line)
  }
  out
}

# ── Full file conversion ─────────────────────────────────────────────────────

convert_rmd <- function(input_path, output_path = NULL) {

  if (!file.exists(input_path))
    stop("File not found: ", input_path)

  #input_path<-"D:/Dropbox/Jschool/Teaching/Courses/Geog4300_Fa25/geog4300_github/scripts/geog4300_f25_S06-1 Point pattern analysis in R.Rmd"
  dir<-dirname(input_path)
  
  file_list<-data.frame(file_list=list.files(dir,pattern = "^geog\\d+.*\\.Rmd$",
                        ignore.case = TRUE)) %>%
    mutate(file_list1=str_replace(file_list,"_f24",""),
          file_list1=str_replace(file_list1,"_f25","")) %>%
    arrange(file_list1) %>%
    mutate(num_new=str_pad(row_number(.)+2,width=2,pad="0")) %>%
    mutate(file_new = str_replace(file_list1, "S\\d+-\\d+", paste0("S", num_new))) %>%
    mutate(file_new = str_replace(file_new,".Rmd",".qmd"))
  
  if (is.null(output_path)) {
    fname <- basename(input_path)
    dir   <- dirname(input_path)
    
    # Get all matching files sorted by their collapsed S##-# number
    output_sel<-file_list %>%
      filter(file_list==fname)
    
    output_path <- file.path(dir, output_sel$file_new)
  }
  
  raw   <- readLines(input_path, warn = FALSE, encoding = "UTF-8")

  # ── Locate YAML front matter ──
  yaml_end <- NA_integer_
  if (length(raw) > 1 && raw[1] == "---") {
    close_idx <- which(raw[-1] %in% c("---", "...")) + 1  # offset for removed first line
    if (length(close_idx) > 0) yaml_end <- close_idx[1]
  }

  if (!is.na(yaml_end)) {
    yaml_lines <- raw[seq(1, yaml_end)]
    body_lines <- if (yaml_end < length(raw)) raw[seq(yaml_end + 1, length(raw))] else character(0)
    yaml_text  <- paste(yaml_lines, collapse = "\n")
    yaml_text  <- convert_yaml(yaml_text)
    yaml_out   <- strsplit(yaml_text, "\n")[[1]]
  } else {
    yaml_out   <- character(0)
    body_lines <- raw
  }

  body_out <- convert_body(body_lines)
  final    <- c(yaml_out, body_out)

  writeLines(final, output_path, useBytes = FALSE)
  message("✔ Converted: ", input_path, " → ", output_path)
  invisible(output_path)
}

all_rmd<-(list.files(dir,pattern = "^geog\\d+.*\\.Rmd$",
                     ignore.case = TRUE,full.names = T))

purrr::map(all_rmd,convert_rmd)

# ── Batch conversion ─────────────────────────────────────────────────────────



convert_dir <- function(dir_path, recursive = FALSE, overwrite = FALSE) {
  files <- list.files(
    dir_path,
    pattern     = "\\.Rmd$",
    ignore.case = TRUE,
    full.names  = TRUE,
    recursive   = recursive
  )
  if (length(files) == 0) {
    message("No .Rmd files found in: ", dir_path)
    return(invisible(character(0)))
  }
  results <- vapply(files, function(f) {
    out <- str_replace(f, regex("\\.Rmd$", ignore_case = TRUE), ".qmd")
    if (!overwrite && file.exists(out)) {
      message("⚠ Skipping (output exists): ", out)
      return(NA_character_)
    }
    convert_rmd(f, out)
  }, character(1))
  invisible(results)
}



# ── Helpers ───────────────────────────────────────────────────────────────────

re_escape <- function(x) str_replace_all(x, "([\\.\\^\\$\\*\\+\\?\\(\\)\\[\\]\\{\\}\\|\\\\])", "\\\\\\1")

# ── CLI entry point ───────────────────────────────────────────────────────────

if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0) {
    cat("Usage:\n")
    cat("  Rscript rmd_to_qmd.R input.Rmd [output.qmd]\n")
    cat("  Rscript rmd_to_qmd.R file1.Rmd file2.Rmd ...\n")
    cat("  Rscript rmd_to_qmd.R --dir ./path/to/dir [--recursive] [--overwrite]\n")
    quit(status = 0)
  }

  if (args[1] == "--dir") {
    dir_path  <- if (length(args) >= 2) args[2] else "."
    recursive <- "--recursive" %in% args
    overwrite <- "--overwrite" %in% args
    convert_dir(dir_path, recursive = recursive, overwrite = overwrite)
  } else if (length(args) == 2 && str_detect(args[2], "\\.qmd$")) {
    # Single file with explicit output path
    convert_rmd(args[1], args[2])
  } else {
    # One or more .Rmd files
    for (f in args) {
      tryCatch(convert_rmd(f), error = function(e) message("✗ Error on ", f, ": ", e$message))
    }
  }
}
