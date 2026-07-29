#
#
#
#
#
#
#
#
#| echo: false
#| message: false

library(tidyverse)
library(fs)
library(knitr)

files <- dir_ls(
  "data",
  recurse = TRUE,
  type = "file"
)

datasets <- tibble(
  File = path_file(files),
  Path = files
) %>%
  mutate(
    Type = str_extract(File, "\\.[^.]+$") %>%
      str_remove("\\.") %>%
      str_to_upper()
  )

kable(
  datasets,
  col.names = c("Dataset", "Location", "Format")
)
#
#
#
#
#
#
```r
datasets <- tibble(
  File = path_file(files),
  Path = files
) %>%
  mutate(
    URL = paste0(
      "https://github.com/communitymaplab/geog4300_datascience/blob/main/",
      Path
    ),
    Link = paste0("[", File, "](", URL, ")")
  ) 
#
#
#
#
library(tidyverse)
library(fs)
library(stringr)

#--------------------------------------------------
# Find datasets
#--------------------------------------------------

data_files <- dir_ls(
  "data",
  recurse = TRUE,
  type = "file"
) |>
  tibble(path = _) |>
  mutate(
    file = path_file(path),
    ext = path_ext(file)
  )

#--------------------------------------------------
# Find scripts
#--------------------------------------------------

script_files <- dir_ls(
  ".scripts",
  recurse = TRUE,
  regexp = "\\.(R|qmd)$",
  type = "file"
)

#--------------------------------------------------
# Regular expressions for common file-reading functions
#--------------------------------------------------

patterns <- c(
  "read_csv\\(",
  "read_tsv\\(",
  "read_delim\\(",
  "read_csv2\\(",
  "read_excel\\(",
  "read_xlsx\\(",
  "st_read\\(",
  "vect\\(",
  "rast\\(",
  "readRDS\\(",
  "read_rds\\(",
  "load\\(",
  "arrow::read_parquet\\(",
  "read_parquet\\(",
  "read_feather\\(",
  "vroom\\(",
  "fread\\("
)

pattern <- paste(patterns, collapse = "|")

#--------------------------------------------------
# Extract filenames from scripts
#--------------------------------------------------

extract_files <- function(script){

  lines <- readLines(script, warn = FALSE)

  tibble(
    line = seq_along(lines),
    text = lines
  ) |>
    filter(str_detect(text, pattern)) |>
    mutate(

      filename = str_match(
        text,
        "[\"']([^\"']+\\.(csv|tsv|txt|xlsx?|gpkg|geojson|shp|rds|RDS|parquet|feather))[\"']"
      )[,2],

      script = path_file(script)
    ) |>
    filter(!is.na(filename)) |>
    select(script, line, filename)
}

usage <- map_dfr(script_files, extract_files) %>%
    mutate(filename=str_replace(filename,"https://github.com/jshannon75/geog4300/raw/refs/heads/master/",""))

#--------------------------------------------------
# Match to datasets
#--------------------------------------------------

dataset_usage <-
  usage |>
  left_join(
    data_files |> select(file, path),
    by = c("filename" = "file")
  ) |>
  arrange(filename, script)

dataset_usage
#
#
#
