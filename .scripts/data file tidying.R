# Load required packages
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("fs")) install.packages("fs")

library(tidyverse)
library(fs)

analyze_project_files <- function(data_folder, script_folder) {
  
  # ==========================================
  # 1. SCAN DATA FOLDER & DETECT EXTENSIONS
  # ==========================================
  all_data_files <- dir_ls(data_folder, recurse = TRUE, type = "file")
  
  # Extract unique extensions
  detected_exts <- all_data_files %>%
    path_ext() %>%
    str_to_lower() %>%
    unique() %>%
    keep(~ .x != "")
  
  # SYSTEM CHECK: Filter out spatial/metadata sidecar files that aren't explicitly read in code
  sidecars <- c("prj", "shx", "dbf", "cpg", "tfw", "hdr", "stx", "xml", "sbx", "sbn")
  detected_exts <- setdiff(detected_exts, sidecars)
  
  cat("Found", length(all_data_files), "total files in the data folder.\n")
  cat("Scanning scripts for these data extensions:", paste(detected_exts, collapse = ", "), "\n\n")
  
  if (length(detected_exts) == 0) {
    stop("No primary data file extensions detected in the data folder.")
  }
  
  # ==========================================
  # 2. SCAN SCRIPTS FOLDER FOR LOADED FILES
  # ==========================================
  all_files_in_hidden <- dir_ls(script_folder, recurse = TRUE, type = "file", all = TRUE)
  
  script_files <- all_files_in_hidden[str_detect(
    str_to_lower(all_files_in_hidden), 
    "\\.(r|rmd|qmd)$"
  )]
  
  cat("Found", length(script_files), "script files (.R, .Rmd, .qmd) in the script folder.\n\n")
  
  # Build regex pattern dynamically
  ext_pattern <- paste0(detected_exts, collapse = "|")
  regex_pattern <- paste0("['\"]([^'\"]+\\.(?:", ext_pattern, "))['\"]")
  
  # Map through scripts safely
  loaded_files_table <- map_df(script_files, function(file) {
    # If file is unreadable or empty, return NULL
    lines <- tryCatch(read_lines(file, lazy = FALSE), error = function(e) return(NULL))
    if (is.null(lines) || length(lines) == 0) return(NULL)
    
    matching_lines <- lines[str_detect(lines, regex_pattern)]
    if (length(matching_lines) == 0) return(NULL)
    
    extracted_files <- str_extract_all(matching_lines, regex_pattern) %>% 
      map(~ str_replace_all(.x, "['\"]", ""))
    
    tibble(
      script_name = path_file(file),
      file_loaded = extracted_files,
      line_content = matching_lines,
      script_path = as.character(file)
    ) %>% 
      unnest(file_loaded)
  })
  
  # Guard rail: If the table is completely empty, initialize it properly
  if (nrow(loaded_files_table) == 0) {
    loaded_files_table <- tibble(
      script_name = character(), 
      file_loaded = character(), 
      file_ext = character(), 
      line_content = character(), 
      script_path = character()
    )
  } else {
    loaded_files_table <- loaded_files_table %>%
      mutate(file_ext = str_to_lower(path_ext(file_loaded))) %>%
      select(script_name, file_loaded, file_ext, line_content, script_path)
  }
  
  # ==========================================
  # 3. IDENTIFY UNUSED DATASETS
  # ==========================================
  data_inventory <- tibble(
    full_path = as.character(all_data_files),
    file_name = path_file(all_data_files),
    file_ext  = str_to_lower(path_ext(all_data_files))
  )
  
  # Safely extract basenames (handling 0-row cases)
  if (nrow(loaded_files_table) > 0) {
    loaded_basenames <- unique(path_file(loaded_files_table$file_loaded))
    
    # Also ignore sidecars when calculating what's "unused" 
    # (e.g., if shape.shp is used, don't flag shape.dbf as an orphan)
    loaded_base_names_sans_ext <- path_ext_remove(loaded_basenames)
    
    unused_files_table <- data_inventory %>% 
      filter(
        !file_name %in% loaded_basenames,
        !path_ext_remove(file_name) %in% loaded_base_names_sans_ext
      ) %>% 
      select(file_name, file_ext, full_path)
  } else {
    # If 0 files were matched, then everything in inventory is technically unused
    unused_files_table <- data_inventory %>% 
      select(file_name, file_ext, full_path)
    cat("Warning: No matching data files were found inside your code strings.\n")
  }
  
  return(list(
    loaded_files = loaded_files_table,
    unused_files = unused_files_table
  ))
}

# --- RUN THE ANALYSIS ---
path_to_data    <- "data"
path_to_scripts <- ".scripts"

results <- analyze_project_files(path_to_data, path_to_scripts) 

results$unused_files<-results$unused_files %>%
  filter(!str_detect(full_path,"PRISM")) %>%
  filter(!str_detect(full_path,"era5")) %>%
  filter(!str_detect(full_path,"pulse"))

# View results safely
View(results$loaded_files)
View(results$unused_files)

#Delete unused files
# WARNING: This permanently deletes the files from your computer.
files_to_delete <- results$unused_files$full_path

if (length(files_to_delete) > 0) {
  # Prompt confirmation in the console so you don't accidentally run it
  response <- readline(prompt = paste("Are you sure you want to permanently delete", length(files_to_delete), "files? (y/n): "))
  
  if (tolower(response) == "y") {
    file_delete(files_to_delete)
    cat("Permanently deleted", length(files_to_delete), "files from the data folder.\n")
  } else {
    cat("Deletion canceled.\n")
  }
} else {
  cat("No unused files found to delete.\n")
}
y
