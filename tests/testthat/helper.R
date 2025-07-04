library(stringr)
library(here)

test_data <- function(test_file) {
    test_data_root_dir <- test_path("data")
    readRDS(file = file.path(test_data_root_dir, test_file))
}


# Function to control both files independently by moving them
setup_test_files <- function(show_extent = FALSE, show_mask = FALSE) {
  app_dir <- here("tests/testthat/data/local_app_files/uploaded-app-files")
  hidden_dir <- here("tests/testthat/data/local_app_files/hidden-files")
  
  # Create hidden directory if it doesn't exist
  if (!dir.exists(hidden_dir)) {
    dir.create(hidden_dir, recursive = TRUE)
  }
  
  # Define file paths
  extent_file_app <- file.path(app_dir, "custom_extent_file/my_custom_ext.gpkg")
  extent_file_hidden <- file.path(hidden_dir, "custom_extent_file/my_custom_ext.gpkg")
  mask_file_app <- file.path(app_dir, "custom_mask_file/my_custom_mask.gpkg")
  mask_file_hidden <- file.path(hidden_dir, "custom_mask_file/my_custom_mask.gpkg")
  
  # Handle extent file
  if (show_extent) {
    # Move from hidden to app dir if needed
    if (file.exists(extent_file_hidden) && !file.exists(extent_file_app)) {
      file.copy(extent_file_hidden, extent_file_app, overwrite = TRUE)
    }
  } else {
    # Move from app to hidden dir if needed
    if (file.exists(extent_file_app)) {
      file.copy(extent_file_app, extent_file_hidden, overwrite = TRUE)
      file.remove(extent_file_app)
    }
  }
  
  # Handle mask file
  if (show_mask) {
    # Move from hidden to app dir if needed
    if (file.exists(mask_file_hidden) && !file.exists(mask_file_app)) {
      file.copy(mask_file_hidden, mask_file_app, overwrite = TRUE)
    }
  } else {
    # Move from app to hidden dir if needed
    if (file.exists(mask_file_app)) {
      file.copy(mask_file_app, mask_file_hidden, overwrite = TRUE)
      file.remove(mask_file_app)
    }
  }
}

# Cleanup function to restore all files to their original locations
restore_test_files <- function() {
  app_dir <- here("tests/testthat/data/local_app_files/uploaded-app-files/")
  hidden_dir <- here("tests/testthat/data/local_app_files/hidden-files/")
  
  # Restore extent file
  extent_file_app <- file.path(app_dir, "custom_extent_file/my_custom_ext.gpkg")
  extent_file_hidden <- file.path(hidden_dir, "custom_extent_file/my_custom_ext.gpkg")
  
  if (file.exists(extent_file_hidden)) {
    file.copy(extent_file_hidden, extent_file_app, overwrite = TRUE)
  }
  
  # Restore mask file  
  mask_file_app <- file.path(app_dir, "custom_extent_file/my_custom_mask.gpkg")
  mask_file_hidden <- file.path(hidden_dir, "custom_extent_file/my_custom_mask.gpkg")
  
  if (file.exists(mask_file_hidden)) {
    file.copy(mask_file_hidden, mask_file_app, overwrite = TRUE)
  }
}