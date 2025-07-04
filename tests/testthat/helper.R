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
  
  # Create subdirectories in hidden dir if they don't exist
  extent_hidden_subdir <- file.path(hidden_dir, "custom_extent_file")
  mask_hidden_subdir <- file.path(hidden_dir, "custom_mask_file")
  
  if (!dir.exists(extent_hidden_subdir)) {
    dir.create(extent_hidden_subdir, recursive = TRUE)
  }
  if (!dir.exists(mask_hidden_subdir)) {
    dir.create(mask_hidden_subdir, recursive = TRUE)
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
      # Create app subdirectory if it doesn't exist
      extent_app_subdir <- file.path(app_dir, "custom_extent_file")
      if (!dir.exists(extent_app_subdir)) {
        dir.create(extent_app_subdir, recursive = TRUE)
      }
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
      # Create app subdirectory if it doesn't exist
      mask_app_subdir <- file.path(app_dir, "custom_mask_file")
      if (!dir.exists(mask_app_subdir)) {
        dir.create(mask_app_subdir, recursive = TRUE)
      }
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
  app_dir <- here("tests/testthat/data/local_app_files/uploaded-app-files")
  hidden_dir <- here("tests/testthat/data/local_app_files/hidden-files")
  
  # Define file paths
  extent_file_app <- file.path(app_dir, "custom_extent_file/my_custom_ext.gpkg")
  extent_file_hidden <- file.path(hidden_dir, "custom_extent_file/my_custom_ext.gpkg")
  mask_file_app <- file.path(app_dir, "custom_mask_file/my_custom_mask.gpkg")
  mask_file_hidden <- file.path(hidden_dir, "custom_mask_file/my_custom_mask.gpkg")
  
  # Restore extent file
  if (file.exists(extent_file_hidden)) {
    # Create app subdirectory if it doesn't exist
    extent_app_subdir <- file.path(app_dir, "custom_extent_file")
    if (!dir.exists(extent_app_subdir)) {
      dir.create(extent_app_subdir, recursive = TRUE)
    }
    file.copy(extent_file_hidden, extent_file_app, overwrite = TRUE)
  }
  
  # Restore mask file - FIX: correct the path!
  if (file.exists(mask_file_hidden)) {
    # Create app subdirectory if it doesn't exist
    mask_app_subdir <- file.path(app_dir, "custom_mask_file")
    if (!dir.exists(mask_app_subdir)) {
      dir.create(mask_app_subdir, recursive = TRUE)
    }
    file.copy(mask_file_hidden, mask_file_app, overwrite = TRUE)
  }
}

# Safe version that checks before removing files
safe_setup_test_files <- function(show_extent = FALSE, show_mask = FALSE) {
  app_dir <- here("tests/testthat/data/local_app_files/uploaded-app-files")
  hidden_dir <- here("tests/testthat/data/local_app_files/hidden-files")
  
  # Create hidden directory if it doesn't exist
  if (!dir.exists(hidden_dir)) {
    dir.create(hidden_dir, recursive = TRUE)
  }
  
  # Create subdirectories in hidden dir if they don't exist
  extent_hidden_subdir <- file.path(hidden_dir, "custom_extent_file")
  mask_hidden_subdir <- file.path(hidden_dir, "custom_mask_file")
  
  if (!dir.exists(extent_hidden_subdir)) {
    dir.create(extent_hidden_subdir, recursive = TRUE)
  }
  if (!dir.exists(mask_hidden_subdir)) {
    dir.create(mask_hidden_subdir, recursive = TRUE)
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
      # Create app subdirectory if it doesn't exist
      extent_app_subdir <- file.path(app_dir, "custom_extent_file")
      if (!dir.exists(extent_app_subdir)) {
        dir.create(extent_app_subdir, recursive = TRUE)
      }
      success <- file.copy(extent_file_hidden, extent_file_app, overwrite = TRUE)
      if (!success) {
        warning("Failed to copy extent file to app directory")
      }
    }
  } else {
    # Move from app to hidden dir if needed
    if (file.exists(extent_file_app)) {
      # Only remove if copy was successful
      success <- file.copy(extent_file_app, extent_file_hidden, overwrite = TRUE)
      if (success) {
        file.remove(extent_file_app)
      } else {
        warning("Failed to backup extent file, not removing original")
      }
    }
  }
  
  # Handle mask file
  if (show_mask) {
    # Move from hidden to app dir if needed
    if (file.exists(mask_file_hidden) && !file.exists(mask_file_app)) {
      # Create app subdirectory if it doesn't exist
      mask_app_subdir <- file.path(app_dir, "custom_mask_file")
      if (!dir.exists(mask_app_subdir)) {
        dir.create(mask_app_subdir, recursive = TRUE)
      }
      success <- file.copy(mask_file_hidden, mask_file_app, overwrite = TRUE)
      if (!success) {
        warning("Failed to copy mask file to app directory")
      }
    }
  } else {
    # Move from app to hidden dir if needed
    if (file.exists(mask_file_app)) {
      # Only remove if copy was successful
      success <- file.copy(mask_file_app, mask_file_hidden, overwrite = TRUE)
      if (success) {
        file.remove(mask_file_app)
      } else {
        warning("Failed to backup mask file, not removing original")
      }
    }
  }
}

# Debug function to check file states
debug_test_files <- function() {
  app_dir <- here("tests/testthat/data/local_app_files/uploaded-app-files")
  hidden_dir <- here("tests/testthat/data/local_app_files/hidden-files")
  
  # Define file paths
  extent_file_app <- file.path(app_dir, "custom_extent_file/my_custom_ext.gpkg")
  extent_file_hidden <- file.path(hidden_dir, "custom_extent_file/my_custom_ext.gpkg")
  mask_file_app <- file.path(app_dir, "custom_mask_file/my_custom_mask.gpkg")
  mask_file_hidden <- file.path(hidden_dir, "custom_mask_file/my_custom_mask.gpkg")
  
  cat("File status:\n")
  cat("Extent file in app dir:", file.exists(extent_file_app), "\n")
  cat("Extent file in hidden dir:", file.exists(extent_file_hidden), "\n")
  cat("Mask file in app dir:", file.exists(mask_file_app), "\n")
  cat("Mask file in hidden dir:", file.exists(mask_file_hidden), "\n")
  
  # Check if directories exist
  cat("\nDirectory status:\n")
  cat("App dir exists:", dir.exists(app_dir), "\n")
  cat("Hidden dir exists:", dir.exists(hidden_dir), "\n")
  cat("Extent app subdir exists:", dir.exists(file.path(app_dir, "custom_extent_file")), "\n")
  cat("Mask app subdir exists:", dir.exists(file.path(app_dir, "custom_mask_file")), "\n")
  cat("Extent hidden subdir exists:", dir.exists(file.path(hidden_dir, "custom_extent_file")), "\n")
  cat("Mask hidden subdir exists:", dir.exists(file.path(hidden_dir, "custom_mask_file")), "\n")
}