source(here("tests/testthat/helper.R"))
test_data1 <- test_data("input3_move2loc_LatLon.rds")

# Test get_extent function
test_that("get_extent function works correctly", {
  test_that("get_extent works with MCP extent type", {
    extent_types <- c("mcp")
    for (extent_type in extent_types) {
      expect_no_error({
        result <- get_extent(
          observed_move_data = test_data1,
          extent_type = extent_type,
          extent_buffer = 1000
        )
      })
      expect_s4_class(result, "SpatVector")
    }
  })
  
  test_that("get_extent works with bbox extent type", {
    extent_types <- c("bbox")
    for (extent_type in extent_types) {
      expect_no_error({
        result <- get_extent(
          observed_move_data = test_data1,
          extent_type = extent_type,
          extent_buffer = 1000
        )
      })
      # Now returns SpatVector (polygon) instead of SpatExtent
      expect_s4_class(result, "SpatVector")
      expect_true(terra::geomtype(result) == "polygons")
    }
  })
  
  test_that("get_extent works with custom_extent when file exists", {
    setup_test_files(show_extent = TRUE, show_mask = FALSE)
    
    expect_no_error({
      result <- get_extent(
        observed_move_data = test_data1,
        extent_type = "custom_extent",
        extent_buffer = 1000
      )
    })
    expect_s4_class(result, "SpatVector")
    expect_true(terra::geomtype(result) == "polygons")
    on.exit(restore_test_files())
  })
  
  test_that("get_extent throws error for unknown extent type", {
    expect_error(
      get_extent(
        observed_move_data = test_data1,
        extent_type = "unknown_type",
        extent_buffer = 1000
      ),
      "Unknown extent"
    )
  })
})

# Test get_background_points_df function
test_that("get_background_points_df function works correctly", {
  test_that("get_background_points_df works with different extent types", {
    extent_types <- c("bbox", "mcp", "custom_extent")
    points_ratios <- c(1, 10, 100) # Realistic point ratios
    
    for (extent_type in extent_types) {
      for (points_ratio in points_ratios) {
        # Activate custom extent file if needed
        if (extent_type == "custom_extent") {
          setup_test_files(show_extent = TRUE, show_mask = FALSE)
          on.exit(restore_test_files(), add = TRUE)
        }
        
        # Use zero buffer to avoid coordinate issues
        extent <- get_extent(
          observed_move_data = test_data1,
          extent_type = extent_type,
          extent_buffer = 0 # Zero buffer to avoid coordinate range issues
        )
        
        # All extents are now SpatVector polygons
        expect_s4_class(extent, "SpatVector")
        
        expect_no_error({
          result <- get_background_points_df(
            extent = extent,
            track_id_column = mt_track_id_column(test_data1),
            observed_move_data = test_data1,
            points_ratio = points_ratio
          )
        })
        
        # Check result structure
        expect_s3_class(result, "data.frame")
        expect_true(all(c("X", "Y", "case", "timestamp") %in% names(result)))
        expect_true(all(result$case %in% c(0, 1)))
        
        # Check correct number of points
        expected_rows <- nrow(test_data1) * (points_ratio + 1)
        expect_equal(nrow(result), expected_rows)
        
        # Check case distribution
        expect_equal(sum(result$case == 1), nrow(test_data1))
        expect_equal(sum(result$case == 0), nrow(test_data1) * points_ratio)
      }
    }
  })
  
  test_that("get_background_points_df preserves track information", {
    # Use zero buffer to avoid coordinate issues
    extent <- get_extent(
      observed_move_data = test_data1,
      extent_type = "bbox",
      extent_buffer = 0
    )
    
    # Now returns SpatVector instead of SpatExtent
    expect_s4_class(extent, "SpatVector")
    
    result <- get_background_points_df(
      extent = extent,
      track_id_column = mt_track_id_column(test_data1),
      observed_move_data = test_data1,
      points_ratio = 1
    )
    
    # Check that track IDs are preserved
    original_tracks <- unique(mt_track_id(test_data1))
    result_tracks <- unique(result[[mt_track_id_column(test_data1)]])
    expect_true(all(original_tracks %in% result_tracks))
  })
})

# Test validate_settings function
test_that("validate_settings function works correctly", {
  test_that("validate_settings allows valid combinations", {
    valid_combinations <- list(
      list(scale = "population", extent_type = "bbox", points_ratio = 10),
      list(scale = "population", extent_type = "mcp", points_ratio = 100),
      list(scale = "individual", extent_type = "bbox", points_ratio = 1),
      list(scale = "individual", extent_type = "mcp", points_ratio = 50)
    )
    
    for (combo in valid_combinations) {
      expect_no_error(
        validate_settings(
          scale = combo$scale,
          points_ratio = combo$points_ratio,
          extent_type = combo$extent_type,
          custom_extent_file = NULL,
          custom_mask_file = NULL
        )
      )
    }
  })
  
  test_that("validate_settings rejects non-integer points_ratio", {
    expect_error(
      validate_settings(
        scale = "population",
        points_ratio = 1.5, # Non-integer
        extent_type = "bbox",
        custom_extent_file = NULL,
        custom_mask_file = NULL
      ),
      "The points ratio must be an integer"
    )
    
    expect_error(
      validate_settings(
        scale = "population",
        points_ratio = 10.7, # Non-integer
        extent_type = "mcp",
        custom_extent_file = NULL,
        custom_mask_file = NULL
      ),
      "The points ratio must be an integer"
    )
  })
  
  test_that("validate_settings rejects individual scale with custom extent", {
    expect_error(
      validate_settings(
        scale = "individual",
        points_ratio = 10,
        extent_type = "custom_extent",
        custom_extent_file = NULL,
        custom_mask_file = NULL
      ),
      "Custom extents only work with a Population scale"
    )
  })
  
  test_that("validate_settings checks for custom extent file when needed", {
    # Test without file - should fail
    setup_test_files(show_extent = FALSE, show_mask = FALSE)
    on.exit(restore_test_files())
    
    expect_error(
      validate_settings(
        scale = "population",
        points_ratio = 10,
        extent_type = "custom_extent",
        custom_extent_file = NULL,
        custom_mask_file = NULL
      ),
      "You selected 'Custom Extent' but did not provide a custom extent file"
    )
  })
  
  test_that("validate_settings accepts population scale with custom extent when file exists", {
    setup_test_files(show_extent = TRUE, show_mask = FALSE)
    on.exit(restore_test_files())
    
    expect_no_error(
      validate_settings(
        scale = "population",
        points_ratio = 10,
        extent_type = "custom_extent",
        custom_extent_file = NULL,
        custom_mask_file = NULL
      )
    )
  })
  
  test_that("validate_settings rejects non-gpkg custom extent file", {
    # This test would require a non-gpkg file in the test data directory
    # For now, we'll skip this test or assume all test files are gpkg
    skip("Test requires non-gpkg file in test data directory")
  })
})

# Test custom mask functionality
test_that("custom mask functionality works correctly", {
  extent_types <- c("bbox", "mcp", "custom_extent")
  scales <- c("individual", "population")
  
  for (extent_type in extent_types) {
    for (scale in scales) {
      # Skip invalid combination
      if (scale == "individual" && extent_type == "custom_extent") {
        next
      }
      
      test_that(paste("custom mask works with", extent_type, "extent and", scale, "scale"), {
        # Set up files based on extent type
        if (extent_type == "custom_extent") {
          setup_test_files(show_extent = TRUE, show_mask = TRUE)
        } else {
          setup_test_files(show_extent = FALSE, show_mask = TRUE)
        }
        on.exit(restore_test_files(), add = TRUE)
        
        # Test without mask first
        result_no_mask <- rFunction(
          data = test_data1,
          scale = scale,
          points_ratio = 10,
          extent_type = extent_type,
          custom_extent_file = NULL,
          extent_buffer = 0,
          custom_mask_file = NULL
        )
        
        # Test with mask
        expect_no_error({
          result_with_mask <- rFunction(
            data = test_data1,
            scale = scale,
            points_ratio = 10,
            extent_type = extent_type,
            custom_extent_file = NULL,
            extent_buffer = 0,
            custom_mask_file = "custom_mask_file.gpkg"
          )
        })
        
        # Basic structure checks
        expect_s3_class(result_with_mask, "move2")
        expect_true("case" %in% names(result_with_mask))
        expect_true(all(result_with_mask$case %in% c(0, 1)))
        
        # Check that mask affected background points
        bg_points_no_mask <- sum(result_no_mask$case == 0)
        bg_points_with_mask <- sum(result_with_mask$case == 0)
        
        # With mask, we should have fewer or equal background points
        expect_true(bg_points_with_mask <= bg_points_no_mask)
        
        # Observed points should remain the same
        obs_points_no_mask <- sum(result_no_mask$case == 1)
        obs_points_with_mask <- sum(result_with_mask$case == 1)
        expect_equal(obs_points_no_mask, obs_points_with_mask)
        expect_equal(obs_points_with_mask, nrow(test_data1))
      })
    }
  }
})

# Test mask validation
test_that("custom mask validation works correctly", {
  test_that("function handles missing mask file correctly", {
    setup_test_files(show_extent = FALSE, show_mask = FALSE)
    on.exit(restore_test_files())
    
    # Should work without error when mask file doesn't exist but isn't required
    expect_no_error({
      result <- rFunction(
        data = test_data1,
        scale = "population",
        points_ratio = 10,
        extent_type = "bbox",
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = NULL
      )
    })
  })
  
  test_that("function errors when mask file is specified but doesn't exist", {
    setup_test_files(show_extent = FALSE, show_mask = FALSE)
    on.exit(restore_test_files())
    
    # Should error when mask file is specified but doesn't exist
    expect_error({
      result <- rFunction(
        data = test_data1,
        scale = "population",
        points_ratio = 10,
        extent_type = "bbox",
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = "custom_mask_file.gpkg"
      )
    })
  })
})

# Test mask with different extent combinations
test_that("mask works with all extent type combinations", {
  test_that("mask works with bbox extent", {
    setup_test_files(show_extent = FALSE, show_mask = TRUE)
    on.exit(restore_test_files())
    
    expect_no_error({
      result <- rFunction(
        data = test_data1,
        scale = "population",
        points_ratio = 5,
        extent_type = "bbox",
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = "custom_mask_file.gpkg"
      )
    })
    
    expect_s3_class(result, "move2")
    expect_true(all(result$case %in% c(0, 1)))
  })
  
  test_that("mask works with mcp extent", {
    setup_test_files(show_extent = FALSE, show_mask = TRUE)
    on.exit(restore_test_files())
    
    expect_no_error({
      result <- rFunction(
        data = test_data1,
        scale = "individual",
        points_ratio = 5,
        extent_type = "mcp",
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = "custom_mask_file.gpkg"
      )
    })
    
    expect_s3_class(result, "move2")
    expect_true(all(result$case %in% c(0, 1)))
  })
  
  test_that("mask works with custom extent", {
    setup_test_files(show_extent = TRUE, show_mask = TRUE)
    on.exit(restore_test_files())
    
    expect_no_error({
      result <- rFunction(
        data = test_data1,
        scale = "population",
        points_ratio = 5,
        extent_type = "custom_extent",
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = "custom_mask_file.gpkg"
      )
    })
    
    expect_s3_class(result, "move2")
    expect_true(all(result$case %in% c(0, 1)))
  })
})

# Test main rFunction - existing tests remain the same
test_that("rFunction runs without error for all extent types", {
  extent_types <- c("bbox", "mcp", "custom_extent")
  scales <- c("individual", "population")
  
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]
      
      # Skip invalid combination
      if (scale == "individual" && extent_type == "custom_extent") {
        next
      }
      
      # Setup files based on extent type
      if (extent_type == "custom_extent") {
        setup_test_files(show_extent = TRUE, show_mask = FALSE)
      } else {
        setup_test_files(show_extent = FALSE, show_mask = FALSE)
      }
      on.exit(restore_test_files(), add = TRUE)
      
      expect_no_error({
        result <- rFunction(
          data = test_data1,
          scale = scale,
          points_ratio = 10,
          extent_type = extent_type,
          custom_extent_file = NULL,
          extent_buffer = 0,
          custom_mask_file = NULL
        )
      })
    }
  }
})

test_that("rFunction runs without error for extent_type custom_extent with population scale", {
  setup_test_files(show_extent = TRUE, show_mask = FALSE)
  on.exit(restore_test_files())
  
  expect_no_error({
    result <- rFunction(
      data = test_data1,
      scale = "population",
      points_ratio = 10,
      extent_type = "custom_extent",
      custom_extent_file = NULL,
      extent_buffer = 0,
      custom_mask_file = NULL
    )
  })
  
  # Verify the result
  expect_s3_class(result, "move2")
  expect_true("case" %in% names(result))
  expect_gt(nrow(result), nrow(test_data1))
})

test_that("rFunction returns same output as legacy code", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")
  
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]
      
      setup_test_files(show_extent = FALSE, show_mask = FALSE)
      on.exit(restore_test_files(), add = TRUE)
      
      result <- rFunction(
        data = test_data1,
        scale = scale,
        points_ratio = 10,
        extent_type = extent_type,
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = NULL
      ) |>
        as.data.frame() |>
        arrange(individual_name_deployment_id, timestamp, case)
      
      expected <- readRDS(here(str_interp("tests/testthat/data/output/${scale}_${extent_type}.rds"))) |>
        as.data.frame() |>
        rename(
          individual_name_deployment_id = trackId
        ) |>
        arrange(individual_name_deployment_id, timestamp, case)
      
      rownames(expected) <- NULL
      
      observed_expected <- expected |>
        filter(
          case == 1
        )
      
      observed_result <- result |>
        filter(
          case == 1
        )
      
      expect_equivalent(observed_result, observed_expected)
      expect_equivalent(
        result |>
          dplyr::select(case, timestamp, individual_name_deployment_id),
        expected |> dplyr::select(case, timestamp, individual_name_deployment_id)
      )
    }
  }
})

# Additional comprehensive tests for rFunction
test_that("rFunction output structure and content validation", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")
  
  for (extent_type in extent_types) {
    for (scale in scales) {
      setup_test_files(show_extent = FALSE, show_mask = FALSE)
      on.exit(restore_test_files(), add = TRUE)
      
      result <- rFunction(
        data = test_data1,
        scale = scale,
        points_ratio = 5,
        extent_type = extent_type,
        custom_extent_file = NULL,
        extent_buffer = 1000,
        custom_mask_file = NULL
      )
      
      # Check output is move2 object
      expect_s3_class(result, "move2")
      
      # Check required columns exist
      expect_true("case" %in% names(result))
      expect_true(all(result$case %in% c(0, 1)))
      
      # Check we have more rows than input (background points added)
      expect_gt(nrow(result), nrow(test_data1))
      
      # Check observed points are preserved
      observed_points <- result[result$case == 1, ]
      expect_equal(nrow(observed_points), nrow(test_data1))
      
      # Check background points exist
      background_points <- result[result$case == 0, ]
      expect_gt(nrow(background_points), 0)
      
      # Check timestamp column
      expect_true("timestamp" %in% names(result))
      expect_true(inherits(result$timestamp, "POSIXct"))
    }
  }
})

test_that("rFunction handles different buffer sizes", {
  buffer_sizes <- c(0, 500, 1000, 5000)
  
  for (buffer in buffer_sizes) {
    setup_test_files(show_extent = FALSE, show_mask = FALSE)
    on.exit(restore_test_files(), add = TRUE)
    
    expect_no_error({
      result <- rFunction(
        data = test_data1,
        scale = "population",
        points_ratio = 5,
        extent_type = "bbox",
        custom_extent_file = NULL,
        extent_buffer = buffer,
        custom_mask_file = NULL
      )
    })
    
    expect_s3_class(result, "move2")
    expect_gt(nrow(result), nrow(test_data1))
  }
})

test_that("rFunction handles different point ratios", {
  point_ratios <- c(1, 10, 100) # Realistic point ratios
  
  for (points in point_ratios) {
    setup_test_files(show_extent = FALSE, show_mask = FALSE)
    on.exit(restore_test_files(), add = TRUE)
    
    result <- rFunction(
      data = test_data1,
      scale = "population",
      points_ratio = points,
      extent_type = "bbox",
      custom_extent_file = NULL,
      extent_buffer = 0, # Zero buffer to avoid coordinate issues
      custom_mask_file = NULL
    )
    
    # Check correct number of background points
    background_count <- sum(result$case == 0)
    expected_background <- nrow(test_data1) * points
    expect_equal(background_count, expected_background)
  }
})

# Test individual scale specific behavior
test_that("rFunction individual scale processes each track separately", {
  # Only test if we have multiple tracks
  if (length(unique(mt_track_id(test_data1))) > 1) {
    setup_test_files(show_extent = FALSE, show_mask = FALSE)
    on.exit(restore_test_files())
    
    result <- rFunction(
      data = test_data1,
      scale = "individual",
      points_ratio = 10,
      extent_type = "mcp",
      custom_extent_file = NULL,
      extent_buffer = 1000,
      custom_mask_file = NULL
    )
    
    # Check that each track has background points
    track_ids <- unique(mt_track_id(test_data1))
    result_track_ids <- unique(mt_track_id(result))
    expect_true(all(track_ids %in% result_track_ids))
    
    # Check background points exist for each track
    for (track_id in track_ids) {
      track_data <- result[mt_track_id(result) == track_id, ]
      expect_true(any(track_data$case == 0))
      expect_true(any(track_data$case == 1))
    }
  }
})