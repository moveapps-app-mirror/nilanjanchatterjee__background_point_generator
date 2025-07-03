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
      expect_s4_class(result, "SpatExtent")
    }
  })
  
  test_that("get_extent works with custom_extent when file exists", {
    # Skip if no custom extent file available
    skip_if(is.null(getAuxiliaryFilePath("custom_extent_file")), 
            "No custom extent file available")
    
    expect_no_error({
      result <- get_extent(
        observed_move_data = test_data1,
        extent_type = "custom_extent",
        extent_buffer = 1000
      )
    })
    expect_s4_class(result, "SpatVector")
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
    extent_types <- c("bbox", "mcp")
    points_ratios <- c(1, 10, 30)
    
    for (extent_type in extent_types) {
      for (points_ratio in points_ratios) {
        extent <- get_extent(
          observed_move_data = test_data1,
          extent_type = extent_type,
          extent_buffer = 1000
        )
        
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
    extent <- get_extent(
      observed_move_data = test_data1,
      extent_type = "bbox",
      extent_buffer = 1000
    )
    
    result <- get_background_points_df(
      extent = extent,
      track_id_column = mt_track_id_column(test_data1),
      observed_move_data = test_data1,
      points_ratio = 10
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
      list(scale = "population", extent_type = "bbox"),
      list(scale = "population", extent_type = "mcp"),
      list(scale = "individual", extent_type = "bbox"),
      list(scale = "individual", extent_type = "mcp"),
      list(scale = "population", extent_type = "custom_extent")
    )
    
    for (combo in valid_combinations) {
      expect_no_error(
        validate_settings(
          scale = combo$scale,
          extent_type = combo$extent_type,
          custom_extent_file = NULL,
          custom_mask_file = NULL
        )
      )
    }
  })
  
  test_that("validate_settings rejects individual scale with custom extent", {
    expect_error(
      validate_settings(
        scale = "individual",
        extent_type = "custom_extent",
        custom_extent_file = NULL,
        custom_mask_file = NULL
      ),
      "Custom extents only work with a Population scale"
    )
  })
  
  test_that("validate_settings checks for custom extent file when needed", {
    # This test assumes getAuxiliaryFilePath returns NULL when no file
    # Skip if custom extent file exists
    skip_if(!is.null(getAuxiliaryFilePath("custom_extent_file")), 
            "Custom extent file exists")
    
    expect_error(
      validate_settings(
        scale = "population",
        extent_type = "custom_extent",
        custom_extent_file = NULL,
        custom_mask_file = NULL
      ),
      "You selected 'Custom Extent' but did not provide a custom extent file"
    )
  })
  
  test_that("validate_settings accepts population scale with custom extent when file exists", {
    # Skip if no custom extent file available
    skip_if(is.null(getAuxiliaryFilePath("custom_extent_file")), 
            "No custom extent file available")
    
    expect_no_error(
      validate_settings(
        scale = "population",
        extent_type = "custom_extent",
        custom_extent_file = "test.gpkg",
        custom_mask_file = NULL
      )
    )
  })
})

# Test main rFunction - existing tests remain the same
test_that("rFunction runs without error for extent_types bbox and mcp", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]
      expect_no_error(rFunction(
        data = test_data1,
        scale = scale,
        points_ratio = 10,
        extent_type = extent_type,
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = NULL
      ))
    }
  }
})

test_that("rFunction runs without error for extent_type custom_extent", {
  extent_types <- c("custom_extent")
  scales <- c("population")
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]
      
      expect_no_error(rFunction(
        data = test_data1,
        scale = scale,
        points_ratio = 10,
        extent_type = extent_type,
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = NULL
      ))
    }
  }
})

test_that("rFunction returns same output as legacy code", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")
  
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]
      
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
      
      expect_equivalent(result |>
                          dplyr::select(case, timestamp, individual_name_deployment_id),
                        expected |> dplyr::select(case, timestamp, individual_name_deployment_id))
    }
  }
})

# Additional comprehensive tests for rFunction
test_that("rFunction output structure and content validation", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")
  
  for (extent_type in extent_types) {
    for (scale in scales) {
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
      
      # Check coordinate columns exist and are numeric
      expect_true(all(c("X", "Y") %in% names(result)))
      expect_true(is.numeric(result$X))
      expect_true(is.numeric(result$Y))
      
      # Check timestamp column
      expect_true("timestamp" %in% names(result))
      expect_true(inherits(result$timestamp, "POSIXct"))
    }
  }
})

test_that("rFunction handles different buffer sizes", {
  buffer_sizes <- c(0, 500, 1000, 5000)
  
  for (buffer in buffer_sizes) {
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
  point_ratios <- c(2, 10, 100)
  
  for (points in point_ratios) {
    result <- rFunction(
      data = test_data1,
      scale = "population",
      points_ratio = point_ratios,
      extent_type = "bbox",
      custom_extent_file = NULL,
      extent_buffer = 1000,
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
    result <- rFunction(
      data = test_data1,
      scale = "individual",
      points_ratio = 1,
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