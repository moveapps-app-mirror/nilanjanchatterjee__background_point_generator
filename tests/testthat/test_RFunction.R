source(here("tests/testthat/helper.R"))

test_data1 <- test_data("input3_move2loc_LatLon.rds")

test_that("function runs without error for extent_types bbox and mcp", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]

      expect_no_error(rFunction(
        data = test_data1,
        scale = scale,
        points = 10,
        extent_type = extent_type,
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = NULL
      ))
    }
  }
})


test_that("function runs without error for extent_types bbox and mcp", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]
      
      expect_no_error(rFunction(
        data = test_data1,
        scale = scale,
        points = 10,
        extent_type = extent_type,
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = NULL
      ))
    }
  }
})


test_that("function runs without error for extent_type custom_extent", {
  extent_types <- c("custom_extent")
  scales <- c("population")
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]
      
      expect_no_error(rFunction(
        data = test_data1,
        scale = scale,
        points = 10,
        extent_type = extent_type,
        custom_extent_file = NULL,
        extent_buffer = 0,
        custom_mask_file = NULL
      ))
    }
  }
})



test_that("function returns same output as legacy code", {
  extent_types <- c("bbox", "mcp")
  scales <- c("individual", "population")
  
  
  for (i in 1:length(extent_types)) {
    for (j in 1:length(scales)) {
      extent_type <- extent_types[i]
      scale <- scales[j]
      
      result <- rFunction(
        data = test_data1,
        scale = scale,
        points = 10,
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
