library(move2)
library(sf)
library(ggplot2)
library(dplyr)
library(data.table)
library(terra)
library(tools)

# Extent Types
BOUNDING_BOX <- "bbox"
MCP <- "mcp"
CUSTOM_EXTENT <- "custom_extent"

# Scale Options
POPULATION <- "population"
INDIVIDUAL <- "individual"


get_extent <- function(observed_move_data, extent_type, extent_buffer) {
  
  # convert all extents to a polygon
  if (extent_type == MCP) {
    extent <- terra::convHull(terra::vect(observed_move_data))
  } else if (extent_type == BOUNDING_BOX) {
    extent <- terra::as.polygons(
      terra::ext(observed_move_data), crs = crs(observed_move_data))
  } else if (extent_type == CUSTOM_EXTENT) {
    extent <- terra::vect(getAuxiliaryFilePath("custom_extent_file"))
  } else {
    stop("Unknown extent")
  }

  custom_mask_file_path <- getAuxiliaryFilePath("custom_mask_file")
  
  if (!is.null(custom_mask_file_path)) {
    custom_mask <- terra::vect(custom_mask_file_path)
    extent <- terra::crop(extent, custom_mask)
  }
  
  buffered_extent <- terra::buffer(extent, extent_buffer)
  
  return(buffered_extent)
}


get_background_points_df <- function(extent, track_id_column,
                                     observed_move_data, points_ratio) {
  
  # generate background points
  spat_sample_args <- list(
    x = extent,
    size = nrow(observed_move_data) * points_ratio,
    method = "random"
  )

  background_points_df <- do.call(terra::spatSample, args = spat_sample_args) |>
    geom() |>
    as.data.frame() |>
    dplyr::select(x, y) |>
    rename(
      X = x,
      Y = y
    ) |>
    mutate(
      case = 0
    )

  # convert observed data to a data frame to be merged with background points
  observed_data_df <- observed_move_data |>
    st_coordinates() |>
    as.data.frame() |>
    mutate(
      case = 1
    )

  # combine background points with observed points and add additional columns
  all_points <- rbindlist(list(
    observed_data_df,
    background_points_df
  )) |>
    mutate(
      timestamp = seq(
        from = min(observed_move_data$timestamp, na.rm = T),
        to = max(observed_move_data$timestamp, na.rm = T),
        length.out = n()
      ),
      !!track_id_column := rep(mt_track_id(observed_move_data), points_ratio + 1)
    )


  return(all_points)
}


validate_settings <- function(scale, extent_type, custom_extent_file, custom_mask_file) {
  if (scale == INDIVIDUAL & extent_type == CUSTOM_EXTENT) {
    stop("Custom extents only work with a Population scale, please update the scale to use custom extent.")
  }

  custom_extent_file_path <- getAuxiliaryFilePath("custom_extent_file")

  if (extent_type == CUSTOM_EXTENT) {
    if (is.null(custom_extent_file_path)) {
      stop("You selected 'Custom Extent' but did not provide a custom extent file. Either upload a gpkg file or select a different extent type.")
    }

    if (file_ext(custom_extent_file_path) != "gpkg") {
      stop("Custom Extent File must be a gpkg file containing one or more polygons.")
    }
  }

  custom_mask_file_path <- getAuxiliaryFilePath("custom_mask_file")

  if (!is.null(custom_mask_file_path)) {
    if (file_ext(custom_mask_file_path) != "gpkg") {
      stop("Custom Mask File must be a gpkg file containing one or more polygons.")
    }
  }
}


rFunction <- function(data, scale, points_ratio, extent_type,
                      custom_extent_file, extent_buffer, custom_mask_file) {
  
  validate_settings(scale, extent_type, extent_buffer)
  original_track_id_column <- mt_track_id_column(data)
  track_attribute_data <- mt_track_data(data)

  if (scale == "population") {
    extent <- get_extent(
      observed_move_data = data,
      extent_type = extent_type,
      extent_buffer = extent_buffer
    )
    
    final_data_df <- get_background_points_df(
      extent = extent,
      track_id_column = original_track_id_column,
      observed_move_data = data,
      points_ratio = points_ratio
    )
  } else {
    filtered_data <- data |>
      filter_track_data(nrow(data) > 5)


    track_list <- list()

    track_ids <- unique(mt_track_id(filtered_data))

    for (i in 1:length(track_ids)) {
      current_track_id <- track_ids[i]
      individual_data <- filtered_data |>
        filter_track_data(.track_id = current_track_id)

      extent <- get_extent(
        observed_move_data = individual_data,
        extent_type = extent_type,
        extent_buffer = extent_buffer
      )

      background_point_df <- get_background_points_df(
        extent = extent,
        track_id_column = original_track_id_column,
        observed_move_data = individual_data,
        points_ratio = points_ratio
      )

      track_list[[i]] <- background_point_df
    }

    final_data_df <- rbindlist(track_list)
  }


  # add track attributes back in
  final_merged_df <- left_join(
    final_data_df,
    track_attribute_data,
    join_by(!!original_track_id_column == !!original_track_id_column)
  ) |>
    dplyr::arrange(!!original_track_id_column, timestamp, case)


  suppressWarnings({
    plot <- ggplot(final_merged_df |> arrange(case), aes(x = X, y = Y)) +
      geom_point(aes(col = as.factor(case), alpha = as.factor(case))) +
      labs(x = "Longitude", y = "Latitude", col = "Point Type") +
      scale_color_discrete(labels = c("Background", "Observed")) +
      scale_alpha_discrete(guide = "none") +
      theme_bw()

    if (scale == INDIVIDUAL) {
      plot <- plot +
        facet_wrap(~ get(original_track_id_column))
    }

    ggsave(plot,
      file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"), "observed_background_points.png"),
      width = 10, height = 8, dpi = 200, units = "in"
    )
  })


  # Convert the final data frame back into a move2 stack
  final_move_data <- mt_as_move2(
    final_merged_df,
    coords = c("X", "Y"),
    time_column = "timestamp", crs = 4326,
    track_id_column = original_track_id_column,
    track_attributes = names(track_attribute_data)
  )

  return(final_move_data)
}
