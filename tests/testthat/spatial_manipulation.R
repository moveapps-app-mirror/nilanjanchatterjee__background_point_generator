library(terra)
library(sf)

# Function to create random polygons within and overlapping a given polygon
create_random_polygons <- function(custom_extent, n_inside = 5, n_overlapping = 5) {
  
  # Get the bounding box of the custom extent
  bbox <- ext(custom_extent)
  xmin <- bbox[1]
  xmax <- bbox[2]
  ymin <- bbox[3]
  ymax <- bbox[4]
  
  # Function to create a random polygon
  create_random_poly <- function(center_x, center_y, max_radius) {
    # Create random vertices around the center
    n_vertices <- sample(4:8, 1)  # Random number of vertices
    angles <- sort(runif(n_vertices, 0, 2 * pi))
    
    # Random radii for each vertex
    radii <- runif(n_vertices, max_radius * 0.3, max_radius)
    
    # Calculate coordinates
    x_coords <- center_x + radii * cos(angles)
    y_coords <- center_y + radii * sin(angles)
    
    # Close the polygon
    x_coords <- c(x_coords, x_coords[1])
    y_coords <- c(y_coords, y_coords[1])
    
    # Create polygon matrix
    poly_matrix <- cbind(x_coords, y_coords)
    
    # Create terra polygon
    poly <- vect(poly_matrix, "polygons")
    
    return(poly)
  }
  
  # Create polygons completely within the custom extent
  inside_polygons <- list()
  
  # Convert to sf for easier point-in-polygon testing
  custom_sf <- sf::st_as_sf(custom_extent)
  
  for(i in 1:n_inside) {
    # Try to find a valid center point inside the polygon
    attempts <- 0
    max_attempts <- 100
    
    repeat {
      attempts <- attempts + 1
      
      # Random center point within bounding box
      center_x <- runif(1, xmin, xmax)
      center_y <- runif(1, ymin, ymax)
      
      # Check if point is inside the custom extent
      test_point <- sf::st_point(c(center_x, center_y))
      test_point <- sf::st_sfc(test_point, crs = sf::st_crs(custom_sf))
      
      if(sf::st_within(test_point, custom_sf, sparse = FALSE)[1,1] || attempts >= max_attempts) {
        break
      }
    }
    
    # Calculate appropriate radius (smaller for inside polygons)
    max_radius <- min((xmax - xmin), (ymax - ymin)) * 0.1
    
    # Create the polygon
    poly <- create_random_poly(center_x, center_y, max_radius)
    crs(poly) <- crs(custom_extent)
    
    # Clip to ensure it's completely inside
    poly_clipped <- crop(poly, custom_extent)
    
    if(nrow(poly_clipped) > 0) {
      inside_polygons[[i]] <- poly_clipped
    }
  }
  
  # Create polygons that overlap (half in, half out)
  overlapping_polygons <- list()
  
  for(i in 1:n_overlapping) {
    # Random center point near the boundary
    # Get a point on or near the boundary
    boundary_buffer <- 0.1 * min((xmax - xmin), (ymax - ymin))
    
    # Expand bounding box slightly for overlap polygons
    center_x <- runif(1, xmin - boundary_buffer, xmax + boundary_buffer)
    center_y <- runif(1, ymin - boundary_buffer, ymax + boundary_buffer)
    
    # Larger radius for overlapping polygons
    max_radius <- min((xmax - xmin), (ymax - ymin)) * 0.15
    
    # Create the polygon
    poly <- create_random_poly(center_x, center_y, max_radius)
    crs(poly) <- crs(custom_extent)
    
    overlapping_polygons[[i]] <- poly
  }
  
  # Remove any NULL polygons
  inside_polygons <- inside_polygons[!sapply(inside_polygons, is.null)]
  overlapping_polygons <- overlapping_polygons[!sapply(overlapping_polygons, is.null)]
  
  # Combine all polygons
  if(length(inside_polygons) > 0) {
    inside_combined <- do.call(rbind, inside_polygons)
  } else {
    inside_combined <- NULL
  }
  
  if(length(overlapping_polygons) > 0) {
    overlapping_combined <- do.call(rbind, overlapping_polygons)
  } else {
    overlapping_combined <- NULL
  }
  
  return(list(
    inside = inside_combined,
    overlapping = overlapping_combined
  ))
}

# Usage example:
# Assuming your custom_extent is already loaded
random_polys <- create_random_polygons(custom_extent, n_inside = 5, n_overlapping = 5)

# Access the results
inside_polygons <- random_polys$inside
overlapping_polygons <- random_polys$overlapping

# Plot to visualize
plot(custom_extent, col = "lightblue", border = "blue", main = "Random Polygons")

if(!is.null(inside_polygons)) {
  plot(inside_polygons, col = "red", alpha = 0.6, add = TRUE)
}

if(!is.null(overlapping_polygons)) {
  plot(overlapping_polygons, col = "green", alpha = 0.6, add = TRUE)
}

# Add legend
legend("topright", 
       legend = c("Custom Extent", "Inside Polygons", "Overlapping Polygons"),
       fill = c("lightblue", "red", "green"),
       alpha = 0.6)

# Alternative: Create simpler circular polygons for faster generation
create_simple_random_polygons <- function(custom_extent, n_inside = 5, n_overlapping = 5) {
  
  bbox <- ext(custom_extent)
  xmin <- bbox[1]
  xmax <- bbox[2]
  ymin <- bbox[3]
  ymax <- bbox[4]
  
  # Function to create a circular polygon
  create_circle <- function(center_x, center_y, radius, n_points = 20) {
    angles <- seq(0, 2 * pi, length.out = n_points + 1)
    x_coords <- center_x + radius * cos(angles)
    y_coords <- center_y + radius * sin(angles)
    
    poly_matrix <- cbind(x_coords, y_coords)
    poly <- vect(poly_matrix, "polygons")
    
    return(poly)
  }
  
  all_polygons <- list()
  
  # Create inside polygons
  for(i in 1:n_inside) {
    center_x <- runif(1, xmin * 1.1, xmax * 0.9)
    center_y <- runif(1, ymin * 1.1, ymax * 0.9)
    radius <- runif(1, 0.05, 0.15) * min((xmax - xmin), (ymax - ymin))
    
    poly <- create_circle(center_x, center_y, radius)
    crs(poly) <- crs(custom_extent)
    all_polygons[[length(all_polygons) + 1]] <- poly
  }
  
  # Create overlapping polygons
  for(i in 1:n_overlapping) {
    # Position near boundary
    center_x <- runif(1, xmin * 0.8, xmax * 1.2)
    center_y <- runif(1, ymin * 0.8, ymax * 1.2)
    radius <- runif(1, 0.1, 0.2) * min((xmax - xmin), (ymax - ymin))
    
    poly <- create_circle(center_x, center_y, radius)
    crs(poly) <- crs(custom_extent)
    all_polygons[[length(all_polygons) + 1]] <- poly
  }
  
  # Combine all polygons
  if(length(all_polygons) > 0) {
    combined <- do.call(rbind, all_polygons)
    return(combined)
  } else {
    return(NULL)
  }
}

# Simple usage:
# simple_random_polys <- create_simple_random_polygons(custom_extent, 5, 5)

custom_extent <- terra::vect(here("tests/testthat/data/local_app_files/uploaded-app-files/custom_extent_file/my_custom_ext.gpkg"))

custom_extent2 <- terra::project(custom_extent, "epsg:4087")

terra::writeVector(custom_extent2, here("tests/testthat/data/local_app_files/uploaded-app-files/custom_extent_file/my_custom_ext.gpkg"), overwrite = T)

simple_random_polys <- create_simple_random_polygons(custom_extent2, 5, 5)
plot(simple_random_polys)

library(tidyterra)
ggplot() +
  geom_spatvector(data = custom_extent) +
  geom_spatvector(data = simple_random_polys)

terra::writeVector(simple_random_polys, filename = here("tests/testthat/data/local_app_files/uploaded-app-files/custom_mask_file/my_custom_mask.gpkg"), overwrite = T)
