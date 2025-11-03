create_polygon_sf <- function(geom_matrix, attr_df, crs = 4326) {
  geom_df <- as.data.frame(geom_matrix)
  
  # Get unique geometry IDs
  unique_geoms <- sort(unique(geom_df$geom))
  
  # Handle case where there are more geometries than attributes
  # This can happen if geometries 1066+ exist but have no attributes
  if (length(unique_geoms) > nrow(attr_df)) {
    warning(paste("Found", length(unique_geoms), 
                  "geometries but only", nrow(attr_df), 
                  "attribute rows. Keeping only first", nrow(attr_df), "geometries."))
    # Filter to only geometries that have corresponding attributes
    geom_df <- geom_df[geom_df$geom <= nrow(attr_df), ]
    unique_geoms <- sort(unique(geom_df$geom))
  }
  
  # Create polygon for each unique geometry ID
  polygons <- lapply(unique_geoms, function(geom_id) {
    # Get all rows for this geometry
    g <- geom_df[geom_df$geom == geom_id, ]
    
    # Split by part (for multipolygons)
    parts <- split(g, g$part)
    
    # Create polygon(s)
    polys <- lapply(parts, function(p) {
      coords <- as.matrix(p[, c("x", "y")])
      
      # CLOSE THE RING: Check if first and last points are identical
      if (nrow(coords) > 0 && !identical(coords[1, ], coords[nrow(coords), ])) {
        coords <- rbind(coords, coords[1, ])
      }
      
      st_polygon(list(coords))
    })
    
    # If multiple parts, create multipolygon
    if (length(polys) > 1) {
      st_multipolygon(polys)
    } else {
      polys[[1]]
    }
  })
  
  # Create sf object - attr_df rows match geometry IDs 1:nrow(attr_df)
  sf_obj <- st_sf(
    attr_df,
    geometry = st_sfc(polygons, crs = crs)
  )
  
  return(sf_obj)
}
diagnose_spatvector <- function(spatvector_object) {
  geom_matrix <- geom(spatvector_object)
  attr_df <- as.data.frame(spatvector_object)
  
  cat("Attribute rows:", nrow(attr_df), "\n")
  cat("Unique geometry IDs:", length(unique(geom_matrix[, "geom"])), "\n")
  cat("Max geometry ID:", max(geom_matrix[, "geom"]), "\n")
  cat("Min geometry ID:", min(geom_matrix[, "geom"]), "\n")
  cat("Are IDs sequential?", identical(unique(geom_matrix[, "geom"]), 1:length(unique(geom_matrix[, "geom"]))), "\n")
  
  # Check if IDs match row numbers
  unique_ids <- sort(unique(geom_matrix[, "geom"]))
  cat("First 10 geometry IDs:", head(unique_ids, 10), "\n")
}
