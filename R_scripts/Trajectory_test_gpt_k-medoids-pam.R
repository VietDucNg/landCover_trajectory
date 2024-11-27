# load necessary packages
library(terra)
library(cluster)
library(dplyr)

# Set your working directory to the folder containing the raster files
# You can set this manually in RStudio or use setwd() function
setwd("C:/Users/dewaardf/oneDrive/Dokumente/1 - Promotion/4 - Eigene Paper/2 BSRLC Norddeutschland - Degradation Trajectories Archetypes/R_tests")

# Load raster files
file_paths <- list.files(pattern = "Test_Alluvial_\\d{4}\\.tiff$", full.names = TRUE)
rasters <- rast(file_paths)

# Extract values from raster stack as a matrix
pixel_values <- as.matrix(rasters)

# Transpose the matrix to have time series for each pixel
pixel_series <- t(pixel_values)

# Create a data frame for further analysis
pixel_df <- as.data.frame(pixel_series)
pixel_df <- cbind(pixel_id = 1:nrow(pixel_df), pixel_df)

# Find unique trajectories
unique_trajectories <- unique(pixel_df[,-1])

# Check the number of unique trajectories
n_unique <- nrow(unique_trajectories)

if (n_unique < 2) {
  cat("Not enough unique trajectories for clustering. At least 2 unique trajectories are required.\n")
} else {
  # Use a clustering algorithm, for example, k-medoids (PAM)
  # Determine the optimal number of clusters (k) using silhouette method
  max_clusters <- 40  # Adjust this value as needed
  sil_width <- numeric(max_clusters)
  for (k in 2:min(max_clusters, n_unique - 1)) {
    pam_fit <- pam(unique_trajectories, k = k)
    sil_width[k] <- pam_fit$silinfo$avg.width
    cat("Silhouette width for", k, "clusters:", sil_width[k], "\n")
  }
  
  # Plot silhouette width to find the optimal k
  plot(1:max_clusters, sil_width, type = "b", xlab = "Number of clusters (k)", ylab = "Silhouette Width")
  
  # Choose the number of clusters with the highest silhouette width
  optimal_k <- which.max(sil_width)
  cat("Optimal number of clusters: ", optimal_k, "\n")
  
  # Perform clustering with optimal k
  final_clustering <- pam(unique_trajectories, k = optimal_k)
  
  # Add cluster assignment to the pixel data frame
  # Create a matching column to facilitate the join
  unique_trajectories_df <- as.data.frame(unique_trajectories)
  unique_trajectories_df$cluster <- final_clustering$clustering
  unique_trajectories_df$trajectory_id <- 1:nrow(unique_trajectories_df)
  
  # Match each pixel's trajectory to the unique trajectories
  pixel_df$trajectory_id <- apply(pixel_df[,-1], 1, function(row) {
    match(TRUE, apply(unique_trajectories, 1, function(uq) all(uq == row)))
  })
  
  # Merge back the cluster assignments using the unique trajectories
  pixel_df <- left_join(pixel_df, unique_trajectories_df, by = "trajectory_id")
  
  # Create a raster of cluster assignments
  cluster_raster <- rast(rasters[[1]])
  values(cluster_raster) <- pixel_df$cluster
  
  # Plot the cluster raster
  plot(cluster_raster, main = "Pixel Clustering Based on Temporal Trajectories")
}