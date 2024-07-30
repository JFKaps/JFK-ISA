# Load necessary packages ---------------------------
library(ggplot2)
library(dbscan)
library(fpc)
library(plotly)
library(factoextra)
library(caret)
library(ggdendro)
library(reshape2)
library(car)
library(heatmaply)
library(party)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)
library(Rlof)
require(dplyr)

# Custom functions 
custom_colors <- c(
  "#D55E00",  # Red
  "#0072B2",  # Blue
  "#F0E442",  # Yellow
  "#009E73",  # Green
  "#CC79A7",  # Pink
  "#56B4E9",  # Sky Blue
  "#E69F00",  # Orange
  "#000000",  # Black
  "#999999",  # Grey
  "#F0E68C",  # Khaki
  "#8B0000",  # Dark Red
  "#4682B4",  # Steel Blue
  "#808000",  # Olive
  "#8A2BE2",  # Blue Violet
  "#A52A2A"   # Brown
)

# Define the extract_pattern and format_strings functions
extract_pattern <- function(name) {
  pattern <- "([A-Za-z]+\\s[0-9]+:[0-9]+)|([A-Za-z]+\\s[[:alpha:]]?-?[0-9]+:[0-9]+)"
  matches <- regmatches(name, gregexpr(pattern, name))
  
  if (length(matches[[1]]) > 0) {
    return(matches[[1]][1])
  } else {
    return(name)
  }
}

format_strings <- function(input_strings) {
  formatted_strings <- gsub("\\s+", "", input_strings)
  formatted_strings <- gsub("([A-Za-z]*)(\\d+):(\\d+)", "\\1(\\2:\\3)", formatted_strings)
  return(formatted_strings)
}

# Define the extract_pattern and format_strings functions
extract_pattern <- function(name) {
  pattern <- "([A-Za-z]+\\s[0-9]+:[0-9]+)|([A-Za-z]+\\s[[:alpha:]]?-?[0-9]+:[0-9]+)"
  matches <- regmatches(name, gregexpr(pattern, name))
  
  if (length(matches[[1]]) > 0) {
    return(matches[[1]][1])
  } else {
    return(name)
  }
}

format_strings <- function(input_strings) {
  formatted_strings <- gsub("\\s+", "", input_strings)
  formatted_strings <- gsub("([A-Za-z]*)(\\d+):(\\d+)", "\\1(\\2:\\3)", formatted_strings)
  return(formatted_strings)
}

# Function to remove outliers
remove_outliers <- function(outliers_df) {
  samples_to_remove <- outliers_df %>% filter(Category == "Outlier") %>% pull(Sample)
  if (length(samples_to_remove) == 0) return()
  
  # Remove rows from cleaned sequence based on sample names
  updated_cleaned_sequence <- cleaned_sequence()[!cleaned_sequence()$Sample %in% samples_to_remove, ]
  
  # Remove columns from cleaned data based on sample names
  updated_cleaned_data <- cleaned_data()[, !colnames(cleaned_data()) %in% samples_to_remove]
  
  cleaned_data(updated_cleaned_data)
  cleaned_sequence(updated_cleaned_sequence)
}

# Principal Component Analysis (PCA) ---------------------------
perform_pca_analysis <- function(data, sequence) {
  
  # Transpose data
  data <- t(data)
  
  # Check for zero dimensions
  if (nrow(data) == 0 || ncol(data) == 0) {
    stop("Error: Cleaned data has zero dimensions and cannot be used for PCA.")
  }
  
  # Check for NA values
  if (anyNA(data)) {
    stop("Error: Cleaned data contains NA values and cannot be used for PCA.")
  }
  
  pca <- prcomp(data, center = TRUE)
  var <- pca$sdev^2
  var_percentage <- var / sum(var) * 100
  cumulative_var_percentage <- cumsum(var_percentage)
  var_explained_first_two <- sum(var[1:2]) / sum(var) * 100
  cat("Percentage of variance explained by first two components:", var_explained_first_two, "%\n")
  
  pca_df <- data.frame(
    Sample = row.names(data),
    PC1 = pca$x[, 1],
    PC2 = pca$x[, 2],
    class = as.factor(sequence$class),
    PC = 1:length(var_percentage),
    VarianceExplained = var_percentage,
    CumulativeVarianceExplained = cumulative_var_percentage
  )
  
  scree_plot <- ggplot(pca_df, aes(x = PC)) +
    geom_point(aes(y = VarianceExplained, color = "Variance Explained"), size = 2) +
    geom_line(aes(y = VarianceExplained, linetype = "Variance Explained"), color = "black", linewidth = 1, show.legend = FALSE) +
    geom_point(aes(y = CumulativeVarianceExplained, color = "Cumulative Variance Explained"), size = 2) +
    geom_line(aes(y = CumulativeVarianceExplained, linetype = "Cumulative Variance Explained"), color = "red", linewidth = 1, show.legend = FALSE) +
    labs(x = "Principal Component", y = "Variance Explained (%)", title = "Variance Explained by Principal Components") +
    scale_color_manual(values = c("Variance Explained" = "black", "Cumulative Variance Explained" = "red"), labels = c("Variance Explained", "Cumulative Variance Explained")) +
    scale_linetype_manual(values = c("Variance Explained" = "solid", "Cumulative Variance Explained" = "dashed")) +
    scale_x_continuous(breaks = seq(0, max(pca_df$PC, na.rm = TRUE), by = 1)) +  # Use na.rm = TRUE to ignore NA values
    theme_bw() +
    theme(legend.position = "top") +
    guides(color = guide_legend(title = "Explained Type"))
  
  list(Scree_plotly = ggplotly(scree_plot), PCA.df = pca_df)
}


create_pca_plot <- function(pca_df, custom_colors) {
  plot_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, sample = Sample, color = class)) +
    geom_point(size = 2) +
    labs(title = "PCA",
         color = "Condition",
         x = sprintf("PC1 (%.2f%%)", pca_df$VarianceExplained[1]),
         y = sprintf("PC2 (%.2f%%)", pca_df$VarianceExplained[2])) +
    scale_color_manual(values = custom_colors) +
    theme_bw()
  
  ggplotly(plot_pca)
}

# K-means ---------------------------
# K-means clustering
kmeans_clustering_with_distances <- function(pca_df, custom_colors, percentile_threshold) {
  # Determine the optimal number of clusters
  wss <- fviz_nbclust(pca_df[, 2:3], kmeans, method = "wss", k.max = nrow(pca_df) - 1)
  ggplotly(wss)
  sil <- fviz_nbclust(pca_df[, 2:3], kmeans, method = "silhouette", k.max = nrow(pca_df) - 1)
  ggplotly(sil)
  gap <- fviz_nbclust(pca_df[, 2:3], kmeans, method = "gap_stat", k.max = nrow(pca_df) - 1)
  ggplotly(gap)
  
  # Extract the optimal number of clusters from silhouette method
  layers <- sil$layers
  for (layer in layers) {
    if (!is.null(layer$data) && "xintercept" %in% names(layer$data)) {
      x_intercepts <- layer$data$xintercept
      print(x_intercepts)
    }
  }
  
  k <- as.integer(x_intercepts[1])
  kmeans_res <- kmeans(pca_df[, 2:3], centers = k)
  cluster_labels <- kmeans_res$cluster
  centroids <- kmeans_res$centers
  
  # Calculate distances to centroids
  distances <- sqrt(rowSums((pca_df[, 2:3] - centroids[cluster_labels, ])^2))
  
  # Calculate the percentile-based distance threshold
  threshold_value <- quantile(distances, percentile_threshold / 100)
  
  # Create data frame with results
  kmeans_df <- data.frame(
    Sample = pca_df$Sample,
    PC1 = pca_df[, 2],
    PC2 = pca_df[, 3],
    DistanceToCentroid = distances,
    cluster = factor(cluster_labels),
    Category = ifelse(distances > threshold_value, "Outlier", "Inlier")
  )
  
  # Return the data frame
  return(kmeans_df)
}

# Hierarchical clustering ---------------------------
# Hierarchical clustering Function
perform_hierarchical_clustering <- function(pca_df, sequence, custom_colors, method = "complete", k = 3, threshold = 3) {
  data_dist <- dist(pca_df[, 2:3])
  hc <- hclust(data_dist, method = method)
  clusters <- cutree(hc, k = k)
  clusters_factor <- factor(clusters, levels = levels(as.factor(sequence$class)))
  
  dimr_hier_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, text = Sample, color = clusters_factor)) +
    geom_point(shape = 16) +
    labs(title = paste("Hierarchical Clustering -", method, "method"), x = "PC1", y = "PC2") +
    scale_color_discrete(name = "Cluster") +
    theme_bw()
  
  # Convert hclust object to dendrogram
  dend_data <- as.dendrogram(hc)
  dendro_data <- dendro_data(dend_data)
  
  # Create hierarchical clustering plot
  hc_plot <- ggplot(dendro_data$segments) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_bw() +
    labs(title = paste("Hierarchical Clustering -", method, "method"),
         x = "Data Points", 
         y = "Height (Distance)")
  
  # Extract heights for each leaf node in the dendrogram
  heights <- data.frame(label = dendro_data$labels$label, height = 0)
  
  for (i in seq_along(dendro_data$segments$yend)) {
    if (!is.na(dendro_data$segments$y[i])) {
      label <- dendro_data$segments$x[i]
      height <- dendro_data$segments$y[i]
      if (label <= nrow(heights)) {
        heights$height[label] <- height
      }
    }
  }
  
  heights_matched <- heights[match(pca_df$Sample, heights$label), "height"]
  
  hc_outliers <- data.frame(
    Sample = pca_df$Sample,
    Cluster = clusters_factor,
    Height = heights_matched,
    Category = ifelse(heights_matched > threshold, "Outlier", "Inlier")  # Use the specified threshold
  )
  
  list(
    hclust_plot = ggplotly(dimr_hier_plot),
    conf_matrix_plot = perform_confusion_matrix(pca_df, sequence, custom_colors, method, k),
    dendrogram_plot = ggplotly(hc_plot),
    hierarchical_outliers = hc_outliers
  )
}

# Function for confusion matrix plot
perform_confusion_matrix <- function(pca_df, sequence, custom_colors, method = "complete", k = 3) {
  data_dist <- dist(pca_df[, c("PC1", "PC2")])
  hc <- hclust(data_dist, method = method)
  clusters <- cutree(hc, k = k)
  
  reference <- factor(sequence$class)
  prediction <- factor(clusters, levels = levels(reference))
  
  cm <- confusionMatrix(prediction, reference)
  
  cm_df <- as.data.frame(cm$table)
  cm_df$Reference <- factor(cm_df$Reference, levels = rev(levels(cm_df$Reference)))
  
  cm_plot <- ggplot(cm_df, aes(x = Prediction, y = Reference, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq)) +
    scale_fill_gradient(low = "white", high = "firebrick") +
    labs(x = "Prediction", y = "Reference") +
    theme_bw()
  
  ggplotly(cm_plot)
}


# Dendrogram Function
perform_dendrogram <- function(pca_df, sequence, custom_colors, method = "complete", threshold) {
  data_dist <- dist(pca_df[, c("PC1", "PC2")])
  hc <- hclust(data_dist, method = method)
  
  dend_data <- as.dendrogram(hc)
  dendro_data <- dendro_data(dend_data)
  
  # Highlight segments above the threshold
  dendro_data$segments$highlight <- dendro_data$segments$y > threshold | dendro_data$segments$yend > threshold
  
  dend_plot <- ggplot() + 
    geom_segment(data = dendro_data$segments, aes(x = x, y = y, xend = xend, yend = yend, color = highlight)) +
    scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"), guide = "none") +  # Red color for outliers
    geom_text(data = dendro_data$labels, aes(x = x, y = y, label = label), hjust = 1, angle = 45, size = 3) +
    scale_x_continuous(breaks = NULL) + # Remove numeric x-axis values
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.ticks.x = element_blank()) +
    labs(title = paste("Dendrogram -", method, "method"), 
         x = "Data Points", 
         y = "Height (Distance)")
  
  ggplotly(dend_plot)
}


# DBSCAN ---------------------------
# kNN Distance Plot Function
perform_kNN_dist_plot <- function(pca_df, k) {
  # Ensure k is numeric and within valid range
  if (!is.numeric(k) || k <= 0 || k > nrow(pca_df)) {
    stop("Invalid k value for kNN distance plot")
  }
  
  # Debugging information
  print(paste("k value:", k))
  print(paste("Dimensions of pca_df:", dim(pca_df)))
  
  # Ensure 'PC' column exists and contains valid values
  if (!"PC" %in% colnames(pca_df)) {
    stop("Error: 'PC' column is missing in pca_df")
  }
  
  if (all(is.na(pca_df$PC))) {
    stop("Error: 'PC' column contains only NA values")
  }
  
  # Remove rows with NA values in the 'PC' column
  pca_df <- pca_df[!is.na(pca_df$PC), ]
  
  # Ensure there are valid values after removing NAs
  if (nrow(pca_df) == 0) {
    stop("Error: No valid data points in 'PC' column after removing NA values")
  }
  
  # Compute kNN distances
  kNN_dist <- dbscan::kNNdist(pca_df[, 2:3], k = k)
  
  # Debugging information
  print(head(kNN_dist))
  
  # Create a data frame for plotting
  kNN_dist_df <- data.frame(Distance = sort(kNN_dist))
  kNN_dist_df$Index <- seq_len(nrow(kNN_dist_df))
  
  # Manually create the kNN distance plot using ggplot2
  kNN_plot <- ggplot(kNN_dist_df, aes(x = Index, y = Distance)) +
    geom_line() +
    geom_hline(yintercept = mean(kNN_dist), color = "red", linetype = "dashed") +
    labs(title = paste("kNN Distance Plot for k =", k), x = "Points sorted by distance", y = "kNN Distance") +
    theme_bw()
  
  return(ggplotly(kNN_plot))
}


# DBSCAN Function
perform_dbscan_clustering <- function(pca_df, eps, min_pts) {
  # Ensure eps and minPts are numeric
  if (!is.numeric(eps) || eps <= 0) {
    stop("Invalid eps value for DBSCAN")
  }
  if (!is.numeric(min_pts) || min_pts <= 0) {
    stop("Invalid min_pts value for DBSCAN")
  }
  
  dbscan_res <- dbscan::dbscan(pca_df[, 2:3], eps = eps, minPts = min_pts)
  
  plot_data <- data.frame(
    Sample = rownames(pca_df), 
    PC1 = pca_df$PC1,
    PC2 = pca_df$PC2,
    Cluster = factor(dbscan_res$cluster),
    Category = ifelse(dbscan_res$cluster == 0, "Outlier", "Inlier")
  )
  
  dbscan_plot <- ggplot(plot_data, aes(text = Sample, x = PC1, y = PC2, color = Cluster)) +
    geom_point() +
    labs(
      title = paste("DBSCAN Clustering - eps:", eps, "- minPts:", min_pts),
      x = sprintf("PC1 (%.2f%%)", pca_df$VarianceExplained[1]),
      y = sprintf("PC2 (%.2f%%)", pca_df$VarianceExplained[2]),
      color = "Cluster"
    ) +
    theme_bw()
  
  list(
    dbscan_plot = ggplotly(dbscan_plot),
    dbscan_outliers = plot_data
  )
}

# HDBSCAN ---------------------------
# HDBSCAN Function
perform_hdbscan_clustering <- function(pca_df, min_pts) {
  # Ensure minPts is numeric
  if (!is.numeric(min_pts) || min_pts <= 0) {
    stop("Invalid min_pts value for HDBSCAN")
  }
  
  res_hdbscan <- hdbscan(dist(pca_df[, 2:3], method = "euclidean"), minPts = min_pts)
  
  plot_data <- data.frame(Sample = rownames(pca_df), 
                          PC1 = pca_df$PC1,
                          PC2 = pca_df$PC2,
                          Cluster = factor(res_hdbscan$cluster),
                          OutlierScore = res_hdbscan$outlier_scores)
  
  hdbscan_plot <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster, text = Sample, outlierscore = OutlierScore)) +
    geom_point(pch = 19) +
    labs(title = paste("HDBSCAN Clustering - MinPts:", min_pts),
         x = sprintf("PC1 (%.2f%%)", pca_df$VarianceExplained[1]),
         y = sprintf("PC2 (%.2f%%)", pca_df$VarianceExplained[2]),
         color = "Cluster", size = "Outlier Score") +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(size = 4)), size = guide_legend(override.aes = list(alpha = 0.5)))
  
  list(
    hdbscan_plot = ggplotly(hdbscan_plot),
    hdbscan_outliers = plot_data
  )
}

# OPTICS ---------------------------
perform_optics_analysis <- function(pca_df, eps, min_pts, eps_cl, custom_colors) {
  # Perform OPTICS clustering
  opt <- dbscan::optics(pca_df[, 2:3], eps = eps, minPts = min_pts)
  
  # Extract clusters based on epsilon threshold
  opt_threshold <- dbscan::extractDBSCAN(opt, eps_cl = eps_cl)
  
  # Debugging information
  print("opt_threshold structure:")
  print(str(opt_threshold))
  
  # Verify that opt_threshold$cluster is a vector
  if (!is.vector(opt_threshold$cluster)) {
    stop("opt_threshold$cluster is not a vector")
  }
  
  # Plot reachability distances
  reachability_plot <- function() {
    plot(opt,
         main = "Reachability Plot",
         ylab = "Reachability Distance",
         xlab = "Order of Points",
         ylim = c(min(opt$reachdist[!is.infinite(opt$reachdist)]),
                  max(opt$reachdist[!is.infinite(opt$reachdist)] + 1)))
  }
  
  # Plot reachability distances with threshold
  reachability_plot_threshold <- function() {
    plot(opt_threshold,
         main = paste("Reachability Plot kt =", eps_cl),
         ylab = "Reachability Distance",
         xlab = "Order of Points",
         ylim = c(min(opt$reachdist[!is.infinite(opt$reachdist)]),
                  max(opt$reachdist[!is.infinite(opt$reachdist)] + 1)))
  }
  
  # Function to create cluster plot using ggplot2
  cluster_plot <- function() {
    clusters <- as.vector(opt_threshold$cluster)
    
    # Debugging information
    print("Unique clusters:")
    print(unique(clusters))
    
    # Check if there are any clusters (excluding noise points labeled as 0)
    valid_clusters <- clusters[clusters != 0]
    if (length(unique(valid_clusters)) < 1) {
      stop("No clusters found. Cannot create cluster plot.")
    }
    
    # Add cluster information to pca_df
    pca_df$Cluster <- as.factor(clusters)
    
    # Ensure the PCA data frame has the required columns
    if (!("VarianceExplained" %in% names(pca_df))) {
      stop("pca_df must contain 'VarianceExplained' column")
    }
    
    # Debugging information
    print("pca_df structure:")
    print(str(pca_df))
    
    # Create a ggplot2 plot
    plot_pca <- ggplot(pca_df, aes(x = PC1, y = PC2, text = Sample, color = Cluster)) +
      geom_point(size = 2) +
      labs(title = "OPTICS Clustering",
           color = "Cluster",
           x = sprintf("PC1 (%.2f%%)", pca_df$VarianceExplained[1]),
           y = sprintf("PC2 (%.2f%%)", pca_df$VarianceExplained[2])) +
      scale_color_manual(values = custom_colors) +
      theme_bw() +
      theme(legend.position = "right",
            legend.justification = "top")
    
    ggplotly(plot_pca)
  }
  
  optics_outliers <- data.frame(
    Sample = rownames(pca_df), 
    Cluster = opt_threshold$cluster,
    Category = ifelse(opt_threshold$cluster == 0, "Outlier", "Inlier")  # Directly create Category column
  )
  
  return(list(
    reachability_plot = reachability_plot,
    reachability_plot_threshold = reachability_plot_threshold,
    cluster_plot = cluster_plot,
    optics_outliers = optics_outliers
  ))
}

# Local Outlier Factor (LOF) ---------------------------
# Updated LOF Analysis Function
calculate_and_plot_lof <- function(pca_df, threshold = 1, k = 4) {
  if (!all(c("PC1", "PC2", "Sample") %in% colnames(pca_df))) {
    stop("pca_df must contain 'PC1', 'PC2', and 'Sample' columns")
  }
  
  pca_df$LOF <- lof(pca_df[, c("PC1", "PC2")], k = k)
  lof_plot <- ggplot(pca_df, aes(x = reorder(Sample, LOF), y = LOF)) +
    geom_line() +
    geom_point(shape = 19) +
    labs(x = "Sample", y = "LOF", title = "Local Outlier Factor (LOF) Scores") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  lof_plotly <- ggplotly(lof_plot)
  
  pca_df$Category <- ifelse(pca_df$LOF > threshold, "Outlier", "Inlier")
  lof_od_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, text = Sample, color = Category)) +
    geom_point() +
    scale_color_manual(values = c("Inlier" = "blue", "Outlier" = "red")) +
    labs(x = sprintf("PC1 (%.2f%%)", pca_df$VarianceExplained[1]), y = sprintf("PC2 (%.2f%%)", pca_df$VarianceExplained[2]), title = "Outlier Detection with LOF", color = "Category") +
    theme_bw()
  lof_od_plotly <- ggplotly(lof_od_plot)
  
  list(
    lof_plotly = lof_plotly,
    lof_od_plotly = lof_od_plotly,
    lof_outliers = pca_df[, c("Sample", "LOF", "Category")]
  )
}


# Heatmap ---------------------------
# Gradient color for heatmap
gradient_col <- ggplot2::scale_fill_gradient2(
  low = "blue", high = "red", 
  midpoint = 0.5, limits = c(-3, 3)
)

# Function to calculate feature variability and select top features
select_top_features <- function(data, top_n = 20) {
  feature_variability <- apply(data, 1, sd)  # Calculate variability across samples (rows)
  sorted_features <- names(sort(feature_variability, decreasing = TRUE))
  
  # Handle cases where top_n exceeds the number of available features
  if (top_n > length(sorted_features)) {
    top_n <- length(sorted_features)
  }
  
  most_variable_features <- sorted_features[1:top_n]
  print(most_variable_features)  # Debug print to check selected features
  most_variable_features
}

# Function to create heatmaply plot
create_heatmap <- function(data, gradient_col, title, dendrogram_option) {
  heatmaply(
    data,
    k_col = 1,  # Number of columns to display
    k_row = 1,  # Number of clusters for rows
    scale_fill_gradient_fun = gradient_col,  # Apply defined color gradient
    dendrogram = dendrogram_option,  # Display selected dendrogram option
    margins = c(NA, NA, NA, NA),  # Margin settings
    main = title,  # Main title
    scale = "none"  # No scaling applied
  )
}

# Volcano --------------------------- 
perform_statistical_testing <- function(data, sequence_df, group1, group2) {
  # Filter data and sequence based on groups
  condition_runs1 <- sequence_df$sample[sequence_df$class == group1]
  condition_runs2 <- sequence_df$sample[sequence_df$class == group2]
  
  # Debug: Print selected condition runs
  print("Condition runs for group1:")
  print(condition_runs1)
  print("Condition runs for group2:")
  print(condition_runs2)
  
  # Ensure the selected columns are present in the data
  condition_runs1 <- intersect(condition_runs1, colnames(data))
  condition_runs2 <- intersect(condition_runs2, colnames(data))
  
  # Debug: Print intersected condition runs
  print("Intersected condition runs for group1:")
  print(condition_runs1)
  print("Intersected condition runs for group2:")
  print(condition_runs2)
  
  # Subset the data based on the intersected condition runs
  condition_data1 <- data[, condition_runs1, drop = FALSE]
  condition_data2 <- data[, condition_runs2, drop = FALSE]
  
  # Create dataframes for each condition
  df_1 <- condition_data1
  df_2 <- condition_data2
  
  # Debug: Print column names and dimensions
  print("Columns in condition_data1:")
  print(colnames(condition_data1))
  print("Columns in condition_data2:")
  print(colnames(condition_data2))
  print("Dimensions of condition_data1:")
  print(dim(condition_data1))
  print("Dimensions of condition_data2:")
  print(dim(condition_data2))
  
  # Prepare ANOVA data
  ANOVA_data <- data
  ANOVA_data$Metabolite <- rownames(ANOVA_data)
  ANOVA_data_long <- melt(ANOVA_data, id.vars = "Metabolite", variable.name = "sample", value.name = "Expression")
  
  # Debug: Print dimensions of melted ANOVA_data
  print("Dimensions of ANOVA_data_long after melt:")
  print(dim(ANOVA_data_long))
  
  # Ensure that the sequence_df contains unique sample values
  sequence_df <- sequence_df[!duplicated(sequence_df$sample), ]
  
  # Merge to add class information
  ANOVA_data_long <- merge(ANOVA_data_long, sequence_df[, c("sample", "class")], by = "sample", all.x = TRUE)
  
  # Debug: Print dimensions after merge
  print("Dimensions of ANOVA_data_long after merge:")
  print(dim(ANOVA_data_long))
  
  # Ensure 'class' is treated as a factor
  ANOVA_data_long$class <- factor(ANOVA_data_long$class)
  
  # Debug: Print unique classes
  print("Unique classes in ANOVA_data_long$class:")
  print(unique(ANOVA_data_long$class))
  
  # Levene's test for homogeneity of variances
  levene_results <- data.frame(Metabolite = character(), p_value = numeric(), stringsAsFactors = FALSE)
  for (metabolite in unique(ANOVA_data_long$Metabolite)) {
    metabolite_data <- subset(ANOVA_data_long, Metabolite == metabolite)
    levene_result <- leveneTest(Expression ~ class, data = metabolite_data)
    p_value_levene <- levene_result$Pr[1]
    levene_results <- rbind(levene_results, data.frame(Metabolite = metabolite, p_value = p_value_levene))
  }
  
  # ANOVA test
  results_df_anova <- data.frame(Metabolite = character(), p_value = numeric(), stringsAsFactors = FALSE)
  for (metabolite in unique(ANOVA_data_long$Metabolite)) {
    metabolite_data <- subset(ANOVA_data_long, Metabolite == metabolite)
    model <- oneway.test(Expression ~ class, data = metabolite_data, var.equal = TRUE)
    p_value <- model$p.value
    results_df_anova <- rbind(results_df_anova, data.frame(Metabolite = metabolite, p_value = p_value))
  }
  
  # Tukey's test
  results_df_tukey <- data.frame(Metabolite = character(), Comparison = character(), diff = numeric(), lwr = numeric(), upr = numeric(), p_value = numeric(), stringsAsFactors = FALSE)
  for (metabolite in unique(ANOVA_data_long$Metabolite)) {
    metabolite_data <- subset(ANOVA_data_long, Metabolite == metabolite)
    is_significant <- results_df_anova$p_value[results_df_anova$Metabolite == metabolite] < 0.05
    if (is_significant) {
      tukey_result <- TukeyHSD(aov(Expression ~ class, data = metabolite_data))
      comparisons <- as.data.frame(tukey_result$class[, c("diff", "lwr", "upr", "p adj")])
      comparisons$Metabolite <- metabolite
      comparisons$Comparison <- rownames(comparisons)
      results_df_tukey <- rbind(results_df_tukey, comparisons)
    }
  }
  
  # Split dataframes by Comparison
  list_of_comparison_dataframes <- split(results_df_tukey, results_df_tukey$Comparison)
  
  # Filter non-significant proteins
  non_significant_proteins <- results_df_anova$Metabolite[results_df_anova$p_value >= 0.05]
  for (i in seq_along(list_of_comparison_dataframes)) {
    list_of_comparison_dataframes[[i]] <- list_of_comparison_dataframes[[i]][!list_of_comparison_dataframes[[i]]$Metabolite %in% non_significant_proteins, ]
  }
  
  # Calculate Log2 Fold change for each comparison
  for (comparison in names(list_of_comparison_dataframes)) {
    comparison_df <- list_of_comparison_dataframes[[comparison]]
    # Ensure the dimensions match before adding Log2FC column
    if (nrow(comparison_df) > 0) {
      comparison_df$Log2FC <- log2(rowMeans(df_2[comparison_df$Metabolite, , drop = FALSE]) / rowMeans(df_1[comparison_df$Metabolite, , drop = FALSE]))
    } else {
      print(paste("Skipping Log2FC calculation for comparison", comparison, "due to dimension mismatch"))
    }
    rownames(comparison_df) <- comparison_df$Metabolite
    list_of_comparison_dataframes[[comparison]] <- comparison_df
  }
  
  return(list(tukey_results = results_df_tukey, comparison_dfs = list_of_comparison_dataframes))
}

create_volcano_plot <- function(results_df, group1, group2, log2fc_threshold, pval_threshold) {
  # Create a new column for coloring points
  results_df$color <- "Below Threshold"
  results_df$color[results_df$`p adj` < pval_threshold & results_df$Log2FC > log2fc_threshold] <- "Upregulated"
  results_df$color[results_df$`p adj` < pval_threshold & results_df$Log2FC < -log2fc_threshold] <- "Downregulated"
  
  # Create the volcano plot
  volcano_plot <- ggplot(results_df, aes(x = Log2FC, y = -log10(`p adj`))) +
    geom_point(aes(color = color), size = 1) +
    xlim(c(min(results_df$Log2FC) - 0.2, max(results_df$Log2FC) + 0.2)) +
    ylim(c(0, max(-log10(results_df$`p adj`)) + 1)) +
    geom_hline(yintercept = pval_threshold, linetype = "dashed", color = "black") +
    geom_vline(xintercept = c(-log2fc_threshold, log2fc_threshold), linetype = "dashed", color = "black") + # Add vertical lines for log2FC thresholds
    labs(title = paste("Volcano Plot", group2, "vs", group1), x = "Log2 Fold Change", y = "-log10(p-value)") +
    theme_bw() +
    scale_color_manual(values = c("Upregulated" = "red", "Downregulated" = "blue", "Below Threshold" = "grey")) +
    guides(color = guide_legend(title = NULL)) +
    geom_text(data = subset(results_df, color == "Upregulated"), aes(label = Metabolite), color = "darkred", size = 2, nudge_x = 0.1, nudge_y = 0.2) + # Label upregulated points
    geom_text(data = subset(results_df, color == "Downregulated"), aes(label = Metabolite), color = "darkblue", size = 2, nudge_x = -0.1, nudge_y = 0.2) # Label downregulated points
  
  ggplotly(volcano_plot)
}