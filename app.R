# Load necessary packages
library(shiny)       # For creating Shiny web applications
library(ggplot2)     # For creating plots
library(plotly)      # For creating interactive plots
library(dbscan)      # For density-based spatial clustering of applications with noise
library(factoextra)  # For extracting and visualizing the results of multivariate data analyses
library(caret)       # For training and plotting classification and regression models
library(ggdendro)    # For creating dendrograms and tree diagrams using ggplot2
library(dplyr)       # For data manipulation
library(heatmaply)   # For creating interactive heatmaps
library(DT)          # For creating data tables

# Set the working directory
setwd("~/Desktop/SDU/Cand/2.Sem/ISA/Code")

# Source the external functions file
source("TestFunctions3.R")

# Define UI logic
ui <- fluidPage(
  titlePanel("Outlier detection, Clustering and Visualization with Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("sequence_file", "Choose Sequence CSV File", accept = ".csv"),  # Input for sequence file
      fileInput("data_file", "Choose Data CSV File", accept = ".csv"),          # Input for data file
      actionButton("load_data", "Load Data"),                                   # Button to load data
      actionButton("run_data_cleaning", "Run Data Cleaning"),                   # Button to run data cleaning
      hr(),                                                                     # Horizontal line for separation
      uiOutput("tab_selection")                                                 # Dynamic UI output for tab selection
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Data",
                           tabsetPanel(
                             tabPanel("Sequence File", DTOutput("sequence_table")),        # Tab for sequence file data
                             tabPanel("Data File", DTOutput("data_table")),                # Tab for data file
                             tabPanel("Cleaned Sequence File", DTOutput("cleaned_sequence_table")),  # Tab for cleaned sequence file
                             tabPanel("Cleaned Data File", DTOutput("cleaned_data_table"))           # Tab for cleaned data file
                           )),
                  tabPanel("Dimensional Reduction",
                           actionButton("run_pca", "Run PCA"),                # Button to run PCA
                           plotlyOutput("pca_plot"),                          # Output for PCA plot
                           plotlyOutput("scree_plot")                         # Output for Scree plot
                  ),
                  tabPanel("Outlier Detection",
                           tabsetPanel(
                             tabPanel("K-means",
                                      selectInput("kmeans_eval_method", "Choose Evaluation Method:",  # Dropdown to select evaluation method for K-means
                                                  choices = c("Within Sum of Square (WSS)" = "wss", "Silhouette" = "silhouette", "Gap Statistic" = "gap_stat")),
                                      actionButton("compute_kmeans_eval", "Compute Evaluation"),      # Button to compute K-means evaluation
                                      plotlyOutput("kmeans_eval_plot"),                                # Output for K-means evaluation plot
                                      numericInput("num_clusters", "Number of Clusters (k):", value = 3, min = 1),  # Input for number of clusters
                                      numericInput("percentile_threshold", "Percentile Threshold:", value = 95, min = 0, max = 100, step = 1), # Input for percentile threshold
                                      actionButton("run_kmeans", "Run K-means"),                      # Button to run K-means
                                      plotlyOutput("kmeans_plot"),                                    # Output for K-means plot
                                      DTOutput("kmeans_outliers"),                                    # Output for K-means outliers table
                                      actionButton("remove_kmeans_outliers", "Remove Selected Outliers"),  # Button to remove selected K-means outliers
                                      actionButton("save_cleaned_kmeans", "Save Cleaned Data")        # Button to save cleaned K-means data
                             ),
                             tabPanel("Hierarchical",
                                      selectInput("clustering_method", "Select Clustering Method:",  # Dropdown to select clustering method
                                                  choices = c("Single" = "single",
                                                              "Complete" = "complete",
                                                              "Average" = "average",
                                                              "Ward's D2" = "ward.D2")),
                                      numericInput("num_clusters_hierarchical", "Number of Clusters (k):", value = 3, min = 1),  # Input for number of clusters
                                      numericInput("threshold", "Dendrogram Threshold (Distance):", value = 5, min = 0),  # Input for dendrogram threshold
                                      actionButton("run_hierarchical", "Run Hierarchical Clustering"),  # Button to run hierarchical clustering
                                      plotlyOutput("hclust_plot"),                                      # Output for hierarchical clustering plot
                                      plotlyOutput("conf_matrix_plot"),                                 # Output for confusion matrix plot
                                      plotlyOutput("dendrogram_plot"),                                  # Output for dendrogram plot
                                      DTOutput("hierarchical_outliers"),                                # Output for hierarchical outliers table
                                      actionButton("remove_hierarchical_outliers", "Remove Selected Outliers"),  # Button to remove selected hierarchical outliers
                                      actionButton("save_cleaned_hierarchical", "Save Cleaned Data")    # Button to save cleaned hierarchical data
                             ),
                             tabPanel("DBSCAN",
                                      numericInput("knn", "Choose k for kNN Distance Plot:", value = 5, min = 1),  # Input for k in kNN
                                      actionButton("compute_knn", "Compute kNN Distance Plot"),                   # Button to compute kNN distance plot
                                      plotlyOutput("knn_plot"),                                                    # Output for kNN plot
                                      numericInput("eps", "Choose epsilon for DBSCAN:", value = 0.5, min = 0.01, step = 0.1),  # Input for epsilon in DBSCAN
                                      numericInput("min_pts_dbscan", "Choose minPts for DBSCAN:", value = 5, min = 1),  # Input for minPts in DBSCAN
                                      actionButton("run_dbscan", "Run DBSCAN"),                                   # Button to run DBSCAN
                                      plotlyOutput("dbscan_plot"),                                                # Output for DBSCAN plot
                                      DTOutput("dbscan_outliers"),                                                # Output for DBSCAN outliers table
                                      actionButton("remove_dbscan_outliers", "Remove Selected Outliers"),         # Button to remove selected DBSCAN outliers
                                      actionButton("save_cleaned_dbscan", "Save Cleaned Data")                   # Button to save cleaned DBSCAN data
                             ),
                             tabPanel("HDBSCAN",
                                      numericInput("min_pts_hdbscan", "Choose minPts for HDBSCAN:", value = 5, min = 1),  # Input for minPts in HDBSCAN
                                      numericInput("threshold_hdbscan", "Outlier Threshold for HDBSCAN:", value = 0.85, min = 0.01, max = 1),  # Input for outlier threshold in HDBSCAN
                                      actionButton("run_hdbscan", "Run HDBSCAN"),                                          # Button to run HDBSCAN
                                      plotlyOutput("hdbscan_plot"),                                                        # Output for HDBSCAN plot
                                      DTOutput("hdbscan_outliers"),                                                        # Output for HDBSCAN outliers table
                                      actionButton("remove_hdbscan_outliers", "Remove Selected Outliers"),                 # Button to remove selected HDBSCAN outliers
                                      actionButton("save_cleaned_hdbscan", "Save Cleaned Data")                            # Button to save cleaned HDBSCAN data
                             ),
                             tabPanel("OPTICS",
                                      numericInput("min_pts_optics", "Choose minPts for OPTICS:", value = 5, min = 1),  # Input for minPts in OPTICS
                                      numericInput("eps_optics", "Choose eps for OPTICS (optional):", value = NA, min = 0.1, step = 0.1),  # Input for eps in OPTICS
                                      numericInput("eps_cl_optics", "Choose cutoff (eps_cl) for OPTICS:", value = 0.5, min = 0.1, step = 0.1),  # Input for eps_cl in OPTICS
                                      actionButton("run_optics", "Run OPTICS"),                                     # Button to run OPTICS
                                      plotOutput("optics_reachability_plot"),                                      # Output for OPTICS reachability plot
                                      plotOutput("reachability_plot_threshold"),                                   # Output for reachability plot threshold
                                      plotlyOutput("cluster_plot"),                                                # Output for cluster plot
                                      DTOutput("optics_outliers"),                                                 # Output for OPTICS outliers table
                                      actionButton("remove_optics_outliers", "Remove Selected Outliers"),          # Button to remove selected OPTICS outliers
                                      actionButton("save_cleaned_optics", "Save Cleaned Data")                     # Button to save cleaned OPTICS data
                             ),
                             tabPanel("LOF",
                                      numericInput("lof_threshold", "Threshold for LOF:", value = 1.5, min = 0, step = 0.1),  # Input for threshold in LOF
                                      numericInput("lof_k", "k for LOF:", value = 4, min = 1),                                 # Input for k in LOF
                                      actionButton("run_lof", "Run LOF"),                                                    # Button to run LOF
                                      plotlyOutput("lof_plot"),                                                             # Output for LOF plot
                                      plotlyOutput("lof_od_plot"),                                                          # Output for LOF outlier detection plot
                                      DTOutput("lof_outliers"),                                                             # Output for LOF outliers table
                                      actionButton("remove_lof_outliers", "Remove Selected Outliers"),                      # Button to remove selected LOF outliers
                                      actionButton("save_cleaned_lof", "Save Cleaned Data")                                 # Button to save cleaned LOF data
                             )
                           )),
                  tabPanel("Visualization",
                           tabsetPanel(
                             tabPanel("Heatmap",
                                      numericInput("num_top_features", "Number of Top Features:", value = 20, min = 1),  # Input for number of top features in heatmap
                                      selectInput("dendrogram_option", "Dendrogram Option:",                          # Dropdown to select dendrogram option
                                                  choices = c("both", "row", "column", "none"), selected = "both"),
                                      actionButton("run_heatmap", "Generate Heatmap"),                                # Button to generate heatmap
                                      plotlyOutput("heatmap_plot"),                                                   # Output for heatmap plot
                                      plotlyOutput("selected_features_heatmap_plot")                                  # Output for selected features heatmap plot
                             ),
                             tabPanel("Volcano Plot",
                                      selectInput("group1", "Select Group 1", choices = NULL),                        # Dropdown to select group 1 for volcano plot
                                      selectInput("group2", "Select Group 2", choices = NULL),                        # Dropdown to select group 2 for volcano plot
                                      numericInput("log2fc_threshold", "Log2 Fold Change Threshold:", value = 2),     # Input for log2 fold change threshold
                                      numericInput("pval_threshold", "P-value Threshold:", value = 0.05),             # Input for p-value threshold
                                      actionButton("run_volcano_plot", "Generate Volcano Plot"),                      # Button to generate volcano plot
                                      plotlyOutput("volcano_plot"),                                                   # Output for volcano plot
                                      DTOutput("volcano_table")                                                       # Output for volcano plot table
                             )
                           )
                  )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  data <- reactiveVal()          # Holds the raw data file
  sequence <- reactiveVal()      # Holds the sequence file
  cleaned_data <- reactiveVal()  # Holds the cleaned data
  cleaned_sequence <- reactiveVal() # Holds the cleaned sequence
  pca_result <- reactiveVal()    # Holds the PCA result
  cleaned_data_for_visualization <- reactiveVal()
  kmeans_df <- reactiveVal()     # Holds k-means clustering result
  hc_result <- reactiveVal()     # Holds hierarchical clustering result
  dbscan_result <- reactiveVal() # Holds dbscan clustering result
  hdbscan_outliers <- reactiveVal() # Holds hdbscan outliers
  optics_results <- reactiveVal() # Holds optics clustering results
  lof_result <- reactiveVal()    # Holds lof results
  
  # Load Data from CSV Files
  observeEvent(input$load_data, {
    req(input$sequence_file, input$data_file)
    
    sequence_file <- input$sequence_file$datapath
    data_file <- input$data_file$datapath
    
    seq_data <- read.csv(sequence_file)
    raw_data <- read.csv(data_file)
    
    sequence(seq_data)
    data(raw_data)
    
    # Update group selection choices
    updateSelectInput(session, "group1", choices = unique(seq_data$class))
    updateSelectInput(session, "group2", choices = unique(seq_data$class))
    
    print("Data loaded successfully.")
    print(paste("Sequence dimensions:", dim(seq_data)))
    print(paste("Data dimensions:", dim(raw_data)))
  })
  
  # Run Data Cleaning without scaling
  observeEvent(input$run_data_cleaning, {
    req(data(), sequence())
    
    # Filter data and sequence based on labels
    filtered_data <- data()[, sequence()[, 'labels'] %in% c("Name", "Sample")]
    filtered_sequence <- sequence()[sequence()[, 'labels'] %in% c("Name", "Sample"), ]
    
    number_of_rows_before <- nrow(filtered_data)
    
    # Make unique row names and perform additional data transformations
    unique_row_names <- make.unique(as.character(filtered_data[, "Compound.Name"]))
    rownames(filtered_data) <- unique_row_names
    filtered_data <- filtered_data[, -1]
    
    # Perform logarithmic transformation and clean data
    filtered_data <- log2(filtered_data)
    filtered_data <- na.omit(filtered_data)
    filtered_sequence <- filtered_sequence[-1, ]
    
    # Debugging information
    print(paste("Number of rows before cleaning:", number_of_rows_before))
    print(paste("Number of rows after cleaning:", nrow(filtered_data)))
    print(paste("Filtered data dimensions:", dim(filtered_data)))
    print(paste("Filtered sequence dimensions:", dim(filtered_sequence)))
    
    # Check for empty data frames
    if (nrow(filtered_data) == 0 || ncol(filtered_data) == 0) {
      showNotification("Error: Cleaned data has zero dimensions after transformation.", type = "error")
      return(NULL)
    }
    
    # Update the reactive values
    cleaned_data(filtered_data)
    cleaned_sequence(filtered_sequence)
  })
  
  # Display Raw Data and Cleaned Data
  output$sequence_table <- renderDT({
    req(sequence())
    datatable(sequence(), options = list(pageLength = 20, autoWidth = TRUE))
  })
  
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(pageLength = 20, autoWidth = TRUE))
  })
  
  output$cleaned_sequence_table <- renderDT({
    req(cleaned_sequence())
    datatable(cleaned_sequence(), options = list(pageLength = 20, autoWidth = TRUE))
  })
  
  output$cleaned_data_table <- renderDT({
    req(cleaned_data())
    datatable(cleaned_data(), options = list(pageLength = 20, autoWidth = TRUE), rownames = TRUE)
  })
  
  # Perform PCA analysis
  observeEvent(input$run_pca, {
    req(cleaned_data(), cleaned_sequence())
    
    cleaned_data_df <- cleaned_data()
    cleaned_sequence_df <- cleaned_sequence()
    
    print(paste("Data dimensions before PCA:", dim(cleaned_data_df)))
    print(paste("Sequence dimensions before PCA:", dim(cleaned_sequence_df)))
    
    # Check if the data has non-zero dimensions
    if (nrow(cleaned_data_df) == 0 || ncol(cleaned_data_df) == 0) {
      showNotification("Error: Cleaned data has zero dimensions and cannot be used for PCA.", type = "error")
      return(NULL)
    }
    
    # Ensure no missing values
    if (anyNA(cleaned_data_df)) {
      showNotification("Error: Cleaned data contains NA values and cannot be used for PCA.", type = "error")
      return(NULL)
    }
    
    pca_result(perform_pca_analysis(cleaned_data_df, cleaned_sequence_df))
  })
  
  # PCA Plot
  output$pca_plot <- renderPlotly({
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    create_pca_plot(pca_df, custom_colors)
  })
  
  # Scree Plot
  output$scree_plot <- renderPlotly({
    req(pca_result())
    pca_result()$Scree_plotly
  })
  
  # Compute and display K-means evaluation plot
  observeEvent(input$compute_kmeans_eval, {
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    method <- input$kmeans_eval_method
    
    eval_plot <- switch(method,
                        "wss" = fviz_nbclust(pca_df[, 2:3], FUNcluster = stats::kmeans, method = "wss", k.max = nrow(pca_df) - 1),
                        "silhouette" = fviz_nbclust(pca_df[, 2:3], FUNcluster = stats::kmeans, method = "silhouette", k.max = nrow(pca_df) - 1),
                        "gap_stat" = fviz_nbclust(pca_df[, 2:3], FUNcluster = stats::kmeans, method = "gap_stat", k.max = nrow(pca_df) - 1)
    )
    
    output$kmeans_eval_plot <- renderPlotly(ggplotly(eval_plot))
  })
  
  # Run K-means Clustering with Distance Calculation and Outlier Detection
  observeEvent(input$run_kmeans, {
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    percentile_threshold <- input$percentile_threshold  # Get the user-defined percentile threshold
    
    # Call the function to perform k-means clustering and update the data table
    df <- kmeans_clustering_with_distances(pca_df, custom_colors, percentile_threshold)
    kmeans_df(df)
    
    output$kmeans_outliers <- renderDT({
      req(kmeans_df())
      datatable(kmeans_df(), options = list(pageLength = 20, autoWidth = TRUE))
    })
    
    output$kmeans_plot <- renderPlotly({
      plot_kmeans <- ggplot(kmeans_df(), aes(x = PC1, y = PC2, color = cluster, text = Sample)) +
        ggtitle("k-means Clustering") +
        geom_point(size = 2) +
        labs(color = "Clusters") +
        xlab("PC1") +
        ylab("PC2") +
        scale_color_manual(values = custom_colors) +
        theme_bw()
      ggplotly(plot_kmeans)
    })
  })
  
  # Hierarchical Clustering Plot
  observeEvent(input$run_hierarchical, {
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    k <- input$num_clusters_hierarchical
    method <- input$clustering_method
    threshold <- input$threshold
    
    hc_res <- perform_hierarchical_clustering(pca_df, cleaned_sequence(), custom_colors, method, k)
    hc_result(hc_res)
    
    # Identify outliers based on the threshold
    hc_res$hierarchical_outliers <- hc_res$hierarchical_outliers %>%
      mutate(Category = ifelse(Height > threshold, "Outlier", "Inlier"))
    
    output$hclust_plot <- renderPlotly({
      hc_res$hclust_plot
    })
    
    output$conf_matrix_plot <- renderPlotly({
      hc_res$conf_matrix_plot
    })
    
    output$dendrogram_plot <- renderPlotly({
      perform_dendrogram(pca_df, cleaned_sequence(), custom_colors, method, threshold)
    })
    
    # Display the complete hierarchical_outliers data frame, including all samples
    output$hierarchical_outliers <- renderDT({
      datatable(hc_res$hierarchical_outliers, options = list(pageLength = 20, autoWidth = TRUE))
    })
  })
  
  
  # Compute and display kNN distance plot
  observeEvent(input$compute_knn, {
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    k <- input$knn
    
    # Debug print
    print(paste("Computing kNN distance plot with k =", k))
    
    output$knn_plot <- renderPlotly({
      perform_kNN_dist_plot(pca_df, k)
    })
  })
  
  # Run DBSCAN
  observeEvent(input$run_dbscan, {
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    eps <- input$eps
    min_pts <- input$min_pts_dbscan
    
    # Debug print
    print(paste("Running DBSCAN with eps =", eps, "and minPts =", min_pts))
    
    dbscan_res <- perform_dbscan_clustering(pca_df, eps, min_pts)
    dbscan_result(dbscan_res)
    
    output$dbscan_plot <- renderPlotly({
      dbscan_res$dbscan_plot
    })
    
    output$dbscan_outliers <- renderDT({
      datatable(dbscan_res$dbscan_outliers[, !names(dbscan_res$dbscan_outliers) %in% c("PC1", "PC2")], options = list(pageLength = 20, autoWidth = TRUE))
    })
  })
  
  # Run HDBSCAN
  observeEvent(input$run_hdbscan, {
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    min_pts <- input$min_pts_hdbscan
    threshold <- input$threshold_hdbscan
    
    print(paste("Running HDBSCAN with minPts =", min_pts))
    
    hdbscan_res <- perform_hdbscan_clustering(pca_df, min_pts)
    hdbscan_outliers(hdbscan_res$hdbscan_outliers)
    
    # Add categorization column based on threshold
    if (nrow(hdbscan_outliers()) > 0 && "OutlierScore" %in% names(hdbscan_outliers())) {
      hdbscan_outliers(hdbscan_outliers() %>%
                         mutate(Category = ifelse(OutlierScore > threshold, "Outlier", "Inlier")))
    } else {
      showNotification("HDBSCAN results are empty or do not contain 'OutlierScore' column.", type = "error")
    }
    
    output$hdbscan_plot <- renderPlotly({
      hdbscan_res$hdbscan_plot
    })
    output$hdbscan_outliers <- renderDT({
      datatable(hdbscan_outliers()[, !names(hdbscan_outliers()) %in% c("PC1", "PC2")], options = list(pageLength = 20, autoWidth = TRUE))
    })
  })
  
  # Run OPTICS
  observeEvent(input$run_optics, {
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    min_pts <- input$min_pts_optics
    eps <- if (is.na(input$eps_optics)) NULL else input$eps_optics
    eps_cl <- input$eps_cl_optics
    
    print(paste("Running OPTICS with minPts =", min_pts, ", eps =", eps, ", and eps_cl =", eps_cl))
    
    tryCatch({
      optics_res <- perform_optics_analysis(pca_df, eps, min_pts, eps_cl, custom_colors)
      optics_results(optics_res)
      
      output$optics_reachability_plot <- renderPlot({
        optics_res$reachability_plot()
      })
      
      output$reachability_plot_threshold <- renderPlot({
        optics_res$reachability_plot_threshold()
      })
      
      output$cluster_plot <- renderPlotly({
        optics_res$cluster_plot()
      })
      
      output$optics_outliers <- renderDT({
        datatable(optics_res$optics_outliers, options = list(pageLength = 20, autoWidth = TRUE))
      })
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      print(paste("Error:", e$message))
    })
  })
  
  # LOF Plot
  observeEvent(input$run_lof, {
    req(pca_result())
    pca_df <- pca_result()$PCA.df
    pca_df$Sample <- rownames(pca_df) # Ensure Sample column is present
    
    threshold <- input$lof_threshold
    k <- input$lof_k
    
    lof_res <- calculate_and_plot_lof(pca_df, threshold = threshold, k = k)
    lof_result(lof_res)
    
    output$lof_plot <- renderPlotly({
      lof_res$lof_plotly
    })
    
    output$lof_od_plot <- renderPlotly({
      lof_res$lof_od_plotly
    })
    
    output$lof_outliers <- renderDT({
      datatable(lof_res$lof_outliers, options = list(pageLength = 20, autoWidth = TRUE))
    })
  })
  
  # Function to remove outliers based on Category column
  remove_outliers <- function(outlier_data) {
    # Identify outliers
    outliers <- outlier_data %>% filter(Category == "Outlier") %>% select(Sample) %>% unlist() %>% as.character()
    
    # Debugging: Print identified outliers
    print("Outliers identified for removal:")
    print(outliers)
    
    # Debugging: Print full outlier data for verification
    print("Full outlier data:")
    print(outlier_data)
    
    # Debugging: Print initial column names of cleaned_data
    print("Initial columns in cleaned_data:")
    print(colnames(cleaned_data()))
    
    # Debugging: Print initial rows in cleaned_sequence
    print("Initial samples in cleaned_sequence:")
    print(cleaned_sequence()$sample)
    
    # Remove outliers from cleaned_data (columns)
    updated_cleaned_data <- cleaned_data()[ , !(colnames(cleaned_data()) %in% outliers)]
    
    # Debugging: Print remaining columns in cleaned_data after outlier removal
    print("Remaining columns in cleaned_data after outlier removal:")
    print(colnames(updated_cleaned_data))
    
    # Remove outliers from cleaned_sequence (rows)
    updated_cleaned_sequence <- cleaned_sequence()[!(cleaned_sequence()$sample %in% outliers), ]
    
    # Debugging: Print remaining samples in cleaned_sequence after outlier removal
    print("Remaining samples in cleaned_sequence after outlier removal:")
    print(updated_cleaned_sequence$sample)
    
    # Update reactive values
    cleaned_data(updated_cleaned_data)
    cleaned_sequence(updated_cleaned_sequence)
    
    # Update the output tables to reflect the removal of outliers
    output$cleaned_data_table <- renderDT({
      req(cleaned_data())
      datatable(cleaned_data(), options = list(pageLength = 20, autoWidth = TRUE), rownames = TRUE)
    })
    
    output$cleaned_sequence_table <- renderDT({
      req(cleaned_sequence())
      datatable(cleaned_sequence(), options = list(pageLength = 20, autoWidth = TRUE))
    })
  }
  
  # Observe events for removing outliers for each clustering method ----
  observeEvent(input$remove_kmeans_outliers, {
    req(kmeans_df())
    print("K-means outlier data before removal:")
    print(kmeans_df())
    remove_outliers(kmeans_df())
    showNotification("K-means outliers removed.", type = "message")
  })
  
  observeEvent(input$remove_hierarchical_outliers, {
    req(hc_result())
    print("Hierarchical outlier data before removal:")
    print(hc_result()$hierarchical_outliers)
    remove_outliers(hc_result()$hierarchical_outliers)
    showNotification("Hierarchical outliers removed.", type = "message")
  })
  
  observeEvent(input$remove_dbscan_outliers, {
    req(dbscan_result())
    print("DBSCAN outlier data before removal:")
    print(dbscan_result()$dbscan_outliers)
    remove_outliers(dbscan_result()$dbscan_outliers)
    showNotification("DBSCAN outliers removed.", type = "message")
  })
  
  observeEvent(input$remove_hdbscan_outliers, {
    req(hdbscan_outliers())
    print("HDBSCAN outlier data before removal:")
    print(hdbscan_outliers())
    remove_outliers(hdbscan_outliers())
    showNotification("HDBSCAN outliers removed.", type = "message")
  })
  
  observeEvent(input$remove_optics_outliers, {
    req(optics_results())
    print("OPTICS outlier data before removal:")
    print(optics_results()$optics_outliers)
    remove_outliers(optics_results()$optics_outliers)
    showNotification("OPTICS outliers removed.", type = "message")
  })
  
  observeEvent(input$remove_lof_outliers, {
    req(lof_result())
    print("LOF outlier data before removal:")
    print(lof_result()$lof_outliers)
    remove_outliers(lof_result()$lof_outliers)
    showNotification("LOF outliers removed.", type = "message")
  })
  
  # Save cleaned data for visualization
  observeEvent(input$save_cleaned_kmeans, {
    cleaned_data_for_visualization(cleaned_data())
    showNotification("Cleaned K-means data saved for visualization.", type = "message")
  })
  
  observeEvent(input$save_cleaned_hierarchical, {
    cleaned_data_for_visualization(cleaned_data())
    showNotification("Cleaned hierarchical data saved for visualization.", type = "message")
  })
  
  observeEvent(input$save_cleaned_dbscan, {
    cleaned_data_for_visualization(cleaned_data())
    showNotification("Cleaned DBSCAN data saved for visualization.", type = "message")
  })
  
  observeEvent(input$save_cleaned_hdbscan, {
    cleaned_data_for_visualization(cleaned_data())
    showNotification("Cleaned HDBSCAN data saved for visualization.", type = "message")
  })
  
  observeEvent(input$save_cleaned_optics, {
    cleaned_data_for_visualization(cleaned_data())
    showNotification("Cleaned OPTICS data saved for visualization.", type = "message")
  })
  
  observeEvent(input$save_cleaned_lof, {
    cleaned_data_for_visualization(cleaned_data())
    showNotification("Cleaned LOF data saved for visualization.", type = "message")
  })
  
  # Generate Heatmap
  observeEvent(input$run_heatmap, {
    req(cleaned_data())
    data <- cleaned_data()
    
    num_top_features <- input$num_top_features  # Get the number of top features from user input
    dendrogram_option <- input$dendrogram_option # Get dendrogram option from user input
    
    # Validate the number of top features
    if (num_top_features > nrow(data)) {
      showNotification("Number of top features exceeds the number of available features. Displaying all features instead.", type = "warning")
      num_top_features <- nrow(data)
    }
    
    # Heatmap Plot
    output$heatmap_plot <- renderPlotly({
      create_heatmap(data, gradient_col, "Heatmap of Cleaned Data", dendrogram_option)
    })
    
    # Selected Features Heatmap Plot
    output$selected_features_heatmap_plot <- renderPlotly({
      selected_features <- select_top_features(data, top_n = num_top_features)  # Use user-specified number of top features
      print(selected_features)  # Debug print
      print(rownames(data))  # Debug print to see the row names of the data
      create_heatmap(data[selected_features, , drop = FALSE], gradient_col, "Heatmap of Selected Features", dendrogram_option)
    })
  })
  
  # Generate Volcano Plot
  observeEvent(input$run_volcano_plot, {
    req(cleaned_data(), cleaned_sequence())
    group1 <- input$group1
    group2 <- input$group2
    
    # Convert thresholds
    log2fc_threshold <- as.numeric(log2(input$log2fc_threshold))
    pval_threshold <- as.numeric(-log10(input$pval_threshold))
    
    if (group1 == group2) {
      showNotification("Please select two different groups for comparison.", type = "error")
      return(NULL)
    }
    
    results <- perform_statistical_testing(cleaned_data(), cleaned_sequence(), group1, group2)
    comparison_key <- paste(group2, "-", group1, sep = "")
    
    if (!comparison_key %in% names(results$comparison_dfs)) {
      showNotification("No significant results for the selected groups.", type = "warning")
      return(NULL)
    }
    
    volcano_df <- results$comparison_dfs[[comparison_key]]
    output$volcano_plot <- renderPlotly({ create_volcano_plot(volcano_df, group1, group2, log2fc_threshold, pval_threshold) })
    
    # Output the data table of upregulated and downregulated features
    output$volcano_table <- renderDT({
      up_down_regulated <- volcano_df %>%
        filter((`p adj` < pval_threshold & Log2FC > log2fc_threshold) | (`p adj` < pval_threshold & Log2FC < -log2fc_threshold)) %>%
        select(Metabolite, Comparison, `p adj`, Log2FC) # Select and reorder columns
      datatable(up_down_regulated, options = list(pageLength = 20, autoWidth = TRUE))
    })
  })
}

shinyApp(ui = ui, server = server)