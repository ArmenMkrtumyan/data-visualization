library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(sf)
library(terra)
library(tmap)
library(mapview)
library(plotly)
library(lubridate)
library(reshape2)

# -------------------------------
# UI Layout
# -------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Water Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction (Metadata)", tabName = "introduction", icon = icon("info")),
      menuItem("Categorical Variables", tabName = "categorical", icon = icon("list")),
      menuItem("Time Series", tabName = "timeseries", icon = icon("chart-line")),
      menuItem("Spatial Data", tabName = "spatial", icon = icon("map"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidRow(
                box(title = "Dataset Introduction", width = 12,
                    p("This dashboard presents water quality data collected from various regions in Armenia.
                       The data includes measurements of key water quality parameters such as BOD5, COD, and NH4,
                       which are used to assess a Pollution Index across different sampling locations."),
                    p(style = "font-size: 0.9em;",
                      "These parameters are crucial indicators of water quality and pollution. The dataset
                      serves as a foundation for analyzing pollution trends and identifying areas that require
                      immediate intervention. Understanding the spatial and temporal patterns of water quality
                      can inform policy-making and conservation efforts.")
                ),
                box(title = "Sampling Distribution Overview", width = 12,
                    p("The Sevan region has the highest number of samples, with a count exceeding 750.
                       This suggests a high level of monitoring or focus on this region, possibly due to its ecological importance."),
                    p(style = "font-size: 0.9em;",
                      "Regions like Northern and Hrazdan are also key monitoring areas, while Southern and Akhuryan regions
                      show fewer samples. These differences in sampling distribution could reflect regional priorities or
                      variations in monitoring resources.")
                ),
                box(title = "Sampling Distribution by Region (Categorical Overview)", width = 12,
                    plotOutput("region_barplot")
                ),
                box(title = "Sampling Locations Overview", width = 12,
                    p("This map shows the distribution of water sampling 
                      locations across Armenia, categorized by region. Each 
                      colored point represents a sampling station, with regions
                      like Sevan and Southern being prominent. The visualization
                      highlights spatial coverage and monitoring density in 
                      key ecological and urban areas.")
                ),
                box(title = "Sampling Locations on Armenia Map", width = 12,
                    plotOutput("armenia_plot", height = "500px"),
                ),
                box(title = "Elevation Data Overview", width = 12,
                    p("This plot illustrates the frequency of water sampling across months. Consistent monitoring is crucial
                      for identifying trends and potential pollution events. Regions with sparse data may need additional focus.")
                ),
                box(title = "Monthly Sampling Frequency", width = 12,
                    plotOutput("monthly_sampling_plot", height = "400px"),
                )
              )
      ),
      
      
      tabItem(tabName = "categorical",
              fluidRow(
                box(title = "Categorical/Continuous Variables", width = 12,
                    p("This tab provides visualizations for categorical and continuous variables related to water quality.")
                )
              ),
              fluidRow(
                box(title = "Select Parameter for Boxplot", width = 4,
                    selectInput(
                      inputId = "parameter",
                      label = "Select a Parameter to Display:",
                      choices = c("Dissolved_Oxygen", "BOD5", "COD", "NH4", "NO3", "PO4"),
                      selected = "Dissolved_Oxygen"
                    ),
                    div(style = "font-size: 0.9em; margin-top: 10px;",
                        p("The boxplots provide a comparative overview of water quality parameters across regions, highlighting key patterns and anomalies. Dissolved oxygen levels are relatively consistent, with Sevan exhibiting slightly higher values, indicating better oxygenation. In contrast, regions like Akhuryan and Hrazdan display greater variability, reflecting localized conditions."),
                        p("Organic pollution, as indicated by BOD5, is generally low across regions, though Akhuryan and Hrazdan reveal occasional spikes, possibly linked to agricultural or industrial runoff. Similarly, COD levels are higher in Akhuryan, suggesting chemical pollution hotspots."),
                        p("Nutrient levels, such as ammonium (NH4) and nitrate (NO3), are mostly low, but Hrazdan stands out with significant variability, hinting at potential nitrogen pollution from fertilizers or wastewater. Phosphate (PO4) levels are uniformly low, though sporadic outliers in Akhuryan and Hrazdan suggest localized phosphorus inputs."),
                        p("Overall, the Sevan and Southern regions display more stable water quality, while Akhuryan and Hrazdan emerge as areas of concern, marked by variability and pollution spikes. These findings underscore the need for targeted monitoring and intervention in pollution-prone regions.")
                    )
                ),
                box(title = "Water Quality Boxplots by Region", width = 8,
                    plotlyOutput(outputId = "boxplot") 
                )
              ),
              fluidRow(
                column(width = 8,
                       
                       box(title = "Nutrients Correlation Heatmap", width = 12,
                           plotlyOutput(outputId = "heatmap_nutrients", height = "500px")
                       )
                ),
                column(width = 4,
                       
                       box(title = "Description", width = 10,
                           p(
                             "The Nutrients Correlation Heatmap displays the pairwise correlations between various nutrient parameters such as Dissolved Oxygen, BOD5, COD, NH4, NO2, NO3, and PO4. 
                             Strong positive correlations (closer to 1) indicate that as one nutrient increases, the other tends to increase as well. 
                             Conversely, strong negative correlations (closer to -1) suggest that as one nutrient increases, the other tends to decrease. 
                             This heatmap helps identify relationships and potential interactions among nutrient levels in water samples."
                           )
                           
                       )
                )
              ),
              fluidRow(
                column(width = 8,
                       box(title = "Metals Correlation Heatmap", width = 12,
                           plotlyOutput(outputId = "heatmap_metals", height = "500px")
                       )
                ),
                column(width = 4,
                       box(title = "Description", width = 10,
                           p("The Metals Correlation Heatmap illustrates the correlations between various metal concentrations including Zn, Cu, Cr, As, Cd, Pb, Ni, Mo, Mn, V, Co, and Fe. 
                             Understanding these correlations is crucial for assessing potential co-contaminations and identifying patterns of metal presence in water bodies. 
                             Positive correlations indicate metals that tend to co-occur, while negative correlations may highlight inverse relationships."
                           )
                           
                       )
                )
              ),
              fluidRow(
                column(width = 8,
                       box(title = "General Parameters Correlation Heatmap", width = 12,
                           plotlyOutput(outputId = "heatmap_general", height = "500px")
                       )
                ),
                column(width = 4,
                       box(title = "Description", width = 10,
                           p("This heatmap represents the relationships between
                             various water quality parameters, helping to 
                             identify how changes in one variable might
                             influence another. Parameters like Suspended_Solids
                             and Be, with near-zero correlations, appear largely
                             independent of the others, meaning their levels are
                             unaffected by changes in the rest of the dataset. 
                             Si's predominantly negative correlations suggest it
                             may play an inverse role in relation to other 
                             chemical or physical properties.")
                           
                       )
                )
              )
      ),
      
      tabItem(tabName = "timeseries",
              fluidRow(
                box(title = "Time Series Analysis", width = 12,
                    "Time series plots and analyses will be displayed here."
                )
              ),
              fluidRow(
                box(title = "Select Parameter for Time Series Analysis", width = 4,
                    selectInput(
                      inputId = "ts_parameter",
                      label = "Select a Parameter to Display:",
                      choices = c("Dissolved_Oxygen", "BOD5", "COD", "NH4"),
                      selected = "Dissolved_Oxygen"
                    ),
                    uiOutput("ts_description")
                ),
                box(title = "Time Series Plot", width = 8,
                    plotlyOutput("time_series_plot", height = "400px")
                )
              )
      ),
      
    
      
      tabItem(tabName = "spatial",
              fluidRow(
                box(title = "Elevation Data Information", width = 12,
                    verbatimTextOutput("elevation_info")
                )
              ),
              fluidRow(
                box(title = "Interactive Pollution Map", width = 12,
                    tmapOutput("pollution_tmap", height = "500px")
                )
              ),
              fluidRow(
                box(title = "Spatial Data Description", width = 12,
                    p("To analyze the relationship between elevation and water
                      quality in Armenia, we processed elevation data from a 
                      Digital Elevation Model (DEM) and matched it with water 
                      sampling station coordinates. We made sure the coordinates
                      are unique and took an average of all the numeric column 
                      for all the unique coordinates. Key water quality 
                      parameters (BOD5, COD, NH4) were normalized to a 0–1 scale
                      to create a Pollution Index, representing overall water 
                      quality. The normalization was done using the difference 
                      of the maximum and minumum values of the columns. This 
                      index was further rescaled and aggregated spatially to 
                      ensure consistency across sampling points. This approach
                      revealed that there is no strong correlation between 
                      elevation and pollution levels. The most polluted areas 
                      are near the capital, Yerevan, and Gyumri.")
                )
              )
      )
      
    )
  )
)

# -------------------------------
# Server Logic
# -------------------------------
server <- function(input, output, session) {
  
  parameter_descriptions <- list(
    "Dissolved_Oxygen" = "Dissolved oxygen (DO) refers to the concentration of oxygen in water, crucial for aquatic health, ecosystem stability, and water usability. Monitoring DO levels is vital for environmental management and sustainable resource use. Seasonal variations in DO are driven by temperature’s effect on oxygen solubility: cooler months retain more oxygen, while warmer months reduce solubility and increase oxygen demand. Higher temperatures also elevate aquatic organisms’ metabolic rates, further lowering DO levels. Proactive management is essential to maintain balance and prevent hypoxic conditions, especially during the stress of warmer months.",
    "BOD5" = "BOD5 measures oxygen use by microorganisms decomposing organic matter over five days. It rises when agricultural runoff or rainfall washes pollutants into water and during warmer months, when higher temperatures boost microbial activity. BOD5 drops in cooler months, as microbial activity slows and increased water flow dilutes pollutants. To reduce BOD5, better agricultural practices and improved wastewater treatment are needed, especially during periods of high agricultural activity.",
    "COD" = "COD measures the oxygen needed to chemically oxidize organic and inorganic matter. Peaks occur in late-year months, often due to agricultural or industrial runoff and reduced water flow, indicating higher pollutant loads and potential stress on aquatic life. Sustained high COD can lead to hypoxic conditions. Early- and mid-year falls likely result from seasonal dilution and fewer pollutants. Late-year peaks may correlate with post-harvest runoff or lower dilution capacity during dry periods.",
    "NH4" = "Ammonium (NH4) serves as an indicator of nitrogen pollution in water bodies, often stemming from agricultural fertilizers, wastewater discharge, and natural decomposition. Elevated NH4 levels can lead to eutrophication, disrupting aquatic ecosystems by causing excessive algal blooms. Seasonal variations typically show higher concentrations in warmer months due to increased agricultural activity and microbial processes, whereas cooler months exhibit reduced levels. Monitoring and controlling NH4 is essential for preserving water quality and preventing ecological damage."
  )
  
  
  # -------------------------------
  # Data Loading and Preprocessing
  # -------------------------------
  
  data_imputed <- reactive({
    req(file.exists("./Data/data_imputed.csv"))
    read.csv("./Data/data_imputed.csv", stringsAsFactors = FALSE)
  })
  
  numeric_data <- reactive({
    req(file.exists("./Data/numeric_data.csv"))
    read.csv("./Data/numeric_data.csv", stringsAsFactors = FALSE)
  })
  
  translation <- c(
    "Սևան" = "Sevan",
    "Հարավային" = "Southern",
    "Արարատյան" = "Araratian",
    "Հյուսիսային" = "Northern",
    "Հրազդան" = "Hrazdan",
    "Ախուրյան" = "Akhuryan"
  )
  
  processed_data <- reactive({
    df <- data_imputed()
    df <- df %>%
      mutate(Region = recode(Region, !!!translation))
    
    df$Date <- as.Date(df$Date, format = "%Y-%m-%d")
    
    df <- df[!is.na(df$Date), ]
    df$YearMonth <- format(df$Date, "%Y-%m")
    
    return(df)
  })
  
  # -------------------------------
  # Generate Monthly Sampling Counts
  # -------------------------------
  
  sampling_counts <- reactive({
    df <- processed_data()
    date_range <- seq(from = floor_date(min(df$Date, na.rm = TRUE), "month"),
                      to = floor_date(max(df$Date, na.rm = TRUE), "month"),
                      by = "month")
    
    full_months <- data.frame(YearMonth = format(date_range, "%Y-%m"))
    counts <- data.frame(table(df$YearMonth))
    colnames(counts) <- c("YearMonth", "Frequency")
    
    sampling_counts_df <- merge(full_months, counts, by = "YearMonth", all.x = TRUE)
    sampling_counts_df$Frequency[is.na(sampling_counts_df$Frequency)] <- 0
    sampling_counts_df$YearMonth <- as.Date(paste0(sampling_counts_df$YearMonth, "-01"))
    
    return(sampling_counts_df)
  })
  
  # -------------------------------
  # Create Monthly Data for Time Series
  # -------------------------------
  
  monthly_data <- reactive({
    df <- processed_data()
    df %>%
      mutate(Month = floor_date(Date, unit = "month")) %>% 
      filter(!is.na(Month)) %>%
      group_by(Month) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE))
  })
  
  # -------------------------------
  # Render Monthly Sampling Frequency Plot
  # -------------------------------
  output$ts_description <- renderUI({
    req(input$ts_parameter)
    selected_description <- parameter_descriptions[[input$ts_parameter]]
    tagList(
      p(style = "font-size: 0.9em; margin-top: 10px;", selected_description)
    )
  })
  
  output$monthly_sampling_plot <- renderPlot({
    
    ggplot(sampling_counts(), aes(x = YearMonth, y = Frequency)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "black") +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "1 month", expand = c(0, 0)) +
      labs(title = "Sampling Frequency Per Month",
           x = "Month",
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # -------------------------------
  # Categorical Variables Plot (in Introduction tab)
  # -------------------------------
  output$region_barplot <- renderPlot({
    ggplot(processed_data(), aes(x = Region)) +
      geom_bar(fill = "skyblue", color = "black") +
      labs(title = "Distribution of Sampling by Region", x = "Region", y = "Count") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$armenia_plot <- renderPlot({
    df <- processed_data()
    req(all(c("X", "Y") %in% names(df)))
    
    req(file.exists("./Data/Armenia_Marzes/Armenia_Marzes.shp"))
    armenia_map <- st_read("./Data/Armenia_Marzes/Armenia_Marzes.shp", quiet = TRUE)
    armenia_map <- st_transform(armenia_map, crs = 4326)
    
    sampling_sf <- st_as_sf(df, coords = c("X", "Y"), crs = 4326)
    
    ggplot() +
      geom_sf(data = armenia_map, fill = "lightgrey", color = "black") +
      geom_sf(data = sampling_sf, aes(color = Region), size = 2, alpha = 0.7) +
      labs(title = "Sampling Locations on Armenia Map", color = "Region") +
      theme_minimal()
  })
  
  # -------------------------------
  # Spatial Data Tmap (Pollution map)
  # -------------------------------
  output$pollution_tmap <- renderTmap({
    df <- processed_data()
    req(file.exists("./Data/ArmDEM.tif"))
    elevation_data <- rast("./Data/ArmDEM.tif")
    elevation_data <- setMinMax(elevation_data)
    
    aggregation_factor <- 16
    elevation_data_agg <- aggregate(elevation_data, fact = aggregation_factor, fun = mean)
    
    elevation_poly <- as.polygons(elevation_data_agg)
    names(elevation_poly)[1] <- "Elevation"
    
    elevation_poly_sf <- st_as_sf(elevation_poly)
    
    stations_sf <- st_as_sf(df, coords = c("X", "Y"), crs = 4326)
    stations_sf <- st_transform(stations_sf, crs = 32638)
    
    elev_vals <- terra::extract(elevation_data, vect(stations_sf))
    stations_sf$Elevation <- elev_vals$ArmDEM
    
    selected_params <- c("BOD5", "COD", "NH4")
    stopifnot(all(selected_params %in% names(stations_sf)))
    
    for (param in selected_params) {
      p_min <- min(stations_sf[[param]], na.rm = TRUE)
      p_max <- max(stations_sf[[param]], na.rm = TRUE)
      stations_sf[[paste0(param, "_norm")]] <- (stations_sf[[param]] - p_min) / (p_max - p_min)
    }
    
    stations_df <- st_drop_geometry(stations_sf)
    coords <- st_coordinates(stations_sf)
    stations_df$X <- coords[,1]
    stations_df$Y <- coords[,2]
    
    aggregated_df <- stations_df %>%
      group_by(X, Y) %>%
      summarise(across(where(is.numeric), mean, na.rm = TRUE))
    
    aggregated_sf <- st_as_sf(aggregated_df, coords = c("X", "Y"), crs = 32638)
    
    normalized_cols <- paste0(selected_params, "_norm")
    aggregated_sf$Pollution_Index <- rowMeans(st_drop_geometry(aggregated_sf)[, normalized_cols], na.rm = TRUE)
    
    p_idx_min <- min(aggregated_sf$Pollution_Index, na.rm = TRUE)
    p_idx_max <- max(aggregated_sf$Pollution_Index, na.rm = TRUE)
    aggregated_sf$Pollution_Index_Rescaled <- (aggregated_sf$Pollution_Index - p_idx_min) / (p_idx_max - p_idx_min)
    
    tmap_mode("view")
    tm_shape(elevation_data) +
      tm_raster(palette = "viridis", title = "Elevation (m)") +
      tm_shape(aggregated_sf) +
      tm_symbols(
        col = "Pollution_Index_Rescaled",
        palette = "-RdYlBu",
        size = 0.5,
        title.col = "Pollution Index",
        style = "cont"
      ) +
      tm_layout(
        main.title = "Aggregated Relative Pollution Levels",
        legend.outside = TRUE
      )
  })
  
  # -------------------------------
  # Integrated Boxplot Functionality
  # -------------------------------
  
  output$boxplot <- renderPlotly({
    df <- processed_data()
    req(input$parameter)
    
    if(!(input$parameter %in% names(df))){
      return(plotly::plotly_empty(type = "scatter", mode = "markers") %>%
               plotly::layout(title = paste("Parameter", input$parameter, "not found in the dataset.")))
    }
    
    p <- ggplot(df, aes(x = Region, y = .data[[input$parameter]])) +
      geom_boxplot(fill = "steelblue", color = "black", outlier.colour = "red", 
                   outlier.shape = 16, outlier.size = 2) +
      labs(
        title = paste(input$parameter, "by Region"),
        x = "Region", 
        y = input$parameter
      ) +
      theme_minimal()
    
    ggplotly(p) %>%
      layout(hovermode = "closest")
  })
  
  # -------------------------------
  # Integrated Heatmap Functionality
  # -------------------------------
  
  create_interactive_heatmap <- function(group_vars, group_name) {
    df <- processed_data()
    missing_vars <- setdiff(group_vars, names(df))
    if(length(missing_vars) > 0){
      stop(paste("The following variables are missing in the dataset:", paste(missing_vars, collapse = ", ")))
    }
    
    group_data <- df[, group_vars, drop = FALSE]
    
    numeric_group_data <- group_data %>% select(where(is.numeric))
    if(ncol(numeric_group_data) < 2){
      stop(paste("Not enough numeric variables in", group_name, "to compute correlations."))
    }
    
    cor_matrix <- round(cor(numeric_group_data, use = "pairwise.complete.obs"), 2)
    cor_data <- melt(cor_matrix)
    
    cor_data <- cor_data[cor_data$Var1 != cor_data$Var2, ]
    
    cor_data <- cor_data[as.character(cor_data$Var1) < as.character(cor_data$Var2), ]
    
    heatmap_plot <- ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                           midpoint = 0, limit = c(-1, 1), name = "Correlation") +
      labs(title = paste("Heatmap of", group_name, "Correlations"),
           x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
    ggplotly(heatmap_plot)
  }
  
  nutrients <- c("Dissolved_Oxygen", "BOD5", "COD", "NH4", "NO2", "NO3", "PO4")
  metals <- c("Zn", "Cu", "Cr", "As", "Cd", "Pb", "Ni", "Mo", "Mn", "V", "Co", "Fe")
  general <- c("Ca", "Mg", "Ba", "Be", "K", "Na", "Cl", "SO4", "Si", "TDS", "EC_microS_cm", "Hardness", "Suspended_Solids")
  
  output$heatmap_nutrients <- renderPlotly({
    tryCatch({
      create_interactive_heatmap(nutrients, "Nutrients")
    }, error = function(e){
      plotly::plotly_empty(type = "scatter", mode = "markers") %>%
        plotly::layout(title = paste("Error creating Nutrients Heatmap:", e$message))
    })
  })
  
  output$heatmap_metals <- renderPlotly({
    tryCatch({
      create_interactive_heatmap(metals, "Metals")
    }, error = function(e){
      plotly::plotly_empty(type = "scatter", mode = "markers") %>%
        plotly::layout(title = paste("Error creating Metals Heatmap:", e$message))
    })
  })
  
  output$heatmap_general <- renderPlotly({
    tryCatch({
      create_interactive_heatmap(general, "General Parameters")
    }, error = function(e){
      plotly::plotly_empty(type = "scatter", mode = "markers") %>%
        plotly::layout(title = paste("Error creating General Parameters Heatmap:", e$message))
    })
  })
  
  # -------------------------------
  # Time Series Plot
  # -------------------------------
  output$time_series_plot <- renderPlotly({
    req(input$ts_parameter)
    
    plot_data <- monthly_data()
    
    if(!(input$ts_parameter %in% names(plot_data))){
      return(plotly::plotly_empty(type = "scatter", mode = "markers") %>%
               plotly::layout(title = paste("Parameter", input$ts_parameter, "not found in the dataset.")))
    }
    
    p <- ggplot(plot_data, aes(x = Month, y = .data[[input$ts_parameter]])) + 
      geom_line(color = "blue") +
      geom_point(color = "darkblue") +
      scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
      labs(
        x = "Date",
        y = input$ts_parameter,
        title = paste("Time Series Analysis of", input$ts_parameter)
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
    
    ggplotly(p)
  })
  
  # -------------------------------
  # PCA & Clustering Analysis
  # -------------------------------
  
  pca_clustering <- reactive({
    req(numeric_data(), data_imputed())
    
    numeric_df <- numeric_data()
    numeric_df <- numeric_df[sapply(numeric_df, is.numeric)]
    
    numeric_scaled <- scale(numeric_df)
    pca_result <- prcomp(numeric_scaled, center = TRUE, scale. = TRUE)
    optimal_k <- 3
    set.seed(123)
    kmeans_result <- kmeans(numeric_scaled, centers = optimal_k, nstart = 25)
    
    pca_scores <- as.data.frame(pca_result$x)
    data_pca <- data_imputed() %>%
      bind_cols(pca_scores) %>%
      mutate(Cluster = as.factor(kmeans_result$cluster))
    
    list(
      pca_result = pca_result,
      data_pca = data_pca,
      kmeans_result = kmeans_result
    )
  })
  
  output$pca_summary <- renderPrint({
    req(pca_clustering())
    summary(pca_clustering()$pca_result)
  })
  
  output$scree_plot <- renderPlotly({
    req(pca_clustering())
    pca_result <- pca_clustering()$pca_result
    scree_plot <- fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50)) +
      ggtitle("Scree Plot: Variance Explained by Principal Components")
    ggplotly(scree_plot)
  })
  
  output$pca_biplot <- renderPlotly({
    req(pca_clustering())
    pca_result <- pca_clustering()$pca_result
    data_pca <- pca_clustering()$data_pca
    
    if(!"Cluster" %in% names(data_pca)){
      return(plotly::plotly_empty(type = "scatter", mode = "markers") %>%
               plotly::layout(title = "Cluster information is missing."))
    }
    
    biplot <- fviz_pca_biplot(
      pca_result, 
      geom.ind = "point", 
      pointshape = 21, 
      pointsize = 2,
      fill.ind = data_pca$Cluster,
      col.var = "black",
      arrowsize = 0.8,
      repel = TRUE
    ) +
      scale_fill_manual(values = c("#E69F00", "#56B4E9", "#009E73")) +
      labs(title = "PCA Biplot with Clusters", fill = "Cluster") +
      theme_minimal()
    
    ggplotly(biplot, tooltip = c("x", "y", "fill")) %>%
      layout(legend = list(title = list(text = '<b>Cluster</b>')))
  })
  
  output$cluster_region_table <- renderTable({
    req(pca_clustering())
    data_pca <- pca_clustering()$data_pca
    table(data_pca$Cluster, data_pca$Region)
  })
  
}

shinyApp(ui = ui, server = server)
