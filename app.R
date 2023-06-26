# Dependencies
library(shiny)
library(readxl)
library(tidyverse)
library(ComplexHeatmap)
library(plotly)
library(shinyjqui)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fileInput('input_matrix', 'Please select your matrix', multiple = FALSE),
  mainPanel(
    jqui_resizable(
      plotOutput("output_matrix")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Input data
  matrix <- reactive({
    req(input$input_matrix)
    data <- read_excel(input$input_matrix$datapath)
  })
  
  output$output_matrix <- renderPlot({
    req(matrix())
    print(matrix())
    # Construct the matrix with log2 transfored values to get the ~normalized distribution
    normalized_counts_degs_heatmap <- matrix() %>%
      tibble::column_to_rownames(var = 'Geneid') %>% 
      as.matrix() %>% 
      log2()
    # Make the initial values equal 0 to be 0 again
    normalized_counts_degs_heatmap[is.infinite(normalized_counts_degs_heatmap)] <- 0
    # Save the colnames
    column_names <- colnames(normalized_counts_degs_heatmap)
    # Calculate the z-score for a better visualisation
    normalized_counts_degs_heatmap_z <-
      base::apply(normalized_counts_degs_heatmap, 1, scale) %>%
      t() 
    # Bring back colnames
    colnames(normalized_counts_degs_heatmap_z) <- column_names
    # Make NaN values 0 again
    normalized_counts_degs_heatmap_z[is.nan(normalized_counts_degs_heatmap_z)] <- 0
    # Paste the conditions
    #colnames(normalized_counts_degs_heatmap_z) <- paste(meta$condition, meta$samples, sep = "-")
    # Order the heatmap according to conditions
    normalized_counts_degs_heatmap_z_o <-
      normalized_counts_degs_heatmap_z[order(rownames(normalized_counts_degs_heatmap_z)), order(colnames(normalized_counts_degs_heatmap_z))]
    # Create the gradient | heatmaply limits are broken so it will only take into the account limits as max and min matrix values | ComplexHeatmap can fix it but it is a shiny app that makes the markdown needed to be hosted
    gradient_col <- ggplot2::scale_fill_gradient2(
      low = "blue", high = "red", 
      midpoint = 0
    )
    # Sort annotations
    #side_annot_top <- as.character(meta$condition) %>% 
    #  sort()
    # Fix the height and width
    degs_height = nrow(normalized_counts_degs_heatmap_z_o)*0.2
    degs_width = ncol(normalized_counts_degs_heatmap_z_o)*0.32
    
    # Sort annotations
    #side_annot_top <- meta$condition %>% 
    #  sort()
    
    col_fun2 = circlize::colorRamp2(c(-1.5, 0, 1.5),
                                    c('mediumblue', 'white', 'red2'), space = 'sRGB')
    
    print(normalized_counts_degs_heatmap_z_o)
    
    plot <- Heatmap(normalized_counts_degs_heatmap_z_o, name = 'z-score', col = col_fun2,
                            cluster_rows = T, cluster_columns = FALSE, show_row_names = F,
                            border = F, show_column_names = TRUE,column_title = '',
                            row_dend_width = unit(1, 'cm'), heatmap_legend_param = list(legend_height = unit(6, "cm")))
    degs_height_clustered = nrow(normalized_counts_degs_heatmap_z_o) * 0.25
  
    # Display as raster
    plot(plot)
    
    })
    
    
}
  
# Run the application 
shinyApp(ui = ui, server = server)

