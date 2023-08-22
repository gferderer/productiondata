# Shiny Dashboard
install.packages("shiny")
install.packages("shinydashboard")
install.packages("ggplot2")
install.packages("plotly")

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)

# Create a vector of labels for each Starter category
starter_labels <- unique(brew_data8.18$Starter)

# Calculate the maximum number of observations per starter
max_observations <- max(table(brew_data8.18$Starter))

# Define the UI interface
ui <- dashboardPage(
  dashboardHeader(title = "Starter Quality Dashboard"),
  dashboardSidebar(
    # Add a dropdown input for selecting the minimum number of observations
    selectInput("min_observation", "Minimum Number of Observations", 
                choices = 1:max_observations, selected = 1)
  ),
  dashboardBody(
    fluidRow(
      # Define the width of the chart area (e.g., 8 out of 12 columns)
      column(width = 8,
             box(
               plotlyOutput("combined_plot")
             )
      )
    )
  )
)

# Define a function to create the scaled boxplot
create_scaled_boxplot <- function(data) {
  ggplot(data, aes(x = Starter)) +
    geom_boxplot(aes(y = scale(Taste.Rating.out.of.10), fill = "Taste Rating"), width = 0.5) +
    geom_point(aes(y = scale(Taste.Rating.out.of.10), color = "Taste Rating"), position = position_jitterdodge(jitter.width = 0.01), size = 3) +
    geom_boxplot(aes(y = scale(Starter.ABV), fill = "Starter ABV"), width = 0.5) +
    geom_point(aes(y = scale(Starter.ABV), color = "Starter ABV"), position = position_jitterdodge(jitter.width = 0.01), size = 3) +
    labs(
      x = "Starter",
      y = "Rescaled Values",
      title = "Combined Scaled Boxplots of Taste Rating and Starter ABV"
    ) +
    theme_minimal() +
    scale_fill_manual(
      values = c("Taste Rating" = "lightskyblue", "Starter ABV" = "turquoise3"),
      name = "Variables"  # Set the legend title
    ) +
    scale_color_manual(
      values = c("Taste Rating" = "royalblue1", "Starter ABV" = "turquoise1"),
      name = "Variables"  # Set the legend title
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
}

# Define the server function
server <- function(input, output) {
  # Create a reactive dataset that filters based on the minimum number of observations
  filtered_data <- reactive({
    subset(brew_data8.18, Starter %in% names(which(table(brew_data8.18$Starter) >= input$min_observation)))
  })
  
  # Render the plot using ggplotly
  output$combined_plot <- renderPlotly({
    create_scaled_boxplot(filtered_data())
  })
}

# Run the Shiny Dashboard app
shinyApp(ui, server)