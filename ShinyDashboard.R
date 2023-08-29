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

# Create a vector of labels for each Starter category
starter_groups <- unique(brew_data8.18$Starter.Group)

# Calculate the number of starter groups
max_startergroup <- max(table(brew_data8.18$Starter.Group))

# Define the UI interface
ui <- dashboardPage(
  dashboardHeader(title = "Starter Quality Dashboard"),
  dashboardSidebar(
    # Add a dropdown input for selecting the minimum number of observations
    selectInput("min_observation", "Minimum Number of Observations", 
                choices = 1:max_observations, selected = 1),
  
    
    # Add a checkboxgroupinput for selecting the starter group
    checkboxGroupInput("starter_group", "Starter Group",
                       choices = starter_groups, selected = starter_groups),
    
    #Action button to select all starters
    actionButton("select_all_starters", "Select All Starters"),
    
    # Action button to unselect all starters
    actionButton("unselect_all_starters", "Unselect All Starters"),
    
    # Add a checkboxgroupinput for selecting the starter identity
    checkboxGroupInput("starter_selection", "Select Starter Identity", 
                choices = starter_labels, selected = starter_labels)
 
    ),
  
  
  dashboardBody(
    fluidRow(
      # Define the width of the chart area (e.g., 8 out of 12 columns)
      column(width = 12,
             box(
               plotlyOutput("combined_plot")
             ),
             column(width = 6,  # Add a new column for the scatter plot with trendlines
                    box(
                      plotlyOutput("scatter_plot_with_trendlines")
                    ),
      )
    )
  )
))
# Define a function to create the scaled boxplot
create_scaled_boxplot <- function(data) {
  # Filter out rows with non-finite values
  data <- data[!is.na(data$Taste.Rating.out.of.10) & !is.na(data$Starter.ABV), ]
  
  ggplot(data, aes(x = Starter)) +
    geom_boxplot(aes(y = scale(Taste.Rating.out.of.10), fill = "Taste Rating"), width = 0.5) +
    geom_point(aes(y = scale(Taste.Rating.out.of.10), color = "Taste Rating"), position = position_jitterdodge(jitter.width = 0.01), size = 1) +
    geom_boxplot(aes(y = scale(Starter.ABV), fill = "Starter ABV"), width = 0.5) +
    geom_point(aes(y = scale(Starter.ABV), color = "Starter ABV"), position = position_jitterdodge(jitter.width = 0.01), size = 1) +
    labs(
      x = "Starter",
      y = "Rescaled Values",
      title = "Combined Scaled Boxplots of Taste Rating and Starter ABV"
    ) +
    theme_minimal() +
    scale_fill_manual(
      values = c("Taste Rating" = "lightskyblue", "Starter ABV" = "salmon1"),
      name = "Variables"  # Set the legend title
    ) +
    scale_color_manual(
      values = c("Taste Rating" = "royalblue1", "Starter ABV" = "salmon"),
      name = "Variables"  # Set the legend title
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
}

# Add a legend with custom colors
starter_abv_trends <- starter_abv_trends + scale_color_manual(values = rainbow(length(unique(filtered_data4$Starter)))) +
  guides(color = guide_legend(title = "Starter Label"))

# Create the scatter plot with trendlines
starter_abv_trends <- ggplot(data = brew_data8.18, aes(x = Date.Brewed, y = Starter.ABV, color = Starter)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines
  labs(
    title = "Scatter Plot of Date Brewed vs. Starter ABV with Trendlines",
    x = "Date Brewed",
    y = "Starter ABV"
  ) +
  theme_minimal()


server <- function(input, output, session) {
  # Create a reactive dataset that filters based on the minimum number of observations, starter group selected, and starter select
  filtered_data <- reactive({
    # Filter data based on minimum observations
    data <- subset(brew_data8.18, Starter %in% names(which(table(brew_data8.18$Starter) >= input$min_observation)))
    
    # Filter data based on starter group (checkboxGroupInput allows multiple selections, so we use %in%)
    if (!is.null(input$starter_group) && length(input$starter_group) > 0) {
      data <- data[data$Starter.Group %in% input$starter_group, ]
    }
    
    # Further filter data based on the selected starter identities
    if (!is.null(input$starter_selection) && length(input$starter_selection) > 0) {
      data <- data[data$Starter %in% input$starter_selection, ]
    }
    
    return(data)
  })
  
  # Create a reactive ggplot object that responds to changes in filtered_data and input$starter_selection
  starter_abv_trends <- reactive({
    ggplot(data = filtered_data(), aes(x = Date.Brewed, y = Starter.ABV, color = Starter)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Scatter Plot of Date Brewed vs. Starter ABV with Trendlines",
        x = "Date Brewed",
        y = "Starter ABV"
      ) +
      theme_minimal()
  })
    
    #adds functionality to the select/unselect all starters
    observeEvent(input$select_all_starters, {
      # Set all starter selections to selected
      updateCheckboxGroupInput(session, "starter_selection", selected = starter_labels)
    })
    
    observeEvent(input$unselect_all_starters, {
      # Unselect all starter selections
      updateCheckboxGroupInput(session, "starter_selection", selected = character(0))
    })
  
  
  # Render the plot using ggplotly
  output$combined_plot <- renderPlotly({
    create_scaled_boxplot(filtered_data())
  })
  
  # Render the scatter plot with trendlines using ggplotly
  output$scatter_plot_with_trendlines <- renderPlotly({
    ggplotly(starter_abv_trends())
  })
}



# Run the Shiny Dashboard app
shinyApp(ui, server)