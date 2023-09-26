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
starter_labels <- unique(brew_data9.21$Starter)

# Calculate the maximum number of observations per starter
max_observations <- max(table(brew_data9.21$Starter))

# Create a vector of labels for each Starter category
starter_groups <- unique(brew_data9.21$Starter.Group)

# Define the UI interface
ui <- dashboardPage(
  dashboardHeader(title = "Starter Quality Dashboard"),
  dashboardSidebar(
    ## Number of Observations
    # Add a dropdown input for selecting the minimum number of observations
    actionButton("update_plot_button", "Update Plot"), # UPDATE BUTTON
      
    selectInput("min_observation", "Minimum Number of Observations", 
                choices = 1:max_observations, selected = 1),
  
## Selecting Starter Group
    #Action button to select all starter groups
    actionButton("select_all_groups", "Select All Starter Groups"),
    
    # Action button to unselect all starters
    actionButton("unselect_all_groups", "Unselect All Starter Groups"),
    
    # Add a checkboxgroupinput for selecting the starter group
    checkboxGroupInput("starter_group", "Starter Group",
                       choices = 1:length(starter_groups), selected = starter_groups),
  
    
## Select All Starters
    #Action button to select all starters
    actionButton("select_all_starters", "Select All Starters"),
    
    # Action button to unselect all starters
    actionButton("unselect_all_starters", "Unselect All Starters"),
    
    # Add a checkboxgroupinput for selecting the starter identity
    checkboxGroupInput("starter_selection", "Select Starter Identity", 
                choices = starter_labels, selected = starter_labels)),
  
  dashboardBody(
    fluidRow(
      # Top row with combined_plot
      box(
        width = 12,
        plotlyOutput("combined_plot")
      )
      )
    )
  )

# Define a function to create the scaled boxplot
create_scaled_boxplot <- function(data) {
  
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
      values = c("Taste Rating" = "mediumturquoise", "Starter ABV" = "salmon1"),
      name = "Variables"  # Set the legend title
    ) +
    scale_color_manual(
      values = c("Taste Rating" = "turquoise4", "Starter ABV" = "salmon"),
      name = "Variables"  # Set the legend title
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
}


## Server 

server <- function(input, output, session) {
  # Create a reactive dataset that filters based on the minimum number of observations, starter group selected, and starter select
  filtered_data <- reactive({
    # Filter data based on minimum observations
    data <- subset(brew_data9.21, Starter %in% names(which(table(brew_data9.21$Starter) >= input$min_observation)))
    
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
 
  ## UPTATE BUTTON 
  # Define a reactive expression to hold the button click count
  button_click_count <- reactiveVal(0)
  
  # Create an observeEvent to update plots when the button is clicked
  observeEvent(input$update_plot_button, {
    # Increment the button click count to trigger reactive updates
    button_click_count(button_click_count() + 1)
  }) 
    
# Adds functionality to the select/unselect all starters
    observeEvent(input$select_all_starters, {
      # Set all starter selections to selected
      updateCheckboxGroupInput(session, "starter_selection", selected = starter_labels)
    })
    
    observeEvent(input$unselect_all_starters, {
      # Unselect all starter selections
      updateCheckboxGroupInput(session, "starter_selection", selected = character(0))
    })
    
# Adds functionality to the select/unselect all starters groups
    
    observeEvent(input$select_all_groups, {
      # Set all starter selections to selected
      updateCheckboxGroupInput(session, "starter_group", selected = starter_groups)
    })
    
    observeEvent(input$unselect_all_groups, {
      # Unselect all starter selections
      updateCheckboxGroupInput(session, "starter_group", selected = character(0))
    })
    
  
  # Render the plot using ggplotly
  output$combined_plot <- renderPlotly({
    create_scaled_boxplot(filtered_data())
  })
 }

# Trying to add an "UPDATE" button so there is less lag. 
# Run the Shiny Dashboard app
shinyApp(ui, server)