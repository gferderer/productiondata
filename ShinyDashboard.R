# Shiny Dashboard
# install.packages("shiny")
# install.packages("shinydashboard")
# install.packages("ggplot2")
# install.packages("plotly")

# library(shiny)
# library(shinydashboard)
# library(ggplot2)
# library(plotly)

### Value Set up
##
# Calculate the maximum number of observations per starter
max_observations <- max(table(brew_data9.21$Starter))

# Create a vector of labels for each Starter category
starter_groups <- unique(brew_data9.21$Starter.Group)

# Vector of labels for each unique Starter and sorts them numerically 
unique_starter <- sort(unique(brew_data9.21$Starter))

### UI
## 
# Define the UI interface
ui <- dashboardPage(
  
# Dashboard header 
  dashboardHeader(title = "Starter Quality Dashboard"),
  
# Dashboard sidebar
  dashboardSidebar(
    
# Update button UI
    actionButton("update_plot_button", "Update Plot"), # UPDATE BUTTON
    
# Pause button UI
    actionButton("pause_plot_button", "Pause Plot"), # Pause BUTTON
    
# Number of Observations  
    # Add a dropdown input for selecting the minimum number of observations
    selectInput("min_observation", "Minimum Number of Observations", 
                choices = 1:max_observations, selected = 1),
  
# Selecting Starter Group
    #Action button to select all starter groups
    actionButton("select_all_groups", "Select All Starter Groups"),
    
    # Action button to unselect all starters
    actionButton("unselect_all_groups", "Unselect All Starter Groups"),
    
    # Add a checkboxgroupinput for selecting the starter group
    checkboxGroupInput("starter_group", "Starter Group",
                       choices = 1:length(starter_groups), selected = starter_groups),
  
# Select All Starters
    #Action button to select all starters
    actionButton("select_all_starters", "Select All Starters"),
    
    # Action button to unselect all starters
    actionButton("unselect_all_starters", "Unselect All Starters"),

# Add a checkboxgroupinput for selecting the starter identity
checkboxGroupInput("starter_selection", "Select Starter Identity",
                   choices = unique_starter, selected = unique_starter)),
    
# Dashboard body
  dashboardBody(
    fluidRow(
      # Top row with combined_plot
      box(
        width = 12,
        plotlyOutput("combined_plot")
      ),
      # Bottom row with two plots sharing equal space
      column(
        width = 6,
        box(
          plotlyOutput("starter_abv_trends")
        )
      ),
      column(
        width = 6,
        box(
          plotlyOutput("starter_taste_trends")
       )
      )
     )
    )
   )

# Define a function to create the scaled boxplot
# Scaled taste rating and ABV
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

# Starter ABV over time scatter plot

# Add a legend with custom colors
starter_abv_trends <- starter_abv_trends + scale_color_manual(values = rainbow(length(unique(brew_data9.21$Starter)))) +
  guides(color = guide_legend(title = "Starter Label"))

# Create the scatter plot for ABV with trendlines
starter_abv_trends <- ggplot(data = brew_data9.21, aes(x = Date.Brewed, y = Starter.ABV, color = Starter)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Date Brewed vs. Starter ABV with Trendlines",
    x = "Date Brewed",
    y = "Starter ABV"
  ) +
  theme_bw()

## Taste by Starter ID over time 
# Add a legend with custom colors
starter_taste_trends <- starter_taste_trends + scale_color_manual(values = rainbow(length(unique(brew_data9.21$Starter)))) +
  guides(color = guide_legend(title = "Starter Label"))

# Create the scatter plot for taste rating with trendlines
starter_taste_trends <- ggplot(data = brew_data9.21, aes(x = Date.Brewed, y = Taste.Rating.out.of.10, color = Starter)) +
  geom_point() +
  labs(
    title = "Scatter Plot of Date Brewed vs. Taste Rating with Trendlines",
    x = "Date Brewed",
    y = "Taste Rating out of 10"
  ) +
  theme_bw()


## Server 
#
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
  
  # Adds functionality to the select/unselect all starters
  observeEvent(input$select_all_starters, {
    # Set all starter selections to selected
    updateCheckboxGroupInput(session, "starter_selection", selected = unique_starter)
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
  
  ## Update button    
  button_click_count <- reactiveVal(0)  
  pause_click_count <- reactiveVal(0)
  
  # Reactive variable that controls whether the plot should be updated
  plot_update_flag <- reactiveVal(FALSE)
  
  # When "Update" button is clicked, button_click_count increments and sets "plot_update_flag" to true  
  observeEvent(input$update_plot_button, {
    # Increment the button click count
    button_click_count(button_click_count() + 1)
    
    # Set the plot_update_flag to TRUE to allow updating the plot
    plot_update_flag(TRUE)
  })
  
  observeEvent(input$pause_plot_button, {
    # Increment the button click count
    pause_click_count(pause_click_count() + 1)
    
    plot_update_flag(FALSE)
  })
  
  # Create a reactive ggplot object that responds to changes in filtered_data and input$starter_selection
  starter_abv_trends <- reactive({
    
    # Use the button_click_count to trigger updates only when the button is clicked
    req(button_click_count())
    
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
  
  # Creates a reactive ggplot object that responds to changes in filtered data
  starter_taste_trends <- reactive({
    
    # Use the button_click_count to trigger updates only when the button is clicked
    req(button_click_count())
    
    ggplot(data = filtered_data(), aes(x = Date.Brewed, y = Taste.Rating.out.of.10, color = Starter)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +  # Add linear regression lines
      labs(
        title = "Scatter Plot of Date Brewed vs. Taste Rating with Trendlines",
        x = "Date Brewed",
        y = "Taste Rating out of 10"
      ) +
      theme_minimal()
  })
  
  # Render combined box plot        
  output$combined_plot <- renderPlotly({
    data_to_plot <- filtered_data()
    
    if (plot_update_flag()) {
      if (!is.null(data_to_plot)) {
        return(create_scaled_boxplot(data_to_plot))
      } else {
        return(NULL)
      }
    } else {
      return(NULL)  # Return NULL to pause the plot rendering
    }
  })
  
# Render the scatter plot with trendlines using ggplotly
  output$starter_abv_trends <- renderPlotly({
    if (plot_update_flag()) {
      data_to_plot <- filtered_data() # Define this line to get the filtered data
      
      if (!is.null(data_to_plot)) {
        return(ggplotly(starter_abv_trends()))
      } else {
        return(NULL)
      }
    } else {
      return(NULL)  # Return NULL to pause the plot rendering
    }
  })
  
# Render the starter_taste_trends scatter plot with trendlines using ggplotly
  
  output$starter_taste_trends <- renderPlotly({
    if (plot_update_flag()) {
      data_to_plot <- filtered_data() # Define this line to get the filtered data
      
      if (!is.null(data_to_plot)) {
        return(ggplotly(starter_taste_trends()))
      } else {
        return(NULL)
      }
    } else {
      return(NULL)  # Return NULL to pause the plot rendering
    }
  })
}

# Run the Shiny Dashboard app
shinyApp(ui, server)