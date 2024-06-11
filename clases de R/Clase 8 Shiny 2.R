# Shiny application from: https://shiny.rstudio.com/tutorial/written-tutorial/lesson6/
# Demonstration of reactive expressions

# Load required packages
library(shiny)
library(gapminder)
library(ggplot2)
library(dplyr)
library(plotly)

# Define UI for the dashboard
ui <- fluidPage(
  
  # Set title
  titlePanel("Gapminder Dashboard"),
  
  # Set sidebar layout
  sidebarLayout(
    
    # Set sidebar panel
    sidebarPanel(
      # Create input for selecting year
      selectInput(inputId = "year", label = "Select year",
                  choices = unique(gapminder$year),
                  selected = 1952),
      
      # Create input for selecting country
      selectInput(inputId = "country", label = "Select country",
                  choices = c("All", unique(gapminder$continent)),
                  selected = "All")
    ),
    
    # Set main panel
    mainPanel(
      # Create 3 plots
      plotlyOutput("plot1"),
      plotlyOutput("plot2"),
      plotlyOutput("plot3")
    )
  )
)

# Define server for the dashboard
server <- function(input, output) {
  
  # Create reactive data frame based on user input
  filtered_data <- reactive({
    gapminder %>%
      filter(year == input$year) %>%
      filter(ifelse(input$country == "All", TRUE, country == input$country))
  })
  
  # Create plot 1 based on filtered data
  output$plot1 <- renderPlotly({
    ggplotly(ggplot(data = filtered_data(), aes(x = gdpPercap, y = lifeExp, color = country)) +
               geom_point(size = 4) +
               labs(x = "GDP per capita", y = "Life expectancy", color = "Country") +
               theme_bw())
  })
  
  # Create plot 2 based on filtered data
  output$plot2 <- renderPlotly({
    ggplotly(ggplot(data = filtered_data(), aes(x = country, y = gdpPercap)) +
               geom_boxplot(fill = "lightblue") +
               labs(x = "Continent", y = "GDP per capita") +
               theme_bw())
  })
  
  # Create plot 3 based on filtered data
  output$plot3 <- renderPlotly({
    ggplotly(ggplot(data = filtered_data(), aes(x = country, y = pop)) +
               geom_bar(stat = "identity", fill = "lightblue") +
               labs(x = "Continent", y = "Population") +
               theme_bw())
  })
}

# Run the dashboard
shinyApp(ui = ui, server = server)

