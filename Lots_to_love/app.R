library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(dplyr)
library(stringr)
library(tools)

lots <- read.csv("lots_to_love_cleaned.csv")


# Define UI for application that draws a histogram----------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  
  # Application title---------------------------------------------------------
  titlePanel("Pittsburgh's Lots to Love Project"),
  
  # Sidebar layout with input and output definitions--------------------------
  sidebarLayout(
    sidebarPanel(
      
      conditionalPanel(
        condition="input.tabs == 'Flex graph'",
        selectInput("x", "Choose a variable for the x axis:",
                  choices = c("Location" = "location",
                              "Project type" = "type", 
                              "Project stage" = "project_stage")),
        hr()),
      
      
      selectInput(inputId = "filter",
                           label = "Locations",
                           choices = c("Braddock", "Brentwood", "Carnegie", "Castle Shannon", 
                                       "Clairton", "Dormont", "East Pittsburgh", "Edgewood",
                                       "Etna", "Green Tree", "Homestead", "Indiana",
                                       "McKeesport", "Millvale", "Monroeville", "Moon",
                                       "Mount Oliver", "Pitcairn", "Pittsburgh", "Robinson", 
                                       "Ross", "Scott", "Scott/Carnegie", "Shaler", 
                                       "South Park", "Swissvale", "White Oak", "Wilkinsburg",
                                       "Unknown"),
                           selected = c("Braddock", "Brentwood", "Carnegie", "Castle Shannon", 
                                       "Clairton", "Dormont", "East Pittsburgh", "Edgewood",
                                        "Etna", "Green Tree", "Homestead", "Indiana",
                                        "McKeesport", "Millvale", "Monroeville", "Moon",
                                        "Mount Oliver", "Pitcairn", "Pittsburgh", "Robinson", 
                                        "Ross", "Scott", "Scott/Carnegie", "Shaler", 
                                        "South Park", "Swissvale", "White Oak", "Wilkinsburg",
                                        "Unknown"), multiple = TRUE

      ),
      
      hr(),
      
      downloadButton(outputId = "download", label = "Download data"),
      
    ),
    
    
    mainPanel(
      tabsetPanel(id = "tabs",
        tabPanel("Flex graph", plotOutput(outputId = "flex_graph")),
      
        tabPanel("Project Types", plotOutput(outputId = "type_graph")),
        tabPanel("Projects Implemented Over Time", plotOutput(outputId = "line_plot")),
        tabPanel("Data Table", dataTableOutput(outputId = "table"))
        #DT::dataTableOutput(outputId = "lotstable"),
        #plot2: projects implemented over time
        #plot3: projects by project stage
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #Create subset
  lots_subset <- reactive({
    req(input$filter) # ensure availablity of value before proceeding
    filter(lots, location %in% input$filter) })
  
  aggregated <- reactive({
      lots_subset() %>%
      group_by(across(all_of(input$x))) %>%
      summarize(count = n())
  })

  output$flex_graph <- renderPlot({
    ggplot(data = aggregated(), aes(y = count)) +
      geom_bar(stat = "identity", aes_string(x = input$x)) +
      theme(axis.text.x = element_text(angle = 45, hjust=1, size = 13))
  })
  
  
  
  output$line_plot <- renderPlot({
    by_year <-
      lots_subset() %>%
      subset(year_implemented != "") %>%
      group_by(year_implemented) %>%
      summarize(count = n())
    
    ggplot(data = by_year, aes(x = year_implemented, y = count)) +
    geom_point() +
    geom_line() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(x = "Year implemented", y = "Number of projects implemented")
  })
  
  output$type_graph <- renderPlot({
    by_type <-
      lots_subset() %>%
      group_by(type) %>%
      summarize(count = n()) 
    
    ggplot(data = by_type, aes(x = type, y = count)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  })
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(data = lots_subset(),
                  rownames = FALSE)
    
  })
  
  output$download <- downloadHandler(
       filename = function() {
         paste('lots_to_love_data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(lots_subset(), con)
       })
}

# Run the application 
shinyApp(ui = ui, server = server)
