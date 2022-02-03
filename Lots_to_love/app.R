library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(dplyr)

lots <- read.csv("lots_to_love_cleaned.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Pittsburgh's Lots to Love Project"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "fill_by",
                         label = "Fill histograms by:",
                         choices = c("No fill", "Location")),
      hr(),
      
      checkboxInput(inputId = "display_locations",
                   label = "Display and filter locations"),
      
      conditionalPanel(
        condition = "input.display_locations == 1",
        checkboxGroupInput(inputId = "filter",
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
                                        "Unknown")
                           )

      ),
      
      hr(),
      
      downloadButton(outputId = "download", label = "Download data"),
      
    ),
    
    
    #fill by project status?
    
    mainPanel(
      tabsetPanel(
        tabPanel("Project Types", plotOutput(outputId = "type_graph")),
        tabPanel("Tables",dataTableOutput(outputId = "table"))
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
    #req(input$filter) # ensure availablity of value before proceeding
    filter(lots, location %in% input$filter)
  })
  
  
  output$type_graph <- renderPlot({
    by_location <-
      lots_subset() %>%
      group_by(type) %>%
      summarize(count = n())
    
    ggplot(data = by_location, aes(x = type, y = count)) +
      geom_bar(stat = "identity") +
      theme(axis.text.x = element_text(angle = 45, hjust=1))
  })
  
  
  output$table <- DT::renderDataTable({
    DT::datatable(data = lots_subset()[,2:5],
                  rownames = FALSE)
    
  })
  
  output$download <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(lots_subset(), con)
       })
}

# Run the application 
shinyApp(ui = ui, server = server)
