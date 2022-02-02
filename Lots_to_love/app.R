library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)

lots <- read.csv("lots_to_love_cleaned.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Pittsburgh's Lots to Love Project"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(inputId = "filter",
                       label = "Location",
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
                                    "Unknown"))
        ),
    
    mainPanel(
        tabsetPanel(
        tabPanel("Data Table",DT::dataTableOutput(outputId = "table"))  
        #tabPanel("Summary", verbatimTextOutput("summary")), 
        ),
        #DT::dataTableOutput(outputId = "lotstable"),
        #plot1: lots by type
        #plot2: projects implemented over time
        #plot3: projects by project stage
        
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
    
    output$table <- DT::renderDataTable({
        DT::datatable(data = lots_subset(),
                      rownames = FALSE)
    
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
