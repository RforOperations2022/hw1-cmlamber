library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(dplyr)
library(stringr)
library(tools)
library(ggwordcloud)

lots <- read.csv("lots_to_love_cleaned.csv")


# Define UI for application that draws a histogram------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("cosmo"),
  
  # Application title-----------------------------------------------------------
  titlePanel("Allegheny County's Lots to Love Projects"),
  
  # Sidebar layout with input and output definitions----------------------------
  sidebarLayout(
    
    sidebarPanel(
      
      # Conditional panel that only shows the x-axis input were applicable------
      conditionalPanel(
        condition="input.tabs == 'Bar Graph'",
        selectInput("x", "Choose a variable for the x axis:",
                  choices = c("Location" = "location",
                              "Needs volunteers" = "need_volunteers",
                              "Project type" = "type", 
                              "Project stage" = "project_stage")),
        
        # Spacing---------------------------------------------------------------
        hr()),
      
      # Input: filter by project type(s)----------------------------------------
      checkboxGroupInput(inputId = "type.filter",
                         label = "Filter by project type(s):",
                         choices = c("Flower Garden", "Food Garden",
                                     "Park Parklet", "Trail Pathway",
                                     "Not specified"),
                         selected = c("Flower Garden", "Food Garden",
                                     "Park Parklet", "Trail Pathway",
                                     "Not specified")),
      
      hr(),
      
      # Input: filter by location(s)--------------------------------------------
      selectInput(inputId = "location.filter",
                           label = "Filter by location(s)",
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
                                        "Unknown"), multiple = TRUE),
      
      hr(),
      
      # Download button---------------------------------------------------------
      downloadButton(outputId = "download", label = "Download filtered data"),
      
    ),
    
    # Output as tabs:-----------------------------------------------------------
    mainPanel(
      tabsetPanel(id = "tabs",
        # Tab 1: Introduction---------------------------------------------------
        tabPanel("Introduction", textOutput("intro.text")),
        # Tab 2: show bar graph with flexible x-axis----------------------------
        tabPanel("Bar Graph", plotOutput(outputId = "flexgraph")),
        # Tab 3: show word cloud based on Partner data--------------------------
        tabPanel("Partner Word Cloud", plotOutput(outputId = "wordcloud")),
        # Tab 4: show line chart------------------------------------------------
        tabPanel("Projects Implemented Over Time", plotOutput(outputId = "lineplot")),
        # Tab 5: show data table------------------------------------------------
        tabPanel("Data Table", dataTableOutput(outputId = "table"))
      )
    )
  )
)

# Define server logic-----------------------------------------------------------
server <- function(input, output) {
  
  # Create subset a subset of data filtering for location-----------------------
  lots.subset <- reactive({
    lots %>%
    filter(location %in% input$location.filter &
             type %in% input$type.filter) })
  
  # Create reactive based on lots.subset for rendering bar graph----------------  
  agg.subset <- reactive({
      lots.subset() %>%
      group_by(across(all_of(input$x))) %>%
      summarize(count = n())
  })

  # Create text for the introduction tab of the app-----------------------------
  output$intro.text <- renderText("Lots to Love is a guide for community 
                                  organizations and residents who are 
                                  interested in transforming vacant lots in
                                  Allegheny County into well-loved spaces. 
                                  This data includes 
                                  vacant lot projects that are implemented, 
                                  in progress, or just an idea. Residents can 
                                  log their projects through Lots to Love and 
                                  vacant lot project records include address, 
                                  description, partners, stage of the project, 
                                  and photos. The data source is the Western
                                  Pennsylvania Regional Data Center 
                                  (https://data.wprdc.org/dataset/lots-to-love).")
  
  
  # Create a bar graph based on aggregated subset-------------------------------
  output$flexgraph <- renderPlot({
    ggplot(data = agg.subset(), aes(y = count)) +
      geom_bar(stat = "identity", aes_string(x = input$x), fill = "cornflowerblue") +
      theme(axis.text.x = element_text(angle = 45, hjust=1, size = 13)) + 
      labs(x = toTitleCase(str_replace_all(input$x, "_", " ")), 
                           y = "Number of Projects",
           title = paste("Projects by", toTitleCase(str_replace_all(input$x, "_", " "))))
  })
  
  # Create a new data frame, grouping subset by partners------------------------
  output$wordcloud <- renderPlot({
    partners.group <-
      lots.subset() %>%
      group_by(partners) %>%
      summarize(count.partners = n())
    
    # Create word cloud based on partners---------------------------------------
    ggplot(partners.group, aes(label = partners, size = count.partners)) +
      geom_text_wordcloud() +
      theme_minimal() +
      scale_size_area(max_size = 16)
    
  })
  
  # Create a new data frame, grouping subset by year_implemented----------------
  output$lineplot <- renderPlot({
    by.year <-
      lots.subset() %>%
      subset(year_implemented != "") %>%
      group_by(year_implemented) %>%
      summarize(year.count = n())
    
    # Create line chart based on year_implemented and project count-------------
    ggplot(data = by.year, aes(x = year_implemented, y = year.count)) +
    geom_point(size = 3) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    theme_bw() +
    labs(x = "Year Implemented", y = "Number of Projects Implemented",
         title = "Projects Implemented Over Time")
  })
  
  # Render data table of lots.subset--------------------------------------------
  output$table <- DT::renderDataTable({
    DT::datatable(data = lots.subset()[,c(2,3,4,5,6,7,8,9,21,22)],
                  rownames = FALSE)
    
  })
  
  
  # Generate file download ---------------------------------------------
  output$download <- downloadHandler(
       filename = function() {
         paste('lots_to_love_data.csv')
       },
       content = function(con) {
         write.csv(lots.subset(), con)
       })
}

# Run the application----------------------------------------------------
shinyApp(ui = ui, server = server)
