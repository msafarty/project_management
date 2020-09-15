require(shiny)
require(dplyr)
require(leaflet)
require(DT)
require(shinyjs)
require(shinyWidgets)
require(fullcalendar)
require(shinythemes)
source("dataStorage/util.R")

ui <- navbarPage(
    title = "Property Management",
    theme = shinytheme("cosmo"),
    tabPanel("Create/Modify Task",
        fluidRow(
            column(2,
                   selectInput(inputId = "taskType", label = "Type of Task:", choices = taskTypeChoices, selected = NULL)
                   ),
            column(2,
                   shiny::textInput(inputId = "jobDesc", label = "Task Description: ")
                   ),
            column(2, 
                   shiny::dateRangeInput(inputId = "taskDateRange", label = "Task Date Range", start = NULL, end = NULL, min = Sys.Date(), max = NULL, format = "mm-dd-yyyy")
                   ),
            column(2,
                   shiny::selectInput(inputId = "assignedTo", label = "Assign task to: ", choices = assignedToChoices, selected = NULL)
                   ),
            column(2, 
                   shiny::selectInput(inputId = "property", label = "Property: ", choices = propertyChoices, selected = NULL)
                   ),
            column(2,
                   actionButton(inputId = "submit", "Submit")
                   )
        ),
        fluidRow(
            leafletOutput("map")
        ),
        fluidRow(DT::dataTableOutput("taskTable"))
        ),
tabPanel("View Calendar",
    
)
)


server <- function(input, output) {

   # propertyData = reactive({
   #    # properties = properties %>% filter(propertyName == input$property)
   #     req(input$taskType)
   #     req(input$property)
   #   properties[which(properties$propertyName == input$property), "taskType"] = input$taskType 
   # })
   
  observeEvent(input$submit, {
      pdata = data.frame(
          "propertyName" = c(input$property),
          "taskType" = c(input$taskType),
          "jobDesc" = c(input$jobDesc),
          "dateStart" = c(input$taskDateRange[1]),
          "dateEnd" = c(input$taskDateRange[2]),
          "assignedTo" = c(input$assignedTo)
      )
      saveData(pdata, input$property)
      fullData = loadData()
      fullData = fullData[,-c(1)]
      
      leafData = dplyr::left_join(fullData, properties, by = "propertyName")
      
      labs <- labsForPropManagement(leafData, "propertyName", "taskType", "assignedTo" )
      
      output$map = renderLeaflet({
          leaflet(leafData) %>%
              addTiles() %>%
              fitBounds(lng1 = -81.15, lng2 = -81.3, lat1 = 29.557, lat2 = 29.542) %>%
              addCircleMarkers(lng = leafData[,"Lon"], lat = leafData[,"Lat"],  
                               color = "darkred", radius = 6, fillColor = "darkred",
                               fillOpacity = 1, opacity = 1,
                               label = lapply(labs, htmltools::HTML))
      })
      
      output$taskTable = renderDT(
          datatable(fullData)
      )
  })
    
   
   
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)
