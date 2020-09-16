require(shiny)
require(dplyr)
require(leaflet)
require(DT)
require(shinyjs)
require(shinyWidgets)
require(fullcalendar)
require(shinythemes)
require(rmarkdown)
require(knitr)
source("projMan/dataStorage/util.R")

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
                   actionButton(inputId = "submit", "Submit", width = "100%")
                   )
        ),
        fluidRow(
            leafletOutput("map")
        ),
        fluidRow(DT::dataTableOutput("taskTable"))
        ),
tabPanel("View Calendar",
         fluidRow(
           column(6, 
           actionButton(inputId = "refresh", "Sync and Load Calendar", width = '100%'),
           offset = 3
         )
         ),
         fluidRow(
         fullcalendar::fullcalendarOutput("calendar", height = "800px") 
         )
  ),
tabPanel("Upload and Download Media",
         fluidRow(
           helpText(HTML("<b>Only</b> accepts <b>.PNG</b> files")),
           fileInput("upload", "Upload image", accept = "image/png"),
           radioButtons('format', 'Document format', c('HTML', 'PDF', 'Word'),
                        inline = TRUE),
           downloadButton("report", "Generate Property Management Report", class = "btn-success")
         )
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
      fullData$propertyName = as.character(fullData$propertyName)
      
      leafData = dplyr::left_join(fullData, properties, by = "propertyName")
      
      labs <- labsForMap(leafData, "propertyName", "taskType", "assignedTo" )
      
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
    
  
  observeEvent(input$refresh, {
  calendardata = reactiveValues()
  calendardata = loadData()
  
  calendardata$color = dplyr::case_when(
    calendardata$assignedTo == "Michael" ~ "red",
    calendardata$assignedTo == "Nate" ~ "green",
    calendardata$assignedTo == "Cakeo" ~ "blue"
  )
  calendarLabels = labsForCalendar(calendardata, "propertyName", "taskType", "assignedTo" )
  output$calendar = renderFullcalendar({
    caldata = data.frame(title = unlist(calendarLabels),
                      start = calendardata$dateStart,
                      end = calendardata$dateEnd,
                      color = calendardata$color)
    fullcalendar::fullcalendar(caldata, height = "750px")
  })
  
  })
   
  
  
  uploadedImage <- reactive({
    inFile <- input$upload
    if(!is.null(inFile)){
      req(input$upload)
      myImg = normalizePath(inFile$datapath)
      return(myImg)
    }
  })
  
  
  
  
  
  output$report <- downloadHandler(
    filename = function() {
      paste("myReport", sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      shiny::withProgress( message = "Downloading VAB Package..", value = 0,
                           {shiny::incProgress(1/10)
                             shiny::incProgress(2/10)
                             shiny::incProgress(4/10)
                             shiny::incProgress(7/10)
                             shiny::incProgress(8/10)
                             shiny::incProgress(1)
                             
                             src <- normalizePath('projMan/Report.Rmd')
                             
                             owd <- setwd(tempdir())
                             on.exit(setwd(owd))
                             file.copy(src, 'Report.Rmd', overwrite = TRUE) 
                             file.copy(uploadedImage(), 'userImage1.png', overwrite = TRUE)
                             
                             params <- list(
                               test = T
                             )
                             
                             out = rmarkdown::render('Report.Rmd', switch(
                               input$format,
                               PDF = pdf_document(), HTML = html_document(), Word = word_document()
                             ), params = params , envir = new.env(parent = globalenv()))
                             
                             file.rename(out, file)
  
      }
      )
      }
    )
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
