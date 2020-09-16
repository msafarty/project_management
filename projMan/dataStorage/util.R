### Holds data for storage



taskTypeChoices = c("A/C Repair", "Fix Break", "Paint", "Routine Maintenance")
assignedToChoices = c("Nate", "Michael", "Cakeo")
propertyChoices = c("A", "B", "C", "D", "E", "F", "G", "H")

properties = data.frame("propertyName" = c("A", "B", "C", "D", "E", "F2", "G", "H"),
                        "Lat" = c(29.553948, 29.544885, 29.56, 29.543, 29.545, 29.5501, 29.551, 29.552),
                        "Lon" = c(-81.175219, -81.257373, -81.2, -81.19, -81.21, -81.22, -81.24, -81.25)#,
                        # "taskType" = c("","","","","","","",""),
                        # "jobDesc" = c("","","","","","","",""),
                        # "dateStart" = c("","","","","","","",""),
                        # "dateEnd" = c("","","","","","","",""),
                        # "assignedTo" = c("","","","","","","","")
                        ) 

labsForMap <- function(data, label1, label2, label3) {
  lapply(seq(nrow(data)), function(i) {
  paste0( data[i, label1], '<br/>', 
          data[i, label2], '<br/>',
          data[i, label3])
})
}

labsForCalendar <- function(data, label1, label2, label3) {
  lapply(seq(nrow(data)), function(i) {
    paste0( data[i, label1], ' - ', data[i, label2], ', assigned to: ',
            data[i, label3]
  )
  })
}


outputDir <- "projMan/dataStorage/responses"

saveData <- function(data,propertyName) {
  # data <- t(data)
  # Create a unique file name
  fileName <- paste0(propertyName,".csv")
  # Write the file to the local system
  write.csv(
    x = data,
    file = file.path(outputDir, fileName), 
    row.names = TRUE, quote = TRUE
  )
}

loadData <- function() {
  # Read all the files into a list
  files <- list.files(outputDir, full.names = TRUE)
  data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
  # Concatenate all data together into one data.frame
  data <- do.call(rbind, data)
  return(data)
}
# write.csv(1, "")