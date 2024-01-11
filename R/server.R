server <- function(input, output, session) {
  library(dplyr)
  library(tidyr)
  library(ROI)
  library(ROI.plugin.symphony)
  library(ompr)
  library(ompr.roi)
  library(magrittr)
  library(ggplot2)
  library(viridisLite)
  library(patchwork)
  library(openxlsx)
  # source("https://git.nikohuru.dk/au-phd/PhysicalActivityandStrokeOutcome/raw/branch/main/side%20projects/assignment.R")
  source(here::here("R/group_assign.R"))
  
  dat <- shiny::reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    # Make laoding dependent of file name extension (file_ext())
    ext <- file_extension(input$file1$datapath)
    
    if (ext == "csv") {
      df <- read.csv(input$file1$datapath,na.strings = c("NA", '""',""))
    } else if (ext %in% c("xls", "xlsx")) {
      df <- openxlsx::read.xlsx(input$file1$datapath,na.strings = c("NA", '""',""))
      
    } else {
      stop("Input file format has to be either '.csv', '.xls' or '.xlsx'")
    }
    
    return(df)
  })
  
  dat_pre <- shiny::reactive({
    
    # req(input$file2)
    # Make laoding dependent of file name extension (file_ext())
    if (!is.null(input$file2$datapath)){
      ext <- file_extension(input$file2$datapath)
      
      if (ext == "csv") {
        df <- read.csv(input$file2$datapath,na.strings = c("NA", '""',""))
      } else if (ext %in% c("xls", "xlsx")) {
        df <- openxlsx::read.xlsx(input$file2$datapath,na.strings = c("NA", '""',""))
        
      } else {
        stop("Input file format has to be either '.csv', '.xls' or '.xlsx'")
      }
      
      return(df)
    } else {
      return(NULL)
    }

  })
  
  assign <-
    shiny::reactive({
      assigned <- group_assignment(
        ds = dat(),
        excess_space = input$ecxess,
        pre_assign = dat_pre()
      )
      return(assigned)
    })
  
  
  output$raw.data.tbl <- shiny::renderTable({
    assign()$export
  })
  
  output$pre.assign <- shiny::renderTable({
    dat_pre()
  })
  
  output$input <- shiny::renderTable({
    dat()
  })
  
  output$assign.plt <- shiny::renderPlot({
    assignment_plot(assign())
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- shiny::downloadHandler(
    filename = "group_assignment.csv",

    content = function(file) {
      write.csv(assign()$export, file, row.names = FALSE)
    }
  )
  
}
