library(shiny)
server <- function(input, output, session) {
  # source("https://git.nikohuru.dk/au-phd/PhysicalActivityandStrokeOutcome/raw/branch/main/side%20projects/assignment.R")
  # source(here::here("R/group_assign.R"))

  dat <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
    # Make laoding dependent of file name extension (file_ext())
    ext <- file_extension(input$file1$datapath)

    tryCatch(
      {
        if (ext == "csv") {
          df <- utils::read.csv(input$file1$datapath,na.strings = c("NA", '""',""))
        } else if (ext %in% c("xls", "xlsx")) {
          df <- openxlsx::read.xlsx(input$file1$datapath,na.strings = c("NA", '""',""))
        } else if (ext == "ods") {
          df <- readODS::read_ods(file = file)
        } else {
          stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx' or '.ods'")
        }
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    return(df)
  })

  dat_pre <- reactive({

    # req(input$file2)
    # Make laoding dependent of file name extension (file_ext())
    if (!is.null(input$file2$datapath)){
      ext <- file_extension(input$file2$datapath)

      if (ext == "csv") {
        df <- utils::read.csv(input$file2$datapath,na.strings = c("NA", '""',""))
      } else if (ext %in% c("xls", "xlsx")) {
        df <- openxlsx::read.xlsx(input$file2$datapath,na.strings = c("NA", '""',""))
      } else if (ext == "ods") {
        df <- readODS::read_ods(file = file)
      } else {
        stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx' or '.ods'")
      }

      return(df)
    } else {
      return(NULL)
    }

  })

  groups <-
    reactive({
      grouped <- prioritized_grouping(
        data = dat(),
        excess_space = input$excess,
        pre_grouped = dat_pre()
      )
      return(grouped)
    })


  plot.overall <- reactive({
    dplyr::case_match(input$overall.plot,
                      "yes"~TRUE,
                      "no"~FALSE,
                      .default=NULL)
  })

  output$raw.data.tbl <- renderTable({
    groups()$export
  })

  output$pre.groups <- renderTable({
    dat_pre()
  })

  output$input <- renderTable({
    dat()
  })

  output$groups.plt <- renderPlot({
    grouping_plot(groups(),overall = plot.overall())
  })

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "prioritized_grouping.csv",

    content = function(file) {
      write.csv(groups()$export, file, row.names = FALSE)
    }
  )

}
