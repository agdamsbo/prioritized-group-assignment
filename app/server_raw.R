library(shiny)
server <- function(input, output, session) {
  # source("https://git.nikohuru.dk/au-phd/PhysicalActivityandStrokeOutcome/raw/branch/main/side%20projects/assignment.R")
  # source(here::here("R/group_assign.R"))

  v <- shiny::reactiveValues(
    ds = NULL,
    pre = NULL
  )

  dat <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
    # Make laoding dependent of file name extension (file_ext())
    out <- read_input(input$file1$datapath)
    v$ds <- "loaded"
    return(out)
  })

  dat_parsed <- reactive({
    req(input$file1)
    if (input$input_type == "default") {
      out <- dat()
    } else if (input$input_type == "prio") {
      req(input$id_var_prio)
      req(input$prio_vars)

      out <- parse_prio_form(
        data = dat(),
        id = input$id_var_prio,
        prio.cols = input$prio_vars
      )
    } else if (input$input_type == "string") {
      req(input$id_var_string)
      req(input$string_var)

      out <- parse_string_form(
        data = dat(),
        id = input$id_var_string,
        string.col = input$string_var,
        pattern = input$string_split
      )
    }
    return(out)
  })


  output$id_var_prio <- shiny::renderUI({
    selectInput(
      inputId = "id_var_prio",
      selected = 1,
      label = "ID column",
      choices = colnames(dat()),
      multiple = FALSE
    )
  })

  output$id_var_string <- shiny::renderUI({
    selectInput(
      inputId = "id_var_string",
      selected = 1,
      label = "ID column",
      choices = colnames(dat()),
      multiple = FALSE
    )
  })

  output$prio_vars <- shiny::renderUI({
    selectizeInput(
      inputId = "prio_vars",
      selected = NULL,
      label = "Priority columns (select from first to lowest)",
      choices = colnames(dat())[-match(input$id_var_prio, colnames(dat()))],
      multiple = TRUE
    )
  })

  output$string_var <- shiny::renderUI({
    selectizeInput(
      inputId = "string_var",
      selected = NULL,
      label = "Column of strings",
      choices = colnames(dat())[-match(input$id_var_string, colnames(dat()))],
      multiple = FALSE
    )
  })

  dat_pre <- reactive({
    # req(input$file2)
    # Make laoding dependent of file name extension (file_ext())
    if (!is.null(input$file2$datapath)) {
      out <- read_input(input$file2$datapath)
    } else {
      out <- NULL
    }
    v$pre <- "loaded"
    return(out)
  })

  groups <-
    reactive({
      grouped <- prioritized_grouping(
        data = dat_parsed(),
        excess_space = input$excess,
        pre_grouped = dat_pre()
      )
      return(grouped)
    })


  plot.overall <- reactive({
    dplyr::case_match(input$overall.plot,
      "yes" ~ TRUE,
      "no" ~ FALSE,
      .default = NULL
    )
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

  output$input_parsed <- renderTable({
    dat_parsed()
  })

  output$groups.plt <- renderPlot({
    grouping_plot(groups(), overall = plot.overall())
  })


  output$uploaded <- shiny::reactive({
    if (is.null(v$ds)) {
      "no"
    } else {
      "yes"
    }
  })

  output$pre_assigned <- shiny::reactive({
    if (is.null(v$pre)) {
      "no"
    } else {
      "yes"
    }
  })

  shiny::outputOptions(output, "uploaded", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "pre_assigned", suspendWhenHidden = FALSE)

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = "prioritized_grouping.ods",
    content = function(file) {
      readODS::write_ods(as.data.frame(groups()$export), file)
    }
  )
}
