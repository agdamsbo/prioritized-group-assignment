library(shiny)
ui <- fluidPage(
  ## -----------------------------------------------------------------------------
  ## Application title
  ## -----------------------------------------------------------------------------

  titlePanel("Assign groups based on costs/priorities.",
             windowTitle = "Group assignment calculator"),
  h5(
    "Please note this calculator is only meant as a proof of concept for educational purposes,
     and the author will take no responsibility for the results of the calculator.
     Uploaded data is not kept, but please, do not upload any sensitive data."
  ),

  ## -----------------------------------------------------------------------------
  ## Side panel
  ## -----------------------------------------------------------------------------


  ## -----------------------------------------------------------------------------
  ## Single entry
  ## -----------------------------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      numericInput(
        inputId = "ecxess",
        label = "Excess space",
        value = 1,
        step = .05
      ),
      p("As default, the program will try to evenly distribute subjects in groups.
        This factor will add more capacity to each group, for an overall lesser cost,
        but more uneven group numbers. More adjustments can be performed with the source script."),
      a(href='https://git.nikohuru.dk/au-phd/PhysicalActivityandStrokeOutcome/src/branch/main/apps/Assignment', "Source", target="_blank"),
      ## -----------------------------------------------------------------------------
      ## File upload
      ## -----------------------------------------------------------------------------

      # Input: Select a file ----

      fileInput(
        inputId = "file1",
        label = "Choose main data file",
        multiple = FALSE,
        accept = c(
          ".csv",".xls",".xlsx"
        )
      ),
      strong("Columns: ID, group1, group2, ... groupN."),
      strong("NOTE: 0s will be interpreted as lowest score."),
      p("Cells should contain cost/priorities.
         Lowest score, for highest priority.
        Non-ranked should contain a number (eg lowest score+1).
         Will handle missings but try to avoid."),

      fileInput(
        inputId = "file2",
        label = "Choose data file for pre-assigned subjects",
        multiple = FALSE,
        accept = c(
          ".csv",".xls",".xlsx"
        )
      ),
      h6("Columns: ID, group"),



      ## -----------------------------------------------------------------------------
      ## Download output
      ## -----------------------------------------------------------------------------

      # Horizontal line ----
      tags$hr(),

      h4("Download results"),

      # Button
      downloadButton("downloadData", "Download")
    ),

    mainPanel(tabsetPanel(
      ## -----------------------------------------------------------------------------
      ## Plot tab
      ## -----------------------------------------------------------------------------

      tabPanel(
        "Summary",
        h3("Assignment plot"),
        p("These plots are to summarise simple performance meassures for the assignment.
          'f' is group fill fraction and 'm' is mean cost in group."),

        plotOutput("assign.plt")

      ),

      tabPanel(
        "Results",
        h3("Raw Results"),
        p("This is identical to the downloaded file (see panel on left)"),

        htmlOutput("raw.data.tbl", container = span)

      ),

      tabPanel(
        "Input data Results",
        h3("Costs/prioritis overview"),


        htmlOutput("input", container = span),

        h3("Pre-assigned groups"),
        p("Appears empty if none is uploaded."),

        htmlOutput("pre.assign", container = span)

      )

    ))
  )
)
