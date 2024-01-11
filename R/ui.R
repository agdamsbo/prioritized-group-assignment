ui <- shiny::fluidPage(
  ## -----------------------------------------------------------------------------
  ## Application title
  ## -----------------------------------------------------------------------------
  
  shiny::titlePanel("Assign groups based on costs/priorities.",
             windowTitle = "Group assignment calculator"),
  shiny::h5(
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
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::numericInput(
        inputId = "ecxess",
        label = "Excess space",
        value = 1,
        step = .05
      ),
      shiny::p("As default, the program will try to evenly distribute subjects in groups. 
        This factor will add more capacity to each group, for an overall lesser cost, 
        but more uneven group numbers. More adjustments can be performed with the source script."),
      shiny::a(href='https://git.nikohuru.dk/au-phd/PhysicalActivityandStrokeOutcome/src/branch/main/apps/Assignment', "Source", target="_blank"),
      ## -----------------------------------------------------------------------------
      ## File upload
      ## -----------------------------------------------------------------------------
      
      # Input: Select a file ----
      
      shiny::fileInput(
        inputId = "file1",
        label = "Choose main data file",
        multiple = FALSE,
        accept = c(
          ".csv",".xls",".xlsx"
        )
      ),
      shiny::strong("Columns: ID, group1, group2, ... groupN."),
      shiny::strong("NOTE: 0s will be interpreted as lowest score."),
      shiny::p("Cells should contain cost/priorities.
         Lowest score, for highest priority.
        Non-ranked should contain a number (eg lowest score+1).
         Will handle missings but try to avoid."),
      
      shiny::fileInput(
        inputId = "file2",
        label = "Choose data file for pre-assigned subjects",
        multiple = FALSE,
        accept = c(
          ".csv",".xls",".xlsx"
        )
      ),
      shiny::h6("Columns: ID, group"),
      
      
      
      ## -----------------------------------------------------------------------------
      ## Download output
      ## -----------------------------------------------------------------------------
      
      # Horizontal line ----
      tags$hr(),
      
      shiny::h4("Download results"),
      
      # Button
      shiny::downloadButton("downloadData", "Download")
    ),
    
    shiny::mainPanel(shiny::tabsetPanel(
      ## -----------------------------------------------------------------------------
      ## Plot tab
      ## -----------------------------------------------------------------------------
      
      shiny::tabPanel(
        "Summary",
        shiny::h3("Assignment plot"),
        shiny::p("These plots are to summarise simple performance meassures for the assignment. 
          'f' is group fill fraction and 'm' is mean cost in group."),
        
        shiny::plotOutput("assign.plt")
        
      ),
      
      shiny::tabPanel(
        "Results",
        shiny::h3("Raw Results"),
        shiny::p("This is identical to the downloaded file (see panel on left)"),
        
        shiny::htmlOutput("raw.data.tbl", container = span)
        
      ),
      
      shiny::tabPanel(
        "Input data Results",
        shiny::h3("Costs/prioritis overview"),
        
        
        shiny::htmlOutput("input", container = span),
        
        shiny::h3("Pre-assigned groups"),
        shiny::p("Appears empty if none is uploaded."),

        shiny::htmlOutput("pre.assign", container = span)
        
      )
      
    ))
  )
)
