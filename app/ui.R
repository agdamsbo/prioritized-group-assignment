library(shiny)
library(bslib)

panels <- list(
  bslib::nav_panel(
    title = "Input data",
    h3("Costs/priorities overview"),
    htmlOutput("input", container = span),
    conditionalPanel(
      condition = "output.pre_assigned=='yes'",
      h3("Pre-assigned groups"),
      # p("Appears empty if none is uploaded."),

      htmlOutput("pre.groups", container = span)
    )
  ),
  bslib::nav_panel(
    title = "Parsed input",
    htmlOutput("input_parsed")),
  bslib::nav_panel(
    title = "Summary",
    shiny::plotOutput("groups.plt")
  ),
  bslib::nav_panel(
    title = "Results",
    htmlOutput("raw.data.tbl", container = span)
  )
)

# tabPanel(
#   "Input data Results",
#   h3("Costs/prioritis overview"),
#
#
#   htmlOutput("input", container = span),
#   conditionalPanel(
#     condition = "output.preassigned=='yes'",
#     h3("Pre-assigned groups"),
#     # p("Appears empty if none is uploaded."),
#
#     htmlOutput("pre.groups", container = span))
#
# ),
# tabPanel(
#   "Summary",
#   h3("Grouping plot"),
#   p("These plots are to summarise simple performance meassures for the assignment.
#           'f' is group fill fraction and 'm' is mean cost in group."),
#
#   plotOutput("groups.plt")
#
# ),
#
# tabPanel(
#   "Results",
#   h3("Raw Results"),
#   p("This is identical to the downloaded file (see panel on left)"),
#
#   htmlOutput("raw.data.tbl", container = span)
#
# )



ui <- bslib::page_sidebar(
  theme = bslib::bs_theme(bootswatch = "minty"),
  ## -----------------------------------------------------------------------------
  ## Application title
  ## -----------------------------------------------------------------------------

  title = "Group allocation based on individual subject prioritization.",
  window_title = "Prioritized grouping calculator",

  ## -----------------------------------------------------------------------------
  ## Side panel
  ## -----------------------------------------------------------------------------


  ## -----------------------------------------------------------------------------
  ## Single entry
  ## -----------------------------------------------------------------------------
  sidebar = bslib::sidebar(
    open = "open",
    p(
      "Please note this calculator is only meant as a proof of concept for educational purposes,
     and the author will take no responsibility for the results of the calculator.
     Uploaded data is not kept, but please, do not upload any sensitive data."
    ),
    a(href = "https://git.nikohuru.dk/au-phd/PhysicalActivityandStrokeOutcome/src/branch/main/apps/Assignment", "Source", target = "_blank"),
    ## -----------------------------------------------------------------------------
    ## File upload
    ## -----------------------------------------------------------------------------

    # Input: Select a file ----

    fileInput(
      inputId = "file1",
      label = "Choose main data file",
      multiple = FALSE,
      accept = c(
        ".csv", ".xls", ".xlsx", ".ods"
      )
    ),
    strong("Columns: ID, group1, group2, ... groupN."),
    strong("NOTE: 0s will be interpreted as lowest score."),
    # p("Cells should contain cost/priorities.
    #    Lowest score, for highest priority.
    #   Non-ranked should contain a number (eg lowest score+1).
    #    Will handle missings but try to avoid."),
    shiny::conditionalPanel(
      condition = "output.uploaded=='yes'",
      numericInput(
        inputId = "excess",
        label = "Excess space (%)",
        value = 20,
        step = 5
      ),
      p("As default, the program will try to evenly distribute subjects in groups.
        This factor will add more capacity to each group, for an overall lesser cost,
        but more uneven group numbers. More adjustments can be performed with the source script."),
      shiny::radioButtons(
        inputId = "input_type",
        label = "Data input type",
        selected = "default",
        choices = list(
          "Columns of groups" = "default",
          "Columns of priorities (Google)" = "prio",
          "Column of strings (Microsoft)" = "string"
        )
      ),
      shiny::conditionalPanel(
        condition = "input.input_type=='prio'",
        uiOutput("id_var_prio"),
        uiOutput("prio_vars")
      ),
      shiny::conditionalPanel(
        condition = "input.input_type=='string'",
        uiOutput("id_var_string"),
        uiOutput("string_var"),
        shiny::textInput(
          inputId = "string_split",
          label = "Pattern to split string",
          value = ";"
        )
      ),
      shiny::radioButtons(
        inputId = "overall.plot",
        label = "Print overall mean grouping priorities/costs only?",
        selected = "no",
        choices = list(
          "Yes" = "yes",
          "No" = "no"
        )
      ),
      shiny::radioButtons(
        inputId = "pre_assign",
        label = "Add pre-assigned grouping (paedagogical return)?",
        selected = "no",
        choices = list(
          "Yes" = "yes",
          "No" = "no"
        )
      ),
      shiny::conditionalPanel(
        condition = "input.pre_assign=='yes'",
        fileInput(
          inputId = "file2",
          label = "Choose data file for pre-assigned subjects",
          multiple = FALSE,
          accept = c(
            ".csv", ".xls", ".xlsx"
          )
        ),
        h6("Columns: ID, group")
      ),
      ## -----------------------------------------------------------------------------
      ## Download output
      ## -----------------------------------------------------------------------------

      # Horizontal line ----
      tags$hr(),
      h4("Download results"),

      # Button
      downloadButton("downloadData", "Download")
    )
  ),
  bslib::navset_card_underline(
    title = "Data and results",
    panels[[1]],
    panels[[2]],
    panels[[3]],
    panels[[4]]
  )
)
