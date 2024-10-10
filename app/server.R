

########
#### Current file: R//prioritized_grouping.R 
########

utils::globalVariables(c("group", "grp", "i", "j", "value"))




















prioritized_grouping <-
  function(data,
           cap_classes = NULL,
           excess_space = 20,
           pre_grouped = NULL,
           seed = 6293812) {
    set.seed(seed = seed)
    # browser()
    requireNamespace("ROI")
    requireNamespace("ROI.plugin.symphony")

    if (!is.data.frame(data)) {
      stop("Supplied data has to be a data frame, with each row
           are subjects and columns are groups, with the first column being
           subject identifiers")
    }

    ## This program very much trust the user to supply correctly formatted data
    cost <- t(data[, -1]) # Transpose converts to matrix
    colnames(cost) <- data[, 1]

    nms_groups <- rownames(cost)
    num_groups <- dim(cost)[1]
    num_sub <- dim(cost)[2]

    ## Adding the option to introduce a bit of head room to the classes by
    ## the groups to a little bigger than the smallest possible
    ## Default is to allow for an extra 20 % fill
    excess <- 1 + (excess_space / 100)

    # generous round up of capacities
    if (is.null(cap_classes)) {
      capacity <- rep(ceiling(excess * num_sub / num_groups), num_groups)
      # } else if (!is.numeric(cap_classes)) {
      #   stop("cap_classes has to be numeric")
    } else if (length(cap_classes) == 1) {
      capacity <- ceiling(rep(cap_classes, num_groups) * excess)
    } else if (length(cap_classes) == num_groups) {
      capacity <- ceiling(cap_classes * excess)
    } else {
      stop("cap_classes has to be either length 1 or same as number of groups")
    }

    ## This test should be a little more elegant
    ## pre_grouped should be a data.frame or matrix with an ID and group column
    with_pre_grouped <- FALSE
    if (!is.null(pre_grouped)) {
      # Setting flag for later and export list
      with_pre_grouped <- TRUE

      # Simple translation to allow pre_grouped to denote indices
      if (is.numeric(pre_grouped[, 2])){
        pre_grouped$pre.groups <- nms_groups[pre_grouped[, 2]]
      } else {
        pre_grouped$pre.groups <- as.character(pre_grouped[, 2])
      }

      # Splitting to list for later merging
      pre <- split(
        pre_grouped[, 1],
        factor(pre_grouped[, 3], levels = nms_groups)
      )
      # Subtracting capacity numbers, to reflect already filled spots
      capacity <- capacity - lengths(pre)
      # Making sure pre_grouped are removed from main data set
      data <- data[!data[[1]] %in% pre_grouped[[1]], ]

      cost <- t(data[, -1])
      colnames(cost) <- data[, 1]

      num_groups <- dim(cost)[1]
      num_sub <- dim(cost)[2]
    }

    ## Simple NA handling. Better to handle NAs yourself!
    cost[is.na(cost)] <- num_groups

    i_m <- seq_len(num_groups)
    j_m <- seq_len(num_sub)

    m <- ompr::MIPModel() |>
      ompr::add_variable(grp[i, j],
        i = i_m,
        j = j_m,
        type = "binary"
      ) |>
      ## The first constraint says that group size should not exceed capacity
      ompr::add_constraint(ompr::sum_expr(grp[i, j], j = j_m) <= capacity[i],
        i = i_m
      ) |>
      ## The second constraint says each subject can only be in one group
      ompr::add_constraint(ompr::sum_expr(grp[i, j], i = i_m) == 1, j = j_m) |>
      ## The objective is set to minimize the cost of the grouping
      ## Giving subjects the group with the highest possible ranking
      ompr::set_objective(
        ompr::sum_expr(
          cost[i, j] * grp[i, j],
          i = i_m,
          j = j_m
        ),
        "min"
      ) |>
      ompr::solve_model(ompr.roi::with_ROI(solver = "symphony", verbosity = 1))

    if (m$status == "error") {
      stop("The algorithm is not able to solve the problem. Please adjust the
           constraints by increasing group capacities and/or excess fill")
    }

    ## Getting groups
    solution <- ompr::get_solution(m, grp[i, j]) |> dplyr::filter(value > 0)

    grouped <- solution |> dplyr::select(i, j)

    if (!is.null(rownames(cost))) {
      grouped$i <- rownames(cost)[grouped$i]
    }

    if (!is.null(colnames(cost))) {
      grouped$j <- colnames(cost)[grouped$j]
    }

    ## Splitting into groups based on groups
    grouped_ls <- split(grouped$j, grouped$i)


    ## Extracting subject cost for the final groups for evaluation
    if (is.null(rownames(cost))) {
      rownames(cost) <- seq_len(nrow(cost))
    }

    if (is.null(colnames(cost))) {
      colnames(cost) <- seq_len(ncol(cost))
    }

    evaluated <- lapply(seq_len(length(grouped_ls)), function(i) {
      ndx <- match(names(grouped_ls)[i], rownames(cost))
      cost[ndx, grouped_ls[[i]]]
    })
    names(evaluated) <- names(grouped_ls)

    if (with_pre_grouped) {
      names(pre) <- names(grouped_ls)
      grouped_all <- mapply(c, grouped_ls, pre, SIMPLIFY = FALSE)

      out <- list(all_grouped = grouped_all)
    } else {
      out <- list(all_grouped = grouped_ls)
    }

    export <- do.call(rbind, lapply(seq_along(out[[1]]), function(i) {
      cbind("ID" = out[[1]][[i]], "Group" = names(out[[1]])[i])
    }))

    out <- c(
      out,
      list(
        evaluation = evaluated,
        groupings = grouped_ls,
        solution = solution,
        capacity = capacity,
        excess = excess,
        pre_grouped = with_pre_grouped,
        cost_scale = levels(factor(cost)),
        input = data,
        export = export
      )
    )
    # exists("excess")

    class(out) <- c("prioritized_groups_list", class(out))

    return(out)
  }
























grouping_plot <- function(data,
                          columns = NULL,
                          overall = FALSE,
                          viridis.option="D",
                          viridis.direction=-1) {
  assertthat::assert_that("prioritized_groups_list" %in% class(data))

  dl <- data[[2]]
  cost_scale <- unique(data[[8]])
  cap <- data[[5]]
  cnts_ls <- lapply(dl, function(i) {
    factor(i, levels = cost_scale)
  })

  y_max <- max(lengths(dl))

  if (overall) {
    ds <- tibble::tibble(
      group = seq_along(dl),
      mean = round(Reduce(c, lapply(dl, mean)), 1)
    )
    out <- ds |>
      ggplot2::ggplot(ggplot2::aes(x = group, y = mean, fill = mean)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::geom_hline(yintercept = 1) +
      ggplot2::scale_fill_viridis_c(option=viridis.option,
                                    direction = viridis.direction) +
      ggplot2::guides(fill = "none") +
      ggplot2::scale_x_continuous(name = "Groups", breaks = ds$group) +
      ggplot2::ylab("Mean priority/cost") +
      ggplot2::labs(
        title = "Overall group-wise mean priority/cost of groupings",
        subtitle = "Horizontal line marking the perfect mean=1 for reference"
      )
  } else {
    out <- lapply(seq_along(dl), function(i) {
      ttl <- names(dl)[i]
      ns <- length(dl[[i]])
      cnts <- cnts_ls[[i]]
      ggplot2::ggplot() +
        ggplot2::geom_bar(ggplot2::aes(cnts, fill = cnts)) +
        ggplot2::scale_x_discrete(
          name = NULL,
          breaks = cost_scale,
          drop = FALSE
        ) +
        ggplot2::scale_y_continuous(name = NULL, limits = c(0, y_max)) +
        ggplot2::scale_fill_manual(
          values = viridisLite::viridis(length(cost_scale),
                                        direction = viridis.direction,
                                        option = viridis.option)
        ) +
        ggplot2::guides(fill = "none") +
        ggplot2::labs(
          title =
            paste0(
              ttl, " (fill=", round(ns / cap[[i]], 1), ";n=", ns, ";mean=",
              round(mean(dl[[i]]), 1), ")"
            )
        )
    }) |>
      patchwork::wrap_plots(ncol = columns)
  }

  return(out)
}













plot.prioritized_groups_list <- function(data, ...) {
  grouping_plot(data, ...)
}

## Helper function for Shiny









file_extension <- function(filenames) {
  sub(
    pattern = "^(.*\\.|[^.]+)(?=[^.]*)",
    replacement = "",
    filenames, perl = TRUE
  )
}












read_input <- function(file, consider.na = c("NA", '""', "")) {
  ext <- file_extension(file)

  tryCatch(
    {
      if (ext == "csv") {
        df <- utils::read.csv(file = file, na = consider.na)
      } else if (ext %in% c("xls", "xlsx")) {
        df <- openxlsx2::read_xlsx(file = file, na.strings = consider.na)
      } else if (ext == "dta") {
        df <- haven::read_dta(file = file)
      } else if (ext == "ods") {
        df <- readODS::read_ods(file = file)
      } else {
        stop("Input file format has to be on of:
             '.csv', '.xls', '.xlsx', '.dta' or '.ods'")
      }
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(shiny::safeError(e))
    }
  )

  df
}


########
#### Current file: app/server_raw.R 
########

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
    df <- read_input(input$file1$datapath)
    return(df)
  })

  dat_pre <- reactive({

    # req(input$file2)
    # Make laoding dependent of file name extension (file_ext())
    if (!is.null(input$file2$datapath)){
      df <- read_input(input$file2$datapath)

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
