#' Parse input data with columns of priorities to columns of groups
#'
#' @description
#' This handles transforming data from a typical Google form to the format
#' compatible with `prioritized_grouping()`.
#'
#' @param data data.frame or tibble
#' @param id id column. Numeric index or column name. Default is 1.
#' @param prio.cols priority columns. Numeric indices or column names.
#' @param sort.cols flag to sort priority columns names/indices. Default=FALSE
#'
#' @return data.frame
#' @export
#'
parse_prio_form <- function(data, id = 1, prio.cols,sort.cols=FALSE) {
  if (is.character(prio.cols)) {
    grp.index <- match(prio.cols, names(data))
  } else {
    grp.index <- prio.cols
  }

  if (sort.cols){
    prio.cols <- sort(prio.cols)
  }

  new.names <- names(data)
  new.names[grp.index] <- seq_along(grp.index)

  data <- setNames(data, new.names)

  out <- split(data, seq_len(nrow(data))) |>
    lapply(\(.x){
      # browser()

      out <- as.data.frame(matrix(c(as.character(.x[[id]]), colnames(.x)[grp.index]), nrow = 1))
      setNames(out, c(
        "id",
        # names(.x[id]),
        unname(unlist(.x[grp.index]))
      ))
    }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(dplyr::across(-1, as.integer))

  # Sorting is not really needed, but a nice touch
  out[c(names(out)[1], sort(names(out)[-1]))]
}


#' Parse input data from column of strings with prioritised group names
#'
#' @description
#' This handles transforming data from a typical Microsoft form to the format
#' compatible with `prioritized_grouping()`.
#'
#' @param data data.frame or tibble
#' @param id id column. Numeric index of column name. Default is 1.
#' @param string.col string column. Numeric index or column name.
#' @param pattern regex pattern to use for splitting priorities string with
#' `strsplit()`.
#' Default is ";".
#'
#' @return data.frame
#' @export
#'
parse_string_form <- function(data, id = 1, string.col,pattern=NULL) {
  if (is.null(pattern)){
    pattern <- ";"
  }

  if (length(string.col) != 1) {
    stop("string.col is required, and has to have length 1")
  }
  if (is.character(string.col)) {
    string.index <- match(string.col, names(data))
  } else {
    string.index <- string.col
  }

  # Cells with NAs are excluded.
  # NAs happen if the priorities are not edited upon form submission, but a
  # default order can not be guessed reliably if group naming is not ordered
  # (like group N, group N+1...)
  out <- data.frame(data[[id]], data[[string.index]]) |>
    na.omit() |>
    (\(.d){
      split(.d, seq_len(nrow(.d)))
    })() |>
    lapply(\(.x){
      grps <- unlist(strsplit(x=.x[[2]],split=pattern))
      out <- as.data.frame(matrix(c(.x[[1]], seq_along(grps)), nrow = 1))
      setNames(
        out,
        c("id", grps)
      )
    }) |>
    dplyr::bind_rows()

  # Sorting is not really needed, but a nice touch
  out[c(names(out)[1], sort(names(out)[-1]))]
}
