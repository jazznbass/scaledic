#' Set dictionary informations to variable
#'
#' @param data A data frame
#' @param vars string or vector with string of variable names
#' @param parameters List containing parameters
#'
#' @return A data frame
#' @export
set_dic <- function(data, vars, parameters, fill_missing = TRUE) {
  if (missing(vars)) vars <- names(data)

  check <- names(parameters) %in% names(.opt)
  if (!all(check)) {
    not_valid <- paste(names(parameters)[which(!check)], sep = ", ")
    stop("Parameter(s) ", not_valid, " not valid.")
  }

  set <- c(
    "item_name", "item_label", "scale", "subscale",
    "subscale_2", "scale_label", "subscale_label",
    "subscale_2_label", "index", "weight", "source", "note", "type"
  )

  missing_param <- set[!(set %in% names(parameters))]
  if (fill_missing && !is.null(missing_param)) {
    message("Parameter(s) ", paste(missing_param, collapse = ", "), " filled in with NAs.\n")
    missing_param <- sapply(missing_param, function(x) NA, simplify = FALSE)
  }

  for(i in vars) {
    dic <- attr(data[[i]], .opt$dic)
    dic <- c(dic, parameters)
    if (fill_missing) {
      tmp <- !(names(missing_param) %in% names(dic))
      dic <- c(dic, missing_param[tmp])
    }
    dic[.opt$class] <- "item"
    attr(data[[i]], .opt$dic) <- dic
  }

  data
}


