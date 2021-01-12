#' Set dictionary information to variable
#'
#' @param data A data frame
#' @param vars string or vector with string of variable names
#' @param ... More dic parameters
#'
#' @return A data frame
#' @export
set_dic <- function(data, vars = NULL, ...) {
  parameters <- list(...)

  if ("data.frame" %in% class(data)) {
    if (is.null(vars)) vars <- names(data)
    for(i in vars) {
      dic <- attr(data[[i]], .opt$dic)
      dic <- c(parameters, dic)
      if (!"item_name" %in% names(dic)) dic <- c(dic, list(item_name = i))
      if (!"class" %in% names(dic)) dic <- c(dic, list(class = "item"))
      dic <- dic[unique(names(dic))]
      attr(data[[i]], .opt$dic) <- dic
      if (is.null(dic_attr(data[[i]], .opt$item_label)))
        dic_attr(data[[i]], .opt$item_label) <- dic_attr(data[[i]], .opt$item_name)

      if (!"dic" %in% class(data[[i]])) class(data[[i]]) <- c("dic", class(data[[i]]))

      return(data)
    }
  }

  if (!"data.frame" %in% class(data)) {
    dic <- attr(data, .opt$dic)
    dic <- c(parameters, dic)

    if (!"class" %in% names(dic)) dic <- c(dic, list(class = "item"))
    if (!"item_name" %in% names(dic))
      dic <- c(dic, list(item_name = gsub(".*\\$", "", deparse(substitute(data)))))

    dic <- dic[unique(names(dic))]
    attr(data, .opt$dic) <- dic
    if (is.null(dic_attr(data, .opt$item_label)))
      dic_attr(data, .opt$item_label) <- dic_attr(data, .opt$item_name)

    if (!"dic" %in% class(data)) class(data) <- c("dic", class(data))

    attr(data, "label") <- dic_attr(data, .opt$item_label)

    return(data)
  }

  #check <- names(parameters) %in% names(.opt)
  #if (!all(check)) {
  #  not_valid <- paste(names(parameters)[which(!check)], sep = ", ")
  #  stop("Parameter(s) ", not_valid, " not valid.")
  #}

  #set <- c(
  #  "item_name", "item_label", "scale", "subscale",
  #  "subscale_2", "scale_label", "subscale_label",
  #  "subscale_2_label", "index", "weight", "source", "note", "type"
  #)

  #missing_param <- set[!(set %in% names(parameters))]
  #if (fill_missing && !is.null(missing_param)) {
  #  message("Parameter(s) ", paste(missing_param, collapse = ", "), " filled in with NAs.\n")
  #  missing_param <- sapply(missing_param, function(x) NA, simplify = FALSE)
  #}

}


