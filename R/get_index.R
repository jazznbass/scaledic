#' Get index
#'
#' @param data A data frame
#' @param filter A logical expression for any dic attribute (e.g. scale == "ITRF" & subscale == "Int")
#' @param names If names is TRUE, a vector with variable names will be returned.
#'
#' @return A vector with indices adhering to the given attribute criteria.
#' @export

get_index <- function(data, filter = NULL, scale = NULL, subscale = NULL,
                      subscale_2 = NULL, names = TRUE, class = NULL) {

  filter <- deparse(substitute(filter))
  if (!is.null(scale) || !is.null(subscale) || !is.null(subscale_2)) {
    filter <- .to_filter(scale = scale, subscale = subscale, subscale_2 = subscale_2)
  }

    .get_index(data = data, filter = filter, names = names, class = NULL)
}

.get_index <- function(data, filter, names = TRUE, class = NULL) {
  if (!is.null(class)) filter <- paste0(filter, " & class == '", class, "'")
  dic <- extract_dic(data)
  id <- with(dic, eval(str2lang(filter)))
  out <- which(names(data) %in% dic$name[id])
  if (names) out <- names(data)[out]
  out
}
