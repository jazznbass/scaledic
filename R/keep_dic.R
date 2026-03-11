#' Keep attributes of a dictionary when evaluating an expression
#'
#' This function evaluates an expression while keeping the attributes of a
#' dictionary object intact. It is useful when you want to perform operations on
#' a dictionary without losing its attributes.
#'
#' @param x A dictionary object.
#' @param expression An expression to evaluate. This can be a character string
#'   or an expression
#' @param envir The environment in which to evaluate the expression. Defaults to
#'   the parent frame.
#' @return A dictionary object with the same attributes as the original, but
#'   with the expression evaluated.
#' @export
keep_dic <- function(x, expression, envir = parent.frame(n = 1)) {

  if (missing(expression)) return(x)

  expression <- substitute(expression)



  # Store the attributes before evaluating the expression
  dic_attr <- dic_attr(x)
  label_attr <- attr(x, "label")

  # Evaluate the expression
  x <- eval(expression, envir = envir)

  # restore the attributes after evaluating the expression
  dic_attr(x) <- dic_attr
  attr(x, "label") <- label_attr
  class(x) <- unique(c("dic", class(x)))
  return(x)
}

#' @export
droplevels.dic <- function(x, ...) {
  # Store the attributes before evaluating the expression
  dic_attr <- dic_attr(x)
  label_attr <- attr(x, "label")

  class(x) <- setdiff(class(x), "dic")
  x <- droplevels(x, ...)

  # restore attributes
  dic_attr(x) <- dic_attr
  attr(x, "label") <- label_attr
  class(x) <- unique(c("dic", class(x)))
  return(x)
}
