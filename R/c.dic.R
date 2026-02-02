#' Concatenate dic vectors while keeping dic metadata from the first argument
#'
#' Keeps dic attributes from the first dic argument and restores them after
#' concatenation.
#'
#' If multiple dic arguments are provided, dic attributes from the other
#' arguments are ignored.
#'
#' @param ... dic vectors to be concatenated.
#' @param recursive logical. Should the result be recursive if any of the
#'  inputs are lists? Default is FALSE.
#' @return A dic vector resulting from concatenation of the provided dic
#'  vectors, with dic attributes taken from the first argument.
#' @export
c.dic <- function(..., recursive = FALSE) {
  args <- list(...)

  # template = first dic argument (must exist because method dispatch)
  template <- args[[1]]

  # strip dic class + attrs on all dic args before concatenation
  strip_one <- function(z) {
    if (inherits(z, "dic")) {
      class(z) <- setdiff(class(z), "dic")
      attr(z, opt("dic")) <- NULL
      attr(z, "label") <- NULL
      attr(z, "labels") <- NULL
    }
    z
  }
  args2 <- lapply(args, strip_one)

  # base concatenation
  out <- do.call(base::c, c(args2, list(recursive = recursive)))

  # restore dic metadata from template
  class(out) <- class(template)
  attr(out, opt("dic")) <- attr(template, opt("dic"))
  attr(out, "label")    <- attr(template, "label")
  attr(out, "labels")   <- attr(template, "labels")

  out
}
