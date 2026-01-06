#' Concatenate dic vectors while keeping dic metadata
#'
#' Keeps dic attributes from the first dic argument and restores them after
#' concatenation.
#'
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
