
.onAttach <- function(lib, pkg, ...) {
  out <- paste0(
    "scaledic ", utils::packageVersion("scaledic"),
    " (", utils::packageDate('scaledic'), ")\n"
  )
  packageStartupMessage(out)

}

.onLoad <- function(lib, pkg, ...) {
  # global options ----------------------------------------------------------
  op <- options()
  op_scan <- list(
    scaledic.string.split = ";",
    scaledic.string.prefix = "\u2551" #\u2502" #"# "
  )

  toset <- !(names(op_scan) %in% names(op))
  if (any(toset)) options(op_scan[toset])

  invisible()

}

utils::globalVariables(c(".", "value_labels"))

