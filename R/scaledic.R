
.onAttach <- function(lib, pkg, ...) {
  out <- paste0(
    "scaledic ", utils::packageVersion("scaledic"),
    " (", utils::packageDate('scaledic'), ")\n"
  )
  packageStartupMessage(out)

}

utils::globalVariables(".")
