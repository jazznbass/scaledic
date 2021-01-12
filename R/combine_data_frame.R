#' Combine data frames
#' Comnine data frames and keep dic information.
#' @param ... data frames with dic information
#'
#' @return A combined data frame with all rows and columns from the provided dataframes.
#' @export

combine_data_frames <- function(...) {
  dfs <- list(...)

  add <- function(base, addon) {

    dic_base <- lapply(base, function(x) if(!is.null(attr(x, "dic"))) attr(x, "dic"))
    dic_base[!sapply(dic_base, is.null)]
    dic_addon <- lapply(addon, function(x) if(!is.null(attr(x, "dic"))) attr(x, "dic"))
    dic_addon[!sapply(dic_addon, is.null)]
    dic <- c(dic_base, dic_addon)

    dup <- sum(duplicated(names(dic)))
    if (dup > 0) warning(dup, " dic duplications")

    base[names(addon)[!(names(addon) %in% names(base))]] <- NA
    addon[names(base)[!(names(base) %in% names(addon))]] <- NA
    data <- rbind(base, addon)

    for(item in names(dic)) {
      if(item %in% names(data)) attr(data[[item]], .opt$dic) <- dic[[item]]
    }
    data <- dic_haven(data)

    data
  }

  out <- dfs[[1]]
  for(i in 2:length(dfs)) {

    out <- add(out, dfs[[i]])
  }
  out
}
