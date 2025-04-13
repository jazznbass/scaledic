#' Combine data frames
#'
#' Combine data frames (rows and columns) and keep dic information.
#'
#' @param ... data frames with dic information
#'
#' @return A combined data frame with all rows and columns from the provided
#'   data frames.
#' @details This function is useful when you want to join several data frames
#'   that contain dic information into one data frame. When a variable that is
#'   entailed in both data frames has dic information, the resulting data frame
#'   will keep the dic information of the data frame that is first listed as an
#'   argument.
#' @export

combine_data_frames <- function(...) {
  dfs <- list(...)

  add <- function(base, addon) {

    dic_base <- lapply(base, function(x) if(!is.null(dic_attr(x))) dic_attr(x))
    dic_base[!sapply(dic_base, is.null)]
    dic_addon <- lapply(addon, function(x) if(!is.null(dic_attr(x))) dic_attr(x))
    dic_addon[!sapply(dic_addon, is.null)]
    dic <- c(dic_base, dic_addon)

    dup <- sum(duplicated(names(dic)))
    if (dup > 0) warning(dup, " dic duplications")

    base[names(addon)[!(names(addon) %in% names(base))]] <- NA
    addon[names(base)[!(names(base) %in% names(addon))]] <- NA
    data <- rbind(base, addon)

    for(item in names(dic)) {
      if(item %in% names(data)) dic_attr(data[[item]]) <- dic[[item]]
    }
    data <- dic_haven(data)

    data
  }

  for(i in 2:length(dfs)) {
    dfs[[1]] <- add(dfs[[1]], dfs[[i]])
  }
  dfs[[1]]
}
