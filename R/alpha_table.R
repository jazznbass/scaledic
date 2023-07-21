#' Table with alpha values
#'
#' Returns a data.frame with item analyses for the provided scales.
#'
#' @param data A data Frame
#' @param scales A list containing vectors with variable names. Each list
#'   element defines one scale. Named list elements are used as labels.
#' @param labels Label names for scales (defaults to named list elements in
#'   'scales').
#' @param round Rounds values to given decimal position.
#' @param CI If TRUE confidence intervals are calculated.
#' @param conf_level Confidence level (e.g. 0.95 for 95 percent).
#' @param check_key Check_key for the psych::alpha function.
#' @param keys Optional key argument for the psych::alpha function.
#' @param RMSEA If TRUE RMSEA is calculated.
#' @param difficulty If TRUE, the difficulty of the item is calculated.
#' @param values Sets maximum and minimum valid values necessary to calculate
#'   item difficulty
#' @param fa If TRUE, a one factor exploratory factor analyses is calculated and
#'   loadings are reported.
#' @return A data frame with concise scale indices.
#' @examples
#' scales <- get_scales(ex_itrf,
#'   Int = scale == "ITRF" & subscale == "Int",
#'   Ext = scale == "ITRF" & subscale == "Ext"
#' )
#' alpha_table(ex_itrf, scales = scales, difficulty = TRUE, values = list(c(0, 3)), RMSEA = TRUE)
#' @export
alpha_table <- function(data,
                        scales,
                        labels = NULL,
                        round = 2,
                        CI = TRUE,
                        conf_level = 0.95,
                        check_key = TRUE,
                        keys = "auto",
                        RMSEA = FALSE,
                        difficulty = FALSE,
                        values = NULL,
                        fa = TRUE) {


  keys <- match.arg(keys)

  if (difficulty && is.null(values)) {
    stop("Can not calculate item difficulty without min and max scale values.")
  }
  if (is.null(labels)) labels <- labels(scales)
  df <- data.frame(Scale = labels)

  if (!is.null(values) && (length(values) != length(scales)))
    values <- rep(values, length(scales))

  for (i in 1:length(scales)) {
    #if (!is.null(keys)) {
    #  key <- keys[[i]] else key <- NULL
    #}
    data_scale <- data[, scales[[i]]]
    .id <- apply(data_scale, 1, function(x) all(is.na(x))) |> which()
    if (length(.id) > 0) {
      message(
        "Removed ", length(id), " rows because all items were missing."
      )
     data_scale <- data_scale[-.id, ]
    }

    .var <- apply(data_scale, 2, var, na.rm = TRUE)

    if (any(.var == 0, na.rm = TRUE)) {
      filter_names <- names(data_scale)[which(.var == 0)]
      message(
        "Variable with no variance dropped from analyses: ",
        paste0(filter_names, collapse = ", ")
      )
      .id <- which(!scales[[i]] %in% filter_names)
      scales[[i]] <- scales[[i]][.id]
      data_scale <- data_scale[, scales[[i]]]
    }

    if (any(is.na(.var), na.rm = TRUE)) {
      filter_names <- names(data_scale)[which(is.na(.var))]
      message(
        "Variable with NA variance dropped from analyses: ",
        paste0(filter_names, collapse = ", ")
      )
      .id <- which(!scales[[i]] %in% filter_names)
      scales[[i]] <- scales[[i]][.id]
      data_scale <- data_scale[, scales[[i]]]
    }

    if (keys == "auto") {
      if (requireNamespace("scaledic", quietly = TRUE)) {
        key <- data_scale |>
          map_dbl(~ as.numeric(scaledic::dic_attr(.x, "weight"))) |>
          sign()
      } else {
        keys <- NULL
        message("Scaledic is not installed, keys can not be extracted automatically.")
      }
    }

    if (!is.null(values)) {
      min <- values[[i]][1]
      max <- values[[i]][2]
    }

    a <- invisible(
      psych::alpha(data_scale, check.key = check_key, keys = key,use = "pairwise")
    )

    if (fa) f <- invisible(psych::fa(data_scale))
    alpha <- a$total$raw_alpha
    df$"n"[i] <- min(a$item.stats$n, na.rm = TRUE)

    df$"n items"[i] <- a$nvar

    if (!CI) df$Alpha[i] <- .nice_num(alpha, 2)

    if (CI) {
      a.CI <- .alpha_CI(
        alpha, nrow(data_scale), length(scales[[i]]), conf_level
      )
      df$Alpha[i] <- glue(
        "{.nice_num(alpha, round)} [{.nice_num(a.CI[1], 2)}, ",
        "{.nice_num(a.CI[2], 2)}]"
      )
    }

    alpha.std <- a$total$std.alpha
    if (!CI) {
      df$"Std.Alpha"[i] <- .nice_num(alpha.std, 2)
    }

    if (CI) {
      a.std.CI <- .alpha_CI(
        alpha.std, nrow(data_scale), length(scales[[i]]), conf_level
      )
      df$"Std.Alpha"[i] <- glue(
        "{.nice_num(alpha.std, 2)} [{.nice_num(a.std.CI[1], 2)}, ",
        "{.nice_num(a.std.CI[2], 2)}]"
      )
    }

    dmin <- round(min(a$item.stats$r.drop), round)
    dmax <- round(max(a$item.stats$r.drop), round)
    mmin <- round(min(a$item.stats$mean), round)
    mmax <- round(max(a$item.stats$mean), round)
    smin <- round(min(a$item.stats$sd), round)
    smax <- round(max(a$item.stats$sd), round)
    if (fa) {
      lmin <- round(min(abs(f$loadings)), round)
      lmax <- round(max(abs(f$loadings)), round)
    } else {
      lmin <- NA
      lmax <- NA

    }
    if (difficulty) {
      dif_min <- round((mmin - min) / (max - min), round)
      dif_max <- round((mmax - min) / (max - min), round)
    }
    df$"Homogeneity"[i] <- .nice_num(a$total$average_r, 2)
    df$"Discriminations"[i] <- glue(
      "[{.nice_num(dmin, 2)}, {.nice_num(dmax, 2)}]"
    )
    if (difficulty) {
      df$"Difficulties"[i] <- glue(
        "[{.nice_num(dif_min, 2)}, {.nice_num(dif_max, 2)}]"
      )
    }
    df$"Means"[i] <- glue("[{mmin}, {mmax}]")
    df$"SDs"[i] <- glue("[{smin}, {smax}]")
    df$"|Loadings|"[i] <- glue("[{.nice_num(lmin, 2)}, {.nice_num(lmax, 2)}]")
    if (RMSEA) df$"RMSEA"[i] <- .nice_num(f$RMSEA[1], 3)
 }

  if (CI) {
    names(df)[which(names(df) == "Alpha")] <- glue("Alpha CI{conf_level*100}%")
    names(df)[which(names(df) == "Std.Alpha")] <- glue(
      "Std.Alph CI{conf_level * 100}%"
    )
  }
  message("Note. values in brackets depict upper and lower bound of ",
      "confidence intervals or [min,max] intervals.")
  df
}

.alpha_CI <- function(alpha, n, items, ci) {
  f <- qf(c(1 - (1 - ci) / 2, (1 - ci) / 2), n - 1, (n - 1) * (items - 1))
  out <- 1 - (1 - alpha) * f
  out
}

