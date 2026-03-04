#' Table with alpha values and item statistics for multiple scales
#'
#' Returns a data.frame with item analyses for the provided scales.
#' Useful for reporting scale reliabilities and item statistics in
#' manuscripts.
#'
#' The function uses the psych::alpha function to calculate Cronbach's alpha
#' and standardized alpha. Additionally, it calculates item means, standard
#' deviations, item discriminations (item-total correlations), and item
#' difficulties (if requested). If the 'fa' argument is set to TRUE, a
#' one-factor exploratory factor analysis is performed, and the minimum and
#' maximum absolute factor loadings are reported.
#' The function can also compute confidence intervals for alpha and
#' standardized alpha.
#' It handles missing data by removing rows with all items missing for each scale.
#' Variables with zero variance are automatically excluded from the analysis,
#' and a message is displayed indicating which variables were dropped.
#' The resulting data frame contains concise scale indices, making it easy to
#' report scale reliabilities and item statistics in manuscripts.
#' @seealso
#' \code{\link[psych]{alpha}}, \code{\link[psych]{fa}}, \code{\link[scaledic]{dic_attr}}
#'
#' @author Jürgen Wilbert
#' @param data A data Frame or tibble containing the item responses.
#' @param scales A list containing vectors with variable names. Each list
#'   element defines one scale. Named list elements are used as labels.
#' @param labels Label names for scales (defaults to named list elements in
#'   'scales').
#' @param round Rounds values to given decimal position.
#' @param CI If TRUE confidence intervals are calculated.
#' @param conf_level Confidence level (e.g. 0.95 for 95 percent).
#' @param check_key Check_key for the psych::alpha function.
#' @param keys Optional key argument for the psych::alpha function.
#' @param keys_from_weights If TRUE, tries to define keys from scaledics
#'   "weights" parameter.
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
#'
#' @export
alpha_table <- function(data,
                        scales,
                        labels = NULL,
                        round = 2,
                        CI = TRUE,
                        conf_level = 0.95,
                        check_key = TRUE,
                        keys = NULL,
                        keys_from_weights = TRUE,
                        RMSEA = FALSE,
                        difficulty = FALSE,
                        values = NULL,
                        fa = TRUE) {

  notify("This function is deprecated. Please use the nice_alpha_table() function from the wmisc package.")

  if (!is.null(keys)) {
    check_key <- FALSE
    keys_from_weights <- FALSE
  }

  if (difficulty && is.null(values)) {
    abort("Can not calculate item difficulty without min and max scale values.")
  }
  if (is.null(labels)) labels <- labels(scales)
  df <- data.frame(Scale = labels)

  if (!is.null(values) && (length(values) != length(scales)))
    values <- rep(values, length(scales))

  for (i in 1:length(scales)) {
    data_scale <- data[, scales[[i]]]
    .id <- apply(data_scale, 1, function(x) all(is.na(x))) |> which()
    if (length(.id) > 0) {
      notify(
        "Removed ", length(.id), " rows because all items were missing."
      )
     data_scale <- data_scale[-.id, ]
    }

    .var <- apply(data_scale, 2, var, na.rm = TRUE)

    if (any(.var == 0, na.rm = TRUE)) {
      filter_names <- names(data_scale)[which(.var == 0)]
      notify(
        "Variable with no variance dropped from analyses: ",
        paste0(filter_names, collapse = ", ")
      )
      .id <- which(!scales[[i]] %in% filter_names)
      scales[[i]] <- scales[[i]][.id]
      data_scale <- data_scale[, scales[[i]]]
    }

    if (any(is.na(.var), na.rm = TRUE)) {
      filter_names <- names(data_scale)[which(is.na(.var))]
      notify(
        "Variable with NA variance dropped from analyses: ",
        paste0(filter_names, collapse = ", ")
      )
      .id <- which(!scales[[i]] %in% filter_names)
      scales[[i]] <- scales[[i]][.id]
      data_scale <- data_scale[, scales[[i]]]
    }

    if (keys_from_weights) {
        keys <- data_scale |>
          sapply(function(.x) as.numeric(scaledic::dic_attr(.x, "weight"))) |>
          sign()
        check_key <- FALSE
    }

    if (!is.null(values)) {
      min <- values[[i]][1]
      max <- values[[i]][2]
    }

    a <- invisible(
      psych::alpha(data_scale, check.key = check_key, keys = keys, use = "pairwise")
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


  attr(df, "note") <- "Values in brackets depict upper and lower bound of confidence intervals or [min,max] intervals."

  message("Note. values in brackets depict upper and lower bound of ",
      "confidence intervals or [min,max] intervals.")
  df
}

.alpha_CI <- function(alpha, n, items, ci) {
  f <- qf(c(1 - (1 - ci) / 2, (1 - ci) / 2), n - 1, (n - 1) * (items - 1))
  out <- 1 - (1 - alpha) * f
  out
}

.nice_num <- function(x, digits = 2) {
  fmt <- paste0("%.", digits, "f")
  sub("^(-?)0.", "\\1.", sprintf(fmt, x))
}

