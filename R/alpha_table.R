#' Table with alpha values
#'
#' Returns a list of alpha cronbachs.
#'
#' @param data A data Frame
#' @param scales A list containing vectors with variable names. Each list element defines one scale. Named list elements are used as labels.
#' @param labels Label names for scales (defaults to named list elements in 'scales').
#' @param round Rounds values to given decimal position.
#' @param CI If TRUE confidence intervals are calculated.
#' @param conf_level Confidence level (e.g. 0.95 for 95 percent).
#' @param check_key Check_key for the psych::alpha function.
#' @param omega If TRUE Omega reliability estimation is calculated.
#' @param keys Optional key argument for the psych::alpha function.
#' @param RMSEA If TRUE RMSEA is calculated.
#' @param values Sets maximum and minimum valid values necessary to calculate item difficulty
#' @export

alpha_table <- function(data, scales, labels = NULL, round = 2, CI = TRUE,
                        conf_level = 0.95, check_key = TRUE, omega = FALSE,
                        keys = NULL, RMSEA = FALSE, difficulty = FALSE,
                        values = NULL) {

  if (difficulty && is.null(values)) {
    stop("Can not calculate item difficulty without min and max scale values.")
  }
  if (is.null(labels)) labels <- labels(scales)
  df <- data.frame(Scale = labels)

  for (i in 1:length(scales)) {
    if (!is.null(keys)) key <- keys[[i]] else key <- NULL
    if (!is.null(values)) {
      min <- values[[i]][1]
      max <- values[[i]][2]
    }

    a <- invisible(psych::alpha(data[, scales[[i]]], check.key = check_key, keys = key))
    if (omega) {
      o <- invisible(psych::omega(data[, scales[[i]]], nfactors = 1, keys = key))
    }
    f <- invisible(psych::fa(data[, scales[[i]]]))
    alpha <- a$total$raw_alpha
    df$"n"[i] <- min(a$item.stats$n, na.rm = TRUE)

    df$"n items"[i] <- a$nvar

    if (!CI) df$Alpha[i] <- substring(round(alpha, round), 2)

    if (CI) {
      a.CI <- .alpha_CI(alpha, nrow(data[, scales[[i]]]), length(scales[[i]]), conf_level)
      df$Alpha[i] <- paste0(substring(round(alpha, round), 2), " (", substring(round(a.CI[1], round), 2), "-", substring(round(a.CI[2], round), 2), ")")
    }

    alpha.std <- a$total$std.alpha
    if (!CI) {
      df$"Std.Alpha"[i] <- substring(round(alpha.std, round), 2)
    }

    if (CI) {
      a.std.CI <- .alpha_CI(alpha.std, nrow(data[, scales[[i]]]), length(scales[[i]]), conf_level)
      df$"Std.Alpha"[i] <- paste0(
          substring(round(alpha.std, round), 2),
          " (",
          substring(round(a.std.CI[1], round), 2),
          "-",
          substring(round(a.std.CI[2], round), 2),
          ")"
      )
    }

    dmin <- round(min(a$item.stats$r.drop), round)
    dmax <- round(max(a$item.stats$r.drop), round)
    mmin <- round(min(a$item.stats$mean), round)
    mmax <- round(max(a$item.stats$mean), round)
    smin <- round(min(a$item.stats$sd), round)
    smax <- round(max(a$item.stats$sd), round)
    lmin <- round(min(abs(f$loadings)), round)
    lmax <- round(max(abs(f$loadings)), round)
    if (difficulty) {
      dif_min <- round((mmin - min) / (max - min), round)
      dif_max <- round((mmax - min) / (max - min), round)
    }
    df$"Homogeneity"[i] <- substring(round(a$total$average_r, round), 2)
    df$"Discrimination"[i] <- paste0(substring(dmin, 2), " - ", substring(dmax, 2))
    if (difficulty) df$"Difficulty"[i] <- paste0(substring(dif_min, 2), " - ", substring(dif_max, 2))
    df$"M"[i] <- paste0(mmin, " - ", mmax)
    df$"SD"[i] <- paste0(smin, " - ", smax)
    if (omega) df$"Omega"[i] <- substring(round(o$omega.tot, round), 2)
    df$"|Loading|"[i] <- paste0(substring(lmin, 2), " - ", substring(lmax, 2))
    if (RMSEA) df$"RMSEA"[i] <- substring(round(f$RMSEA[1], 3), 2)
  }

  if (CI) {
    names(df)[which(names(df) == "Alpha")] <- paste0("Alpha(CI", conf_level * 100, "%)")
    names(df)[which(names(df) == "Std.Alpha")] <- paste0("Std.Alpha(CI", conf_level * 100, "%)")
  }

  df
}

.alpha_CI <- function(alpha, n, items, ci) {
  out <- 1 - (1 - alpha) * qf(c(1 - (1 - ci) / 2, (1 - ci) / 2), n - 1, (n - 1) * (items - 1))
  out
}

