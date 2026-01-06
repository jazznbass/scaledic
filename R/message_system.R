messages <- new.env()
messages$messages <- list()
messages$last_messages <- list()
messages$depth <- 0

init_messages <- function() {
  messages$depth <- messages$depth + 1
}

add_message <- function(..., collapse = "", frame = -1, detail = 1) {
  msg <- paste(c(...), collapse = collapse)
  messages$messages <- c(
    messages$messages,
    list(list(msg = msg, call = deparse(sys.call(frame)[[1]])), detail = detail)
  )
}

print_messages <- function(warning = FALSE,
                           header = TRUE,
                           header_prefix = "!",
                           details = 1) {

  messages$depth <- messages$depth - 1

  if (messages$depth > 0) return()

  msg <- messages$messages
  if (length(messages$messages) > 0) {

    # convert to data frame
    msg <- messages$messages |>
      unlist() |>
      matrix(ncol = 3, byrow = TRUE) |>
      as.data.frame()

    # filter by details
    msg <- msg[msg[[3]] <= details, 1:2]

    # split messages by call
    msg_list <- split(msg[[1]], msg[[2]])

    for(i_msg in 1:length(msg_list)) {
      msg <- table(msg_list[[i_msg]])
      for(i in seq_along(msg)){
        if (msg[i] > 1) names(msg)[i] <- paste0(names(msg)[i], " (", msg[i], "x)")
      }
      msg <- paste0(1:length(msg), ": ", names(msg), collapse = "\n")

      if (header){
        msg <- paste0(header_prefix, " (", names(msg_list[i_msg]), ")\n", msg, "\n")
      }
      else {
        msg <- paste0("\n", msg, "\n")
      }
      if (warning) warning(msg, call. = FALSE) else message(msg)
    }
  }
  messages$last_messages <- messages$messages
  messages$messages <- list()
}
