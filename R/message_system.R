messages <- new.env()
messages$messages <- list()
messages$last_messages <- list()
messages$depth <- 0

init_messages <- function() {
  messages$depth <- messages$depth + 1
}

#' Add a message to the message system
#'
#' This function adds a message to the internal message system. Messages can be printed later using `print_messages()`.
#' #' @param ... Components of the message to be concatenated.
#' @param collapse A string to separate the components of the message.
#' @param frame An integer or character indicating the frame from which the message originates.
#' @param detail An integer indicating the detail level of the message.
#' @param warning Logical. If TRUE, the message is treated as a warning.
#' @return None. The function modifies the internal message environment.
#' @examples
#' init_messages()
#' add_message("This is a test message.")
#' print_messages()
#' @export
#' @keywords internal
#'
add_message <- function(...,
                        collapse = "",
                        frame = -1,
                        detail = 1,
                        warning = FALSE) {
  msg <- paste(c(...), collapse = collapse)

  if (is.character(frame)) {
    call <- frame
  } else {
    call <- deparse(sys.call(frame)[[1]])
  }
  messages$messages <- c(
    messages$messages,
    list(list(msg = msg, call = call), detail = detail, warning = warning)
  )
}

print_messages <- function(header = TRUE,
                           header_prefix = "!",
                           details = 1) {

  messages$depth <- messages$depth - 1

  if (messages$depth > 0) return()

  msg <- messages$messages
  if (length(messages$messages) > 0) {

    # convert to data frame
    msg <- messages$messages |>
      unlist() |>
      matrix(ncol = 4, byrow = TRUE) |>
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

      if (getOption("scaledic.print.messages")) message(msg)
    }
  }
  messages$last_messages <- messages$messages
  messages$messages <- list()
}
