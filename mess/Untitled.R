

test1 <- function() {
  init_messages()
  init_messages(); on.exit(print_messages())
  test2("A")
  add_message("test1")
  test2("B")
  test2("C")
}

test2 <- function(x) {
  init_messages()
  init_messages(); on.exit(print_messages())
  if (x == "C") stop("ups!")
  add_message(x)

}
