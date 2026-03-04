

test1 <- function() {
  init_messages()
  
  test2("A")
  notify("test1")
  test2("B")
  test2("C")
}

test2 <- function(x) {
  init_messages()
  
  if (x == "C") abort("ups!")
  notify(x)

}
