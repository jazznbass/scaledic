# Add a message to the message system

This function adds a message to the internal message system. Messages
can be printed later using `print_messages()`. \#' @param ... Components
of the message to be concatenated.

## Usage

``` r
notify(..., collapse = "", frame = -1, detail = 1, warning = FALSE)
```

## Arguments

- collapse:

  A string to separate the components of the message.

- frame:

  An integer or character indicating the frame from which the message
  originates.

- detail:

  An integer indicating the detail level of the message.

- warning:

  Logical. If TRUE, the message is treated as a warning.

## Value

None. The function modifies the internal message environment.

## Author

Juergen Wilbert

## Examples

``` r
init_messages()
#> Error in init_messages(): could not find function "init_messages"
notify("This is a test message.")
print_messages()
#> Error in print_messages(): could not find function "print_messages"
```
