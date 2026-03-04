# vctrs support for class `dic`
# -------------------------------------------------------------------------
# vctrs double-dispatch methods for casting between `dic` and base types
# -------------------------------------------------------------------------

# Rulebook for casting:
# - When casting from base type to `dic`, create a new `dic` object
#   with default metadata and the appropriate type.
# - When casting from `dic` to base type, keeps the dic attributes except `type`,
#   which is updated to reflect the new base type. The class of the new object changed from
#   class("dic", old_base_type) to class("dic", new_base_type).
# - When casting between two `dic` objects, preserve the metadata of the
#   `to` object and convert the underlying data to the appropriate type.


.get_base_class <- function(x) {
  if (inherits(x, "integer")) return("integer")
  if (inherits(x, c("double", "numeric"))) return("numeric")
  if (inherits(x, "character")) return("character")
  if (inherits(x, "logical")) return("logical")
  if (inherits(x, "factor")) return("factor")
  NULL
}

.cast <- function(x, to) {

  # from base type (x) to dic (to)
  if (inherits(to, "dic")) {
    type <- .get_base_class(to)
    item_name <- match.call()[[2]] |> as.character()
    to <- new_dic(x, dic_attributes = list(item_name = item_name,
                                      item_label = item_name,
                                      type = type,
                                      weight = 1))
    return(to)
  }

  # from dic (x) to base type (to)
  base_class <- .get_base_class(to)

  if (base_class == "integer") {
    dic_attr(x, opt("type")) <- "integer"
    x[] <- as.integer(x)
    return(x)
  }

  if (base_class == "numeric") {
    dic_attr(x, opt("type")) <- "numeric"
    x[] <- as.double(x)
    return(x)
  }

  if (base_class == "character") {
    dic_attr(x, opt("type")) <- "character"
    x[] <- as.character(x)
    return(x)
  }

  if (base_class == "logical") {
    dic_attr(x, opt("type")) <- "logical"
    x[] <- as.logical(x)
    return(x)
  }

  if (base_class == "factor") {
    dic_attr(x, opt("type")) <- "factor"
    x[] <- as.factor(x)
    return(x)
  }
  warning("Casting to unsupported type; returning original dic object.")
  x
}



## dic.dic

#' @exportS3Method vctrs::vec_ptype2 dic.dic
vec_ptype2.dic.dic <- function(x, y, ...) {
  x
}

#' @exportS3Method vctrs::vec_cast dic.dic
vec_cast.dic.dic <- function(x, to, ...) {
  .cast(x, to)
}

## dic.integer

#' @exportS3Method vctrs::vec_ptype2 dic.integer
vec_ptype2.dic.integer <- function(x, y, ...) {
  x
}

#' @exportS3Method vctrs::vec_cast dic.integer
vec_cast.dic.integer <- function(x, to, ...) {
  .cast(x, to)
}

#' @exportS3Method vctrs::vec_ptype2 integer.dic
vec_ptype2.integer.dic <- function(x, y, ...) {
  y
}

#' @exportS3Method vctrs::vec_cast integer.dic
vec_cast.integer.dic <- function(x, to, ...) {
  .cast(x, to)
}

## dic.double

#' @exportS3Method vctrs::vec_ptype2 dic.double
vec_ptype2.dic.double <- function(x, y, ...) {
  x
}

#' @exportS3Method vctrs::vec_cast dic.double
vec_cast.dic.double <- function(x, to, ...) {
  .cast(x, to)
}

#' @exportS3Method vctrs::vec_ptype2 double.dic
vec_ptype2.double.dic <- function(x, y, ...) {
  y
}

#' @exportS3Method vctrs::vec_cast double.dic
vec_cast.double.dic <- function(x, to, ...) {
  .cast(x, to)
}

## dic.character

#' @exportS3Method vctrs::vec_ptype2 dic.character
vec_ptype2.dic.character <- function(x, y, ...) {
  x
}

#' @exportS3Method vctrs::vec_cast dic.character
vec_cast.dic.character <- function(x, to, ...) {
  .cast(x, to)
}

#' @exportS3Method vctrs::vec_ptype2 character.dic
vec_ptype2.character.dic <- function(x, y, ...) {
  y
}

#' @exportS3Method vctrs::vec_cast character.dic
vec_cast.character.dic <- function(x, to, ...) {
  .cast(x, to)
}

## dic.logical

#' @exportS3Method vctrs::vec_ptype2 dic.logical
vec_ptype2.dic.logical <- function(x, y, ...) {
  x
}

#' @exportS3Method vctrs::vec_cast dic.logical
vec_cast.dic.logical <- function(x, to, ...) {
  .cast(x, to)
}

#' @exportS3Method vctrs::vec_ptype2 logical.dic
vec_ptype2.logical.dic <- function(x, y, ...) {
  y
}

#' @exportS3Method vctrs::vec_cast logical.dic
vec_cast.logical.dic <- function(x, to, ...) {
  .cast(x, to)
}

## dic.factor

#' @exportS3Method vctrs::vec_ptype2 dic.factor
vec_ptype2.dic.factor <- function(x, y, ...) {
  x
}

#' @exportS3Method vctrs::vec_cast dic.factor
vec_cast.dic.factor <- function(x, to, ...) {
  .cast(x, to)
}

#' @exportS3Method vctrs::vec_ptype2 factor.dic
vec_ptype2.factor.dic <- function(x, y, ...) {
  y
}

#' @exportS3Method vctrs::vec_cast factor.dic
vec_cast.factor.dic <- function(x, to, ...) {
  .cast(x, to)
}
