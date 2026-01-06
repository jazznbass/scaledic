# R/vctr.R
# vctrs support for dic -------------------------------------------------------
# Policy:
# - If a dic vector participates, keep dic and copy metadata from the dic template.
# - For dic + dic: left template wins.
# - For base + dic (any order): dic template wins.
#
# Key: implement enough vctrs generics to avoid fallback recursion.

# -----------------------------------------------------------------------------
# Core helpers

# strip dic class only (keep underlying base class like integer/double/...)
#' @export
#' @keywords internal
vec_proxy.dic <- function(x, ...) {
  class(x) <- setdiff(class(x), "dic")
  x
}

.restore_dic <- function(x, template) {
  class(x) <- class(template)
  attr(x, opt("dic")) <- attr(template, opt("dic"))
  attr(x, "label")    <- attr(template, "label")
  attr(x, "labels")   <- attr(template, "labels")
  x
}

#' @export
#' @keywords internal
vec_restore.dic <- function(x, to, ...) {
  .restore_dic(x, to)
}

# 0-length prototype without calling `[.dic` or vctrs restore
.dic_ptype0 <- function(template) {
  p <- vec_proxy.dic(template)[0]
  .restore_dic(p, template)
}

.cast_base_to_dic <- function(x, to) {
  out <- vctrs::vec_cast(x, vec_proxy.dic(to))
  .restore_dic(out, to)
}

.cast_dic_to_base <- function(x, to) {
  vctrs::vec_cast(vec_proxy.dic(x), to)
}

# -----------------------------------------------------------------------------
# vctrs generics needed to prevent recursion fallbacks

#' @export
#' @keywords internal
vec_ptype_full.dic <- function(x, ...) "dic"

#' @export
#' @keywords internal
vec_ptype_abbr.dic <- function(x, ...) "dic"

#' @export
#' @keywords internal
vec_init.dic <- function(x, n, ...) {
  out <- vctrs::vec_init(vec_proxy.dic(x), n)
  .restore_dic(out, x)
}

#' @export
#' @keywords internal
vec_slice.dic <- function(x, i, ...) {
  out <- vctrs::vec_slice(vec_proxy.dic(x), i)
  .restore_dic(out, x)
}

# Important for assignment during combining/recycling
#' @export
#' @keywords internal
vec_proxy_assign.dic <- function(x, i, value, ...) {
  template <- x
  out <- vctrs::vec_proxy_assign(vec_proxy.dic(x), i, vec_proxy.dic(value))
  .restore_dic(out, template)
}

# -----------------------------------------------------------------------------
# dic <-> dic

#' @export
#' @keywords internal
vec_ptype2.dic.dic <- function(x, y, ...) {
  .dic_ptype0(x) # left template wins
}

#' @export
#' @keywords internal
vec_cast.dic.dic <- function(x, to, ...) {
  .restore_dic(vec_proxy.dic(x), to) # keep target metadata
}

# -----------------------------------------------------------------------------
# base <-> dic (explicit base types vctrs dispatches on)

# integer
#' @export
#' @keywords internal
vec_ptype2.integer.dic <- function(x, y, ...) .dic_ptype0(y)
#' @export
#' @keywords internal
vec_ptype2.dic.integer <- function(x, y, ...) .dic_ptype0(x)
#' @export
#' @keywords internal
vec_cast.integer.dic <- function(x, to, ...) .cast_base_to_dic(x, to)
#' @export
#' @keywords internal
vec_cast.dic.integer <- function(x, to, ...) .cast_dic_to_base(x, to)

# double
#' @export
#' @keywords internal
vec_ptype2.double.dic <- function(x, y, ...) .dic_ptype0(y)
#' @export
#' @keywords internal
vec_ptype2.dic.double <- function(x, y, ...) .dic_ptype0(x)
#' @export
#' @keywords internal
vec_cast.double.dic <- function(x, to, ...) .cast_base_to_dic(x, to)
#' @export
#' @keywords internal
vec_cast.dic.double <- function(x, to, ...) .cast_dic_to_base(x, to)

# logical
#' @export
#' @keywords internal
vec_ptype2.logical.dic <- function(x, y, ...) .dic_ptype0(y)
#' @export
#' @keywords internal
vec_ptype2.dic.logical <- function(x, y, ...) .dic_ptype0(x)
#' @export
#' @keywords internal
vec_cast.logical.dic <- function(x, to, ...) .cast_base_to_dic(x, to)
#' @export
#' @keywords internal
vec_cast.dic.logical <- function(x, to, ...) .cast_dic_to_base(x, to)

# character
#' @export
#' @keywords internal
vec_ptype2.character.dic <- function(x, y, ...) .dic_ptype0(y)
#' @export
#' @keywords internal
vec_ptype2.dic.character <- function(x, y, ...) .dic_ptype0(x)
#' @export
#' @keywords internal
vec_cast.character.dic <- function(x, to, ...) .cast_base_to_dic(x, to)
#' @export
#' @keywords internal
vec_cast.dic.character <- function(x, to, ...) .cast_dic_to_base(x, to)

# factor
#' @export
#' @keywords internal
vec_ptype2.factor.dic <- function(x, y, ...) .dic_ptype0(y)
#' @export
#' @keywords internal
vec_ptype2.dic.factor <- function(x, y, ...) .dic_ptype0(x)
#' @export
#' @keywords internal
vec_cast.factor.dic <- function(x, to, ...) .cast_base_to_dic(x, to)
#' @export
#' @keywords internal
vec_cast.dic.factor <- function(x, to, ...) .cast_dic_to_base(x, to)
