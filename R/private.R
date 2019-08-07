.opt <- list(
  "item_label" = "item_label",
  "item_label_short" = "item_label_short",
  "scale" = "scale",
  "subscale" = "subscale",
  "subscale_2" = "subscale_2",
  "scale_label" = "scale_label",
  "subscale_label" = "subscale_label",
  "subscale_2_label" = "subscale_2_label",
  "index" = "index",
  "weight" = "weight",
  "source" = "source",
  "note" = "note",
  "type" = "type",
  "values" = "values",
  "missing" = "missing",
  "dic" = "dic",
  "class" = "class"
)

.vars <- as.data.frame(matrix(c(
  "item_label", "item_label", "ITEM",
  "item_label_short", "item_label_short", "LABEL",
  "scale", "scale", "SCALE",
  "subscale", "subscale", "SUB_SCALE",
  "subscale_2", "subscale_2", "SUB_SCALE_2",
  "scale_label", "scale_label", "SCALE_LABEL",
  "subscale_label", "subscale_label", "SUB_SCALE_LABEL",
  "subscale_2_label", "subscale_2_label", "SUB_SCALE_2_LABEL",
  "index", "index", "INDEX",
  "weight", "weight", "WEIGHT",
  "source", "source", "SOURCE",
  "note", "note", "NOTE",
  "type", "type", "TYPE",
  "values", "values", "VALUES",
  "missing", "missing", "MISSING",
  "dic", "dic", ""
),
ncol = 3,
byrow = TRUE,
dimnames = list(NULL, c("internal", "label", "dicname"))
))

.get_dic_items <- function(data) {

  which(
    sapply(data,
      function(x) !is.null(attr(x, .opt$dic)) && dic_attr(x, .opt$class) == "item"
    )
  )

}


