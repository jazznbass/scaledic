# relbeliefs

set.seed(21)
n <- 20
n_scale <- 5
scales <- c("rel", "sui")
n_var <- n_scale * length(scales)
dat <- cbind(
  matrix(sample(1:6, 2 * n, replace = TRUE), ncol = 2),
  matrix(sample(1:5, 3 * n, replace = TRUE), ncol = 3),
  matrix(sample(0:4, n_scale * n, replace = TRUE), ncol = n_scale)
)
dat <- as.data.frame(dat)
names(dat) <- paste(rep(scales, each = n_scale), rep(1:n_scale, length(scales)), sep = "_")
dat$gender <- sample(c("m", "f", "d"), n, replace = TRUE)
dat$age <- sample(6:10, n, replace = TRUE)

for (i in 1:5) dat[sample(nrow(dat), 1), sample(ncol(dat), 1)] <- sample(c(11, 55,66), 1)
for (i in 1:5) dat[sample(nrow(dat), 1), sample(ncol(dat), 1)] <- -999

dat

item_names <- c(
  "How often do you attend church or other religious meetings?",
  "How often do you spend time in private religious activities, such as prayer, meditation or Bible study?",
  "In my life, I experience the presence of the Divine (i.e., God)",
  "My religious beliefs are what really lie behind my whole approach to life",
  "I try hard to carry my religion over into all other dealings in life",
  paste("Did you feel", c("tense", "blue", "irritated", "inferior"), "in the last week?"),
  "Did you have problems falling asleep in the last week?"
)

value_labels <- c(
  "1 = Never; 2 = Once a year or less; 3 = A few times a year; 4 = A few times a month; 5 = Once a week; 6 = More than once/week",
  "1 = Rarely or never; 2 = A few times a month; 3 = Once a week; 4 = Two or more times/week; 5 = Daily; 6 = More than once a day",
  rep("1 = Definitely not true; 2 = Tends not to be true; 3 = Unsure; 4 = Tends to be true; 5 = Definitely true of me", 3),
  rep(c("0 = not at all; 4 = extremely"), 5)
)

ex_scaledic_dic <- data.frame(
  item_name = names(dat),
  item_label = c(item_names, "gender", "age"),
  scale = c(rep(c("rel", "sui"), each = 5), rep("misc",2)),
  scale_label = c(rep(c("Religious beliefs", "Suicide tendency"), each = 5), rep("Miscellaneous",2)),
  values = c(rep("1:6", 2), rep("1:5", 3), rep("0:4", 5), "'m', 'f', 'd'", "5:11"),
  value_labels = c(value_labels, "m = male; f = female; d = diverse", ""),
  type = c(rep("integer", n_var), "factor", "integer"),
  weight = 1,
  missing = c(rep("-999", n_var), "", "-999")
)


ex_scaledic_data <- dat

usethis::use_data(
  ex_scaledic_data,
  ex_scaledic_dic,
  overwrite = TRUE
)

