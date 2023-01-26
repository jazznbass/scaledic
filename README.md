# scaledic - A dictionary for scales <img src="man/figures/hex-135.png" align="right"/>

Have you ever thought it might be helpful to have additional information about a variable in a data frame, giving you a longer description, the scale it belongs to, whether it has reverse coding, valid values, value labels? And wouldn't it be nice to be able to check for typos, impute missing values, create scale scores, switch between long and short labels in a graph, etc. with just a few simple commands in R?

This is what *scaledic* aims to do. *scaledic* is an R package for extending data frames and tibbles with several scale-related attributes. It is designed to implement (psychometric) scale information for items in a data frame. This includes values, labels, subscales, weights, etc. A number of functions help to organise, extract, replace and impute missing values, find typos, build scale scores etc.

At the moment, *scaledic* already works and is up to the task, but is still in an experimental stage where I may change the basic syntax.
The documentation is also poor. I am working on that.

Basically *scaledic* loads a dictionary file containing all relevant information and applies it to a data frame. Each variable corresponding to the ones described in the dictionary gets a new attribute `dic` which contains a list of all dictionary values for that variable.

The package contains a sample data frame `ITRF` with 4767 participants who filled in the integrated teacher report form questionnaire. `dic_ITRF` is a corresponding dictionary file for the ITRF (as the original data were collected in Germany, the dictionary file also has the German item labels).
