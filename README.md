# scaledic - Dictionary for scales

scaledic is an R package for extending data frames and tibbles with several scale related attributes.    
It is designed to implement (psychometric) scale information to items of a data frame. 
  These include values, labels, sub scales, weights etc.. A couple of functions help to organize, extract, replace and impute missing values, find typos, build scale scores etc.

For now, scaledic is already working and up to the task but still in an experimental stage where I might change the basic syntax.  
Also documentation is very poor. I am working on that.

Basically, scaledic loads a dictionary file that contains all relevant information and applies these to a data frame. Here, every variable corresponding to the ones describes in the dictionary gets a new attribute `dic` which contains a list with all dictionary values for that variable.  

The package contains an example data frame `ITRF` with 4767 participants that filled out the "integrated teacher report formula" questionnaire. `dic_ITRF` is a corresponding dictionary file for the `ITRF` (as the original data were collected in Germany the dictionary file has also the German labels).  

See  `example(scaledic)`.








