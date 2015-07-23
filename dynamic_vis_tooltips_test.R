library(ggvis)
library(dplyr)
head(mtcars)

mtc=mtcars
#mtc$id=1:nrow(mtc)
mtc$id=row.names(mtc)

vovtooltip <- function(x){
  if (is.null(x)) return(NULL)
  row=mtc[mtc$id == x$id, ]
  paste0(names(row), ": ", format(row), collapse = "<br />")
}

mtc %>% 
  ggvis(x = ~wt, y = ~mpg, key := ~id) %>% 
  layer_points() %>%
  add_tooltip(vovtooltip,"hover")
