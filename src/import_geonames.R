import_geonames <- function(path) {
  require(tidyverse)
  geonames = read_tsv(path,
                      col_names = F,
                      col_types = cols(.default = "c"),
                      quote = "")
  
  colnames = read_delim("data/geonames/colnames.txt",
                        col_names = F,
                        delim=" : ") %>%
    mutate(cols = trimws(X1))
  
  colnames(geonames) = colnames$cols
  
  return(geonames)
}