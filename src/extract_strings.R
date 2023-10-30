extract_strings <- function(path,
                            columns_list,
                            property,
                            data_type) {
  # path = (relative) path to where the data file(s) can be found
  # columns_list = path to a file listing colnames to import
  # dwc_property = colname which contains the name strings to match
  # data_type = format of the data file(s)
  ## "DwC-A" = a Darwin Core Archive (unzipped). occurrence.txt will be used
  ## "dissco" = a JSON document as exported from the DiSSCo sandbox
  require(tidyverse)
  columns = readLines(columns_list,
                      warn = F) %>%
    c(property)
  
  if (data_type == "DwC-A") {
    data = read_tsv(path,
                    quote="",
                    col_select = all_of(columns),
                    col_types = cols(.default = "c"))
  }
  if (data_type == "dissco") {
    require(jsonlite)
    raw = fromJSON(path,simplifyVector = F)
    data = tibble(!!property := sapply(raw,
                                       function(x) 
                                         x$data$attributes$originalData[[paste0("dwc:",
                                                                                sym(property))]]),
                  countryCode = sapply(raw,
                                       function(x) 
                                         ifelse(!is.null(x$data$attributes$originalData$`dwc:countryCode`),
                                                x$data$attributes$originalData$`dwc:countryCode`,
                                                NA)),
                  occurrenceID = sapply(raw,
                                        function(x) 
                                          x$data$attributes$physicalSpecimenId),
                  gbifID = sapply(raw,
                                  function(x) 
                                    x$data$attributes$id))
  }
  return(data) 
}

parse_strings <- function(data,
                          property) {
  require(magrittr)
  
  unknowns = readLines("src/static/unknowns.txt",
                       warn = F)
  
  countrynames_filenames = list.files("data/countrynames",
                            pattern = ".tsv",
                            full.names = T)
  
  countrynames_list = list()
  for (i in countrynames_filenames) {
    countrynames_list[[i]] = read_tsv(i,
                                      col_types = cols(.default = "c"))
  }
  
  countrynames = countrynames_list %>%
    bind_rows() %>%
    filter(nchar(countryLabel) > 2,
           nchar(countryAltLabel) > 2) %>%
    mutate(countryLabel = gsub("[^a-z]",
                               "",
                               tolower(countryLabel)),
           countryAltLabel = gsub("[^a-z]",
                               "",
                               tolower(countryAltLabel))) %>%
    select(-country) %>%
    unlist(use.names = F) %>%
    unique()
  
  chunk_unknowns = readLines("src/static/unchunks.txt",
                             warn = F)
  
  parsed_names = data %>%
    count(!!sym(property),
          countryCode) %>%
    filter(!is.na(!!sym(property)),
           !(!!sym(property)%in%unknowns),
           !is.na(countryCode)) %>%
    mutate(locid = !!sym(property)) %>%
    separate_rows(!!sym(property),
                  sep=",|;| -|:|\\(|/|\'|\"") %>%
    mutate(chunk = gsub("[^a-z]",
                        "",
                        tolower(!!sym(property)))) %>%
    filter(chunk!="",
           nchar(chunk) > 2,
           !chunk%in%countrynames,
           !chunk%in%chunk_unknowns) %>%
    mutate(checkid1 = paste0(chunk,countryCode),
           checkid2 = paste0(chunk,locid)) %>%
    rownames_to_column("rownr")
  
  return(parsed_names)
}