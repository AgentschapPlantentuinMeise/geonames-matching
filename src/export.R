export <- function(match_results,
                   data,
                   property,
                   foldername,
                   export_type) {
  
  if (export_type$dwc_geo == "true") {
    match_results %>%
      export_to_dwc_geo(data,
                        property,
                        foldername,
                        "geo")
  }
  
  if (export_type$ambiguous == "true") {
    match_results %>%
      ambiguous_results(omit = F) %>%
      export_to_dwc_geo(data,
                        property,
                        foldername,
                        "ambiguous-geo")
  }
  
  if (export_type$fst == "true") {
    match_results %>%
      save_fst(foldername)
  }
  
  if (export_type$dissco == "true") {
    match_results %>%
      export_dissco_annotation(data,
                               property,
                               foldername)
  } 
}

export_to_dwc_geo <- function(match_results,
                              data,
                              property,
                              foldername,
                              export_type) {
  match_results %<>%
    select(-countryCode) %>%
    left_join(data,
              by = c("locid" = property),
              relationship = "many-to-many") %>%
    mutate(locationID = paste0("https://www.geonames.org/",
                               geonameid),
           locationRemarks = paste0("Score: ",
                                       score,
                                    ", # of matches: ",
                                    n,
                                    ", Geonames label: ",
                                    name),
           ) %>%
    select(gbifID,
           occurrenceID,
           locationID,
           !!property,
           countryCode,
           locationRemarks)
  
  filename = foldername %>%
    generate_filename(export_type,
                      "txt")
  write_tsv(match_results,filename)
}

export_dissco_annotation <- function(match_results,
                                     data,
                                     property,
                                     foldername) {
  require(uuid)
  require(jsonlite)
  match_results %<>%
    left_join(data,
              by = c("locid" = property),
              relationship = "many-to-many")
  res = vector("list", dim(match_results)[1])
  max = max(match_results$score)
  
  for (i in 1:dim(match_results)[1]) {
    guid = UUIDgenerate()
    res[[i]]$data = list(id = guid,
                         type = "Annotation",
                         attribution = list(id = guid,
                                            version = 1,
                                            type = "Annotation",
                                            motivation = "linking",
                                            target = list(id = match_results$gbifID[i],
                                                          type = "digital_specimen",
                                                          indvProp = "dwc:locationID"),
                                            body = list(type = "dwc:locationID",
                                                        value = paste0("https://www.geonames.org/",
                                                                       match_results$geonameid[i]),
                                                        description = paste0("geonames label: ",
                                                                             match_results$name[i]),
                                                        score = match_results$score[i]/max)))
  }
  
  resp = toJSON(res,
                pretty = T,
                auto_unbox = T)
  
  filename = foldername %>%
    generate_filename("dissco",
                      "json")
  
  write(resp,filename)
}

generate_filename <- function(foldername,
                              type,
                              extension) {
  timestamp = Sys.time() %>%
    as.character() %>%
    gsub("\\..*","",.) %>%
    gsub(":",".",.) %>%
    gsub(" ","_",.)
  
  dir = type %>%
    paste0("data/output/",.)
  
  if (!dir.exists("data/output")) {
    dir.create("data/output")
  }
  
  foldername %<>%
    gsub("/occurrence.txt","",.,fixed = T) %>%
    gsub(".*/","",.) %>%
    paste0(dir,
           "/",
           .,
           "_",
           timestamp,
           ".",
           extension)
  
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
  
  return(foldername)
}

save_fst <- function(df,
                     foldername) {
  require(fst)
  filename = foldername %>%
    generate_filename("fst",
                      "fst")
  write_fst(df,filename)
}

ambiguous_results <- function(match_results,
                              omit) {
  ambiguous = match_results %>%
    filter(n > 1)
  if (omit) {
    match_results %<>%
      filter(!locid%in%ambiguous$locid)
  } else {
    match_results %<>%
      filter(locid%in%ambiguous$locid)
  }
  return(match_results)
}