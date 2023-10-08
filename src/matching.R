# Match a single name (and its derivatives, i.e. first name, last name, initials)
# to a set of Wikidata item labels and aliases
# Doing it per name allows parallelization
match_string <- function(locstring,
                         geo_sub) {
  require(tidyverse)
  matches = filter(geo_sub,
                   text == locstring$chunk|
                     text2 == locstring$chunk)
  
  return(matches)
}

# Validation is done per result, so it can be parallelized
match_validate <- function(result,
                           rmode = "best",#can also be "cut" or "all"
                           cut) {
  require(tidyverse)
  require(magrittr)
  
  #nomatch if no match was found or only initials matched
  nomatch = tibble(geonameid = as.character(NA),
                   name = as.character(NA),
                   id = as.character(NA),
                   score = 0)
  
  if (dim(result)[1] == 0) {
    return(nomatch[-1,])
  }
  
  #collapse each result in a list of each possible wikidata qid
  #and sum the scores
  result %<>%
    group_by(geonameid) %>%
    summarize(name = first(name),
              id = first(geonameid),
              score = n()) %>%
    arrange(desc(score))
  
  if (rmode == "best") {
    return(result[1,])
  } else if (rmode == "cut") {
    if (!is.null(cut)) {
      return(result[1:min(cut,
                          dim(result)[1])])
    } else {
      stop("Cut parameter not specified.")
    }
  } else if (rmode == "all") {
    return(result)
  }
}

# Try to estimate the number of cores
# The number can be fixed in the configuration
# If a "+" is added, this function can also check if actually more are available
# This function does NOT take into account what the system currently can use,
# only what the hardware seems to have on offer.
assess_cores <- function(cores) {
  if (grepl("+",cores)) {
    require(doParallel)
    cores = max(detectCores(logical = F),
                gsub("\\+",
                     "",
                     cores),
                na.rm = T)
  }
  return(as.numeric(cores))
}

threading <- function(data,
                      f,
                      num_threads = 4,
                      arg = list(),
                      pkg = c("tidyverse",
                              "magrittr"),
                      srcs = "src/matching.R",
                      envir = NULL) {
  # data = a list or tibble to multithread
  # f = a function (or function name) that is vectorized 
  ## and will be applied in parallel to data
  # num_threads = number of cores the cluster can use
  # arg = list with named arguments for function f
  # pkg = #required packages for the function, might be unneeded argument
  # srcs = required function files for the function, might be unneeded argument
  
  start_overhead_time = Sys.time()
  require(parallel)
  require(doParallel)
  
  #convert data to list if it's a tibble
  if (is_tibble(data)|
      is.data.frame(data)) {
    data %<>%
      split(seq(dim(data)[1]))
  }
  
  #create the cluster for threading
  cl = makePSOCKcluster(num_threads)
  registerDoParallel(cl)
  
  #load the required objects into the cluster
  if (length(arg) > 0) {
    if (is.null(envir)) {
      envir = .GlobalEnv
    } else if (envir == "local") {
      envir = environment()
      list2env(arg,envir = envir)
    }
    
    clusterExport(cl,
                  unlist(names(arg)),
                  envir = envir)
  }
  
  #load the arguments that specify other requirements in the cluster
  clusterExport(cl,
                c("pkg","srcs"),
                envir = environment())
  
  #execute those other requirements
  clusterEvalQ(cl, {
    for (i in 1:length(pkg)) {
      library(pkg[i],
              character.only = T)
    }
    for (i in 1:length(srcs)) {
      source(srcs[i])
    }
  })
  
  #wrap the function to be used to avoid argument misspecification
  #wrapping might no longer be needed after much troubleshooting
  #but don't break things that work
  custom_wrapper <- function(x) {
    if (length(arg) > 0) {
      do.call(f,
              c(list(x),
                lapply(names(arg),get)))
    } else {
      do.call(f,
              list(x))
    }
  }
  
  #multithread the process, log time
  start_time = Sys.time()
  resu = foreach(mat = data) %dopar% custom_wrapper(mat)
  end_time = Sys.time()
  stopCluster(cl)
  
  parallel_time = end_time - start_time
  overhead_time = start_time - start_overhead_time
  print("Parralel processing time:")
  print(parallel_time)
  print("Overhead of setting up cluster time:")
  print(overhead_time)
  
  return(resu)
}

matches_process <- function(data,
                            parsed_names) {
  require(tidyverse)
  df = data %>%
    bind_rows(.id = "rownr") %>%
    left_join(parsed_names,
              by = c("rownr" = "checkid1"))
  return(df)
}

match_wrapper <- function(names,
                          geonames,
                          cores,
                          rmode) {
  names_u = names %>%
    filter(!duplicated(checkid1))
  
  countries = names_u %>%
    count(countryCode)
  
  resu = list()
  for (i in 1:dim(countries)[1]) {
    names_sub = names_u %>%
      filter(countryCode == countries$countryCode[i])
    
    geo_sub = geonames %>%
      filter(`country code` == countries$countryCode[i])
    
    geo_sub %<>%
      mutate(text = gsub("[^a-z]",
                         "",
                         tolower(name)),
             text2 = gsub("[^a-z|,]",
                          "",
                          tolower(alternatenames))) %>%
      separate_rows(text2,
                    sep=",")
    print(paste(countries$countryCode[i],
                countries$n[i],
                sep=": "))
    matching_results = threading(data = names_sub,
                                 f = match_string,
                                 num_threads = cores,
                                 arg = list(geo_sub = geo_sub),
                                 envir = "local")
    
    names(matching_results) = pull(names_sub,
                                   checkid1)
    resu %<>%
      c(matching_results)
  }
  validated_results = threading(data = resu,
                                f = match_validate,
                                num_threads = cores,
                                arg = list(rmode = rmode),
                                envir = "local")
  names(validated_results) = names(resu)

  return(validated_results)
}