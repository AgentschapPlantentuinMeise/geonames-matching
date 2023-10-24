# Check if all required packages are installed. If not, install them.
source("src/pkg.R")
pkgLoad()

# Load parameters for the workflow
library(ini)
config = read.ini("config.ini")

# Import the person name strings to match and parse the names for later testing
source("src/extract_strings.R")
## Keep the data in memory to connect the matched strings back to specimens
## after matching
data = extract_strings(path = config$source$data, # data file location
                       columns_list = config$source$columns, # properties to import
                       property = config$source$property, # property with the names
                       data_type = config$source$data_type) # type of data file

## Parse the name strings into first, last name, initials and
## try to interpret different syntaxes and teams using the dwc_agent ruby gem
parsed_names = parse_strings(data,
                             config$source$property)

# Import geonames data
source("src/import_geonames.R")
geonames = import_geonames(config$source$wikifile)

source("src/matching.R")
## Determine the set of cores that can be used on this machine for
## parallel computing
cores = assess_cores(config$matching$cores)

matching_results = match_wrapper(parsed_names,
                                 geonames,
                                 cores,
                                 config$matching$rmode)

## Filter the matches by a set of rules
## Also convert to a tibble for easier exporting of results
processed_results = matches_process(matching_results,
                                    parsed_names)

# Export the matched names into the specified export format
source("src/export.R")
processed_results %>%
  export(data = data,
         property = config$source$property,
         foldername = config$source$data,
         export_type = config$export)
