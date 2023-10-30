# geonames_matching
This R workflow extracts locality (or other) strings from a Darwin Core archive and attempts to automatically match them to items in Geonames dumps. The workflow is designed in a modular and configurable manner, so that it can be adapted for other data models or use cases if desired. It also makes use of parralel processing to compensate for the fairly slow matching approach.

# Requirements
[R](https://www.r-project.org/) 4.3 or higher need to be installed for the workflow to run. Dependencies should install automatically. 

# Data
Extract the contents of the Darwin Core archive to be processed to the `data/gbif sets` folder. Different paths can be specified in the `config.ini` file if needed.

# Configuration
Edit the config.ini file to adjust parameters of the workflow. The following parameters are currently available:

## source
`data` : Path to the data (typically a file).

`property` : The property from where the strings will be taken (e.g. column name for a Darwin Core archive).

`columns` : A file listing the columns to extract from the Darwin Core source material.

`wikifile` : Destination of where a cache of Geonames record data can be found. Normally the latest dump at https://download.geonames.org/export/dump/allCountries.zip is used.

`data_type` : format of the data file to be read. Can currently be either `DwC-A` for an (unzipped) Darwin Core Archive or `dissco` for a JSON object exported from the DiSSCo prototype infrastructure.

## matching
`cores` : Set the number of cores to use for parallel processing. Set a low number such as 2 if you're not sure. You can also set a low number with a + and then the workflow will estimate the number of cores available on your system. Note that this does not check for availability of computational resources, so be conservative if you're not sure on availability.

`rmode` : Mode for the match validation. Can be "best" to only return the highest scored match, "cut" to return up to a certain number of matches or "all" to return all possible matches.

`parallel_cut` : The number of strings above which parallelization is implemented.

## export
`dwc_geo` : Set to `true` if an export to Darwin Core Occurence Core should be made. Will be saved to `data/output/geo`.

`fst` : Set to true if an export as a fast binary format should be made. This is useful for saving results without relatively long read/write operations. Will be saved to `data/output/fst`.

`ambiguous` : Set to true if a Darwin Core Occurrence file should be exported that only includes ambiguous matches. More useful for post-hoc validation. Will be saved to `data/output/ambiguous-geo`.

`dissco` : Set to true if a JSON export should be made according to the DiSSCo annotation data model. Will be saved to `data/output/dissco`. No support for handles so the annotations will have a GUID as an identifier.

## rebuild
`filename` : Path to the fst file from which a matched set can be rebuilt. Useful for re-exporting if new export formats are available without having to redo all matching processes.

# Run
To run the workflow, run the `run.R` script using either Rscript or an R IDE such as Rstudio.
