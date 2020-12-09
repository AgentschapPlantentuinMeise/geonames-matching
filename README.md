# geonames-matching
Match specimen record localities to geonames

This script attempts to connect specimen record locality strings to Geonames records. Decimal coordinate data for specimens is not recognized, but is used in a validation step.

The script is written in R and makes use of a Geonames data dump. These are regularly updated and can be downloaded from the Geonames website.

Required information for specimens is a locality text string and an ISO country code. Decimal latitude and longitude are optional.
