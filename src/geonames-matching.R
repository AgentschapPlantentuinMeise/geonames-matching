library(tidyverse)
library(magrittr)


##
###geonames raw data
##
data = read_tsv("data/geonames/allCountries.txt",
                col_names = F,
                col_types = cols(.default = "c"),
                quote = "")

#set colnames
colnames = read_delim("data/geonames/colnames.txt",
                      col_names = F,
                      delim=" : ") %>%
  mutate(cols = trimws(X1))

colnames(data) = colnames$cols

# geonames ids for BGBM specimens
# bgbm = read_csv("Botanical-Data-Export-Mathias.csv",
#                 col_types = cols(.default = "c"))
# 
# #remove the uri prefix to obtain the numeric ids
# bgbm$geoid = gsub("https://www.geonames.org/",
#                   "",
#                   bgbm$Geonames,
#                   fixed=T)
# bgbm$geoid = gsub("http://www.geonames.org/",
#                   "",
#                   bgbm$geoid,
#                   fixed=T)

##
###Meise raw data:
##
#(this merges two parts as latest database export was interrupted)
#remove records that throw an error upon tsv import
#this is typically due to tabs (\t) within a cell
exp = read_tsv("../publis/source files/01102020a.TXT",
               quote="",
               col_types = cols(.default = "c"))
ditched <- exp %>%
  slice(problems(.)$row) %>%
  mutate(reason = "import problem")
if (dim(problems(exp))[1]>0) {
  exp %<>% slice(-problems(.)$row)
}
expb = read_tsv("../publis/source files/01102020b.TXT",
                quote="",
                col_types = cols(.default = "c"))
exp = rbind(exp,expb)

##
###list all unique locality strings, separate by country code
##
locs = count(exp,
             LOCALITY,
             COUNTRY_CODE)
locs %<>% arrange(desc(n))
locs %<>% filter(!is.na(LOCALITY),
                 LOCALITY!="S.L.")

#add percentage scores for info
locs$cum = cumsum(locs$n)
sum = sum(locs$n)
locs$cumperc = locs$cum/sum
locs$perc = locs$n/sum

#mint an id for a unique locality
locs$locid = locs$LOCALITY

#separate locality on common separators
#not on space, as many placenames have spaces in their name
locs.split = separate_rows(locs,
                           LOCALITY,
                           sep=",|;| -|:|\\(|/|\'|\"")

#convert to alphabetic chars only and remove now empty substrings
locs.split$text = gsub("[^a-z]",
                       "",
                       tolower(locs.split$LOCALITY))
locs.split %<>% filter(text!="")

#check id for unique substrings to match to geonames, per country
locs.split$check = paste0(locs.split$text,
                          locs.split$COUNTRY_CODE)

#check id to connect substrings back to the unique locality strings
locs.split$check2 = paste0(locs.split$text,
                           locs.split$locid)

#deduplicate the substrings per country, 
#to remove redundancy from the matching process
locs.split2 = locs.split %>% 
  filter(!duplicated(check))

##
###list all country codes of the locality list
##
countries = count(locs.split2,
                  COUNTRY_CODE)
countries %<>% arrange(desc(n))

##
###Matching loop
##

#slow!
#per country, match a-z substrings of locality to geonames labels
locs.split2$geoid = NA
locs.split2$lat = NA
locs.split2$long = NA
locs.split2$geoname = NA
locs.split2$geoaltname = NA
locs.split2$cn = NA
out = locs.split2[1,]
for (i in 1:dim(countries)[1]) {
  apm = filter(locs.split2,
               COUNTRY_CODE==countries$COUNTRY_CODE[i])
  geo = filter(data,
               `country code`==countries$COUNTRY_CODE[i])
  geo %<>% 
    mutate(text = gsub("[^a-z]",
                       "",
                       tolower(name)),
           text2 = gsub("[^a-z|,]",
                        "",
                        tolower(alternatenames))) %>%
    separate_rows(text2,
                  sep=",")
  print(paste(countries$COUNTRY_CODE[i],
              countries$n[i],
              sep=": "))
  for (j in 1:dim(apm)[1]) {
    match = filter(geo,
                   apm$text[j]==text|
                     apm$text[j]==text2)
    apm$cn[j] = dim(match)[1]
    apm$geoid[j] = paste(match$geonameid,
                         collapse="|")
    apm$geoname[j] = paste(match$name,
                           collapse="|")
    apm$geoaltname[j] = paste(match$alternatenames,
                              collapse="|")
    apm$lat[j] = paste(match$latitude,
                       collapse="|")
    apm$long[j] = paste(match$longitude,
                        collapse="|")
  }
  out = rbind(out,apm)
}
out = out[-1,]

##
###re-add all duplicate substrings for different locality strings
##
redu = filter(locs.split,
              !check2%in%out$check2)
redu = left_join(redu,
                 select(out,
                        check,
                        geoid,
                        lat,
                        long,
                        geoname,
                        geoaltname,
                        cn),
                 by=c("check"="check"))
#reorder the check value, only if necessary:
#redu %<>%
#  mutate(check3 = check2) %>%
#  select(-check2) %>%
#  rename(check2=check3)
out.redu = rbind(out,
                 redu)

##
###filter out if no match or multiple geoids were found
##

#at least one match with geonames
matchfound = filter(out.redu,
                    geoid!="")

#matches multiple geonames records
#might be auto-disambiguatable using coordinates
multi = filter(out.redu,
               grepl("|",
                     geoid,
                     fixed=T))

#one-to-one match of the substring to geonames
singlematch = filter(matchfound,
                     !grepl("|",
                            geoid,
                            fixed=T))

##
###save as files, so not redo the loop every time
##
# time = format(Sys.time(), 
#               "%Y-%m-%d %I.%M%p")
# write_tsv(out,
#           paste0("raw allcountrymatching ",
#                      time,
#                      ".tsv"),
#           na="")

##
###filter out if multiple parts of the locality string matched geonames labels
##
#although these are not necessarily different?
dupl = filter(singlematch,
              duplicated(locid))
ambigu = filter(singlematch,
                locid%in%dupl$locid)

singlematch2 = filter(singlematch,
                      !locid%in%ambigu$locid)
##
###remove substrings that are too short and likely false positives
##
singlematch3 = filter(singlematch2,
                      nchar(text)>3)
##
###Add geonames ids to specimen records
##
exp3 = filter(exp,
              LOCALITY%in%singlematch3$locid)
exp3 = left_join(exp3,
                 select(singlematch3,
                        locid,
                        geoid,
                        lat,
                        long),
                 by=c("LOCALITY"="locid"))
#write_tsv(exp3,
#          "enriched specimen data.txt",
#          na="")

##
###overlap with BGBM geonames ids
##
# bgbmmatch = filter(singlematch3,
#                    geoid%in%bgbm$geoid)

##
###Use georeferenced coordinates of the specimens for validation
##

#join with specimen data to compare present coordinates
exp2 = filter(exp,
              LOCALITY%in%singlematch3$locid,
              !is.na(LATITUDE_DECIMAL))
exp2 %<>% select(LOCALITY,
                 LATITUDE_DECIMAL,
                 LONGITUDE_DECIMAL)
exp2$check = paste0(exp2$LOCALITY,
                    exp2$LATITUDE_DECIMAL)
exp2 = filter(exp2,
              !duplicated(check))
exp2 %<>% select(-check)
coordcheck= left_join(singlematch3,
                      exp2,
                      by=c("locid"="LOCALITY"))

#difference between decimal coordinates
coordcheck %<>%
  mutate(latdiff = abs(as.numeric(LATITUDE_DECIMAL) - as.numeric(lat)),
         longdiff = abs(as.numeric(LONGITUDE_DECIMAL) - as.numeric(long)))

#check the minimum per locality, as specimen coordinates may very well be wrong
coordcheck2 = coordcheck %>%
  group_by(locid) %>%
  summarise(latd = min(latdiff,
                       na.rm=T),
            longd = min(longdiff,
                        na.rm=T))

#validate using 2 as the cut-off: this is high because many matches 
#are on a large scale, e.g. province

#fit the criterion
coordchecked = filter(coordcheck2,
              longd<2,
              latd<2)

#have no specimen coordinate data
nocoord = filter(coordcheck2,
              is.infinite(longd)|
                is.infinite(latd))

#do not fit the criterion
coordinvalid = filter(coordcheck,
              !locid%in%coordchecked$locid,
              !locid%in%nocoord$locid)

#re-add all original data, incl geoid
coordcheckedval = filter(singlematch3,
                         locid%in%coordchecked$locid)
noncoordval = filter(singlematch3,
                     locid%in%nocoord$locid)
