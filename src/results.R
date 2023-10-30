
unk = readLines("src/static/unknowns.txt")
locality_n = data %>%
  filter(!locality%in%unk,!is.na(locality))

locality_c = data %>%
  count(locality) %>%
  filter(!locality%in%unk,!is.na(locality))

locality_m = processed_results %>%
  count(locid)

locality_mm = locality_m %>%
  filter(n > 1)

locality_truemm = processed_results %>%
  count(locid,chunk) %>%
  filter(duplicated(locid))

coverage = locality_n %>%
  filter(locality%in%processed_results$locid)
