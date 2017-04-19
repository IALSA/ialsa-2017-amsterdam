# descriptives for the manuscript

ds <- ds_ms


ds %>% dplyr::glimpse()

ds %>% TabularManifest::histogram_continuous("cogact_old")
ds %>% TabularManifest::histogram_continuous("soc_net")



ds %>% TabularManifest::histogram_continuous("socact_old")
summary(ds$soc_net)
ds %>% TabularManifest::histogram_continuous("social_isolation")
