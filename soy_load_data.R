## September 11, 2016

## NASA MSU Telecoupling
## Soybean trade 
## data loading for further analyses

## Danica Schaffer-Smith
## djs50@duke.edu

## Requires "soypath" file path object for it to run.

###############################################################################################
## Prepare the data
## Country id crosswalk table
## from: http://www.fao.org/countryprofiles/iso3list/en/
countries <- read.csv(file.path(soypath,"data/FAO_country_ids_crosswalk.csv"), header = T, stringsAsFactors = F)

## World Countries data
## data are from: http://ec2-54-201-183-195.us-west-2.compute.amazonaws.com/cow/
cow <- read.csv(file.path(soypath,"data/cow.csv"), header = T, stringsAsFactors = F)
head(cow)

## World Countries xy coordinates
## data area from: https://developers.google.com/public-data/docs/canonical/countries_csv
#coords <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/cow_centroids.csv", header = T, stringsAsFactors = F)
coords <- read.csv(file.path(soypath,"data/country_centroids_all.csv"), header = T, stringsAsFactors = F)
head(coords)
#coords <- merge(coords, countries, by.x = "ISO3136", by.y = "ISO2")
#head(coords)

## Read in the soybean trade data
soy1 <- read.csv(file.path(soypath,"data/FAOSTAT_matrix_soybeans.csv"), header = T, stringsAsFactors = F)

## Filter columns
soy = soy1[, c("Reporter.Countries","Partner.Countries","Element","Year","Unit","Value")]

## Fix multiple mismatches for the country names ---------------------------
## Combine the Chinas
soy$Reporter.Countries[soy$Reporter.Countries=="China, mainland"] <- "China"
soy$Reporter.Countries[soy$Reporter.Countries=="China, Taiwan Province of"] <- "China"
soy$Reporter.Countries[soy$Reporter.Countries=="China, Hong Kong SAR"] <- "China"
soy$Reporter.Countries[soy$Reporter.Countries=="China, Macao SAR"] <- "China"
soy$Partner.Countries[soy$Partner.Countries=="China, mainland"] <- "China"
soy$Partner.Countries[soy$Partner.Countries=="China, Taiwan Province of"] <- "China"
soy$Partner.Countries[soy$Partner.Countries=="China, Hong Kong SAR"] <- "China"
soy$Partner.Countries[soy$Partner.Countries=="China, Macao SAR"] <- "China"

## Separate Belgium and Belgium-Luxembourg
soy$Reporter.Countries[soy$Reporter.Countries=="Belgium-Luxembourg"] <- "Luxembourg"
soy$Partner.Countries[soy$Partner.Countries=="Belgium-Luxembourg"] <- "Luxembourg"

## Make Virgin Islands match
soy$Reporter.Countries[soy$Reporter.Countries=="British Virgin Islands"] <- "Virgin Islands, British" 
soy$Partner.Countries[soy$Partner.Countries=="British Virgin Islands"] <- "Virgin Islands, British" 

## Make Cote d'Ivoire match
soy$Reporter.Countries[soy$Reporter.Countries=="C?te d'Ivoire"] <- "Cote d'Ivoire" 
soy$Partner.Countries[soy$Partner.Countries=="C?te d'Ivoire"] <- "Cote d'Ivoire" 
soy$Reporter.Countries[soy$Reporter.Countries=="C\xf4te d'Ivoire"] <- "Cote d'Ivoire" 
soy$Partner.Countries[soy$Partner.Countries=="C\xf4te d'Ivoire"] <- "Cote d'Ivoire" 

## Make Congo match
soy$Reporter.Countries[soy$Reporter.Countries=="Democratic Republic of the Congo"] <- "Congo, the Democratic Republic of the" 
soy$Partner.Countries[soy$Partner.Countries=="Democratic Republic of the Congo"] <- "Congo, the Democratic Republic of the"

## Make Ethiopea PDR match
soy$Reporter.Countries[soy$Reporter.Countries=="Ethiopea PDR"] <- "Ethiopea" 
soy$Partner.Countries[soy$Partner.Countries=="Ethiopea PDR"] <- "Ethiopea"

## Make Iran match
soy$Reporter.Countries[soy$Reporter.Countries=="Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"
soy$Partner.Countries[soy$Partner.Countries=="Iran (Islamic Republic of)"] <- "Iran, Islamic Republic of"

## Make Micronesia match
soy$Reporter.Countries[soy$Reporter.Countries=="Micronesia (Federated States of)"] <- "Micronesia, Federated States of"
soy$Partner.Countries[soy$Partner.Countries=="Micronesia (Federated States of)"] <- "Micronesia, Federated States of"

## Make Korea match
soy$Reporter.Countries[soy$Reporter.Countries=="Republic of Korea"] <- "Korea"
soy$Partner.Countries[soy$Partner.Countries=="Republic of Korea"] <- "Korea"

## Make Republic of Moldova Match
soy$Reporter.Countries[soy$Reporter.Countries=="Republic of Moldova"] <- "Moldova, Republic of"
soy$Partner.Countries[soy$Partner.Countries=="Republic of Moldova"] <- "Moldova, Republic of"

## Make R?union Match
soy$Reporter.Countries[soy$Reporter.Countries=="R?union"] <- "Reunion"
soy$Partner.Countries[soy$Partner.Countries=="R?union"] <- "Reunion"

## Make R\xe9union Match
soy$Reporter.Countries[soy$Reporter.Countries=="R\xe9union"] <- "Reunion"
soy$Partner.Countries[soy$Partner.Countries=="R\xe9union"] <- "Reunion"

## Make United States of America Match
soy$Reporter.Countries[soy$Reporter.Countries=="United States of America"] <- "United States"
soy$Partner.Countries[soy$Partner.Countries == "United States of America"] <- "United States"

## Serbia and Montenegro dissolved in 2006... Split the amounts equally?
sm.report <- soy[which(soy$Reporter.Countries=="Serbia and Montenegro"),]
sm.partner <- soy[which(soy$Partner.Countries=="Serbia and Montenegro"),]
sm <- rbind(sm.report, sm.partner)
## Copy to two new dfs
serbia <- sm
montenegro <- sm
## Give each country 1/2 the trade value
serbia$Value <- 0.5*(sm$Value)
montenegro$Value <- 0.5*(sm$Value)
## Overwrite fields with updated country names
soy$Reporter.Countries[soy$Reporter.Countries=="Serbia and Montenegro"] <- "Serbia"
soy$Partner.Countries[soy$Partner.Countries=="Serbia and Montenegro"] <- "Serbia"
soy$Reporter.Countries[soy$Reporter.Countries=="Serbia and Montenegro"] <- "Montenegro"
soy$Partner.Countries[soy$Partner.Countries=="Serbia and Montenegro"] <- "Montenegro"
## Bind these modified records back to the main df
soy <- rbind(soy, serbia, montenegro)
## remove the oudated records
soy <- soy[!soy$Reporter.Countries=="Serbia and Montenegro",]
soy <- soy[!soy$Partner.Countries=="Serbia and Montenegro",]

## Make Sudan Match
soy$Reporter.Countries[soy$Reporter.Countries=="Sudan(former)"] <- "Sudan"
soy$Partner.Countries[soy$Partner.Countries=="Sudan(former)"] <- "Sudan"

## Make Tanzania Match
soy$Reporter.Countries[soy$Reporter.Countries=="United Republic of Tanzania"] <- "Tanzania, United Republic of"
soy$Partner.Countries[soy$Partner.Countries=="United Republic of Tanzania"] <- "Tanzania, United Republic of"

## Make The former Yugoslav Republic of Macedonia Match
soy$Reporter.Countries[soy$Reporter.Countries=="The former Yugoslav Republic of Macedonia"] <- "Macedonia, the former Yugoslav Republic of"
soy$Partner.Countries[soy$Partner.Countries=="The former Yugoslav Republic of Macedonia"] <- "Macedonia, the former Yugoslav Republic of"

## Make the USSR match (outdated name)
soy$Reporter.Countries[soy$Reporter.Countries=="USSR"] <- "Russian Federation"
soy$Partner.Countries[soy$Partner.Countries=="USSR"] <- "Russian Federation"

## Make Yugoslav SFR Match (outdated name)
soy$Reporter.Countries[soy$Reporter.Countries=="Yugoslav SFR"] <- "Yugoslavia"
soy$Partner.Countries[soy$Partner.Countries=="Yugoslav SFR"] <- "Yugoslavia"

## Shorten Bolivia (not a fan of the "Plurinational State" tag)
soy$Reporter.Countries[grep("Bolivia", soy$Reporter.Countries)] <- "Bolivia"
soy$Partner.Countries[grep("Bolivia", soy$Partner.Countries)] <- "Bolivia"


# Do a bit more data cleanup ----------------------------------------------

## Group data by country names, Export units and Year
library(reshape2)
library(dplyr)
grouped <- group_by(soy, Reporter.Countries, Partner.Countries, Element, Year)
soy.cleaned <- dplyr::summarize(grouped, sum = sum(Value))

length(unique(soy.cleaned$Reporter.Countries)) # 126 reporting countries (exporting)
length(unique(soy.cleaned$Partner.Countries)) # 221 partners (importers)

