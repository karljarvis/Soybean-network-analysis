## May 16, 2017

## NASA MSU Telecoupling
## Examination of relationships between soybean exports and amount of deforestation in large clearings over time by country (2000-2013) (data from Austin et al. 2017)
## Looking also at relationships between GNI per capita / Income Groups (World Bank Data)

## Danica Schaffer-Smith
## djs50@duke.edu

## Load the libraries
#library(network)  # basic 'statnet' network library
#library(maps)     # basic R map plotting library

#################################################################################################################3
## Apply to the soy data

# Data: Import, format, and filter -------------------------------------------------------

## Global trade matrix data from FAOSTAT
#trade <- read.csv("C:/Users/djs50/Desktop/Soybeans/data/FAOSTAT_data/Trade_DetailedTradeMatrix_E_All_Data.csv", header = T, stringsAsFactors = F)
#names(trade)
#The pairwise countries of interest are "Reporter.Countries" and "Partner.Countries". 
#The weights for each timestep are contained in the "YXXXX" columns. 

## Country id crosswalk table
## from: http://www.fao.org/countryprofiles/iso3list/en/
#soypath = file.path("~/GoogleDriveNAU/telecoupling/Soybean_network_analysis")
#soypath = "C:/Users/djs50/Documents/telecoupling"
soypath = "C:/Users/djs50/Desktop/telecoupling"

source(file.path(soypath, "soy_load_data.R"))

## Load packages
library(plyr)
library(tidyr)
library(reshape2)
library(dplyr)
library(quantmod)
library(ggplot2)

# Do a bit more data cleanup ----------------------------------------------

## Determine total export value sent by reporter countries by year
#library(reshape2)
#library(dplyr)
exports <- filter(soy, Element == "Export Value") %>% # Filter to keep only Export Value
  group_by(Reporter.Countries,Year) %>%
  dplyr::summarize(totValue = sum(Value)) %>%
  mutate(export_MUSD = totValue*0.001) # convert Export Value to Millions USD

## Determine total export value received by partner countries by year
imports <- filter(soy, Element == "Export Value") %>% # Filter to keep only Export Value
  group_by(Partner.Countries,Year) %>%
  dplyr::summarize(totValue = sum(Value)) %>%
  mutate(import_MUSD = totValue*0.001) # convert Export Value to Millions USD

## Add Zeros for the years without data???

## Merge into one data frame
exports.imports <- merge(exports, imports, by.x = c("Reporter.Countries", "Year"), by.y = c("Partner.Countries", "Year"), all = T)
str(exports.imports) # 3855 records. 
length(unique(exports.imports$Reporter.Countries)) # 221 countries total

# Calculate the differencein exports and imports year to year by country
#library(quantmod)
#exports.imports$dt_export_MUSD <- with(exports.imports, ave(export_MUSD, Reporter.Countries, FUN=Delt, type = "arithmetic"))
#exports.imports$dt_import_MUSD <- with(exports.imports, ave(import_MUSD, Reporter.Countries, FUN=Delt))
exports.imports <- exports.imports %>% arrange(Reporter.Countries, Year) %>% mutate(diff_export_MUSD = ((export_MUSD-lag(export_MUSD))/ (Year - lag(Year))))
exports.imports <- exports.imports %>% arrange(Reporter.Countries, Year) %>% mutate(diff_import_MUSD = ((import_MUSD-lag(import_MUSD))/ (Year - lag(Year))))

## NaN, -Inf, Inf, and NA values introduced
## NaN <- 0, no change between years
## Inf <- 1, indicates that the change exceeded 100% increase
## Inf <- -1, indicates that the change exceeded 100% decrease
## NA values occur for the first year of the time series -- no preceding year can be used to calculate change
#exports.imports$dt_export_MUSD[ is.nan(exports.imports$dt_export_MUSD) ] <- 0
#exports.imports$dt_export_MUSD[exports.imports$dt_export_MUSD == Inf] <- 1
#exports.imports$dt_export_MUSD[exports.imports$dt_export_MUSD == -Inf] <- -1
#exports.imports$dt_import_MUSD[ is.nan(exports.imports$dt_import_MUSD) ] <- 0
#exports.imports$dt_import_MUSD[exports.imports$dt_import_MUSD == Inf] <- 1
#exports.imports$dt_import_MUSD[exports.imports$dt_import_MUSD == -Inf] <- -1

## -------------------------------------------------------------------------
## Read in the Hansen Data with deforestation data by country by year

hansen <- read.csv("C:/Users/djs50/Desktop/telecoupling/data/Austin_et_al_2017_data/patch_dynamics_pantropics_reformat_djs_20170516.csv", header = T, stringsAsFactors = F)
#replace(hansen$p.year, hansen$p.year== c(1,2,3,4,5,6,7,8,9,10,11,12,13), c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
## Read in the Global Forest Watch (Hansen) Tree Cover >50% estimate for 2000
## Our loss data are for pixels with 50% cover also.
## Names have been corrected to match those in the soybean export dataset
forest2000 <- read.csv("C:/Users/djs50/Desktop/telecoupling/data/GFW_data/tree_cover_reformat_djs_20170607.csv", header = T, stringsAsFactors = F)
forest2000 <- forest2000 %>% mutate(cover.2000.gt50.ha = gt50_ha + gt75_ha)

## Merge with Hansen data
hansen <- merge(hansen, forest2000, by.x = "country", by.y = "Country")
length(unique(hansen$country)) # 112 unique countries in the dataset could be matched

## Some tweaking
hansen <- select(hansen, -loss_pa, -GDPpcPPP, -climadomain, -subregion, -gt10_ha, -gt15_ha, -gt20_ha, -gt25_ha, -gt30_ha, -gt50_ha, -gt75_ha) # remove some fields
hansen$p.year <- mapvalues(hansen$p.year, from=c(1,2,3,4,5,6,7,8,9,10,11,12,13), to=c(2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013))
hansen$p.type <- factor(hansen$p.type, levels = c("1", "2", "3", "4"))
hansen$income00 <- factor(hansen$income00, levels = c("L", "LM", "UM", "H"))

## Tabulate total deforested area in each year
##mutate(hansen, change.area = group_by(p.type, (p.area - lag(p.area) / (year - lag(year)))))
hansen <- hansen %>% 
  group_by(country, p.year, income00, cover.2000.gt50.ha) %>% 
  arrange(p.year) %>%
  dplyr::summarize(tot.loss.ha = sum(p.area))

# Calculate the change in p.area year to year
#hansen$dt.tot.p.area <- with(hansen, ave(tot.p.area, country, FUN=Delt))
#hansen$dt.p.freq <- with(hansen, ave(p.freq, country, FUN=Delt))
hansen <- hansen %>% 
  group_by(country, p.year) %>% 
  mutate(diff.loss.ha = (tot.loss.ha-lag(tot.loss.ha)))

#test <- hansen %>%
#  group_by(country, p.year) %>%
#  arrange(p.year) %>%
#  mutate(diff.p.area = (tot.p.area - lag(tot.p.area) / (p.year - lag(p.year))))
#View(test)

## NOT RUN

# Calculate the change in p.area year to year
#hansen$dt.tot.p.area <- with(hansen, ave(tot.p.area, country, FUN=Delt))
#hansen$dt.p.freq <- with(hansen, ave(p.freq, country, FUN=Delt))
#hansen <- hansen %>% 
#  group_by(country, p.year) %>% 
#  mutate(diff.area = ((tot.p.area-lag(tot.p.area)) / (p.year - lag(p.year))))

## NaN, -Inf, Inf, and NA values introduced
## NaN <- 0, no change between years
## Inf <- 1, indicates that the change exceeded 100% increase
## Inf <- -1, indicates that the change exceeded 100% decrease
## NA values occur for the first year of the time series -- no preceding year can be used to calculate change
#hansen$dt.tot.p.area[ is.nan(hansen$dt.tot.p.area) ] <- 0
#hansen$dt.tot.p.area[hansen$dt.tot.p.area == Inf] <- 1
#hansen$dt.tot.p.area[hansen$dt.tot.p.area == -Inf] <- -1


## -------------------------------------------------------------------------
## Read in the GDP data by country

gdp <- read.csv("C:/Users/djs50/Desktop/telecoupling/data/GDP_World_Bank_data/API_3_DS2_en_csv_v2_reformat_djs_20170515.csv", header = T, stringsAsFactors = F)
keep <- c("GNI per capita, Atlas method (current US$)")
keep.gdp <- gdp[gdp$Indicator.Name %in% keep, ]
gathered.gdp <- gather(keep.gdp, key, value, -Country.Name, -Country.Code, -Indicator.Name, -Indicator.Code)
names(gathered.gdp)[5] <- "Year"
names(gathered.gdp)[6] <- "GNI_per_capita_USD"
year <- substring(gathered.gdp$Year, 2, nchar(gathered.gdp$Year))
gathered.gdp$Year <- as.numeric(year) # make year numeric

gathered.gdp <- gathered.gdp %>% 
  group_by(Country.Name, Year) %>%
  mutate(diff.GNI = GNI_per_capita_USD-lag(GNI_per_capita_USD))
  
## NOT RUN
# Calculate the change in gdp year to year
#gathered.gdp$dt.GNI <- with(gathered.gdp, ave(GNI_per_capita_USD, Country.Name, FUN=Delt))
#gathered.gdp <- gathered.gdp %>% mutate(dt.GNI = ((((GNI_per_capita_USD-lag(GNI_per_capita_USD))/lag(GNI_per_capita_USD)) / (Year - lag(Year)))))
#test <- gathered.gdp %>% 
#  group_by(Country.Name, Year) %>%
#  mutate(dt.GNI = (((GNI_per_capita_USD/lag(GNI_per_capita_USD)) / (Year - lag(Year)))*100))

## Replace erroneous NaN and Inf values!
#gathered.gdp$dt.GNI[ is.nan(gathered.gdp$dt.GNI) ] <- 0
#gathered.gdp$dt.GNI[gathered.gdp$dt.GNI == Inf] <- 1
#gathered.gdp$dt.GNI[gathered.gdp$dt.GNI == -Inf] <- -1

## -------------------------------------------------------------------------
## Read in the income class data by country

income <- read.csv("C:/Users/djs50/Desktop/telecoupling/data/Income_class_World_Bank_data/Income_classes_reformatted_djs_20170515.csv", header = T, stringsAsFactors = F)
gathered.income <- gather(income, key, value, -Code, -Country)
names(gathered.income)[1] <- "Country.Code"
names(gathered.income)[2] <- "Country.Name"
names(gathered.income)[3] <- "Year"
names(gathered.income)[4] <- "Income_Group"
year <- substring(gathered.income$Year, 2, nchar(gathered.income$Year))
gathered.income$Year <- year

## --------------------------------------------------------------------------
## Merge GDP and income class to Hansen deforestation data

socioeco <- merge(hansen, gathered.income, by.x = c("country", "p.year"), by.y = c("Country.Name", "Year"))
socioeco <- merge(socioeco, gathered.gdp, by.x = c("country", "p.year"), by.y = c("Country.Name", "Year"))

length(unique(hansen$country)) # Hansen dataset had 112 unique countries
length(unique(socioeco$country)) # The merged data has 104 unique countries

## Make country names match those in the soybean export dataset
socioeco$country[socioeco$country=="Bahamas, The"] <- "Bahamas"
socioeco$country[socioeco$country=="British Virgin Islands"] <- "Virgin Islands, British"
socioeco$country[socioeco$country=="Congo, Rep."] <- "Congo" 
socioeco$country[socioeco$country=="Congo, Dem. Rep."] <- "Congo, the Democratic Republic of the"
socioeco$country[socioeco$country=="Lao PDR"] <- "Lao People's Democratic Republic"
socioeco$country[socioeco$country=="Micronesia, Fed. Sts."] <- "Micronesia, Federated States of"
socioeco$country[socioeco$country=="Sudan"] <- "Sudan (former)"
socioeco$country[socioeco$country=="Tanzania"] <- "Tanzania, United Republic of"
socioeco$country[socioeco$country=="Venezuela, RB"] <- "Venezuela (Bolivarian Republic of)"
socioeco$country[socioeco$country=="Virgin Islands (U.S.)"] <- "United States Virgin Islands"

## Add soybean import/export attributes
socioeco <- merge(socioeco, exports.imports, by.x = c("country", "p.year"), by.y = c("Reporter.Countries", "Year"))
length(unique(socioeco$country)) # 100 countries
pantropics <- unique(socioeco$country)
#write.csv(pantropics, "C:/Users/djs50/Desktop/telecoupling/pantropical_countries_list.csv")

## NOT RUN ## 
## Replace Inf with NA
#socioeco <- do.call(data.frame,lapply(socioeco, function(x) replace(x, is.infinite(x),NA)))

## Compute log change in deforestation and exports year to year -- will be undefined for all negative values.
#socioeco$log_dt_export_MUSD <- log10(socioeco$dt_export_MUSD)
#socioeco$log_dt.p.area <- log10(socioeco$dt.tot.p.area)

## There are 32 countries that have complete data without NaNs for the log change in exports and log change in deforestation
## There are 144 complete records for countries with small patch type, 18 medium, 5 large, 2 extra large.
## 61 UM, 74 LM, 29 L, 5 H income country records

## Filter by only dominant soy exporters
#median(socioeco$export_MUSD, na.rm = T) # median soy export value was 0.183 million usd
#countries <- filter(socioeco, export_MUSD >= 0.183)
#dom.countries <- unique(countries$country) # This makes just 29 countries to track instead of the whole pantropics
#socioeco.domprod <- filter(socioeco, country %in% dom.countries) # dominant producers
#socioeco.domprod <- filter(socioeco, export_MUSD >= 0.183)
#unique(socioeco.domprod$country)

################

## Compute cumulative sums of deforestation area and export values over the whole period
test1 <- socioeco%>%
  group_by(country, income00, cover.2000.gt50.ha) %>%
  dplyr::summarise(sum.loss.ha = sum(tot.loss.ha, na.rm = T),
            sum.importMUSD = sum(import_MUSD, na.rm = T),
            sum.exportMUSD = sum(export_MUSD, na.rm = T),
            sum.GNI = sum(GNI_per_capita_USD, na.rm = T)) %>%
  mutate(log.sum.exportMUSDcor = ifelse(sum.exportMUSD >0, log10(sum.exportMUSD/cover.2000.gt50.ha), NA),
         log.sum.importMUSDcor = ifelse(sum.importMUSD >0, log10(sum.importMUSD/cover.2000.gt50.ha), NA),
         log.sum.loss.hacor = ifelse(sum.loss.ha >0, log10(sum.loss.ha/cover.2000.gt50.ha), NA)
         )

#test1 <- test1[!is.na(test1$income00),] # remove countries which have no starting income group in 2000

## Countries to highlight:
## L: Indonesia
## LM: Guyana, Peru, or Honduras?
## UM: Brazil
highlight.countries <- chighlight.countries <- c("Indonesia", "Brazil", "Cameroon", "Jamaica", "Paraguay", "South Africa")
test1.highlight <- test1[test1$country %in% highlight.countries, ]

p1 <- ggplot(test1, aes(x = log10(sum.exportMUSD/cover.2000.gt50.ha), y = log(sum.loss.ha/cover.2000.gt50.ha), label = country))+
  geom_point(size = 0.5)+
  #geom_text(size =3)+
  stat_smooth(method = "lm", se = F, size = 0.5) +
  #labs(x = "Cumulative Exports (Millions USD)", y = "Cumulative \nForest Loss (ha)") +
  labs(x = "Cumulative Exports (Millions USD)", y = "Cumulative Forest Loss (ha)") +
  #scale_x_continuous(limits = c(-30, 5)) +
  #scale_y_continuous(limits = c(-30, 10))
  theme_bw()+
  theme(axis.text = element_text(size = 6))+
  theme(axis.title = element_text(size = 6))+
  #facet_grid(income00 ~ .)
## Add text labels for highlighted countries
  geom_text(data = test1.highlight, size = 2)
## Save Figure
#ggsave("H:/telecoupling/figures/CumExports_predict_CumForestLoss_forest_area_corrected.png", plot = p1, width = 7.5, height = 5, units = "cm", dpi = 300)


## Plot Figure 5, Manuscript Revision #2:
p1 <- ggplot(test1, aes(x = log10(sum.exportMUSD/cover.2000.gt50.ha), y = log(sum.loss.ha/cover.2000.gt50.ha), label = country))+
  geom_point(size = 0.5)+
  #geom_text(size =3)+
  stat_smooth(method = "lm", se = F, size = 0.5) +
  #labs(x = "Cumulative Exports (Millions USD)", y = "Cumulative \nForest Loss (ha)") +
  labs(x = "Cumulative Exports (Millions USD)", y = "Cumulative Forest Loss (ha)") +
  #scale_x_continuous(limits = c(-30, 5)) +
  #scale_y_continuous(limits = c(-30, 10))
  #geom_text(data = test1.highlight, size = 3)+
  theme_bw()+
  theme(text = element_text(size = 16))+
  #theme(axis.text = element_text(size = 6))+
  #theme(axis.title = element_text(size = 6))+
  #facet_grid(income00 ~ .)
  ## Add text labels for highlighted countries
  geom_text(data = test1.highlight, size = 6)
## Save Figure
#ggsave("D:/telecoupling/figures/CumExports_predict_CumForestLoss_forest_area_correctedV2.png", plot = p1, width = 7.5, height = 5, units = "cm", dpi = 300)
png(file.path('C:/Users/djs50/Desktop/telecoupling/figures', 'FIG05_CumExports_predict_CumForestLoss_forest_area_correctedV3.png'), width=400, height=400)
grid.arrange(p1, ncol=1)
dev.off() 

## Highlight significant regressions by income group
highlight.groups <- c("L", "LM", "UM", "H")
test1.highlight.groups <- test1[test1$income00 %in% highlight.groups, ]

ggplot(test1, aes(x = log10(sum.exportMUSD/cover.2000.gt50.ha), y = log(sum.loss.ha/cover.2000.gt50.ha), col = income00, label = country))+
  #scale_shape_discrete(solid = F, legend = F)+
  geom_point(mapping = aes(shape = income00, color = income00), size = 2)+
  scale_shape(solid = F, name = "Income Level\n(2000)")+
  scale_color_discrete(name = "Income Level\n(2000)")+
  #geom_text(size =3)+
  stat_smooth(data = test1.highlight.groups, method = "lm", se = F) +
  labs(x = "log10 Cumulative Export Value (Millions USD)", y = "log10 Cumulative Deforested Area (ha)", fill = "Income Group\n(2000)") +
  #guide_legend(title="Income Group\n(2000)")+
  #guides(fill = guide_legend(title = "Income Group\n(2000)"))+
  theme_bw()
#facet_grid(p.type ~ .)

## Limit to complete cases for regression.
test1.complete <- test1[complete.cases(test1),]# Remove some na values for the variables of interest
test1.complete <- test1.complete[is.finite(test1.complete$log.sum.exportMUSDcor) & is.finite(test1.complete$log.sum.loss.hacor),] # Remove Inf values

fit1 <- lm(log.sum.loss.hacor~log.sum.exportMUSDcor, data = test1.complete, na.action = na.exclude)
summary(fit1)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1.27580 -0.31271  0.01305  0.28160  0.96918 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            -0.3817     0.2110  -1.809   0.0766 .  
#log.sum.exportMUSDcor   0.1648     0.0320   5.148 4.64e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.4599 on 49 degrees of freedom
#Multiple R-squared:  0.351,	Adjusted R-squared:  0.3378 
#F-statistic: 26.51 on 1 and 49 DF,  p-value: 4.642e-06

## Regressions by Starting Income Categories in 2000
fits_all <- test1.complete %>%
  group_by(income00) %>%
  do(fit = lm(log.sum.loss.hacor~log.sum.exportMUSDcor, .))
fits_all # fit list column holds one lm for each country. 

## Examine the results of the lms for each country
library(broom)
results <- fits_all %>%
  glance(fit)
results[which(results$p.value <= 0.05),]

#income00 r.squared adj.r.squared     sigma statistic      p.value    df      logLik       AIC
#<fctr>     <dbl>         <dbl>     <dbl>     <dbl>        <dbl> <int>       <dbl>     <dbl>
#1        L 0.4215276     0.3926040 0.4929350 14.573821 1.077436e-03     2 -14.6059197 35.211839
#2       LM 0.6493206     0.6259420 0.3388531 27.774115 9.425768e-05     2  -4.6608646 15.321729
#3       UM 0.5054638     0.4348158 0.2996121  7.154676 3.177677e-02     2  -0.7921314  7.584263


## Evaluate whether there is a relationship between the year to year difference in deforested area and the difference in soybean exports
test2 <- socioeco %>% 
  mutate(log_diff_export_MUSDcor = log10(diff_export_MUSD/cover.2000.gt50.ha),
         log_diff_import_MUSDcor = log10(diff_import_MUSD/cover.2000.gt50.ha),
         log.diff.loss.hacor = log10(diff.loss.ha/cover.2000.gt50.ha),
         log.diff.GNIcor = log10(diff.GNI/cover.2000.gt50.ha)
)

test2.complete <- test2[!is.na(test2$log.diff.loss.hacor),]# Remove some na values for the variables of interest
test2.complete <- test2.complete[!is.na(test2.complete$log_diff_export_MUSDcor),]# Remove some na values for the variables of interest
test2.complete$country <- as.factor(test2.complete$country)

ggplot(test2.complete, aes(x = log_diff_export_MUSDcor, y = log.diff.loss.hacor, color = income00))+
  geom_point()+
  #geom_text(size =3)+
  stat_smooth(method = "lm", se = F)+
  labs(x = "log10 Difference in Export Value (Millions USD)", y = "log10 Difference in Deforested Area (ha)") +
  theme_bw()
#facet_grid(p.type ~ .)

## Regressions significant by income category? 
highlight.groups <- c("L", "LM", "UM")
test2.highlight.groups <- test2[test2$income00 %in% highlight.groups, ]

ggplot(test2.complete, aes(x = log_diff_export_MUSDcor, y = log.diff.loss.hacor, col = income00, label = country))+
  #scale_shape_discrete(solid = F, legend = F)+
  geom_point(mapping = aes(shape = income00, color = income00))+
  scale_shape(solid = F, name = "Income Level\n(2000)")+
  scale_color_discrete(name = "Income Level\n(2000)")+
  #geom_text(size =3)+
  stat_smooth(data = test2.highlight.groups, method = "lm", se = F) +
  labs(x = "log10 Difference in Export Value (Millions USD)", y = "log10 Difference in Deforested Area (ha)", fill = "Income Group\n(2000)") +
  #guide_legend(title="Income Group\n(2000)")+
  #guides(fill = guide_legend(title = "Income Group\n(2000)"))+
  theme_bw()
#facet_grid(p.type ~ .)

## Linear model for pooled data
test2.complete <- test2.complete[is.finite(test2.complete$log_diff_export_MUSDcor), ]
test2.complete <- test2.complete[is.finite(test2.complete$log.diff.loss.hacor), ]
fit1 <- lm(log.diff.loss.hacor ~ log_diff_export_MUSDcor, data = test2.complete, na.action = na.exclude)
summary(fit1)
#Residuals:
#  Min       1Q   Median       3Q      Max 
#-2.15252 -0.34756  0.09768  0.36363  1.74191 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             -2.24321    0.28748  -7.803 9.23e-12 ***
#  log_diff_export_MUSDcor  0.10704    0.03892   2.750  0.00717 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 0.6046 on 92 degrees of freedom
#Multiple R-squared:  0.07598,	Adjusted R-squared:  0.06594 
#F-statistic: 7.565 on 1 and 92 DF,  p-value: 0.007167

## Regressions by Country
fits_all2 <- test2.complete %>%
  group_by(country) %>%
  do(fit = lm(log.diff.loss.hacor~log_diff_export_MUSDcor, .))
fits_all2 # fit list column holds one lm for each country. 

## Examine the results of the lms for Income Category
library(broom)
results2 <- fits_all2 %>%
  glance(fit)
results2[which(results2$p.value <= 0.05),]
#country r.squared adj.r.squared     sigma statistic    p.value    df   logLik       AIC
#<fctr>     <dbl>         <dbl>     <dbl>     <dbl>      <dbl> <int>    <dbl>     <dbl>
#  1 South Africa 0.9314679     0.8972018 0.0912636  27.18338 0.03487418     2 5.286553 -4.573107



#########################################################
## Do richer countries import more soybean over time?

## Very non-linear. Not appropriate to evaluate w/ linear regression.
ggplot(test1, aes(x = sum.GNI, y = sum.importMUSD))+
  geom_point()+
  #geom_text(size =3)+
  geom_smooth(method = "lm") #+
#facet_grid(Income_Group ~ .)

## When viewed on a log scale, we see a relationship between GNI and imports.
ggplot(test1, aes(x = log10(sum.GNI), y = log10(sum.importMUSD)))+
  geom_point()+
  #geom_text(size =3)+
  geom_smooth(method = "lm") #+
#facet_grid(Income_Group ~ .)


## Limit to complete cases for regression.
test1.complete2 <- test1[complete.cases(test1),]# Remove some na values for the variables of interest
test1.complete2 <- test1.complete2[is.finite(test1.complete2$log.sum.importMUSDcor) & is.finite(log10(test1.complete2$sum.GNI)),] # Remove Inf values

fit.rich.imports <- lm(log10(sum.importMUSD) ~ log10(sum.GNI), na.action=na.exclude, data = test1.complete2)
summary(fit.rich.imports)
#Residuals:
#Min      1Q  Median      3Q     Max 
#-3.7819 -0.9622  0.1297  1.1295  3.3619 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     -5.0865     1.1677  -4.356 3.72e-05 ***
#  log10(sum.GNI)   1.2762     0.2713   4.704 9.91e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.543 on 84 degrees of freedom
#Multiple R-squared:  0.2085,	Adjusted R-squared:  0.1991 
#F-statistic: 22.13 on 1 and 84 DF,  p-value: 9.907e-06

fit.rich.imports2 <- lm(log10(sum.importMUSD/cover.2000.gt50.ha) ~ log10(sum.GNI/cover.2000.gt50.ha), na.action=na.exclude, data = test1.complete2)
summary(fit.rich.imports2)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.5130 -1.0426  0.1103  0.9721  2.7399 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                       -4.46621    0.20972  -21.30  < 2e-16 ***
#  log10(sum.GNI/cover.2000.gt50.ha)  0.67067    0.08425    7.96 7.29e-12 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.428 on 84 degrees of freedom
#Multiple R-squared:   0.43,	Adjusted R-squared:  0.4232 
#F-statistic: 63.36 on 1 and 84 DF,  p-value: 7.292e-12

## Same regression with correction for forested area in 2000 applied. 
## When viewed on a log scale, we see a relationship between GNI and imports.
p2 <- ggplot(test1, aes(x = log10((sum.GNI/1000)/cover.2000.gt50.ha), y = log10(sum.importMUSD/cover.2000.gt50.ha)))+
  geom_point(size = 0.5)+
  #geom_text(size =3)+
  geom_smooth(method = "lm", se = F, size = 0.5) +
  #labs(x = "Cumulative GNI (Millions USD)", y = "Cumulative Imports\n(Millions USD)") +
  labs(x = "Cumulative GNI (Millions USD)", y = "Cumulative Imports (Millions USD)") +
  theme_bw() +
  ggtitle("B")+
  theme(axis.text = element_text(size = 4))+
  theme(axis.title = element_text(size = 4))+
  theme(plot.title = element_text(size = 6))
#facet_grid(income00 ~ .)

## Save Figure
##ggsave("H:/telecoupling/figures/CumGNIMUSD_predict_CumImportsMUSD_forest_area_corrected.png", plot = p2, width = 7.5, height = 5, units = "cm", dpi = 300)

## Difference in Imports predicted by difference in GNI corrected by Forest Cover in 2000? 
fit1 <- lm(log_diff_import_MUSDcor ~ log.diff.GNIcor, data = test2.complete, na.action = na.exclude)
summary(fit1)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-2.1541 -0.8726  0.3168  0.8757  1.6296 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -2.8095     0.8140  -3.451 0.002848 ** 
#  log.diff.GNIcor   0.8571     0.1941   4.416 0.000333 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.183 on 18 degrees of freedom
#(82 observations deleted due to missingness)
#Multiple R-squared:   0.52,	Adjusted R-squared:  0.4933 
#F-statistic:  19.5 on 1 and 18 DF,  p-value: 0.0003335


## Do richer countries export more soybean over time?

## Very non-linear. Not appropriate to evaluate w/ linear regression.
## When viewed on a log scale, we see a relationship between GNI and imports.
ggplot(test1, aes(x = log10(sum.GNI), y = log10(sum.exportMUSD)))+
  geom_point()+
  #geom_text(size =3)+
  geom_smooth(method = "lm") #+
#facet_grid(Income_Group ~ .)

## Limit to complete cases for regression.
test1.complete3 <- test1[complete.cases(test1),]# Remove some na values for the variables of interest
test1.complete3 <- test1.complete3[is.finite(test1.complete3$log.sum.exportMUSDcor) & is.finite(log10(test1.complete3$sum.GNI)),] # Remove Inf values

fit.rich.exports <- lm(log10(sum.exportMUSD) ~ log10(sum.GNI), na.action=na.exclude, data = test1.complete3)
summary(fit.rich.exports)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.6432 -1.3461 -0.0515  1.0376  4.7572 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)     -1.9812     2.1367  -0.927    0.358
#log10(sum.GNI)   0.4709     0.4881   0.965    0.339

#Residual standard error: 1.861 on 48 degrees of freedom
#Multiple R-squared:  0.01902,	Adjusted R-squared:  -0.001415 
#F-statistic: 0.9308 on 1 and 48 DF,  p-value: 0.3395

fit.rich.exports2 <- lm(log10(sum.exportMUSD/cover.2000.gt50.ha) ~ log10(sum.GNI/cover.2000.gt50.ha), na.action=na.exclude, data = test1.complete3)
summary(fit.rich.exports2)
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-3.6567 -1.2192 -0.0665  1.1215  3.6719 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                        -5.0895     0.4120 -12.354  < 2e-16 ***
#  log10(sum.GNI/cover.2000.gt50.ha)   0.6067     0.1587   3.824 0.000379 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.774 on 48 degrees of freedom
#Multiple R-squared:  0.2335,	Adjusted R-squared:  0.2175 
#F-statistic: 14.62 on 1 and 48 DF,  p-value: 0.0003787

p3 <- ggplot(test1, aes(x = log10((sum.GNI/1000)/cover.2000.gt50.ha), y = log10(sum.exportMUSD/cover.2000.gt50.ha)))+
  geom_point(size = 0.5)+
  #geom_text(size =3)+
  geom_smooth(method = "lm", se = F, size = 0.5) +
  #labs(x = "Cumulative GNI (Millions USD)", y = "Cumulative Exports\n(Millions USD)") +
  labs(x = "Cumulative GNI (Millions USD)", y = "Cumulative Exports (Millions USD)") +
  theme_bw() +
  ggtitle("A") +
  theme(axis.text = element_text(size = 4))+
  theme(axis.title = element_text(size = 4))+
  theme(plot.title = element_text(size = 6))
#facet_grid(income00 ~ .)

## Save Figure
#ggsave("H:/telecoupling/figures/CumGNIMUSD_predict_CumExportsMUSD_forest_area_corrected.png", plot = p3, width = 7.5, height = 5, units = "cm", dpi = 300)

## Difference in Exports predicted by difference in GNI corrected by Forest Cover in 2000? 
test2.complete <- test2 <- test2[is.finite(test2$log_diff_export_MUSDcor) & is.finite(test2$log.diff.GNIcor),] # Remove Inf values
fit1 <- lm(log_diff_export_MUSDcor ~ log.diff.GNIcor, data = test2.complete, na.action = na.exclude)
summary(fit1)
#Residuals:
#  Min     1Q Median     3Q    Max 
#-3.440 -1.125 -0.410  1.689  2.896 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)      -5.1080     0.4098 -12.466  < 2e-16 ***
#  log.diff.GNIcor   0.5423     0.1047   5.178 1.71e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.606 on 78 degrees of freedom
#Multiple R-squared:  0.2558,	Adjusted R-squared:  0.2462 
#F-statistic: 26.81 on 1 and 78 DF,  p-value: 1.707e-06



## -------------------------------------------------------------------------
## Merge only Income/GDP data to the cleaned soy data

## Fix country names in income and GDP data to match soy names
gathered.income$Country.Name[gathered.income$Country.Name=="Bahamas, The"] <- "Bahamas"
gathered.income$Country.Name[gathered.income$Country.Name=="British Virgin Islands"] <- "Virgin Islands, British"
gathered.income$Country.Name[gathered.income$Country.Name=="Congo, Dem. Rep."] <- "Congo, the Democratic Republic of the"
gathered.income$Country.Name[gathered.income$Country.Name=="Congo, Rep."] <- "Congo"
gathered.income$Country.Name[gathered.income$Country.Name=="China, Hong Kong SAR"] <- "China"
gathered.income$Country.Name[gathered.income$Country.Name=="Czechoslovakia (former)"] <- "Czechoslovakia"
gathered.income$Country.Name[gathered.income$Country.Name=="Iran, Islamic Rep."] <- "Iran, Islamic Republic of"
gathered.income$Country.Name[gathered.income$Country.Name=="Korea, Dem. Rep."] <- "Democratic People's Republic of Korea"
gathered.income$Country.Name[gathered.income$Country.Name=="Korea, Rep."] <- "Korea"
gathered.income$Country.Name[gathered.income$Country.Name=="Kyrgyz Republic"] <- "Kyrgyzstan"
gathered.income$Country.Name[gathered.income$Country.Name=="Lao PDR"] <- "Lao People's Democratic Republic"
gathered.income$Country.Name[gathered.income$Country.Name=="Macao SAR, China"] <- "China"
gathered.income$Country.Name[gathered.income$Country.Name=="Macedonia, FYR"] <- "Macedonia, the former Yugoslav Republic of"
gathered.income$Country.Name[gathered.income$Country.Name=="Micronesia, Fed. Sts."] <- "Micronesia, Federated States of"
gathered.income$Country.Name[gathered.income$Country.Name=="Moldova"] <- "Moldova, Republic of"
gathered.income$Country.Name[gathered.income$Country.Name=="Sudan"] <- "Sudan (former)"
gathered.income$Country.Name[gathered.income$Country.Name=="Tanzania"] <- "Tanzania, United Republic of"
gathered.income$Country.Name[gathered.income$Country.Name=="Venezuela, RB"] <- "Venezuela (Bolivarian Republic of)"
gathered.income$Country.Name[gathered.income$Country.Name=="Virgin Islands (U.S.)"] <- "United States Virgin Islands"
gathered.income$Country.Name[gathered.income$Country.Name=="Yemen, Rep."] <- "Yemen"
gathered.income$Country.Name[gathered.income$Country.Name=="Yugoslavia (former)"] <- "Yugoslavia"

## Serbia and Montenegro Split in 2006 - assign income level info to both separated countries

gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Bahamas, The"] <- "Bahamas"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="British Virgin Islands"] <- "Virgin Islands, British"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Congo, Dem. Rep."] <- "Congo, the Democratic Republic of the"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Congo, Rep."] <- "Congo"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="China, Hong Kong SAR"] <- "China"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Iran, Islamic Rep."] <- "Iran, Islamic Republic of"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Korea, Dem. People's Rep."] <- "Democratic People's Republic of Korea"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Korea, Rep."] <- "Korea"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Kyrgyz Republic"] <- "Kyrgyzstan"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Lao PDR"] <- "Lao People's Democratic Republic"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Macao SAR, China"] <- "China"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Macedonia, FYR"] <- "Macedonia, the former Yugoslav Republic of"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Micronesia, Fed. Sts."] <- "Micronesia, Federated States of"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Moldova"] <- "Moldova, Republic of"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Sudan"] <- "Sudan (former)"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Tanzania"] <- "Tanzania, United Republic of"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Venezuela, RB"] <- "Venezuela (Bolivarian Republic of)"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Virgin Islands (U.S.)"] <- "United States Virgin Islands"
gathered.gdp$Country.Name[gathered.gdp$Country.Name=="Yemen, Rep."] <- "Yemen"

## Merge dataset with soybeans (producer data = Reporter.Countries)
socio.soy <- merge(soy.cleaned, gathered.gdp, by.x = c("Reporter.Countries", "Year"), by.y = c("Country.Name", "Year"))
socio.soy <- merge(socio.soy, gathered.income, by.x = c("Reporter.Countries", "Year"), by.y = c("Country.Name", "Year"))



########################################################################################
## Map representation of Deforestation - Soybean Export relationships 2000-2012
## a) nodes scaled by forest area in 2000, b) flows of cumulative export value, c) cumulative forest loss 

## Load packages:
library(countrycode)
library(network) # basic 'statnet' network library
library(maps)

## Getting data prepared 

## Create the node list
nodes <- unique(exports.imports$Reporter.Countries)
str(nodes) #221 unique countries

## Convert the country names to the ISO2C standard codes so that other attributes
iso2 <- countrycode(nodes, "country.name", "iso2c", warn = T) # we lost the Pacific Islands Trust Territory!
#iso3n <- countrycode(nodes, "country.name", "iso3n", warn = T) # lost Pacific Islands Trust Territory, Unspecified, Yugoslavia
#region <- countrycode(nodes, "country.name", "region", warn = T) # we lost Czechoslovakia, Pacific Islands Trust Territory, Yugoslavia!
#continent <- countrycode(nodes, "country.name", "continent", warn = T) # we lost Czechoslovakia, Pacific Islands Trust Territory, Yugoslavia!

## bind this to the nodes df
nodes <- data.frame(nodes, iso2, stringsAsFactors = F)

## Add coordinate attributes for each node (country centroids)
## World Countries xy coordinates
## data area from: https://developers.google.com/public-data/docs/canonical/countries_csv
coords <- read.csv(file.path(soypath,"data/country_centroids_all.csv"), header = T, stringsAsFactors = F)
head(coords)
coords$ISO3136[coords$SHORT_NAME == 'Namibia'] <- "NA" ## Fix value that should NOT be NA. 

test <- merge(nodes, coords, by.x = "iso2", by.y = "ISO3136")
str(test) # 216 countries. Some lost due to mismatches.
nodes <- test

## Merge in deforestation and GNI attributes
#socioeco <- read.csv("H:/telecoupling/scratch/socioeco_data.csv", stringsAsFactors = F) # Annual
#socioeco.cum <- read.csv("H:/telecoupling/scratch/socioeco_data_cumulative.csv", stringsAsFactors = F) # cumulative over the study period

test <- merge(nodes, test1, by.x = "nodes", by.y = "country", all = T)
nodes<- test

## Create links representing cumulative exports from 2000-2012
cum.soy <- soy %>% 
  filter(Element == "Export Value") %>% # Filter to keep only Export Value
  filter(Year >1999 & Year <2013) %>%
  group_by(Reporter.Countries, Partner.Countries) %>%
  dplyr::summarize(totValue = sum(Value)) %>%
  mutate(totexport_MUSD = totValue*0.001)%>% # convert Export Value to Millions USD
  select(-totValue)

## Check for nodes that are missing
cum.soy$Reporter.Countries[!cum.soy$Reporter.Countries %in% nodes$nodes] # All reporter countries are in nodes set
cum.soy$Partner.Countries[!cum.soy$Partner.Countries %in% nodes$nodes] # Netherlands Antilles (former) and Unspecified are missing. Remove them? 

## Remove these!
cum.soy <- filter(cum.soy, !grepl('Unspecified', Partner.Countries))
#cum.soy <- filter(cum.soy, !grepl("Netherlands Antilles (former)", Partner.Countries))
#cum.soy <- cum.soy[!grepl("Netherlands Antilles (former)", cum.soy$Partner.Countries), ]
cum.soy <- cum.soy[!cum.soy$Partner.Countries == "Netherlands Antilles (former)", ]

links <- data.frame(from = cum.soy$Reporter.Countries, to = cum.soy$Partner.Countries, weights = cum.soy$totexport_MUSD, stringsAsFactors = F)
head(links) ## ADD from.lat, from.lon, to.lat, to.lon
nrow(links) # 2328 links are in the cumulative network

## Keep only transactions involving pantropical countries
links <- links %>% 
  #filter(to %in% hansen$country | from %in% hansen$country) %>% ## Exports from AND imports to pantropical countries
  filter(from %in% hansen$country) %>% ## ONLY exports from pantropical countries
  filter(weights > 0) # This limits to 970 observations
str(links) # Now just 663 edges...still a lot!

## Keep only those nodes involved in trade with pantropical countries
nodes <- nodes %>% 
      #filter(nodes %in% links$from | nodes %in% links$to) # 
      filter(nodes %in% links$from | nodes %in% links$to)
str(nodes) # Now just 155 countries

socioecoNet<-network(links, 
                     matrix.type='edgelist',
                     directed=TRUE,  # this will be a directed network
                     ignore.eval=FALSE,  # confusingly, this tells it to include edge weights
                     names.eval='weights'  # names for the edge weights
)


## attach the appropriate lat and long coordinates
## need to subset to the vertices actually in the network
socioecoNet%v%'lon'<-sapply(network.vertex.names(socioecoNet),function(name){
  nodes[nodes$nodes==name,]$LONG
})
socioecoNet%v%'lat'<-sapply(network.vertex.names(socioecoNet),function(name){
  nodes[nodes$nodes==name,]$LAT
})

## Attach vertex attributes for cumulative imports and exports
socioecoNet%v%'exportMUSD'<-sapply(network.vertex.names(socioecoNet),function(name){
  nodes[nodes$nodes == name, ]$sum.exportMUSD
})
socioecoNet%v%'importMUSD'<-sapply(network.vertex.names(socioecoNet),function(name){
  nodes[nodes$nodes == name, ]$sum.importUSD
})

## Add vertex attributes for forested area in 2000 (ha w/ cover >50%)
#socioecoNet%v%'forest00ha'<-sapply(network.vertex.names(socioecoNet),function(name){
#  nodes[nodes$nodes == name, ]$cover.2000.gt50.ha
#})

## Add vertex attributes for forested area in 2000 (Kha w/ cover >50%)
nodes$cover.2000.gt50.Kha <- nodes$cover.2000.gt50.ha/1000
socioecoNet%v%'forest00Kha'<-sapply(network.vertex.names(socioecoNet),function(name){
  nodes[nodes$nodes == name, ]$cover.2000.gt50.Kha
})

## Add vertex attributes for cumulative deforested area (Kha) detected by Hansen et al. 
nodes$sum.loss.Kha <- nodes$sum.loss.ha/1000
socioecoNet%v%'totlossKha'<-sapply(network.vertex.names(socioecoNet),function(name){
  nodes[nodes$nodes == name, ]$sum.loss.Kha
})

## Add vertex attributes for cumulative GNI
socioecoNet%v%'totGNI'<-sapply(network.vertex.names(socioecoNet),function(name){
  nodes[nodes$nodes == name, ]$sum.GNI
})

## Add vertex attribute for income category in 2000
#socioecoNet%v%'income00'<-sapply(network.vertex.names(socioecoNet),function(name){
#  nodes[nodes$nodes == name, ]$income00
#})

## Verify that we have the attributes we want
socioecoNet # Edge attribute should show "weights" if we matched everything correctly!!!

## Nice plotting options with ggnetworkmap
## Plotting the network
# create a world map
library(ggplot2)
library(maps)
library(network)
#library(sna)
library(GGally)
#library(ggmap)

## World Base Map
world <- fortify(map("world", plot = FALSE, fill = TRUE))
world <- ggplot(world, aes(x = long, y = lat)) +
  coord_cartesian(xlim = c(-180,180), ylim=c(-50,90)) +
  geom_polygon(aes(group = group), color = "grey65",
               fill = "#f9f9f9", size = 0.2)

#####################################################################
## FigXA
## Plot the exporter size by export value
pXa <- ggnetworkmap(world, socioecoNet,
                    #weight = ((forest00Kha/904922.010)*10),
                    weight = forest00Kha,
                    #node.group = NULL,
                    node.color = 'darkgreen',
                    node.alpha = 0.9,
                    segment.size = 0.1,
                    segment.color = 'black', # No segments
                    segment.alpha = 0.1,
                    arrow.size = 0.05) +  
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(0,0,0,0),
        legend.margin = margin(0,0,0,0),
        legend.position = "right") +
  guides(col = guide_legend(ncol = 3))
ggsave("D:/telecoupling/prelim_results/forest2000_dotplotV3.png", plot = pXa, width = 16, height = 7, units = "cm", dpi = 300)

## FigXB
## Plot the cumulative forest loss
pXb <- ggnetworkmap(world, socioecoNet, 
                    weight = totlossKha,
                    #node.group = NULL,
                    node.color = 'darkred',
                    node.alpha = 0.9,
                    segment.size = 0.2,
                    segment.color = 'black',
                    segment.alpha = 0.1,
                    arrow.size = 0.05) + # No segments 
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(0,0,0,0),
        legend.margin = margin(0,0,0,0),
        legend.position = "right") +
  guides(col = guide_legend(ncol = 3)) 
ggsave("D:/telecoupling/prelim_results/forestloss_2000-2012_dotplotV3.png", plot = pXb, width = 16, height = 7, units = "cm", dpi = 300)

## FigXC
## Plot cumulative value of all exports from each node
#ggnetworkmap(world, socioecoNet,
#                    #weight = ((forest00Kha/904922.010)*10),
#                    weight = exportMUSD,
#                    #node.group = NULL,
#                    node.color = NA, # no nodes
#                    #node.alpha = 0,
#                    #node.size = 0,
#                    segment.size = 0.25,
#                    #segment.size = 1,
#                    segment.color = 'black',
#                    segment.alpha = 0.2,
#                    arrow.size = 0.25
#) + # No nodes
pXc <- ggnetworkmap(world, socioecoNet,
                    #weight = ((forest00Kha/904922.010)*10),
                    weight = exportMUSD,
                    #node.size = 3,
                    #node.group = NULL,
                    node.color = 'dodgerblue',
                    node.alpha = 0.9,
                    segment.size = 0.2,
                    segment.color = 'black', # No segments
                    segment.alpha = 0.1,
                    arrow.size = 0.05) + 
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.box.margin = margin(0,0,0,0),
        legend.margin = margin(0,0,0,0),
        legend.position = "right") +
  guides(col = guide_legend(ncol = 3)) 
ggsave("D:/telecoupling/prelim_results/cum_exports_2000-2012_dotplotV3.png", plot = pXc, width = 16, height = 7, units = "cm", dpi = 300)

## Better to show this only including the exports!???

## Figure 9 - Cumulative Imports & Exports in relation to GNI

## Figure 10A & B - Cumulative Imports & Exports in relation to GNI


#####################################################################

## June 4, 2018 - assess relationship between exports and production from FAO data provided by Jack

prod <- read.csv("C:/Users/djs50/Desktop/telecoupling/data/pantropical_countries_im_ex_production_2000-2012_FAO_forest.csv", header = T, stringsAsFactors = F)
prod$import.tonnes <- as.numeric(prod$import.tonnes)
prod$export.tonnes <- as.numeric(prod$export.tonnes)

## Does production volume predict the export volume? 
plot(prod$prod.tonnes, prod$export.tonnes)
plot(log(prod$prod.tonnes), log(prod$export.tonnes))

prod$log.export.tonnes<- ifelse(prod$export.tonnes >0, log(prod$export.tonnes), NA)
prod$log.prod.tonnes <- ifelse(prod$prod.tonnes >0, log(prod$prod.tonnes), NA)

summary(lm(prod$log.export.tonnes~prod$log.prod.tonnes, na.action = na.omit))

##Does production volume predict the value of exports? 
plot(prod$prod.tonnes, prod$sum.exportMUSD) # doesn't look like a strong relationship when plotted as actual sum MUSD ~ sum tonnes
plot(log(prod$prod.tonnes), log(prod$sum.exportMUSD)) # ok now we have a relationship!

prod$log.sum.exportMUSD <- ifelse(prod$sum.exportMUSD >0, log(prod$sum.exportMUSD), NA)
#prod$log.prod.tonnes <- ifelse(prod$prod.tonnes >0, log(prod$prod.tonnes), NA)

summary(lm(prod$log.sum.exportMUSD~prod$log.prod.tonnes, na.action = na.omit))
#Call:
#  lm(formula = prod$log.sum.exportMUSD ~ prod$log.prod.tonnes, 
#     na.action = na.omit)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-7.1104 -1.7775  0.3227  1.6567  4.6994 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          -12.4219     2.0484  -6.064 1.02e-06 ***
#  prod$log.prod.tonnes   1.0651     0.1569   6.786 1.34e-07 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2.69 on 31 degrees of freedom
#(15 observations deleted due to missingness)
#Multiple R-squared:  0.5977,	Adjusted R-squared:  0.5847 
#F-statistic: 46.05 on 1 and 31 DF,  p-value: 1.339e-07

## Does estimated land area used for soybean predict the forest cover loss estimate?? 
plot(prod$prod.area.ha, prod$sum.loss.ha)
plot(log(prod$prod.area.ha), log(prod$sum.loss.ha))

#prod$log.export.tonnes<- ifelse(prod$export.tonnes >0, log(prod$export.tonnes), NA)
#prod$log.prod.tonnes <- ifelse(prod$prod.tonnes >0, log(prod$prod.tonnes), NA)

summary(lm(prod$sum.loss.ha~prod$prod.area.ha, na.action = na.omit))
#Call:
#  lm(formula = prod$sum.loss.ha ~ prod$prod.area.ha, na.action = na.omit)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-5233585  -765150  -572324   122639 15421006 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)       7.725e+05  3.780e+05   2.044   0.0467 *  
#  prod$prod.area.ha 1.447e+00  1.190e-01  12.163 5.65e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 2572000 on 46 degrees of freedom
#Multiple R-squared:  0.7628,	Adjusted R-squared:  0.7576 
#F-statistic: 147.9 on 1 and 46 DF,  p-value: 5.645e-16
