library(tidyverse)
library(sf)
library(rworldmap)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(mclust)



# reading in of annual crop yield data from OurWorldInData

crop_yields <- read.csv("https://catalog.ourworldindata.org/explorers/agriculture/latest/crop_yields/crop_yields.csv", header = TRUE)



# subsetting of data to most recent year with complete data

crop_yields_current <- crop_yields[crop_yields$year == 2019,]

crop_yields_current <- crop_yields_current[,-2]



# removal of yield outliers resulting from extremely low levels of annual grain production.
crop_yields_current[crop_yields_current$country == "Oman", "cereal_yield"] <- NA

crop_yields_current[crop_yields_current$country == "United Arab Emirates", "cereal_yield"] <- NA

crop_yields_current[crop_yields_current$country == "Kuwait", "cereal_yield"] <- NA

crop_yields_current[crop_yields_current$country == "Qatar", "cereal_yield"] <- NA

crop_yields_current[crop_yields_current$country == "Saint Vincent and the Grenadines", "cereal_yield"] <- NA



# standardization of country names in data set

crop_yields_current[crop_yields_current$country == "Russia","country"] <- "Russian Federation"

crop_yields_current[crop_yields_current$country == "Russia","country"] <- "Russian Federation"

crop_yields_current[crop_yields_current$country == "South Korea","country"] <- "Republic of Korea"

crop_yields_current[crop_yields_current$country == "Democratic Republic of Congo","country"] <- "Democratic Republic of the Congo"

crop_yields_current[crop_yields_current$country == "Congo","country"] <- "Republic of the Congo"

crop_yields_current[crop_yields_current$country == "Cote d'Ivoire","country"] <- "Côte d'Ivoire"

crop_yields_current[crop_yields_current$country == "Laos","country"] <- "Lao PDR"



#removal of non-countries from dataset

non_countries <- c("Africa",
                   "Africa (FAO)",
                   "Americas (FAO)",
                   "Asia",
                   "Asia (FAO)",
                   "Caribbean (FAO)",
                   "Central America (FAO)",
                   "Central Asia (FAO)",
                   "China (FAO)",
                   "Eastern Africa (FAO)",
                   "Eastern Asia (FAO)",
                   "Eastern Europe (FAO)",
                   "Europe",
                   "Europe (FAO)",
                   "European Union (27)",
                   "European Union (27) (FAO)",
                   "High-income countries",
                   "Land Locked Developing Countries (FAO)",
                   "Least Developed Countries (FAO)",
                   "Low Income Food Deficit Countries (FAO)",
                   "Low-income countries",
                   "Lower-middle-income countries",
                   "Micronesia (FAO)",
                   "Middle Africa (FAO)",
                   "Net Food Importing Developing Countries (FAO)",
                   "North America",
                   "Northern Africa (FAO)",
                   "Northern America (FAO)",
                   "Northern Europe (FAO)",
                   "Oceania",
                   "Oceania (FAO)",
                   "Small Island Developing States (FAO)",
                   "South America",
                   "South America (FAO)",
                   "South-eastern Asia (FAO)",
                   "Southern Africa (FAO)",
                   "Southern Asia (FAO)",
                   "Southern Europe (FAO)",
                   "Upper-middle-income countries",
                   "Western Africa (FAO)",
                   "Western Asia (FAO)",
                   "Western Europe (FAO)",
                   "World")

crop_yields_countries <- crop_yields_current[!crop_yields_current$country %in% non_countries, ]



# reading in of fertilizer usage data for 2019

fert <- read.csv("../data/fertilizer_data.csv", header = FALSE)



#cleaning of fertilizer data

fert_thin <- fert[5:271,]

colnames(fert_thin) <- fert[5,]

fert_thinner <- fert_thin[-1,c(1,2,64)]

colnames(fert_thinner)[3] <- 'Fertilizer'

fert_thinner$Fertilizer <- as.numeric(fert_thinner$Fertilizer)

fert_thinner$Fertilizer[fert_thinner$Fertilizer > 600] <- NA



# getting world map data 

map <- getMap(resolution = "low")

country_data <- map@data

world_map <- ne_countries(scale = "medium", returnclass = "sf")



# joining world map data to fertilizer and grain yield data

world_data <- world_map %>%
  left_join(fert_thinner, by = c("adm0_a3" = "Country Code")) %>%
  left_join(crop_yields_countries, by = c("name_long" = "country")) 



# world map of grain yield in 2019

ggplot(data = world_data) +
  geom_sf(aes(fill = cereal_yield), color = "white") +
  scale_fill_viridis_c(option = "C", na.value = "gray90") +
  theme_minimal() +
  labs(fill = "mt/ha") +
  theme(legend.position = "right") +
  labs(title = "                                        Grain Yield in Tonnes per Hectare for 2019")+
  theme(plot.title = element_text(size = 13, face = "bold"))



# world map of fertilizer usage in 2019

ggplot(data = world_data) +
  geom_sf(aes(fill = Fertilizer), color = "white") +
  scale_fill_viridis_c(option = "C", na.value = "gray90") +
  theme_minimal() +
  labs(fill = "kg/ha") +
  theme(legend.position = "right") +
  labs(title = "                                        Fertilizer Usage in kg per Hectare for 2019")+
  theme(plot.title = element_text(size = 13, face = "bold"))



# scatter plot of raw data
plot(world_data$Fertilizer,world_data$cereal_yield, pch = 19,
     ylab = 'Cereal Yield,
     Tonnes per Hectare',
     xlab = 'Fertilizer in Kilograms per Hectare',
     main = "Fertilizer vs. Cereal Yield")



# grid search for optimal parameter of logistic transformation of fertilizer variable

b <- seq(from = 0.001, to = 1, by = 0.0001)

yield_fert <- na.omit(data.frame('Fertilizer' = world_data$Fertilizer,
                                 'Yield' = world_data$cereal_yield))

cor_b <- sapply(b, function(b_value) { 
  cor(yield_fert[,2], 1/(1 + exp(-b_value*yield_fert[,1])))
})

print(b[which.max(cor_b)])



# logistic transformation and linear model

logit_transform_fertilizer <- (200/(1+exp(-(0.0184)*world_data$Fertilizer))-100)

lm_logistic_transform <- lm(world_data$cereal_yield~logit_transform_fertilizer)



# scatter plot after transformation with linear model line

plot(logit_transform_fertilizer,world_data$cereal_yield,
     pch = 19,
     ylab = 'Cereal Yield, Tonnes per Hectare',
     xlab = 'Fertilizer, Percent Saturation',
     main = "Fertilizer vs. Cereal Yield (with transformation)")

abline(a = lm_logistic_transform$coefficients[1],
       b = lm_logistic_transform$coefficients[2])



# fitting of Gaussian mixture model

fert_yield <- data.frame('name' = world_data$name_long,
                         'Fert' = logit_transform_fertilizer,
                         'Cereal_yield' = world_data$cereal_yield)

fert_yield <- na.omit(fert_yield)

gmm_fert <- Mclust(data = fert_yield[2:3])

fert_yield$class <- gmm_fert$classification



#joining group membership results to fertilizer and yield data

world_data <- world_data %>% left_join(fert_yield, by = c("name_long" = "name"))

world_data$class <- as.factor(world_data$class)



# colorblind-friendly palette for GMM group plots

cb_friendly <- c("#E69F00", "#56B4E9", "#009E73","#CC79A7", "#F0E442", "#0072B2", 
               "#D55E00", "#CC79A7", "#999999")

mclust.options("classPlotColors" = cb_friendly)



# scatter plot with overlaid GMM groups

plot(gmm_fert, what = 'classification',
     ylab = 'Cereal Yield, Tonnes per Hectare',
     xlab = 'Fertilizer, Precent Saturation',
     main = "GMM of Fertilizer vs. Cereal Yield (with transformation)")

mtext("GMM of Fertilizer vs. Cereal Yield (with transformation)", side = 3, line = 1.5, font = 2, cex = 1.2)



# world map with GMM groups

ggplot(data = world_data) +
  geom_sf(aes(fill = class), color = "white") +
  scale_fill_manual(values = c(
    '1' = "#E69F00", 
    '2' = "#56B4E9",
    '3' = "#009E73",
    '4' = "#CC79A7"), na.value = "gray90")+
  theme_minimal() +
  labs(fill = "Cluster") +
  theme(legend.position = "right") +
  labs(title = "                                                  Gaussian Mixture Model Groups") +
  theme(plot.title = element_text(size = 13, face = "bold"))
