##***************************
## Lack of a latitudinal pattern in cryptic diversity of Tardigrades
##
## Nicolas Bailly
##
## 2025-10-10
##
##***************************

## _ Install packages--------

install.packages ('tidyverse')
install.packages ('viridis')
install.packages('ggridges')
install.packages('stringr')
install.packages('assertthat')
install.packages('maps')

## _ Load packages ----------

library ('tidyverse')
library ('viridis')
library('ggridges')
library('stringr')
library('assertthat')
library('maps')

## _ Load the data  --------
data_raw <- read_tsv (file= "../data/tardigrade_data.tsv")
head (data_raw)
problems(data_raw)
#To ensure that the warning does not affect the columns that I am going to use.

## _ Data exploration and manipulation--------

## __ Determining the variables of interest ----------

names(data_raw)
#To determine if the data set contained the columns representing the columns of interest.

## __ Creating the data subset ----------

?cat

cat("Sample size before removing NAs:", nrow(data_raw))
# Needed as a reproducibility checkpoint to show original data size before filtering.

data_final <- na.omit(data_raw[,c(8,22,15,55)]) 
#Needed for all observations to have the necessary values to calculate the BIN/species ratios and be categorized by a site ID and zone.

problems(data_final)
#To ensure there are no problems with the 'data_final' sub set.

cat("Sample size after removing NAs:", nrow(data_final))
#Needed as a reproducibility checkpoint to track data loss and determine if the filtering step worked.

anyNA(data_final)
#Performance check to ensure that the data set does not contain any NAs.

## __ Separation of latitude and longitude ----------

data_final <- data_final %>% separate (col=4, into = c('latitude','longitude'), sep= ',', convert=TRUE)
#Needed to later assign a site ID (based on latitude and longitude) and zoneto each observation.
#/** 'Separate()' separates a column into latitude and longitude columns. via https://stackoverflow.com/questions/61332750/in-r-is-there-a-way-of-separating-this-column-into-latitude-and-longitude-colum*/

## __ Removal of special characters ----------

class(data_final$latitude)
class(data_final$longitude)
#Needed to see if the values needed for the later categorization into zones and site IDs are in the numeric form.

str (data_final$latitude) 
str (data_final$longitude)
#Needed to ensure that the characters '[' and ']' had to be removed to treat the values as numeric. 

?str_remove_all
vignette("regular-expressions", package = "stringr")

data_final$latitude<-str_remove_all(data_final$latitude, '\\[')
data_final$longitude <- str_remove_all(data_final$longitude, '\\]')
#Had to be done so that values can be converted into the numeric form.

data_final$latitude <- as.numeric(data_final$latitude)
data_final$longitude <- as.numeric(data_final$longitude)
#Needed to later group the observations into Polar or Temperate zones and to assign them a site ID.
#/** 'as.numeric()' used to convert each value from character to numeric. via https://www.educative.io/answers/what-is-the-asnumeric-function-in-r*/

class(data_final$latitude)
class(data_final$longitude)
#Performance check to ensure that the values within the longitude and latitude columns are numeric and ready for further analyses.

## __ Checking Coordinate Data for Entry Errors ----------

assert_that(all(data_final$latitude >= -90 & data_final$latitude <= 90))
assert_that(all(data_final$longitude >= -180 & data_final$longitude <= 180))
#To ensure that all geographical coordinates fall within valid ranges (latitude: -90 to 90, longitude: -180 to 180) to avoid the misclassification of sites into incorrect latitudinal zones.
#/** 'assert_that()' used to ensure that there are no entry errors in the longitude and latitude. via https://www.rdocumentation.org/packages/assertthat/versions/0.2.1/topics/assert_that*/


## __ Categorization of data into  zones ----------

?abs
data_final<- data_final %>% mutate  (zone=
                                          case_when (
                                            abs(latitude) >= 60 ~ 'Polar',
                                            abs(latitude) >= 40 &
                                              abs(latitude)< 60 ~ 'Temperate', 
                                            abs(latitude) < 40  ~ 'Other') )

data_final <- data_final %>% filter (zone %in% c('Temperate','Polar') )
#Needed to remove any data that did not qualify as Polar or Temperate, as it is not relevant to the research question. Such latitudinal thresholds were based on the information from meteoblue. via https://content.meteoblue.com/en/research-education/educational-resources/meteoscool/general-climate-zones. 
#/** Use of 'mutate()' to make new columns. via https://dplyr.tidyverse.org/reference/mutate.html*/
#/** Use of 'case_when()' to assign values to the newly created variable zone. via https://dplyr.tidyverse.org/reference/case_when.html*/
#/** Use of 'filter()' to only subset both zones. via https://dplyr.tidyverse.org/reference/filter.html*/

?length
cat("Number of unique zones:", length(unique(data_final$zone)))
#Reproducibility checkpoint to ensure the correct categorization of the observations into either Polar or Temperate zones.

## __ Creating of site ids  ----------

?round
?paste

data_final <- data_final %>%
  mutate(site_id = paste(round(latitude, digits=3), round(longitude, digits=3), sep = '_'))
#Needed because the comparison in cryptic diversity between both zones is done at a site-level.

cat("Number of unique site IDs:", n_distinct(data_final$site_id))
#Needed to confirm the number of site-level observations used in the subsequent Wilcoxon Rank-Sum Tests.

## _ Site-specific summary of cryptic diversity  ----------

## __ Calculation of BIN/species ratio at the site level  ----------

?summarise
?n_distinct
?group_by

summary_by_site <- data_final %>% 
  group_by(zone,site_id) %>%
  summarise(num_bins=n_distinct(bin_uri),
            num_species=n_distinct(species),
            .groups='drop'
  ) %>% filter (num_species >0) %>% 
  mutate(bin_species_ratio=num_bins/num_species)
#Needed to calculate the BIN/species ratio that will be then used to compare the cryptic diversity across different sites.
#/** Use of '.groups=drop' to ensure that the data frame goes back to being ungrouped. via https://stackoverflow.com/questions/78422148/understanding-the-purpose-of-groups-drop-in-dplyrs-summarise-function*/

## __ Summary of BIN and Species Counts at the site level by Zone  ----------

site_summary_binspecies <- summary_by_site %>%
  group_by(zone) %>%
  summarise(
    mean_bins = mean(num_bins),
    median_bins = median(num_bins),
    sd_bins = sd(num_bins),
    mean_species = mean(num_species),
    median_species = median(num_species),
    sd_species = sd(num_species),
    .groups = 'drop')
#Shows a preliminary indication that cryptic diversity in the polar zones is higher than in temperate zones. As such, statistical tests are required to see if such difference is significant.

## _ Caveats of the dataset  ----------
 
## __ Sampling imbalance between both zones  ----------

?n
?ggplot
?geom_bar
?labs
?theme
?scale_fill_viridis_d

site_counts <- summary_by_site %>%
  group_by(zone) %>%
  summarise(num_sites = n(), .groups = 'drop')

ggplot(site_counts, aes(x = zone, y = num_sites, fill = zone)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Sampled Sites per Zone",
       x = "Zone",
       y = "Number of Sites") +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#The sampling imbalance suggests the need to use relative frequencies or density plots to compare the distribution of the BIN/species rates across several sites. Please refer to lines 272 and 249 for the relevant plots.
#/** Use of 'scale_y_continuous()' and 'expand()' to adjust the values in the y-axis to fit the bar values of the temperate zone. via https://ggplot2.tidyverse.org/reference/expansion.html*/
#/** Use of 'hjust()' to adjust the location of the title of the plot. via https://stackoverflow.com/questions/7263849/what-do-hjust-and-vjust-do-when-making-a-plot-using-ggplot*/
#/** Use of 'theme_minimal()' and 'theme()' to show a clean visual style and modify the position of the title, respectively. via https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html*/
#/** Use of 'scale_fill_viridis_d()' to apply a color-blind palette for discrete categories. via https://ggplot2.tidyverse.org/reference/scale_viridis.html*/

## __ Geographical sampling bias between both zones  ----------

?geom_polygon
?geom_point
?coord_fixed
world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "gray90", color = "gray70") +
  geom_point(data = data_final, aes(x = longitude, y = latitude, color = zone),
             alpha = 0.7, size = 2) +
  coord_fixed(1.3) +  
  scale_color_viridis_d() +
  labs(title = "Sampling Sites by Zone",
       x = "Longitude",
       y = "Latitude",
       color = "Zone") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
#Shows that the results of this study are not generalizable to continents like South America or Australia.
#/** Use of 'coord_fixed()' to set up the ratio between y- and x-axis units. via https://stackoverflow.com/questions/26894534/howto-automatically-set-fixed-coordinate-ratio-coord-fixed-when-x-and-y-axis*/
#/** Use of 'map_data()' to gather the data needed to plot the map. via https://www.rdocumentation.org/packages/ggplot2/versions/3.5.2/topics/map_data*/
#/** Use of 'geom_polygon()' to draw the shapes of countries, continents and other regions within the map. via https://stackoverflow.com/questions/65930166/create-a-polygon-within-a-graph-in-ggplot-r*/

## _ Statistical Analysis  ----------

## __ Normality Testing and Choice of Statistical Test  ----------

?shapiro.test
shapiro.test(summary_by_site$bin_species_ratio[summary_by_site$zone == 'Polar'])
#The p-value is below 0.05. Hence, the data set of the 'Polar' zone is not normally distributed. It is necessary to perform two one-way Wilcoxon Rank-Sum Tests to test the significance in the differences.
##/** Use of '==' to apply the Shapiro text to only the polar zone. via https://stackoverflow.com/questions/28176650/what-is-the-difference-between-and-in-r*/

## __ Hypothesis Testing of Zone Differences  ----------

?wilcox.test
?factor
?levels

summary_by_site$zone <- factor(summary_by_site$zone, levels = c("Temperate", "Polar"))
wilcox1<-wilcox.test(bin_species_ratio ~ zone, data = summary_by_site, alternative = "greater")
p1<-round(wilcox1$p.value, 2)
#The p-value is above 0.5. Hence, hypothesis 1 is incorrect.
#/** Use of 'factor()' as a variable containing several levels and levels as the number of zones in that order. via https://stackoverflow.com/questions/55199310/working-with-dataframes-within-r-what-is-level-and-factor*/#

summary_by_site$zone <- factor(summary_by_site$zone, levels = c("Polar", "Temperate"))
wilcox2<-wilcox.test(bin_species_ratio ~ zone, data = summary_by_site, alternative = "greater")
p2<-round(wilcox2$p.value, 2)
#The p-value is above 0.5. Hence, hypothesis 2 is incorrect.

## _ Visualization of BIN/Species Ratio Distributions  ----------

?aes
?labs

count_data_rel <- summary_by_site %>%
  group_by(zone, bin_species_ratio) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(zone) %>%
  mutate(percentage = (count / sum(count)) * 100) %>%
  mutate(bin_species_ratio = round(bin_species_ratio, digits=2))

ggplot(count_data_rel, aes(x = factor(bin_species_ratio), y = percentage, fill = zone)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Relative Frequency of BIN/Species Ratios by Zone",
    x = "BIN/Species Ratio",
    y = "Percentage of Sites"
  ) +
  scale_fill_viridis_d() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) 
#Needed to show a direct comparison of how often certain levels of cryptic diversity occur in Polar vs. Temperate sites, regardless of the number of sites sampled.
#/** Use of 'round()' to set all the BIN/species ratios displayed to two decimal places. via https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/Round*/#

?annotate

ggplot(summary_by_site, aes(x = bin_species_ratio, y = zone, fill = zone)) +
  geom_density_ridges(alpha = 0.7, scale = 1) +
  labs(title = "Distribution of BIN/Species Ratio by Zone and Wilxocon tests",
       x = "BIN/Species Ratio",
       y = "Zone") +
  scale_fill_viridis_d() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = "Wilcoxon tests", 
           hjust = 1.92, vjust = 1, size = 4, fontface = "bold") +
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste0("Polar > Temperate p = ", p1), 
           hjust = 1.1, vjust = 2.2, size = 4) +
  annotate("text", 
           x = Inf, y = Inf, 
           label = paste0("Temperate > Polar p = ", p2), 
           hjust = 1.1, vjust = 3.5, size = 4)
#Needed to show the skewness of the distributions that may not be apparent in bar plots or summary statistics.
#/** Use of 'geom_density_ridges()' to visualize the distributions grouped by zone. via https://r-charts.com/distribution/ggridges/*/#
#/** Use of 'paste0()' to combine multiple elements into one. via https://www.digitalocean.com/community/tutorials/paste-in-r#how-to-use-paste0-function-in-r*/#
#/** Use of 'x = Inf, y = Inf' to place the text on the top right corner of the panel. via https://stackoverflow.com/questions/22488563/ggplot2-annotate-layer-position-in-r
#/** Use of 'annotate()' to write text within the plot. via  https://ggplot2.tidyverse.org/reference/annotate.html*/#

rm(list = ls())







