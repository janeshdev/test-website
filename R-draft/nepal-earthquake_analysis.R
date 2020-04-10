# Load Libraries
library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)
library(tidyr)
library(wordcloud)
library(visdat)

library(rgdal)
library(scales)

# Analyzing Nepal earthquake data 
all_month <- read.csv("../data_test/np-quake-master/data/all_month_merged.csv", 
                      stringsAsFactors = FALSE)
all_month$country <- 
  as.character(lapply(1:nrow(all_month), 
                      FUN = function(x) 
                        tail(strsplit(all_month$place[x], 
                                      split = ",")[[1]],n=1L)))

# Process date 
all_month <- all_month %>% 
  mutate(date = gsub(time, pattern = "T", replacement = " ") %>% 
           gsub(., pattern = ".[0-9]+Z", replacement = "") %>% 
           ymd_hms(.))


# Remove all the white spaces from column country
all_month$country <- gsub("[[:space:]]", "", x = all_month$country)


# Add month, day, and julian 
all_month <- all_month %>% 
  mutate(day = day(date), 
         month = month(date), 
         julian = julian(date, origin = ymd("2015-01-01")) %>% 
           floor() %>% as.numeric())

# Now filter for the country Nepal
nepal_data <- all_month %>% dplyr::filter(country == "Nepal")

ggplot(nepal_data) + geom_point(aes(longitude, latitude, color = mag)) + 
  scale_color_continuous(low = "red", high = "blue")

# Load Nepal Map 
library(leaflet)
head(nepal_data)

summary(nepal_data)

# nepal_data <- nepal_data %>%
#   dplyr::filter(mag > 5.5)


m <- leaflet(options = leafletOptions(miniZoom =8, maxZoom = 8)) %>% 
  addTiles() %>% # Add default OpenStreetMap map tiles
  leaflet::addCircleMarkers(lng = nepal_data$longitude, 
                            lat = nepal_data$latitude, 
                            color = as.factor(nepal_data$mag),  
             label = paste("mag=", nepal_data$mag, "\n", "location = ", nepal_data$place))
m


library(ggmap)


## April 22
nepal_data %>% ggplot(aes(date, mag)) + geom_line() + geom_point() + 
  xlab("Date") + ylab("Magnitude") + 
  coord_cartesian(xlim = c(as.POSIXct("2015-04-25 00:00:00", tz = "UTC"), 
                           as.POSIXct("2015-04-30 00:00:00", tz = "UTC")))

# Total number of earthquakes on April 25th 
nepal_data %>% dplyr::filter(date > as.POSIXct("2015-04-24 23:59:59", tz = "UTC"), 
                             date < as.POSIXct("2015-04-26 00:00:00", tz = "UTC")) %>% 
  count()

# 
# nepal_data1 <- nepal_data %>% 
#   dplyr::filter(date > as.POSIXct("2015-04-1 00:00:00", tz = "UTC"), 
#                 date < as.POSIXct("2015-04-30 23:59:00", tz = "UTC")) 

nepal_data2 <- nepal_data %>% 
  dplyr::filter(date > as.POSIXct("2015-04-24 23:59:59", tz = "UTC"), 
                date < as.POSIXct("2015-04-26 00:00:00", tz = "UTC"))

nepal_data2$size <- cut(nepal_data2$mag, 
                        c(2, 3.9, 4.9, 5.9, 6.9, 7.9), 
                        labels = c("3.3 to 3.9", ">3.9 to 4.9", ">4.9 to 5.9", ">5.9 to 6.9", ">6.9 to 7.9" ))

pallet <- colorFactor(c("gray32", "dodgerblue4",  "slateblue4", "purple", "firebrick1"),
                      domain = c("3.3 to 3.9", ">3.9 to 4.9", ">4.9 to 5.9", ">5.9 to 6.9", ">6.9 to 7.9"))

# Plot boxplot from April 25 to April 30

nepal_data1 %>% 
  ggplot() + 
  geom_boxplot(aes(date, mag, group = day)) 
  #geom_point(aes(date, mag, group = day)) 

dim(nepal_data)
library(sf)

pal <- colorQuantile(palette = "YlOrRd", domain = nepal_data2$mag)
#YlOrRd, viridis

library(tidyverse)
m <- leaflet(options = leafletOptions(miniZoom =8, maxZoom = 8), 
             #width = "100%", 
             data = nepal_data2) %>% 
  addTiles() %>% # Add default OpenStreetMap map tiles
  leaflet::addCircleMarkers(lng = ~longitude, 
                            lat = ~latitude, 
                            stroke = FALSE, 
                            fillOpacity = 0.8,
                            #radius = ~ifelse(mag < 3.9, 4, 5),
                            #fillColor = ~ pal(mag),
                            fillColor = ~pallet(size),
                            #weight = factor(nepal_data2$mag),
                            label = paste("mag=", nepal_data2$mag,
                                          "\n", "location = ",
                                          nepal_data2$place)
                            )
m
library(rgdal)

# Number of earthquakes in nepal from April 25 to April 30th 

nepal_data %>% dplyr::filter(date > ymd("2015-04-25"), 
                             date < ymd("2015-05-01")) %>%
  group_by(day) %>% 
  summarise(average_mag = mean(mag), 
            min_mag = min(mag), 
            max_mag = max(mag), 
            n_count = n())


# Calculate number of earthquake in each country that has more than two instances

all_month %>% head()

unique(all_month$country)

all_month$day <- day(all_month$date)
all_month$month <- month(all_month$date)
all_month$julian <- julian(all_month$date, origin = ymd("2015-01-01")) %>% 
  floor() %>% as.numeric()

tt <- all_month %>% 
  dplyr::filter(mag > 3.0) %>%
  group_by(country, julian) %>% 
  summarise(count = n(), 
            magnitude = mean(mag)) %>% 
  dplyr::filter(count > 5) 

unique(tt$country)

tt

ggplot(tt) + geom_col(aes(country, y=count)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


all_month


alaska <- all_month %>% 
  dplyr::filter(mag > 3.0) %>%
  group_by(country, julian) %>% 
  summarise(count = n(), 
            magnitude = max(mag)) %>% 
  dplyr::filter(count > 5) %>% 
  dplyr::filter(country == "Alaska") 

colSums(alaska[, -1])
head(alaska)

ggplot(alaska) + geom_point(aes(julian, magnitude))

all_month %>% 
  dplyr::filter(mag > 3.0) %>%
  group_by(country, julian) %>% 
  summarise(count = n(), 
            max = max(mag)) %>% 
  dplyr::filter(count > 5)


give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

# Nepal monthwise box plot
all_month %>%  
  dplyr::filter(country == "Nepal", 
                date > ymd_hms("2015-01-01 00:00:00"), 
                date < ymd_hms("2016-01-01 00:00:00")) %>% 
  ggplot(aes(cut(date, breaks="month"), mag)) + geom_boxplot() +
  stat_summary(fun.data = give.n, geom = "text", hjust = -0.5, color = "blue") + 
  xlab("Date") + ylab("Magnitude")

# Use library googlesheets to read Nepal earthquake data
library(googlesheets)

# Authenticate googlesheets
googlesheets::gs_auth()

fatal <- gs_title(x = "JD_Key Government Figures - Nepal Earthquake - 2015.xlsx")

fatalities <- googlesheets::gs_read(ss = fatal, ws = "05-June-2015")

head(fatalities)

fatalities %>% tail()

# Use visdat to visualize the type of data in a dataframe
visdat::vis_dat(fatalities, palette = "qual", sort_type = FALSE)
visdat::vis_miss(fatalities)

# Total number of fatalities by District
fatalities %>% dplyr::filter(!is.na(DIST_ID)) %>% 
  dplyr::filter(Tot_Deaths > 1) %>% 
  ggplot() + geom_col(aes(x = reorder(DISTRICT, -Tot_Deaths), 
                          Tot_Deaths, fill = "red")) + 
  xlab("District") + ylab("Total number of deaths") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text = element_text(size = 12))

# Total number of injured by District
fatalities %>% dplyr::filter(!is.na(DIST_ID)) %>% 
  dplyr::filter(Total_Injured > 50) %>% 
  ggplot() + geom_col(aes(x = reorder(DISTRICT, -Total_Injured), Total_Injured)) + 
  geom_text(aes(x = DISTRICT, Total_Injured, label = Total_Injured), 
            vjust = -0.5, color = "blue") + 
  xlab("District") + ylab("Total number of Injured People") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14))

# Total number of government / Public buildings destroyed 
fatalities %>% dplyr::filter(!is.na(DIST_ID)) %>%
  dplyr::filter(GovtBuild_Damage > 1, PublicBuild_Damage > 1) %>% 
  dplyr::select(DISTRICT, GovtBuild_Damage, PublicBuild_Damage) %>% 
  tidyr::gather(key = "variable", value = "value", -DISTRICT) %>% 
  ggplot() + geom_bar(aes(x = reorder(DISTRICT, -value), y = value, 
                          fill = variable), stat = "identity") + 
  facet_wrap( ~ variable, ncol = 1, scale = "free_y") + 
  xlab("DISTRICT") + ylab("Number of damaged buildings") + labs(fill = "") + 
  theme(axis.text.x = element_text(angle = 90, hjust =1))

# Total population
sum(fatalities$`Total Population`, na.rm = TRUE)


wordcloud(reorder(fatalities$DISTRICT,  fatalities$Total_Injured))

fatalities %>% dplyr::filter(!is.na(DIST_ID)) %>% 
  reorder(DISTRICT, - Total_Injured) %>% head()

fatalities <- arrange(fatalities, desc(Total_Injured))
library(tm)

# Word cloud by number of injuries
wordcloud_jd <- function(words, freq){
  wordcloud(words = words, freq = freq, min.freq = 5, 
            random.order = FALSE, random.color = FALSE, scale = c(10.,.8 ),
            colors= c("indianred1","indianred2","indianred3","indianred"))
}

wordcloud_jd(words = fatalities$DISTRICT, freq = fatalities$Total_Injured)
wordcloud_jd(words = fatalities$DISTRICT, freq = fatalities$Tot_Deaths)
wordcloud_jd(words = fatalities$DISTRICT, freq = fatalities$Death_Male)
wordcloud_jd(words = fatalities$DISTRICT, freq = fatalities$Death_Female)

