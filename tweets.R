
library(ggplot2)
library(lubridate)
library(scales)
library(dplyr)

tweets <- read.csv("data/GettingAndCleaningData-master/tweets.csv", 
                   stringsAsFactors = FALSE)

head(tweets)


tweets$timestamp <- ymd_hms(tweets$timestamp)
tweets$timestamp <- with_tz(tweets$timestamp, "America/Los_Angeles")

head(tweets)

# Tweets by year, month, and day

ggplot(data = tweets, aes(x = timestamp)) + 
  geom_histogram(aes(fill = ..count..)) +
  theme(legend.position = "none") + 
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Now plot 

ggplot(data = tweets, aes(x = year(timestamp))) + 
  geom_histogram(breaks = seq(2010, 2015, by =1), aes(fill = ..count..)) + 
  theme(legend.position = "none") + 
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


# pattern over the days of the week


ggplot(data = tweets, aes(x = wday(timestamp, label = TRUE))) + 
  geom_bar(breaks = seq(0.5, 7.5, by = 1), aes(fill = ..count..)) + 
  theme(legend.position = "none") + 
  xlab("Day of the week") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")

# Chi square test
# Hypothesis that I tweet at the same rate on all days and I got this 
# distribution of tweets just by chance and random sampling

chisq.test(table(wday(tweets$timestamp, label = TRUE)))

# My chi-squared test indicates that I cannot reject the null hypothesis. 

myTable <- table(wday(tweets$timestamp, label = TRUE))
mean(myTable[c(2:5)]) / mean(myTable[c(1, 6, 7)])

# The values for Monday through Thursday are 0.9512 less than other days. 

chisq.test(table(wday(tweets$timestamp, label = TRUE)), p = c(21, 20, 20, 20, 20, 21, 21)/143)



# ggplot

ggplot(data = tweets, aes(x = month(timestamp, label = TRUE))) + 
  geom_bar(breaks = seq(0.5, 7.5, by = 1), aes(fill = ..count..)) + 
  theme(legend.position = "none") + 
  xlab("Day of the week") + ylab("Number of tweets") + 
  scale_fill_gradient(low = "midnightblue", high = "aquamarine4")


# Tweets by time of day 

tweets$timeonly <- as.numeric(tweets$timestamp - trunc(tweets$timestamp, "days"))
tweets$timeonly


tweets[(minute(tweets$timestamp) == 0 & second(tweets$timestamp) == 0), 11] <- NA

mean(is.na(tweets$timeonly))

class(tweets$timeonly) <- "POSIXct, origin = America/Los_Angeles"
ggplot(data = tweets, aes(x = timeonly)) + 
  geom_histogram(aes(fill = ..count..)) + 
  theme(legend.position = "none") + 
  xlab("Time") + ylab("Number of tweets")


# Late night tweets

latenighttweets <- tweets[(hour(tweets$timestamp) < 6),]

latenighttweets


ggplot(data = latenighttweets, aes(x = timestamp)) + 
  geom_histogram(aes(fill = ..count..)) + 
  theme(legend.position = "none")


# Tweets with hashtags

ggplot(tweets, aes(factor(grepl("#", tweets$text)))) + 
  geom_bar(fill = "midnightblue") + 
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  ylab("Number of tweets") + 
  ggtitle("Tweets with Hashtags") + 
  scale_x_discrete(labels = c("No hashtags", "Tweets with hashtags"))

names(tweets)

# Retweeted Tweets

ggplot(tweets, aes(factor(!is.na(retweeted_status_id)))) +
  geom_bar(fill = "midnightblue") + 
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  ylab("Number of tweets") + 
  scale_x_discrete(labels = c("Not retweeted", "Retweeted tweets"))

tweets %>% select(retweeted_status_id) %>%
  dplyr::filter(!is.na(retweeted_status_id)) %>% count()

tweets$retweeted_status_id

# Now let's look at replied tweets

ggplot(tweets, aes(factor(!is.na(in_reply_to_status_id)))) + 
  geom_bar(fill = "midnightblue") + 
  theme(legend.position = "none", axis.title.x = element_blank()) + 
  ylab("Number of tweets") + 
  ggtitle("Replied tweets") + 
  scale_x_discrete(labels = c("Not in reply", "Replied tweets"))

# Now let's categorize tweets, reply and retweets

tweets$type <- "tweet"
tweets$type 
tweets[(!is.na(tweets$retweeted_status_id)), "type"] <- "RT"
tweets[(!is.na(tweets$in_reply_to_status_id)), "type"] <- "reply"
tweets$type <- as.factor(tweets$type)
tweets$type <- factor(tweets$type, levels(tweets$type)[c(3,1,2)])

ggplot(data = tweets, aes(x = timestamp, fill = type)) + 
  geom_histogram() + 
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_manual(values = c("midnightblue", "deepskyblue4", "aquamarine3"))


# Plot retweets / tweets/ reply as proportion 

ggplot(data = tweets, aes(x = timestamp,fill = type)) + 
  geom_bar(stat = "identity") 
  xlab("Time") + ylab("Number of tweets") + 
  scale_fill_manual(values = c("midnightblue", "deepskyblue4", "aquamarine3"))
