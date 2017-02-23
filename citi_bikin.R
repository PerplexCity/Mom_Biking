library(ggplot2)
library(plyr)
bbi<-element_text(face="bold.italic", color="black")

citi_1 <- read.csv("~/Dropbox/Hunter/bike/201601-citibike-tripdata.csv")
citi_2 <- read.csv("~/Dropbox/Hunter/bike/201602-citibike-tripdata.csv")
citi_3 <- read.csv("~/Dropbox/Hunter/bike/201603-citibike-tripdata.csv")
citi_4 <- read.csv("~/Dropbox/Hunter/bike/201604-citibike-tripdata.csv")
citi_5 <- read.csv("~/Dropbox/Hunter/bike/201605-citibike-tripdata.csv")
citi_6 <- read.csv("~/Dropbox/Hunter/bike/201606-citibike-tripdata.csv")
citi_7 <- read.csv("~/Dropbox/Hunter/bike/201607-citibike-tripdata.csv")
citi_8 <- read.csv("~/Dropbox/Hunter/bike/201608-citibike-tripdata.csv")
citi_9 <- read.csv("~/Dropbox/Hunter/bike/201609-citibike-tripdata.csv")
citi_10 <- read.csv("~/Dropbox/Hunter/bike/201610-citibike-tripdata.csv")
citi_11 <- read.csv("~/Dropbox/Hunter/bike/201611-citibike-tripdata.csv")
citi_12 <- read.csv("~/Dropbox/Hunter/bike/201612-citibike-tripdata.csv")

names(citi_10) <- names(citi_9)
names(citi_11) <- names(citi_9)
names(citi_12) <- names(citi_9)

citi_all <- rbind(citi_1, citi_2, citi_3, citi_4, citi_5, citi_6,
                  citi_7, citi_8, citi_9, citi_10, citi_11, citi_12)

ggplot(subset(citi_all, gender==1 | gender==2), aes(x = factor(1), fill = factor(gender))) + 
  geom_bar(width = 1) + coord_polar(theta = "y")

citi_all$age <- ifelse(citi_all$birth.year > 1996, "<20",
                       ifelse(citi_all$birth.year < 1997 & citi_all$birth.year>1976, "20-40",
                              ifelse(citi_all$birth.year < 1977 & citi_all$birth.year>1956, "40-60",
                                     ifelse(citi_all$birth.year < 1957, "> 60",
                                            NA))))

citi_backup <- citi_all

citi_full <- subset(citi_all, (gender==1 | gender ==2) & !is.na(birth.year))

ggplot(citi_age, aes(x = factor(1), fill = factor(age))) + 
  geom_bar(width = 1) + coord_polar(theta = "y")

citi_full$route <- paste(citi_full$start.station.id, citi_full$end.station.id, sep=" ")

citi_summary <- ddply(citi_full,~route,summarise,count_men=sum(gender==1), 
                      count_women = sum(gender==2), median_age = median(birth.year))

stations <- unique(citi_full[,c(4,5)])

citi_summary$count <- citi_summary$count_men + citi_summary$count_women
citi_top <- subset(citi_summary, count>999)
dim(citi_top)

citi_top$median_age <- 2016 - citi_top$median_age

citi_top$women <- citi_top$count_women/citi_top$count
citi_top$men <- citi_top$count_men/citi_top$count

ggplot(citi_top, aes(x=reorder(route, median_age), y=median_age)) + 
  geom_bar(stat="identity", fill="blue", width = 1) +
  scale_y_continuous(breaks=c(0,10,20,30,40,50)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        title=bbi) +
    labs(title="Citi Bike Routes \n ordered by Median Age", y="Median Age")

ggplot(citi_top, aes(x=reorder(route, men), y=men)) + 
  geom_bar(stat="identity", fill="blue", width = 1) +
  scale_y_continuous(breaks=c(0.25, 0.5, 0.75),
                     labels =c("25%", "50%", "75%")) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        title=bbi) +
  labs(title="Citi Bike Routes \n ordered by % Men", y="% Men")

citi_mom <- subset(citi_full, gender == 2 & birth.year == 1961)

citi_mom_summary <- ddply(citi_mom,~route,summarise, 
                      count=sum(gender==2))


