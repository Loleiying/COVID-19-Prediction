# importing library
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(janitor))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(naniar))

# importing dataset
train=clean_names(read.csv("D:/Group Project/train_cum.csv", na.strings=c("","NA")))

# peeking dataset
head(train)
str(train)

# type casting
train$country_region=as.character(train$country_region)
train$province_state=as.character(train$province_state)
train$date=as.character(train$date)
train$country_province=as.character(train$country_province)

# seeking for NA
sum(is.na(train))

# generating worldwide trend

# picking data only from the last recorded date
seventy=train %>% 
  filter(date=="2020-03-31") %>%
  arrange(country_region,cum_cases)
head(seventy)

# calculating total cummulative cases each country
country.cum=aggregate(seventy$cum_cases,by=list(seventy$country_region),FUN=sum) %>%
  arrange(-x)
colnames(country.cum) <- c("country_region","total")

# generating barplot
ggplot(data=country.cum)+
  geom_col(mapping=aes(x=reorder(country_region,total),y=total)) +
  labs(title = "Barplot of Total Cummulative Cases in 173 Countries as of 03-31-2020",
       y="Total Number of Cummulative Cases",
       x="Country") +
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

# rescaling
ggplot(data=country.cum)+
  geom_col(mapping=aes(x=reorder(country_region,total),y=log(total))) +
  labs(title = "Barplot of Log(Total Cummulative Cases) in 173 Countries as of 03-31-2020",
       y="Log(Total Number of Cummulative Cases)",
       x="Country") +
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

# 20 highest
twenty.highest.cum=head(country.cum,20)

# generating barplot
ggplot(data=twenty.highest.cum)+
  geom_col(mapping=aes(x=reorder(country_region,total),y=total)) +
  labs(title = "Barplot of 20 Highest Total Cummulative Cases as of 03-31-2020",
       y="Total Number of Cummulative Cases",
       x="Country") +
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

# rescaling
ggplot(data=twenty.highest.cum)+
  geom_col(mapping=aes(x=reorder(country_region,total),y=log(total))) +
  labs(title = "Barplot of 20 Highest Log(Total Cummulative Cases) as of 03-31-2020",
       y="Log(Total Number of Cummulative Cases)",
       x="Country") +
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

# 20 lowest
twenty.lowest.cum=tail(country.cum,20)

#generating barplot
ggplot(data=twenty.lowest.cum)+
  geom_col(mapping=aes(x=reorder(country_region,total),y=total)) +
  labs(title = "Barplot of 20 Lowest Total Cummulative Cases as of 03-31-2020",
       y="Total Number of Cummulative Cases",
       x="Country") +
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

# rescaling
ggplot(data=twenty.lowest.cum)+
  geom_col(mapping=aes(x=reorder(country_region,total),y=log(total))) +
  labs(title = "Barplot of 20 Lowest Log(Total Cummulative Cases) as of 03-31-2020",
       y="Log(Total Number of Cummulative Cases)",
       x="Country") +
  theme(plot.title=element_text(hjust=0.5))+
  coord_flip()

# generating day-by-day trend

# top 20 of highest and lowest
twenty.highest.names=c(twenty.highest.cum$country_region)
twenty.lowest.names=c(twenty.lowest.cum$country_region)

twenty.highest=filter(train,country_region %in% twenty.highest.names)
twenty.lowest=filter(train,country_region %in% twenty.lowest.names)

# calculating day-by-day cumulative cases for each of top 20 highest and lowest
twenty.highest.dbd=aggregate(twenty.highest$cum_cases,by=list(twenty.highest$date,twenty.highest$country_region),FUN=sum)
twenty.lowest.dbd=aggregate(twenty.lowest$cum_cases,by=list(twenty.lowest$date,twenty.lowest$country_region),FUN=sum)

colnames(twenty.highest.dbd)=c("date","country_region","total")
colnames(twenty.lowest.dbd)=c("date","country_region","total")

# removing year
# gsub("2020-","",twenty.highest.dbd$date)
# gsub("2020-","",twenty.lowest.dbd$date)

# typecasting
twenty.highest.dbd$date = as.Date(twenty.highest.dbd$date)
twenty.lowest.dbd$date = as.Date(twenty.lowest.dbd$date)

# generating plot
ggplot(data=twenty.highest.dbd)+
  geom_line(mapping=aes(x=date,y=total,group=country_region,color=country_region),size=0.35)+
  scale_x_date(date_labels="%d %b",date_breaks="1 week")+
  labs(title="Linegraph of 20 Highest Weekly Total Cumulative Cases until 03-31-2020")+
  theme(plot.title=element_text(hjust=0.5))

ggplot(data=twenty.lowest.dbd)+
  geom_line(mapping=aes(x=date,y=total,group=country_region,color=country_region),size=0.35)+
  scale_x_date(date_labels="%d %b",date_breaks="1 week")+
  labs(title="Linegraph of 20 Lowest Weekly Total Cumulative Cases until 03-31-2020")+
  theme(plot.title=element_text(hjust=0.5))
