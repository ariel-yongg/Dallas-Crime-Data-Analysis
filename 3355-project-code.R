
## Data Cleaning

## DALLAS DATA CLEANING

# read the data
dcrime = read.csv("crime.csv")
lcrime = read.csv("crime_la.csv")

# Convert the date column to Date type
as.Date(dcrime$Arrest.Date, format = "%m/%d/%Y")

# Convert the time column to POSIXct
as.POSIXct(dcrime$Arrest.Time, format = "%I:%M:%S %p")

# Remove state column
dcrime$Arrest.State <- NULL

# Convert day of week to factor
dcrime$Arrest.Day.of.The.Week <- as.factor(dcrime$Arrest.Day.of.The.Week)

# Convert race to factor
dcrime$Arrestee.Race <- as.factor(dcrime$Arrestee.Race)

# Convert sex to factor
dcrime$Arrestee.Sex <- as.factor(dcrime$Arrestee.Sex)

# Convert drug related to factor
dcrime$Drug.Related <- as.factor(dcrime$Drug.Related)

# Convert drug type to factor
dcrime$Drug.Type <- as.factor(dcrime$Drug.Type)

## LA data cleaning

# Convert Date Occurred
lcrime$Date.Occurred <- as.Date(lcrime$Date.Occurred, format = "%m/%d/%Y")

# Convert Date Reported
lcrime$Date.Reported <- as.Date(lcrime$Date.Reported, format = "%m/%d/%Y")

# Convert Time Occurred
lcrime$Time.Occurred <- as.character(lcrime$Time.Occurred)
as.POSIXct(lcrime$Time.Occurred, format = "%H%M")


## DATA ANALYSIS SECTION

## Figure 3.1	Month vs. Frequency of Month Graph
## CRIME BY MONTH

# read data
setwd("~/3355") 
dcrime <- read.csv("crime_dallas.csv")
lcrime <- read.csv("crime_la.csv")

# get packages
library(ggplot2)
library(lubridate)

# extract month from date
l_months <- format(as.Date(lcrime$Date.Reported, format = "%m/%d/%Y"), "%m")
d_months <- format(as.Date(mdy(dcrime$Arrest.Date), format = "%Y-%m-%d"), "%m")

# get table of months and its frequency
dfreq <- data.frame(count = table(d_months))
lfreq <- data.frame(count = table(l_months))

# relabel
colnames(dfreq) <- c("month", "frequency")
colnames(lfreq) <- c("month", "frequency")

# display plot for dallas
ggplot() +
  geom_line(data = dfreq, aes(x = as.numeric(month), y = frequency),  color = "#899AD6", size = 1) +
  ggtitle("Crime by Month in Dallas") +
  xlab("Month") + ylab("Frequency of Crime") +
  scale_x_continuous(name = "Month", breaks = 1:12, labels = month.name[1:12]) 

# display plot for la
ggplot() +
  geom_line(data = lfreq, aes(x = as.numeric(month), y = frequency), color = "#E47080", size = 1) +
  ggtitle("Crime by Month in Los Angeles") +
  xlab("Month") + ylab("Frequency of Crime") +
  scale_x_continuous(name = "Month", breaks = 1:12, labels = month.name[1:12]) 

# normalize data using log transformation
# amount of total crime too large of a gap -> must normalize for better comparison
dfreq$dnorm <- scale(dfreq$frequency)
lfreq$lnorm <- scale(lfreq$frequency)

# display plot dallas vs la
ggplot() +
  geom_line(data = dfreq, aes(x = as.numeric(month), y = dnorm, color = "Dallas"), size = 1) +
  geom_line(data = lfreq, aes(x = as.numeric(month), y = lnorm, color = "Los Angeles"), size = 1) +
  ggtitle("Crime by Month in Dallas vs. Los Angeles") +
  xlab("Month") + 
  ylab("Frequency of Crime") +
  scale_color_manual(name = "City", values = c("Dallas" = "#899AD6", "Los Angeles" = "#E47080")) +
  #theme(axis.text = element_text(size = 7)) +
  scale_x_continuous(name = "Month", breaks = 1:12, labels = month.name[1:12])

## Figure 3.2 Crime by Location in Dallas

library(ggplot2)

# Load your data
dcrime <- read.csv("C:/Users/mathg/Downloads/crime (1).csv")

# Count the number of observations for each location
location_counts <- as.data.frame(table(dcrime$Arrest.Location))

# Get the top 5 locations
top_locations <- location_counts[order(location_counts$Freq, decreasing = TRUE),][1:5,]

# Abbreviate the location names to one word
top_locations$Var1 <- sapply(strsplit(as.character(top_locations$Var1), " "), `[`, 1)

# Load ggplot2 library
library(ggplot2)

# Create a barplot using ggplot
ggplot(data = top_locations, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", fill = "blue") +
  ggtitle("Top 5 Arrest Locations in Dallas") +
  xlab("Location") +
  ylab("Total Number of Crime")

## Figure 3.2	Crime by Location in Los Angeles

library(ggplot2)
lacrime <- read.csv("C:/Users/mathg/Downloads/crime_la.csv")
lacrime$Premise.Description <- substr(lacrime$Premise.Description, 1, 12)
top5 <- names(sort(table(lacrime$Premise.Description), decreasing = TRUE)[1:5])
lacrime_top5 <- lacrime[lacrime$Premise.Description %in% top5, ]
ggplot(lacrime_top5, aes(x = Premise.Description)) +
  geom_bar(fill = "blue") +
  xlab("Location") +
  ylab("Total Number of Crimes")

## Figure 3.3	Age of Arrestee vs. Weapon in Use

# Load the necessary libraries
library(ggplot2)
library(dplyr)

# Read in the data
dcrime <- read.csv("C:/Users/mathg/Downloads/crime (1).csv")

# Find the top 5 arrest weapons by number of observations
top_weapons <- dcrime %>%
  group_by(Arrest.Weapon) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(5) %>%
  pull(Arrest.Weapon)

# Filter the data to only include the top 5 arrest weapons
dcrime_filtered <- dcrime %>%
  filter(Arrest.Weapon %in% top_weapons)

# Create the boxplot
ggplot(dcrime_filtered, aes(x = Arrest.Weapon, y = Arrestee.Age.At.Arrest.Time, fill = Arrest.Weapon)) +
  geom_boxplot() +
  labs(x = "Weapons", y = "Age of Arrestee") +
  scale_fill_discrete(name = "Arrest Weapon")

## Figure 3.4	Drug-related Crime Pie Chart 
df = as.data.frame(table(dcrime[which(dcrime$Drug.Related == "Yes"), ]$Drug.Type))

ggplot(df, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void() +
  labs(title="Drug Types", fill="Category")

## Figure 3.5	Crime by Day of the Week
# Load the data
dcrime <- read.csv("C:/Users/mathg/Downloads/crime (1).csv")

# Create a factor variable for the days of the week
dcrime$Arrest.Day.of.The.Week <- factor(dcrime$Arrest.Day.of.The.Week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Create a table of counts for each day of the week
counts <- table(dcrime$Arrest.Day.of.The.Week)

# Create the bar plot
barplot(counts, xlab = "Day of the Week", ylab = "Number of Arrests", main = "Bar Plot of Arrests by Day of the Week")

