# !diagnostics off
library(readr)
library(dplyr)
library(tidyr)
library(mice)
library(ggplot2)
library(stlplus)
#Load the weather data of Mt. Rushmore, and rename some columns to make their names more concise.
wx_data <- read_csv("Mt-Rushmore-wx-data.txt", 
                    col_types = cols(Date = col_date(format = "%Y-%m-%d")), 
                    skip = 2)
View(wx_data)
colnames(wx_data)[colnames(wx_data) == "MaxTemperature"] <- "MaxTemp"
colnames(wx_data)[colnames(wx_data) == "MinTemperature"] <- "MinTemp"

#Use Mac Terminal to combine all CSV sets into one document on the number of visits to Rushmore. 
#For the above step, click System Preferences, click Keyboard, and select -> Shortcuts -> Services -> New Terminal at Folder
#Download all the monthly daily visit sets as CSVs, and then place them all in one folder
#Open the Mac Terminal for that folder, and enter cat *.csv >merged.csv
#A merged CSV file with all the combined data will appear in the folder
#The file will be called merged.csv. Rename it Rushmore-visits-TOTAL.csv 
#Place the file in the same working directory as the wx_data file

#Load that set as the dataframe "visits".
visits <- read.csv("Rushmore-visits-TOTAL.csv", skip = 3)
distinct(visits)
View(visits)

#Clean the data and delete unnecessary rows. Delete February 29 for the simplicity of analysis.
visits <- subset(visits, Description == "Daily Visitation")
visits <- subset(visits, Date != "February 29")
visits <- subset(visits, Date != "February 30")
visits <- subset(visits, Date != "February 31")
visits <- subset(visits, Date != "April 31")
visits <- subset(visits, Date != "June 31")
visits <- subset(visits, Date != "September 31")
visits <- subset(visits, Date != "November 31")

#Create a column to represent the year, and then delete unnecessary columns.
rownames(visits) <- seq(length=nrow(visits))
visits$normdays <- as.numeric(rownames(visits))
visits$normdays <- visits$normdays - 1
visits$Year <- 1993 + visits$normdays/365
visits$Year <- floor(visits$Year)
visits$normdays <- NULL
visits$SameMonthLastYear <- NULL
visits$PercentChange <- NULL

#Convert the column for the number of visits to a numeric column, and be sure all values are positive.
visits$ThisMonth <- gsub("," , "", visits$ThisMonth)
visits$ThisMonth <- as.numeric(visits$ThisMonth)
visits$ThisMonth <- abs(visits$ThisMonth)

#Unify the Date column and convert it to date format.
visits <- unite(visits, Date, Date, Year, sep = ", ", remove = TRUE)
visits$Date <- as.Date(visits$Date, format = "%B %d, %Y")
summary(visits$Date)
visits <- subset(visits, !is.na(Date))

#Make a few final changes with column name
#Delete the Description column now that the dates are organized
colnames(visits)[colnames(visits) == "ThisMonth"] <- "DailyVisits"
visits$Description <- NULL

#Save the cleaned set as a CSV file.
write.csv(visits, "Rushmore_TOTAL-update.csv")

#Merge the weather and daily visit sets.
Rushmore <- merge(visits, wx_data, by = "Date")

#Save the merged Rushmore dataset as a CSV file.
View(Rushmore)
summary(Rushmore)
write.csv(Rushmore, "Rushmore-data.csv")


#Fill the NA values for temps by imputation. 
#Do it on a monthly basis to get reasonable temperature estimates.
#Create a column to depict where the NA values for temps are, so they can easily be found.
Rushmore$NA_temp <- ifelse((is.na(Rushmore$MaxTemp & Rushmore$MinTemp)), 1,0)
table(Rushmore$NA_temp)

Rushmore <- separate(Rushmore, Date, c("Year", "Month", "Day"), sep = "-", remove = FALSE)
View(Rushmore)

#January
Rushmore_01 <- filter(Rushmore, Month %in% c("01"))
Rushmore_Jan <- Rushmore_01[c("MaxTemp", "MinTemp")]
summary(Rushmore_Jan)
set.seed(256)
Rushmore_Jan_impute <- complete(mice(Rushmore_Jan))
summary(Rushmore_Jan_impute)

Rushmore_01$MaxTemp <- Rushmore_Jan_impute$MaxTemp
Rushmore_01$MinTemp <- Rushmore_Jan_impute$MinTemp
summary(Rushmore_01)

#February
Rushmore_02 <- filter(Rushmore, Month %in% c("02"))
Rushmore_Feb <- Rushmore_02[c("MaxTemp", "MinTemp")]
summary(Rushmore_Feb)
set.seed(256)
Rushmore_Feb_impute <- complete(mice(Rushmore_Feb))
summary(Rushmore_Feb_impute)

Rushmore_02$MaxTemp <- Rushmore_Feb_impute$MaxTemp
Rushmore_02$MinTemp <- Rushmore_Feb_impute$MinTemp
summary(Rushmore_02)

#March
Rushmore_03 <- filter(Rushmore, Month %in% c("03"))
Rushmore_Mar <- Rushmore_03[c("MaxTemp", "MinTemp")]
summary(Rushmore_Mar)
set.seed(256)
Rushmore_Mar_impute <- complete(mice(Rushmore_Mar))
summary(Rushmore_Mar_impute)

Rushmore_03$MaxTemp <- Rushmore_Mar_impute$MaxTemp
Rushmore_03$MinTemp <- Rushmore_Mar_impute$MinTemp
summary(Rushmore_03)

#April
Rushmore_04 <- filter(Rushmore, Month %in% c("04"))
Rushmore_Apr <- Rushmore_04[c("MaxTemp", "MinTemp")]
summary(Rushmore_Apr)
set.seed(256)
Rushmore_Apr_impute <- complete(mice(Rushmore_Apr))
summary(Rushmore_Apr_impute)

Rushmore_04$MaxTemp <- Rushmore_Apr_impute$MaxTemp
Rushmore_04$MinTemp <- Rushmore_Apr_impute$MinTemp
summary(Rushmore_04)

#May
Rushmore_05 <- filter(Rushmore, Month %in% c("05"))
Rushmore_May <- Rushmore_05[c("MaxTemp", "MinTemp")]
summary(Rushmore_May)
set.seed(256)
Rushmore_May_impute <- complete(mice(Rushmore_May))
summary(Rushmore_May_impute)

Rushmore_05$MaxTemp <- Rushmore_May_impute$MaxTemp
Rushmore_05$MinTemp <- Rushmore_May_impute$MinTemp
summary(Rushmore_05)

#June
Rushmore_06 <- filter(Rushmore, Month %in% c("06"))
Rushmore_Jun <- Rushmore_06[c("MaxTemp", "MinTemp")]
summary(Rushmore_Jun)
set.seed(256)
Rushmore_Jun_impute <- complete(mice(Rushmore_Jun))
summary(Rushmore_Jun_impute)

Rushmore_06$MaxTemp <- Rushmore_Jun_impute$MaxTemp
Rushmore_06$MinTemp <- Rushmore_Jun_impute$MinTemp
summary(Rushmore_06)

#July
Rushmore_07 <- filter(Rushmore, Month %in% c("07"))
Rushmore_Jul <- Rushmore_07[c("MaxTemp", "MinTemp")]
summary(Rushmore_Jul)
set.seed(256)
Rushmore_Jul_impute <- complete(mice(Rushmore_Jul))
summary(Rushmore_Jul_impute)

Rushmore_07$MaxTemp <- Rushmore_Jul_impute$MaxTemp
Rushmore_07$MinTemp <- Rushmore_Jul_impute$MinTemp
summary(Rushmore_07)

#August
Rushmore_08 <- filter(Rushmore, Month %in% c("08"))
Rushmore_Aug <- Rushmore_08[c("MaxTemp", "MinTemp")]
summary(Rushmore_Aug)
set.seed(256)
Rushmore_Aug_impute <- complete(mice(Rushmore_Aug))
summary(Rushmore_Aug_impute)

Rushmore_08$MaxTemp <- Rushmore_Aug_impute$MaxTemp
Rushmore_08$MinTemp <- Rushmore_Aug_impute$MinTemp
summary(Rushmore_08)

#September
Rushmore_09 <- filter(Rushmore, Month %in% c("09"))
Rushmore_Sep <- Rushmore_09[c("MaxTemp", "MinTemp")]
summary(Rushmore_Sep)
set.seed(256)
Rushmore_Sep_impute <- complete(mice(Rushmore_Sep))
summary(Rushmore_Sep_impute)

Rushmore_09$MaxTemp <- Rushmore_Sep_impute$MaxTemp
Rushmore_09$MinTemp <- Rushmore_Sep_impute$MinTemp
summary(Rushmore_09)

#October
Rushmore_10 <- filter(Rushmore, Month %in% c("10"))
Rushmore_Oct <- Rushmore_10[c("MaxTemp", "MinTemp")]
summary(Rushmore_Oct)
set.seed(256)
Rushmore_Oct_impute <- complete(mice(Rushmore_Oct))
summary(Rushmore_Oct_impute)

Rushmore_10$MaxTemp <- Rushmore_Oct_impute$MaxTemp
Rushmore_10$MinTemp <- Rushmore_Oct_impute$MinTemp
summary(Rushmore_10)

Rushmore_10$MaxTemp[which(Rushmore_10$Date == "2013-10-04")] <- 32
Rushmore_10$MaxTemp[which(Rushmore_10$Date == "2013-10-05")] <- 35
Rushmore_10$MaxTemp[which(Rushmore_10$Date == "2013-10-06")] <- 45
Rushmore_10$MinTemp[which(Rushmore_10$Date == "2013-10-04")] <- 15
summary(Rushmore_10)

#November
Rushmore_11 <- filter(Rushmore, Month %in% c("11"))
Rushmore_Nov <- Rushmore_11[c("MaxTemp", "MinTemp")]
summary(Rushmore_Nov)
set.seed(256)
Rushmore_Nov_impute <- complete(mice(Rushmore_Nov))
summary(Rushmore_Nov_impute)

Rushmore_11$MaxTemp <- Rushmore_Nov_impute$MaxTemp
Rushmore_11$MinTemp <- Rushmore_Nov_impute$MinTemp
summary(Rushmore_11)

#December
Rushmore_12 <- filter(Rushmore, Month %in% c("12"))
Rushmore_Dec <- Rushmore_12[c("MaxTemp", "MinTemp")]
summary(Rushmore_Dec)
set.seed(256)
Rushmore_Dec_impute <- complete(mice(Rushmore_Dec))
summary(Rushmore_Dec_impute)

Rushmore_12$MaxTemp <- Rushmore_Dec_impute$MaxTemp
Rushmore_12$MinTemp <- Rushmore_Dec_impute$MinTemp
summary(Rushmore_12)

#Merge the imputed monthly sets back to one dataset, and set the dates in order.
Rushmore <- bind_rows(Rushmore_01, Rushmore_02, Rushmore_03, Rushmore_04, 
                      Rushmore_05, Rushmore_06, Rushmore_07, Rushmore_08,
                      Rushmore_09, Rushmore_10, Rushmore_11, Rushmore_12)

Rushmore <- Rushmore[order(Rushmore$Date), ] 
Rushmore$Year <- NULL
Rushmore$Month <- NULL
Rushmore$Day <- NULL
rownames(Rushmore) <- seq(length=nrow(Rushmore))
View(Rushmore)
str(Rushmore)
summary(Rushmore)
#Look over the imputed values by clicking on the old NA_temp column
#Delete the column if the numbers look plausible (Some missing temps from October 2013 needed to be fixed)
Rushmore$NA_temp <- NULL
write.csv(Rushmore, "Rushmore.csv")

library(ggplot2)
ggplot(Rushmore, aes(x = MaxTemp, y = DailyVisits)) + labs(x = "Daily High Temperature (°F)", y = "Daily Visitors",
      title = "Mt. Rushmore Visits vs. Daily High Temp (°F)", 
      caption = "Based on data from January 1993 to May 2017") + geom_point(alpha = 0.2) 

#Time series decompositions
daily_v_ts <- ts(Rushmore$DailyVisits, frequency = 365, start=c(1993,1), end=c(2017,151))
str(daily_v_ts)
plot(daily_v_ts, main = "Daily Visits to Mt. Rushmore", xlab = "Year", ylab = "Number of Daily Visits")
daily_v_stl <- stl(daily_v_ts, s.window="periodic")
plot(daily_v_stl, main = "Decomposition of Daily Visits to Mt. Rushmore")

hi_temp_ts <- ts(Rushmore$MaxTemp, frequency = 365, start=c(1993,1), end=c(2017,151))
plot(hi_temp_ts, main = "Daily High Temperatures at Mt. Rushmore", xlab = "Year", ylab = "Daily High (°F)")
str(hi_temp_ts)
hi_temp_stl <- stl(hi_temp_ts, s.window="periodic")
plot(hi_temp_stl, main = "Decomposition of Daily High Temperatures at Mt. Rushmore")

lo_temp_ts <- ts(Rushmore$MinTemp, frequency = 365, start=c(1993,1), end=c(2017,151))
plot(lo_temp_ts, main = "Daily Low Temperatures at Mt. Rushmore", xlab = "Year", ylab = "Daily Low (°F)")
str(lo_temp_ts)
lo_temp_stl <- stl(lo_temp_ts, s.window="periodic")
plot(lo_temp_stl, main = "Decomposition of Daily Low Temperatures at Mt. Rushmore"))

precip_ts <- ts(Rushmore$Precipitation, frequency = 365, start=c(1993,1), end=c(2017,151))
plot(precip_ts, main = "Daily Precipitation at Mt. Rushmore", xlab = "Year", ylab = "Precipitation (inches)")
str(precip_ts)
precip_stl <- stlplus(precip_ts, s.window = "periodic")
plot(precip_stl, main = "Decomposition of Daily Precipitation at Mt. Rushmore")

#Extract the remainders, and then plot them against one another
daily_v_remain <- daily_v_stl$time.series[,"remainder"]
hi_temp_remain <- hi_temp_stl$time.series[,"remainder"]
lo_temp_remain <- lo_temp_stl$time.series[,"remainder"]
precip_remain <- precip_stl$data[,"remainder"]

#Plot the variables
plot(hi_temp_remain, daily_v_remain, main = "Daily Visits vs Highs", 
     xlab = "Daily Highs (deviation from trend, °F)", ylab = "Daily Visits (deviation from trend)")
abline(lm(daily_v_remain ~ hi_temp_remain), col="red") #regression line

plot(lo_temp_remain, daily_v_remain, main = "Daily Visits vs Lows", 
     xlab = "Daily Lows (deviation from trend, °F)", ylab = "Daily Visits (deviation from trend)")
abline(lm(daily_v_remain ~ lo_temp_remain), col="red") #regression line

plot(precip_remain, daily_v_remain, main = "Daily Visits vs Precipitation", 
     xlab = "Daily Precipitation (deviation from trend, inches)", ylab = "Daily Visits (deviation from trend)")
abline(lm(daily_v_remain ~ precip_remain), col="red") #regression line


#Complete a linear regression analysis
V_reg <- lm(daily_v_remain ~ hi_temp_remain + lo_temp_remain + precip_remain, data = Rushmore)
summary(V_reg)

V_reg2 <- lm(daily_v_remain ~ hi_temp_remain + precip_remain, data = Rushmore)
summary(V_reg2)

###############     END     ####################
