library(readxl)
df <- read.csv("gcar_data.csv")
head(df)

df <- df[c(-1,-15)]
head(df)

sum(is.na(df))

colnames(df)

str(df)

new_col <- colnames(df)

new_col[6] <- "price"
new_col[9] <- "transmission"
new_col[10] <- "fuel"
new_col[13] <- "milage"

colnames(df) <- new_col

head(df)

table(df$brand)
table(df$model)

table(df$registration_date)

library(dplyr)
library(lubridate)

df <- df %>%
  mutate(registration_date = if_else(grepl("^[0-1][0-9]/[0-9]{4}$", registration_date),
                                     my(registration_date),
                                     as.Date(NA)))

table(df$fuel)

df <- df %>%
  mutate(fuel = ifelse(grepl("^[^0-9]*$", fuel), fuel, NA))

df$price <- as.numeric(df$price)

summary(df$price)

table(df$power_kw)

df <- df %>%
  mutate(power_kw = ifelse(grepl("^[0-9]+(?:\\.[0-9]+)?$", power_kw), power_kw, NA))

table(df$power_ps)

df <- df %>%
  mutate(power_ps = ifelse(grepl("^[0-9]+(?:\\.[0-9]+)?$", power_ps), power_ps, NA))

table(df$transmission)
table(df$fuel)

table(df$fuel_consumption_l_100km)

# Assuming your dataframe is named df
zeros <- c("- (g/km)","- (l/100 km)","0 kWh/100 km","0 l/100 km")

df$fuel_consumption_l_100km[df$fuel_consumption_l_100km =="- (l/100 km)" ] <- 0
df$fuel_consumption_l_100km[df$fuel_consumption_l_100km =="- (g/km)"] <- 0
df$fuel_consumption_l_100km[df$fuel_consumption_l_100km =="0 kWh/100 km"] <- 0
df$fuel_consumption_l_100km[df$fuel_consumption_l_100km =="0 l/100 km"] <- 0
df$fuel_consumption_l_100km[df$fuel_consumption_l_100km ==" "] <- 0


sum(is.na(df$fuel_consumption_l_100km))



# Load necessary libraries
library(dplyr)
library(tidyr)

# Example data - replace this with your actual data frame


class(df$fuel_consumption_l_100km)

# Cleaning process
clean_data <- df %>%
  # Remove units and unwanted characters
  mutate(fuel_consumption_l_100km = gsub(" l/100 km| kg/100 km| kWh/100 km", "", fuel_consumption_l_100km)) %>%
  mutate(fuel_consumption_l_100km = gsub(",", ".", fuel_consumption_l_100km)) %>%
  mutate(fuel_consumption_l_100km = gsub("4.300", "4.3", fuel_consumption_l_100km)) %>%
  # Convert fuel to numeric
  mutate(fuel_consumption_l_100km = as.numeric(fuel_consumption_l_100km)) %>%
  # Replace non-numeric values with NA
  mutate(fuel_consumption_l_100km = ifelse(is.na(fuel_consumption_l_100km), NA, fuel_consumption_l_100km))

# View the cleaned data
table(clean_data$fuel_consumption_l_100km)
sum(is.na(clean_data$fuel_consumption_l_100km))


clean_data$fuel_consumption_l_100km[clean_data$fuel_consumption_l_100km > 22.9] <- NA

table(clean_data$fuel_consumption_l_100km)


df <- clean_data

table(df$fuel_consumption_l_100km)


table(df$fuel_consumption_g_km)




# Load necessary libraries
library(dplyr)
library(tidyr)

# Example data - replace this with your actual data frame

# Cleaning process
clean_data <- df %>%
  # Remove units and unwanted characters
  mutate(fuel_consumption_g_km = gsub(" g/km| km Reichweite", "", fuel_consumption_g_km)) %>%
  mutate(fuel_consumption_g_km = gsub(",", ".", fuel_consumption_g_km)) %>%
  # Handle specific cases (e.g., '1.020' should become '1.02')
  mutate(fuel_consumption_g_km = gsub("^1.020$", "1.02", fuel_consumption_g_km)) %>%
  # Convert emissions to numeric
  mutate(fuel_consumption_g_km = as.numeric(fuel_consumption_g_km)) %>%
  # Replace non-numeric values with NA
  mutate(fuel_consumption_g_km = ifelse(is.na(fuel_consumption_g_km), NA, fuel_consumption_g_km))

# View the cleaned data
table(clean_data$fuel_consumption_g_km)

df <- clean_data

df$fuel_consumption_g_km[df$fuel_consumption_g_km > 300] <- NA

table(df$fuel_consumption_g_km)


summary(df$milage)

table(df$fuel)

df$fuel[df$fuel =="Automatic"] <- NA
df$fuel[df$fuel =="KETTE NEUE"] <- NA
df$fuel[df$fuel =="Manual"] <- NA
df$fuel[df$fuel =="Unknown"] <- NA

tail(table(df$power_kw),20)

df$power_kw[df$power_kw > 735] <- NA

tail(table(df$power_ps),20)

df$power_ps[df$power_ps > 999] <- NA


# Cleaning process
clean_data <- df %>%
  # Convert to character for manipulation
  mutate(year = as.character(year)) %>%
  # Remove units and unwanted characters
  mutate(year = gsub(" km|/", "", year)) %>%
  mutate(year = gsub(",", ".", year)) %>%
  # Convert variable to numeric
  mutate(year = as.numeric(year)) %>%
  # Replace non-numeric values with NA
  mutate(year = ifelse(is.na(year), NA, year))

df$year <- clean_data$year

table(df$year)

df$year[df$year > 2023] <- NA
df$year[df$year < 1995] <- NA

str(df)
df$brand <- as.factor(df$brand)
df$model <- as.factor(df$model)
df$color <- as.factor(df$color)
df$year <- as.factor(df$year)
df$price <- as.numeric(df$price)
df$power_kw <- as.numeric(df$power_kw)
df$power_ps <- as.numeric(df$power_ps)
df$transmission <- as.factor(df$transmission)
df$fuel <- as.factor(df$fuel)
df$fuel_consumption_g_km <- as.numeric(df$fuel_consumption_g_km)
df$fuel_consumption_l_100km <- as.numeric(df$fuel_consumption_l_100km)

str(df)

summary(df)

a <- sum(is.na(df))
b <- dim(df)[1]*dim(df)[2]

missing_percent <- a/b

print(missing_percent)

set.seed(412)
library(missForest)
df <-  prodNA(df, noNA = 0.02)

a <- sum(is.na(df))
b <- dim(df)[1]*dim(df)[2]

missing_percent <- a/b

print(missing_percent)


df <- df %>%
  mutate(
    fuel_consumption_l_100km = if_else(fuel == "Electric", 0, fuel_consumption_l_100km),
    fuel_consumption_g_km = if_else(fuel == "Electric", 0, fuel_consumption_g_km)
  )

df$power_kw[df$power_kw > 735] <- NA

tail(table(df$power_ps),20)

df$power_ps[df$power_ps > 999] <- NA

summary(df)

####################

library(ggplot2)

data <- df

#########################
# imputation
str(df)
sum(is.na(df))
library(mice)
library(VIM)


mice_df <- df
cn <- colnames(mice_df)
cn
cn[11] <- "fuel_100"
cn[12] <- "fuel_g"
cn[4] <- "reg_date"

colnames(mice_df) <- cn
colnames(mice_df)


mice_plot <- aggr(mice_df[,c("brand","model")], col=c('darkred','orange'),
                  numbers=TRUE, sortVars=TRUE, prop=FALSE,
                  labels=names(c("brand","model")), cex.axis=.6,
                  gap=3, ylab=c("Missing data","Pattern"))
mice_plot


table(df$brand)
table(df$model)

head(mice_df[,c("brand","model")],30)

data <- mice_df[,c("brand","model")]

data$brand <- as.character(data$brand)
data$model <- as.character(data$model)

str(data)

# Sample data

# Function to extract the first word
get_first_word <- function(string) {
  if (!is.na(string)) {
    words <- unlist(strsplit(string, " "))
    return(words[1])
  } else {
    return(NA)
  }
}

# Impute missing brand values
data$brand <- ifelse(is.na(data$brand), sapply(data$model, get_first_word), data$brand)

# Impute missing model values
data$model <- ifelse(is.na(data$model) & !is.na(data$brand), data$brand, data$model)

# View updated data

sum(is.na(data$brand))
sum(is.na(data$model))

df$brand <- data$brand
df$model <- data$model

mice_plot2 <- aggr(df[,c("brand","model")], col=c('darkred','orange'),
                   numbers=TRUE, sortVars=TRUE, prop=FALSE,
                   labels=names(c("brand","model")), cex.axis=.6,
                   gap=3, ylab=c("Missing data","Pattern"))
mice_plot2

df$brand <- as.factor(df$brand)
df$model <- as.factor(df$model)

summary(df)

table(df$color)
df$color <- as.factor(df$color)
df$color[df$color== ""] <- NA
sum(is.na(df$color))





get_mode <- function(x) {
  tab <- table(x)  # Create a table of frequency counts
  mode_value <- names(which.max(tab))  # Find the mode
  return(mode_value)
}

# Calculate mode for the 'color' column
mode_color <- get_mode(df$color)

# Replace NA values with the mode
df$color[is.na(df$color)] <- mode_color

# Check updated data
sum(is.na(df$color))


str(df)

mice_plot3 <- aggr(df[,c("registration_date","year")], col=c('darkred','orange'),
                   numbers=TRUE, sortVars=TRUE, prop=FALSE,
                   labels=names(c("reg_date","year")), cex.axis=.6,
                   gap=3, ylab=c("Missing data","Pattern"))

data <- data.frame(
  year = c(2001, NA, 1995, NA, NA),
  registration_date = as.Date(c(NA, "2022-12-01", NA, "2018-05-20", NA)),
  stringsAsFactors = FALSE
)
str(data)

data <- df[,c("year","registration_date")]
data$year <- as.character(data$year)
data$year <- as.numeric(data$year)
str(data)

str(data)
# Impute missing year values using registration_date
data$year <- ifelse(is.na(data$year) & !is.na(data$registration_date),
                    as.integer(format(data$registration_date, "%Y")), 
                    data$year)

# View updated data
print(data)
sum(is.na(data$year))

df$year <- data$year

sum(is.na(df$year))

#reg year olmad??

df$year <- as.factor(df$year)
str(df)

colSums(is.na(df))/nrow(df)

names(table(df$brand))
?mapply

mice_plot4 <- aggr(df[,c("brand","price")], col=c('darkred','orange'),
                   numbers=TRUE, sortVars=TRUE, prop=FALSE,
                   labels=names(c("brand","price")), cex.axis=.6,
                   gap=3, ylab=c("Missing data","Pattern"))




data <- df[,c("brand","price")]
data$brand <- as.character(data$brand)
str(data)
# Normalize brand names to ensure consistency
data$brand <- tolower(data$brand)  # Convert to lower case
data$brand <- gsub("-", "", data$brand)  # Remove dashes

# Impute missing prices with the mean price of the corresponding brand
data <- data %>%
  group_by(brand) %>%
  mutate(price = if_else(is.na(price), mean(price, na.rm = TRUE), price))

# Print the updated data frame
print(data)

sum(is.na(data$brand))



df$price <- data$price

data <- df[,c("brand","power_kw")]
data$brand <- as.character(data$brand)
str(data)
# Normalize brand names to ensure consistency
data$brand <- tolower(data$brand)  # Convert to lower case
data$brand <- gsub("-", "", data$brand)  # Remove dashes

# Impute missing prices with the mean price of the corresponding brand
data <- data %>%
  group_by(brand) %>%
  mutate(power_kw = if_else(is.na(power_kw), mean(power_kw, na.rm = TRUE), power_kw))

# Print the updated data frame
print(data)

sum(is.na(data$power_kw))

df$power_kw <- data$power_kw

str(df)
sum(is.na(df$power_kw))



data <- df[,c("brand","power_ps")]
data$brand <- as.character(data$brand)
str(data)
# Normalize brand names to ensure consistency
data$brand <- tolower(data$brand)  # Convert to lower case
data$brand <- gsub("-", "", data$brand)  # Remove dashes

# Impute missing prices with the mean price of the corresponding brand
data <- data %>%
  group_by(brand) %>%
  mutate(power_ps = if_else(is.na(power_ps), mean(power_ps, na.rm = TRUE), power_ps))

# Print the updated data frame
print(data)

sum(is.na(data$power_ps))

df$power_ps <- data$power_ps


colSums(is.na(df))/nrow(df)

table(df$fuel)

mice_plot4 <- aggr(df[,c("transmission","fuel")], col=c('darkred','orange'),
                   numbers=TRUE, sortVars=TRUE, prop=FALSE,
                   labels=names(c("trans","fuel")), cex.axis=.6,
                   gap=3, ylab=c("Missing data","Pattern"))


fuel_data <- df[,c("fuel","fuel_consumption_l_100km","fuel_consumption_g_km")]

mice_plot4 <- aggr(fuel_data, col=c('darkred','orange'),
                   numbers=TRUE, sortVars=TRUE, prop=FALSE,
                   labels=names(c("fuel","fuel100","fuelg")), cex.axis=.6,
                   gap=3, ylab=c("Missing data","Pattern"))


library(dplyr)

str(df)


table(df$transmission)
sum(is.na(df$transmission))

trans_df <- df[,"transmission"]

data <- df[,c("transmission","price")]

str(data)
data$transmission <- as.character(data$transmission)
set.seed(123) # for reproducibility
known_transmissions <- data$transmission[!is.na(data$transmission)]
data$transmission[is.na(data$transmission)] <- sample(known_transmissions, sum(is.na(data$transmission)), replace = TRUE)
data$transmission
table(data$transmission)
table(df$transmission)
sum(is.na(df$transmission))

df$transmission <- data$transmission

colSums(is.na(df))/nrow(df)

data <- df[,c("transmission","fuel")]
str(data)
data$fuel <- as.character(data$fuel)


set.seed(123) # for reproducibility
known_fuels <- data$fuel[!is.na(data$fuel)]
data$fuel[is.na(data$fuel)] <- sample(known_fuels, sum(is.na(data$fuel)), replace = TRUE)

df$fuel <- data$fuel


colSums(is.na(df))/nrow(df)


data <- df[,c("brand","fuel_consumption_l_100km")]
data$brand <- as.character(data$brand)
str(data)
# Normalize brand names to ensure consistency
data$brand <- tolower(data$brand)  # Convert to lower case
data$brand <- gsub("-", "", data$brand)  # Remove dashes

# Impute missing prices with the mean price of the corresponding brand
data <- data %>%
  group_by(brand) %>%
  mutate(fuel_consumption_l_100km = if_else(is.na(fuel_consumption_l_100km), mean(fuel_consumption_l_100km, na.rm = TRUE), fuel_consumption_l_100km))

# Print the updated data frame

sum(is.na(data$fuel_consumption_l_100km))
df$fuel_consumption_l_100km <- data$fuel_consumption_l_100km

data <- df[,c("brand","fuel_consumption_g_km")]
data$brand <- as.character(data$brand)
str(data)
# Normalize brand names to ensure consistency
data$brand <- tolower(data$brand)  # Convert to lower case
data$brand <- gsub("-", "", data$brand)  # Remove dashes

# Impute missing prices with the mean price of the corresponding brand
data <- data %>%
  group_by(brand) %>%
  mutate(fuel_consumption_g_km = if_else(is.na(fuel_consumption_g_km), mean(fuel_consumption_g_km, na.rm = TRUE), fuel_consumption_g_km))

# Print the updated data frame

sum(is.na(data$fuel_consumption_g_km))

df$fuel_consumption_g_km <- data$fuel_consumption_g_km


colSums(is.na(df))/nrow(df)
df$year <- as.character(df$year)
df$year <- as.numeric(df$year)
df$age <- 2023-df$year
df$year <- as.factor(df$year)
str(df)

# 
# milage_age <- df[,c("milage","age")]
# head(milage_age)
# sum(is.na(milage_age))
# colSums(is.na(milage_age))
# 
# 
# data <- milage_age[complete.cases(milage_age$age), ]
# data
# colSums(is.na(data))
# 
# model <- lm(milage~age,data=data)
# 
# summary(model)
# 
# pred = predict(model, newdata = ic(data))   #ic(): extracts incomplete cases from a data set.
# pred
# 
# data$milage[as.numeric(names(pred))]= pred
# head(pred)


mice_plot5 <- aggr(df[,c("milage","age")], col=c('darkred','orange'),
                   numbers=TRUE, sortVars=TRUE, prop=FALSE,
                   labels=names(c("milage","age")), cex.axis=.6,
                   gap=3, ylab=c("Missing data","Pattern"))


data <- df[,c("brand","milage")]
data$brand <- as.character(data$brand)
str(data)
# Normalize brand names to ensure consistency
data$brand <- tolower(data$brand)  # Convert to lower case
data$brand <- gsub("-", "", data$brand)  # Remove dashes

# Impute missing prices with the mean price of the corresponding brand
data <- data %>%
  group_by(brand) %>%
  mutate(milage = if_else(is.na(milage), mean(milage, na.rm = TRUE), milage))

# Print the updated data frame

sum(is.na(data$milage))

df$milage <- data$milage


colSums(is.na(df))/nrow(df)

str(df)

df$transmission <- as.factor(df$transmission)
df$fuel <- as.factor(df$fuel)

table(df$fuel_consumption_l_100km)
summary(df)

df <- subset(df, select = -registration_date)

df <- na.omit(df)

sum(is.na(df))
write.csv(df, "processed.csv", row.names = FALSE)
(100000-99875)/100000

colSums(is.na(df))/nrow(df)