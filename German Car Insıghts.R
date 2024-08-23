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

## Barplot of brands
brand_counts <- data %>%
  group_by(brand) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# Select top 15 brands
top_brands <- brand_counts[1:20, ]

# Combine others
others_count <- sum(brand_counts$count[21:length(brand_counts$count)])
top_brands <- rbind(top_brands, data.frame(brand = "others", count = others_count))

# Plot
ggplot(top_brands, aes(y = reorder(brand, -count), x = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 20 Brands ",
       x = "Frequency",
       y = "Brand") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + # Adjust text size for better visibility
  coord_flip() # Flip coordinates to make it horizontal

summary(df$price)


hist(df$price,xlim=c(0,1000000),breaks = "Scott",col = "steelblue",
     main = "Histogram of Price in Euros",xlab = "Price",ylab = "Frequency")


boxplot(df$price)

# Filter the data to include only prices within the specified range (0 to 1000000)
filtered_df <- df[df$price >= 0 & df$price <= 100000, ]

# Create a boxplot of the 'price' variable
ggplot(data = filtered_df, aes(x = "", y = price)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(x = "", y = "Price (Euro)", title = "Boxplot of Price") +
  theme_minimal()

pr2 <- na.omit(df$price)
mean(pr2)
median(pr2)
max(pr2)



head(table(df$price),30)

qqnorm(df$price)
qqline(df$price)

logdata <- log(df$price)

hist(logdata)

qqnorm(logdata)
qqline(logdata)

boxplot(logdata)
summary(df)

options(scipen=999)
hist(df$milage,breaks = "Scott",xlim=c(0,500000),main = "Histogram of Milage in km",
     xlab="Milage",col="pink")
?hist


milage_zeros <- subset(df,milage== 0)
table(milage_zeros$year)

head(milage_zeros$year)

barplot(table(df$model))


library(dplyr)
library(ggplot2)

# Assuming your data frame 'df' has been loaded as 'data'
data <- df

# Create a bar plot of the top 20 models
model_counts <- data %>%
  group_by(model) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice_head(n = 25)  # Select the top 20 models

# Plot
ggplot(model_counts, aes(x = reorder(model, -count), y = count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  ylim(c(0,4000))+
  labs(title = "Top 25 Models",
       x = "Model",
       y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) # Adjust text angle for better visibility # Flip coordinates to make it horizontal





# Load the required library
library(treemap)

# Example data
data <- data.frame(
  brand = c("Brand_A", "Brand_B", "Brand_C", "Brand_A", "Brand_B"),
  model = c("Model_X", "Model_Y", "Model_Z", "Model_X", "Model_Z"),
  value = c(10, 20, 15, 25, 30)  # This is just a placeholder, you should replace it with your actual data
)




# Create the treemap
treemap(
  df,
  index = c("brand", "model"),
  vSize = "value",
  algorithm = "pivotSize",
  title = "Treemap of Brands and Models",
  fontsize.labels = c(12, 10),
  fontsize.title = 16
)



br_md_tree <- df[,c("brand","model")]
head(br_md_tree,50)


br_md_tree$count <- 1

# Create the treemap
treemap(
  subset_brands,
  index = c("brand", "model"),
  vSize = "count",
  title = "Treemap of Brands and Models",
  fontsize.labels = c(10, 10),
  fontsize.title = 12
)


subset_brands <- subset(br_md_tree, brand %in% c("audi", "ford", "bmw", "hyundai", "kia", "fiat", "citroen", "dacia"))

head(subset_brands)

subset_brands$count <- 1

my_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f")

# Create the treemap with custom parameters
treemap(
  subset_brands,
  index = c("brand", "model"),
  vSize = "count",
  title = "Treemap of Selected Brands and Models",
  fontsize.labels = c(14, 10), # Font sizes for brand and model labels
  fontsize.labels.labels = 10, # Font size for count labels
  fontsize.title = 12,
  width = 10,  # Adjust the width as needed
  height = 8,  # Adjust the height as needed
  palette = my_colors,  # Use custom colors
  border.col = "pink",  # Add white borders
  fontfamily.labels = c("Arial", "Arial", "Arial"),  # Use Arial font for labels
  align.labels = list(
    c("center", "center"), # Align brand and model labels to center
    c("right", "top")  # Align count labels to top right
  ),
  overlap.labels = 0.5  # Allow label overlap for better visibility
)



treemap(
  subset_brands,
  index = c("brand", "model"),
  vSize = "count",
  title = "Treemap of Selected Brands and Models",
  fontsize.labels = c(12, 10), # Font sizes for brand and model labels
  fontsize.labels.labels = 7, # Font size for count labels
  fontsize.title = 16,
  width = 12,  # Adjust the width as needed
  height = 10,  # Adjust the height as needed
  palette = my_colors,  # Use custom colors
  border.col = "pink",  # Add white borders
  fontfamily.labels = c("Arial", "Arial", "Arial"),  # Use Arial font for labels
  align.labels = list(
    c("center", "center"), # Align brand and model labels to center
    c("right", "top")  # Align count labels to top right
  ),
  overlap.labels = 0.5  # Allow label overlap for better visibility
)


multiple_scatter_df <- df[,c("price","power_kw","power_ps","fuel_consumption_l_100km","fuel_consumption_g_km","milage","age")]
sum(is.na(multiple_scatter_df))

str(multiple_scatter_df)

library(bestNormalize)


norm_trans <- orderNorm(multiple_scatter_df$price)
print(norm_trans)

transformed_price<- predict(norm_trans, multiple_scatter_df$price)

library(nortest)
hist(transformed_price)
ad.test(transformed_price)

multiple_scatter_df$price <- transformed_price

library(ggplot2)
plot1 <- ggplot(multiple_scatter_df, aes(x = power_ps, y = price)) +
  geom_point(color="pink") +
  geom_smooth(method = "lm", se = FALSE,col="red",lwd=0.7) +
  labs(title = "Scatter Plot of Power of Engine in \nMetric hp vs Price")+
  xlab("Power hp")+
  ylab("Price")+
  theme_minimal()
plot1


plot2 <- ggplot(multiple_scatter_df, aes(x = power_kw, y = price,color="")) +
  geom_point(color="steelblue") +
  geom_smooth(method = "lm", se = FALSE,col="red",lwd=0.7) +
  labs(title = "Scatter Plot of Power of Engine in \nKilowatts vs Price")+
  xlab("Power kW")+
  ylab("Price")+
  theme_minimal()
plot2



plot3 <- ggplot(multiple_scatter_df, aes(x = fuel_consumption_l_100km, y = price)) +
  geom_point(color="darkgreen") +
  geom_smooth(method = "lm", se = FALSE,col="red",lwd=0.7) +
  labs(title = "Scatter Plot of fuel consumption of the \nvehicle in liters per 100 kilometers vs Price")+
  xlab("Fuel consumption of the vehicle in liters per 100 kilometers")+
  ylab("Price")+
  theme_minimal()
plot3


plot4 <- ggplot(multiple_scatter_df, aes(x = fuel_consumption_g_km, y = price )) +
  geom_point(color="purple") +
  geom_smooth(method = "lm", se = FALSE,col="red",lwd=0.7) +
  labs(title = "Scatter Plot of fuel consumption of the \nvehicle in grams per kilometer vs Price")+
  xlab("Fuel consumption of the vehicle in grams per kilometer")+
  ylab("Price")+
  theme_minimal()
plot4

filtered_mil <- filter(multiple_scatter_df,milage <= 400000)
options(scipen = 999)
plot5_alternative <- ggplot(filtered_mil, aes(x = milage, y = price)) +
  geom_point(color="grey") +
  geom_smooth(method = "lm", se = FALSE,col="steelblue",lwd=0.7) +
  labs(title = "Scatter Plot of Milage in Km vs Price")+
  xlab("Milage in Km")+
  ylab("Price")+
  theme_minimal()
plot5_alternative

plot6 <- ggplot(multiple_scatter_df,aes(x=age,y=price))+
  geom_point(color="red",position = "jitter",alpha=0.5)+
  geom_smooth(method = "lm", se = FALSE,col="steelblue",lwd=0.7) +
  labs(title = "Scatter Plot of Age vs Price")+
  xlab("Age")+
  ylab("Price")+
  theme_minimal()
plot6  




library(gridExtra)
grid.arrange(plot1, plot2, plot3,plot4,plot5_alternative,plot6, nrow = 3)







library(corrplot)
par(bg = "#F8F8FF")

cor_Data <- multiple_scatter_df[,-3]
corrs <- cor(cor_Data)
corrplot(corrs, method = 'number')
corrplot(corrs, method = 'color', type="lower") 

corrplot(corrs)
bubble_plot_df <- df[,c("price","milage","transmission")]
bubble_plot_df <- na.omit(bubble_plot_df)
bubble_plot_df$milage[bubble_plot_df$milage== 0] <- 1
library(ggplot2)




library(bestNormalize)
bestNormalize(bubble_plot_df$price)

norm_trans <- orderNorm(bubble_plot_df$price)

# View the transformation object
print(norm_trans)

# Transform the data
transformed_price_for_bubble_plot <- predict(norm_trans, bubble_plot_df$price)

hist(transformed_price_for_bubble_plot)
ad.test(transformed_price_for_bubble_plot)
bubble_plot_df$price <- transformed_price_for_bubble_plot


buble_plot_milage_limited <- subset(bubble_plot_df, milage <=400000)

head(buble_plot_milage_limited)

options(scipen=999)
bubble_plot2 <- ggplot(buble_plot_milage_limited, aes(x= (milage), y=price,col=transmission))+
  geom_point(alpha= 0.2)+
  geom_smooth(method = "loess", se = FALSE,col="pink",lwd=0.4)+
  xlab("Milage")+
  ylab("Price")+
  labs(title = "Bubble Plot of Milage vs\nOrder Normed Price by Transmission Type")+
  theme_minimal()
bubble_plot2

bubble_plot_df$price_milage <- bubble_plot_df$price*bubble_plot_df$milage 

bubble_plot_df$transmission <- relevel(bubble_plot_df$transmission, ref = "Unknown")
model_for_bubble <- lm(price~milage+price_milage+transmission,data=bubble_plot_df)
options(scipen=10)

summary(model_for_bubble)



library(lmtest)
dwtest(model_for_bubble)

bptest(model_for_bubble)
qqnorm(residuals(model_for_bubble))
qqline(residuals(model_for_bubble))

library(RColorBrewer)

# Generate a list of 5 colors using a palette from RColorBrewer
colors <- brewer.pal(4, "Set1")
colors

bp_df <- na.omit(bp_df)


ggplot(bp_df, aes(x = transmission, y = price)) +
  geom_boxplot(color=colors) +
  labs(x = "Transmission Type", y = "Price") +
  ggtitle("Boxplot of Prices by Transmission Type") +
  facet_wrap(~transmission, scales = "free_x")+
  theme_minimal()


str(df)


ggplot(df, aes(x = year)) +
  geom_bar(fill="steelblue") +
  labs(x = "Year", y = "Count") +
  ggtitle("Barplot of Years")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


library(lattice)
library(mosaic)

histogram(~power_kw|transmission,data=df,type="count",
          xlab="Height of Students in inches", ylab="Frequency",
          main="Histogram of Height",breaks = "Scott") 

semi_auto <- (subset(df,transmission== "Semi-automatic"))
head(semi_auto)
max(na.omit(semi_auto$power_kw))

ggplot(df, aes(x = power_ps)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "black") +
  labs(x = "Power PS", y = "Frequency") +
  ggtitle("Histogram of Power PS by Transmission Type") +
  facet_wrap(~transmission, nrow = 1)

summary(df$power_kw)

summary(df)

str(df)

head(df,5)

table(df$transmission)
table(df$fuel)
library(vcd)


filtered_df <- df[df$fuel %in% c("Diesel", "Petrol", "Hybrid", "Electric", "LPG"), ]
table(filtered_df$fuel)

tab <- table(filtered_df$transmission, filtered_df$fuel)

tab <- tab[, colSums(tab) > 0]  # Remove columns with all zeros
tab <- tab[rowSums(tab) > 0, ]

# Create the mosaic plot
mosaic(tab, main = "Mosaic Plot of Transmission Type and Fuel Type",))
tab



# Create the data table
data <- matrix(c(23052, 2280, 3377, 318, 21884,
                 12815, 54, 709, 567, 29950),
               nrow = 2, byrow = TRUE,
               dimnames = list(c("Automatic", "Manual"),
                               c("D", "E", "H", "LPG", "P")))

# Convert to table
data_table <- as.table(data)

# Create mosaic plot with custom colors
mosaic(data_table, main = " Spine Plot of Fuel Type and Transmission Type",
       shade=TRUE,legend=FALSE)

?mosaic


df$brand
library(dplyr)


table(df$fuel)

filtered_df$fuel <- as.character(filtered_df$fuel)
filtered_df$fuel <- as.factor(filtered_df$fuel)
table(filtered_df$fuel)

ftp_bp <- filtered_df[,c("fuel","power_kw")]


library(RColorBrewer)

# Generate a list of 5 colors using a palette from RColorBrewer
colors <- brewer.pal(5, "Set1")
colors

ftp_bp <- na.omit(ftp_bp)


ggplot(ftp_bp, aes(x = fuel, y = power_kw)) +
  geom_boxplot(color=colors) +
  labs(x = "Fuel Type", y = "Power KW") +
  ggtitle("Boxplot of Power KW by Fuel Type") +
  facet_wrap(~fuel, scales = "free")+
  theme_minimal()

table(df$year)
df$year <- as.character(df$year)
df$year <- as.numeric(df$year)

df <- mutate(df,age= 2023-year )
df$year <- as.factor(df$year)
df$age <- as.numeric(df$age)

table(df$transmission)
df$transmission <- as.character(df$transmission)

transmission_filter_df <- filter(df,transmission== c("Automatic","Manual"))
table(transmission_filter_df$transmission)

ttfc <- transmission_filter_df[,c("transmission","fuel_consumption_l_100km")]
ttfc <- na.omit(ttfc)
head(ttfc)
ttfc$transmission <- as.factor(ttfc$transmission)
str(ttfc)


ggplot(ttfc, aes(x = transmission, y = fuel_consumption_l_100km)) +
  geom_boxplot(color=c("blue","red")) +
  labs(x = "Transmission Type", y = "Fuel Consumption") +
  ggtitle("Boxplot of Fuel Consumption by Transmission Type") +
  facet_wrap(~transmission, scales = "free")+
  theme_minimal()



df$transmission <- as.factor(df$transmission)


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

(100000-99875)/100000

colSums(is.na(df))/nrow(df)

audi <- filter(df,brand=="audi")
table(audi$model)
audi_f6 <- tail(sort(table(audi$model)))

bmw <- filter(df,brand=="bmw")
table(bmw$model)
bmw_f6 <- tail(sort(table(bmw$model)))

ford <- filter(df,brand=="ford")
ford_f6 <- tail(sort(table(ford$model)))


top_3_cars <- matrix(c(audi_f6,bmw_f6,ford_f6),nrow = 3,byrow = T,dimnames = list(c("audi","bmw","ford"),
                                                                        c(1,2,3,4,5,6)))

top_3_cars
?matrix

chi_test_result <- chisq.test(top_3_cars)
print(chi_test_result)

bubble_df <- df[,c("price","milage","transmission")]

bubble_df$milage[bubble_df$milage==0] <- 1
min(bubble_df$price)
min(bubble_df$milage)


bubble_df$transmission <- relevel(bubble_df$transmission, ref = "Unknown")
bubble_model <- lm(log(price)~log(milage)+transmission,data=bubble_df)
summary(bubble_model)

ad.test(df$price)


# Shapiro-Wilk test for normality for each transmission type
library(dplyr)
df %>%
  group_by(transmission) %>%
  summarise(p_value = ad.test(price)$p.value)

# Alternatively, visualize using Q-Q plots
par(mfrow = c(2, 2)) # Layout for 4 plots
for (trans in unique(df$transmission)) {
  qqnorm(df$price[df$transmission == trans], main = trans)
  qqline(df$price[df$transmission == trans])
}



library(nortest)
atest <- ad.test(df$price)
atest$p.value

library(car)
leveneTest(price ~ transmission, data = df)


kruskal.test(price ~ transmission, data = df)

install.packages("dunn.test")
library(dunn.test)
dunn.test(df$price, df$transmission, method = "bonferroni")


df_for_auto <- df
df_for_auto$transmission <- as.character(df_for_auto$transmission)
df_for_auto$fuel <- as.character(df_for_auto$fuel)


auto_df <- df_for_auto %>%
  filter(transmission == "Automatic" & fuel %in% c("Diesel", "Petrol", "Hybrid", "Electric", "LPG"))
table(auto_df$transmission)
auto_fuel <- table(auto_df$fuel)

manual_df <- df_for_auto %>% 
  filter(transmission== "Manual"& fuel %in% c("Diesel", "Petrol", "Hybrid", "Electric", "LPG"))
manuel_fuel <- table(manual_df$fuel)


trans_matrix <- matrix(c(auto_fuel,manuel_fuel),byrow = T,nrow=2,dimnames = list(c("Auto","Manuel"),c("Diesel","Electric","Hybrid","LPG","Petrol")))
trans_matrix


chi_test_result <- chisq.test(trans_matrix)
print(chi_test_result)

str(df)


fuel_power_df <- df
fuel_power_df$fuel <- as.character(fuel_power_df$fuel)


fp_df <- fuel_power_df %>%
  filter(fuel %in% c("Diesel", "Petrol", "Hybrid", "Electric", "LPG"))

fp_df$fuel <- as.factor(fp_df$fuel)
table(fp_df$fuel)

library(dplyr)
fp_df %>%
  group_by(fuel) %>%
  summarise(p_value = ad.test(power_kw)$p.value)

# Alternatively, visualize using Q-Q plots
par(mfrow = c(3, 2)) # Layout for 4 plots
for (trans in unique(fp_df$fuel)) {
  qqnorm(fp_df$power_kw[fp_df$fuel == trans], main = trans)
  qqline(fp_df$power_kw[fp_df$fuel == trans])
}

kruskal_result <- kruskal.test(power_kw ~ fuel, data = fp_df)
print(kruskal_result)

library(dunn.test)
dunn.test(fp_df$power_kw, fp_df$fuel, method = "bonferroni")


fuelcons_trans <- df[,c("transmission","fuel_consumption_l_100km")]

fuelcons_trans$transmission <- as.character(fuelcons_trans$transmission)

fct <- fuelcons_trans %>% 
  filter(transmission %in% c("Automatic","Manual"))
fct

fct$transmission <- as.factor(fct$transmission)

str(fct)



# Check normality assumption
# Shapiro-Wilk test for normality
ad.test(fct$fuel_consumption_l_100km[fct$transmission == "Manual"])
ad.test(fct$fuel_consumption_l_100km[fct$transmission == "Automatic"])

# Visual inspection (optional)
# Example: Histograms
hist(fct$fuel_consumption_l_100km[fct$transmission == "Manual"], main = "Manual Transmission", xlab = "Fuel Consumption",breaks = "Scott")
hist(fct$fuel_consumption_l_100km[fct$transmission == "Automatic"], main = "Automatic Transmission", xlab = "Fuel Consumption",breaks = "Scott")

# Check homogeneity of variance assumption
# Levene's test for homogeneity of variance
library(car)
leveneTest(fuel_consumption_l_100km ~ transmission, data = fct)
# Visual inspection (optional)
# Example: Boxplots
boxplot(fuel_consumption_l_100km ~ transmission, data = fct, main = "Fuel Consumption by Transmission Type")


t_test_result <- t.test(fuel_consumption_l_100km ~ transmission, data = fct)

result <- wilcox.test(fct$fuel_consumption_l_100km[fct$transmission == "Manual"],
                      fct$fuel_consumption_l_100km[fct$transmission == "Automatic"])

result
# Step 5: Interpret the Results
# Print the results
print(t_test_result)

# Check the p-value to determine statistical significance
if (t_test_result$p.value < 0.05) {
  print("There is a significant difference in fuel consumption between manual and automatic transmissions.")
} else {
  print("There is no significant difference in fuel consumption between manual and automatic transmissions.")
}



###########################
library(caret)

cor_data <- df[,c("price","milage","age","fuel_consumption_l_100km","fuel_consumption_g_km","power_kw")]

corrs <- cor(cor_data)
library(corrplot)
corrplot(corrs,method = "number")

library(caret)
set.seed(412)

df <- read.csv("df.csv")

str(df)
num_df <- df[,c("price","power_kw","fuel_consumption_l_100km","fuel_consumption_g_km",
                "milage","age")]


scale_variables <- function(dataframe, variable_names) {
  # Iterate over each variable name
  for (variable_name in variable_names) {
    # Extract the variable
    variable <- dataframe[[variable_name]]
    
    # Compute the minimum and maximum of the variable
    min_value <- min(variable)
    max_value <- max(variable)
    
    # Perform min-max scaling
    scaled_variable <- (variable - min_value) / (max_value - min_value)
    
    # Replace the variable in the dataframe with the scaled values
    dataframe[[variable_name]] <- scaled_variable
  }
  
  # Return the dataframe with scaled variables
  return(dataframe)
}


colnames(num_df)

scaled_df <- scale_variables(num_df, c("price","power_kw","fuel_consumption_l_100km",
                                       "fuel_consumption_g_km","milage","age"))
scaled_df


scaled_df$transmission <- df$transmission

scaled_df$transmission <- as.factor(scaled_df$transmission)

str(scaled_df)
library(dplyr)
library(caret)
train_ind = scaled_df$price %>%
  createDataPartition(p = 0.8, list = FALSE) #createDataPartition helps you define train set index #ratio: 80% and 20%
train  = scaled_df[train_ind, ]
test = scaled_df[-train_ind, ]


d_original=dim(scaled_df)
d_train=dim(train)
d_test=dim(test)
dimens=cbind(d_original,d_train,d_test)
rownames(dimens)=c("number of rows","number of columns")
dimens

str(train)

library(bestNormalize)

norm_trans <- orderNorm(train$price)

# View the transformation object
print(norm_trans)

# Transform the datalibrary()
transformed_data <- predict(norm_trans, train$price)
library(nortest)
hist(transformed_data)
ad.test(transformed_data)

train$price <- transformed_data
hist(train$price)
ad.test(train$price)
train$transmission <- relevel(train$transmission, ref = "Unknown")

str(train)



model <- lm(price~., data=train)
summary(model)

library(car)
vif(model)

durbinWatsonTest(model)



ad.test(residuals(model))

library(lmtest)
bptest(model)

str(train)

# Assuming your data is in a dataframe called `data` and the model is `lm_model`
library(car)  # for influencePlot
library(dplyr)

# Fit the model
lm_model <- lm(price ~ ., data = train)
summary(lm_model)
# Calculate leverage, Cook's distance, and studentized residuals
leverage <- hatvalues(lm_model)
cooks_distance <- cooks.distance(lm_model)
studentized_residuals <- rstudent(lm_model)

# Define thresholds for identifying outliers/influential points
leverage_threshold <- 2 * (ncol(train) + 1) / nrow(train)
cooks_threshold <- 4 / (nrow(train) - ncol(train) - 1)
residual_threshold <- 2  # Common threshold for studentized residuals

# Identify outliers/influential points
outliers <- which(leverage > leverage_threshold | cooks_distance > cooks_threshold | abs(studentized_residuals) > residual_threshold)

# Remove outliers/influential points from the data
cleaned_data <- train[-outliers, ]

# Refit the model with cleaned data
lm_model_cleaned <- lm(price ~ ., data = cleaned_data)

# Check the summary of the new model
summary(lm_model_cleaned)
table(cleaned_data$transmission)

car::vif(lm_model_cleaned)

vif(lm_model)

durbinWatsonTest(lm_model_cleaned)
ad.test(residuals(lm_model_cleaned))
par(mfrow=c(2,2))
plot(lm_model_cleaned)

library(lmtest)
bptest(model)


attach(train)
colnames(train)
plot(price,age)

detach(train)

# Apply transformations
# Apply transformations with checks for non-finite values
train$log_power_kw <- ifelse(is.finite(log(train$power_kw)), log(train$power_kw), NA)
train$log_fuel_consumption_g_km <- ifelse(is.finite(log(train$fuel_consumption_g_km)), log(train$fuel_consumption_g_km), NA)
train$log_fuel_consumption_l_100km <- ifelse(is.finite(log(train$fuel_consumption_l_100km)), log(train$fuel_consumption_l_100km), NA)
train$log_mileage <- ifelse(is.finite(log(train$milage + 1)), log(train$milage + 1), NA)
train$age_squared <- ifelse(is.finite(train$age^2), train$age^2, NA)


# Fit the model with transformed variables
model_transformed <- lm(price~log_power_kw + log_fuel_consumption_g_km + 
                          log_fuel_consumption_l_100km + log_mileage + age_squared+transmission, data = train)


summary(model_transformed)

plot(model_transformed)

str(train)
# Check the model diagnostics
par(mfrow = c(2, 2))
plot(model_transformed)

# Summary of the transformed model
summary(model_transformed)
library(dplyr)
table(test$transmission)
# Assuming your dataset is named 'df'
# Filter the dataset based on the "transmission" column
filtered_test <- subset(test, transmission %in% c("Manual", "Automatic"))
filtered_train <- subset(train, transmission %in% c("Manual", "Automatic"))

pred = lm_model_cleaned %>%  predict(filtered_test)
pred  #see predictions

pred2 <- lm_model_cleaned %>%  predict(filtered_train)
pred2

trainPred <- predict(norm_trans,pred2,inverse = TRUE)

testPred <- predict(norm_trans, pred, inverse = TRUE)

# Assuming your actual values are in filtered_train$actual and filtered_test$actual

# Actual values
actual_train <- filtered_train$price
actual_test <- filtered_test$price

# RMSE
rmse_train <- sqrt(mean((actual_train - trainPred)^2))
rmse_test <- sqrt(mean((actual_test - testPred)^2))

# MAE
mae_train <- mean(abs(actual_train - trainPred))
mae_test <- mean(abs(actual_test - testPred))

# MAPE
mape_train <- mean(abs((actual_train - trainPred) / actual_train)) 
mape_test <- mean(abs((actual_test - testPred) / actual_test)) 

# MPE
mpe_train <- mean((actual_train - trainPred) / actual_train) 
mpe_test <- mean((actual_test - testPred) / actual_test) 

# Print results
cat("Train RMSE: ", rmse_train, "\n")
cat("Test RMSE: ", rmse_test, "\n")
cat("Train MAE: ", mae_train, "\n")
cat("Test MAE: ", mae_test, "\n")
cat("Train MAPE: ", mape_train, "%\n")
cat("Test MAPE: ", mape_test, "%\n")
cat("Train MPE: ", mpe_train, "%\n")
cat("Test MPE: ", mpe_test, "%\n")

###### Machine Learning Method

data <- read.csv("df.csv")
data <- data[,c("price","power_kw","power_ps","fuel_consumption_l_100km","fuel_consumption_g_km",
                "milage","age","transmission")]


library(caret)
library(dplyr)


dummies <- dummyVars(~ transmission, data = data)
transmission_dummies <- predict(dummies, newdata = data)




transmission_dummies <- as.data.frame(transmission_dummies)
data <- cbind(data %>% select(-transmission), transmission_dummies)

# Display the first few rows of the modified data
head(data)

# View the current names of the new columns


colnames(data)


colnames(data)[10] <- "tr_semi_auto"

colnames(data)








scale_variables <- function(dataframe, variable_names) {
  # Iterate over each variable name
  for (variable_name in variable_names) {
    # Extract the variable
    variable <- dataframe[[variable_name]]
    
    # Compute the minimum and maximum of the variable
    min_value <- min(variable)
    max_value <- max(variable)
    
    # Perform min-max scaling
    scaled_variable <- (variable - min_value) / (max_value - min_value)
    
    # Replace the variable in the dataframe with the scaled values
    dataframe[[variable_name]] <- scaled_variable
  }
  
  # Return the dataframe with scaled variables
  return(dataframe)
}




scaled_df <- scale_variables(data, c("price","power_kw","power_ps","fuel_consumption_l_100km",
                                             "fuel_consumption_g_km","milage","age","transmissionAutomatic",
                                             "transmissionManual","tr_semi_auto","transmissionUnknown"))
scaled_df


library(caret)
library(nnet)
library(MLmetrics)

# Load your data
# Assuming your data is in a data frame called `data`
# and the target variable is called `price`
# Split the data into train and test sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(scaled_df$price, p = .8, 
                                  list = FALSE, 
                                  times = 1)
trainData <- scaled_df[trainIndex, ]
testData <- scaled_df[-trainIndex, ]

# Separate features and target variable
trainX <- trainData[, -which(names(trainData) == "price")]
trainY <- trainData$price
testX <- testData[, -which(names(testData) == "price")]
testY <- testData$price

trainData <- cbind(trainX, price = trainY)
testData <- cbind(testX, price = testY)

install.packages("NeuralNetTools")
library(NeuralNetTools)
set.seed(123)
nnModel <- neuralnet(price ~ ., data = trainData, linear.output = TRUE,stepmax = 1e6)

nnModel$weights


dim(trainData)
plot(nnModel,rep="best",
     show.weights = TRUE, 
     information = FALSE,
     col.hidden = 'black',
     col.intercept = 'black',
     col.hidden.synapse = 'blue',
     col.synapse = 'red',
     col.entry = 'black',
     col.entry.synapse = 'lightblue',
     fill="lightblue")

tuneGrid <- expand.grid(size = c(1, 3, 5),
                        decay = c(0.001, 0.01, 0.1))

# Train the model with hyperparameter tuning
set.seed(123)
tunedNNModel <- train(trainX, trainY,
                      method = "nnet",
                      linout = TRUE,
                      trace = FALSE,
                      tuneGrid = tuneGrid,
                      trControl = trainControl(method = "cv", number = 10))

# Check the best tuned parameters
bestParams <- tunedNNModel$bestTune
print(bestParams)

bestModel <- tunedNNModel$finalModel

extract_weights <- function(model) {
  weights <- model$wts
  n <- model$n
  nlayer <- length(n) - 1
  index <- 0
  for (i in 1:nlayer) {
    cat("Layer", i, "weights:\n")
    for (j in 1:n[i]) {
      for (k in 1:n[i + 1]) {
        index <- index + 1
        cat("From", j, "to", k, ":", weights[index], "\n")
      }
    }
  }
}

# Extract and print the weights
(extract_weights(bestModel))


nnModel_tuned <- neuralnet(price ~ ., data = trainData, linear.output = TRUE,stepmax = 1e6,
                           hidden=3,learningrate = 0.001)
nnModel_tuned$weights
summary(nnModel_tuned)
par(mar = c(5, 8, 4, 2) + 0.1, oma = c(1, 1, 1, 1))
plot(nnModel_tuned, rep = "best",
     show.weights = FALSE,
     information = FALSE,
     fill = 'lightblue',  # Fill color for nodes
     circle_col = 'lightblue', # Color for nodes
     arrow_col = 'blue',  # Color for arrows
     intercept_col = 'darkred', # Color for intercepts
     line_stag = 0.5,     # Stagger lines to reduce overlap
     cex = 0.7,           # Size of node labels
     cex.axis = 0.7,      # Size of axis labels
     cex.lab = 0.7,       # Size of variable labels
     fontsize = 16,       # Font size
     col.edge = "black",  # Edge color for arrows
     pos.col = 'blue',    # Color for positive weights
     neg.col = 'red',     # Color for negative weights
     alpha = 0.7)






# Make predictions on the training set
trainPred <- predict(tunedNNModel, trainX)

# Make predictions on the test set
testPred <- predict(tunedNNModel, testX)

MPE <- function(pred, actual) {
  mean((pred - actual) / actual) 
}


# Calculate RMSE, MAPE, MAE, and MPE for train set
trainRMSE <- RMSE(trainPred, trainY)
trainMAPE <- MAPE(trainPred, trainY)
trainMAE <- MAE(trainPred, trainY)
trainMPE <- MPE(trainPred, trainY)

# Calculate RMSE, MAPE, MAE, and MPE for test set
testRMSE <- RMSE(testPred, testY)
testMAPE <- MAPE(testPred, testY)
testMAE <- MAE(testPred, testY)
testMPE <- MPE(testPred, testY)

# Print performance metrics
cat("Training Set Performance Metrics:\n")
cat("RMSE: ", trainRMSE, "\n")
cat("MAPE: ", trainMAPE, "\n")
cat("MAE: ", trainMAE, "\n")
cat("MPE: ", trainMPE, "\n\n")

cat("Test Set Performance Metrics:\n")
cat("RMSE: ", testRMSE, "\n")
cat("MAPE: ", testMAPE, "\n")
cat("MAE: ", testMAE, "\n")
cat("MPE: ", testMPE, "\n")


####SVM


library(caret)
library(e1071)
library(MLmetrics)

model <- svm(price ~ ., data = trainData)
print(model)



set.seed(123)
svmModel <- train(trainX, trainY,
                  method = "svmRadial",
                  trControl = trainControl(method = "cv", number = 10),
                  tuneLength = 10)
svmModel


# Define the grid of hyperparameters
tuneGrid <- expand.grid(sigma = c(0.01, 0.05, 0.1, 0.5),
                        C = c(0.5, 1, 2, 5, 10))

# Train the SVM model with hyperparameter tuning
set.seed(123)
tunedSVMModel <- train(trainX, trainY,
                       method = "svmRadial",
                       trControl = trainControl(method = "cv", number = 10),
                       tuneGrid = tuneGrid)
# Check the best tuned parameters
bestParams <- tunedSVMModel$bestTune
print(bestParams)

# Make predictions on the training set
trainPred <- predict(tunedSVMModel, trainX)

# Make predictions on the test set
testPred <- predict(tunedSVMModel, testX)


# Define Mean Percentage Error (MPE) function

# Calculate RMSE, MAPE, MAE, and MPE for train set
trainRMSE <- RMSE(trainPred, trainY)
trainMAPE <- MAPE(trainPred, trainY)
trainMAE <- MAE(trainPred, trainY)
trainMPE <- MPE(trainPred, trainY)

# Calculate RMSE, MAPE, MAE, and MPE for test set
testRMSE <- RMSE(testPred, testY)
testMAPE <- MAPE(testPred, testY)
testMAE <- MAE(testPred, testY)
testMPE <- MPE(testPred, testY)

# Print performance metrics
cat("Training Set Performance Metrics:\n")
cat("RMSE: ", trainRMSE, "\n")
cat("MAPE: ", trainMAPE, "\n")
cat("MAE: ", trainMAE, "\n")
cat("MPE: ", trainMPE, "\n\n")

cat("Test Set Performance Metrics:\n")
cat("RMSE: ", testRMSE, "\n")
cat("MAPE: ", testMAPE, "\n")
cat("MAE: ", testMAE, "\n")
cat("MPE: ", testMPE, "\n")

## Random Forest

library(randomForest)

rf1<-randomForest(price~.,data=trainData)
rf1
plot(rf1)

rf2 <- randomForest(price~.,data=trainData,ntree=600)
rf2
options(scipen = 999)
plot(rf2,main = "Number of Trees vs Error")

rf3 <- randomForest(price~.,data=trainData,ntree=450)
rf3
plot(rf3)

which.min(rf1$mse)


oob.err <- double(10)
test.err <- double(10)

#mtry is no of Variables randomly chosen at each split

for(mtry in 1:10) {
  rf=randomForest(price~ . , data = trainData ,mtry=mtry,ntree=450) 
  oob.err[mtry] = rf$mse[450] #Error of all Trees fitted
  
  pred<-predict(rf,testData[-11,]) #Predictions on Test Set for each Tree
  test.err[mtry]= mean((testData[,11] - pred)^2) #Mean Squared Test Error
  
  cat(mtry," ") #printing the output to the console
  
}

oob.err
test.err

matplot(1:mtry , cbind(oob.err,test.err), pch=20 , col=c("red","orange"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","orange"),cex=0.4)


optimal_mtry <- 5

final_rfModel <- randomForest(price ~ ., data = trainData, mtry = optimal_mtry, ntree = 450)

(final_rfModel)
plot(final_rfModel)


parameters<-tuneRF(trainData[,-4],trainData[,4])

final_rfModel <- randomForest(price ~ ., data = trainData, mtry = 3, ntree = 450)
final_rfModel


trainPred <- predict(final_rfModel, trainX)

# Make predictions on the test set
testPred <- predict(final_rfModel, testX)

# Calculate Performance Metrics
trainRMSE <- RMSE(trainPred, trainY)
trainMAPE <- MAPE(trainPred, trainY)
trainMAE <- MAE(trainPred, trainY)
trainMPE <- MPE(trainPred, trainY)

testRMSE <- RMSE(testPred, testY)
testMAPE <- MAPE(testPred, testY)
testMAE <- MAE(testPred, testY)
testMPE <- MPE(testPred, testY)

# Print performance metrics
cat("Training Set Performance Metrics:\n")
cat("RMSE: ", trainRMSE, "\n")
cat("MAPE: ", trainMAPE, "\n")
cat("MAE: ", trainMAE, "\n")
cat("MPE: ", trainMPE, "\n\n")

cat("Test Set Performance Metrics:\n")
cat("RMSE: ", testRMSE, "\n")
cat("MAPE: ", testMAPE, "\n")
cat("MAE: ", testMAE, "\n")
cat("MPE: ", testMPE, "\n")

varImpPlot(final_rfModel)


#######XGBOOST
# Define the grid of hyperparameters
tuneGrid <- expand.grid(nrounds = c(50,100,150),
                        max_depth = c(1,2,3),
                        eta = c(0.01, 0.1),
                        gamma = c(0, 0.1),
                        colsample_bytree = c(0.6,0.8),
                        min_child_weight = c(1, 5),
                        subsample = c(0.5, 0.75, 1.0))

# Train the XGBoost model with hyperparameter tuning
set.seed(123)
tunedXGBModel <- train(trainX, trainY,
                       method = "xgbTree",
                       trControl = trainControl(method = "cv", number = 5),
                       tuneGrid = tuneGrid)

tunedXGBModel$bestTune


# Check the best tuned parameters
bestParams <- tunedXGBModel$bestTune
print(bestParams)

# Make predictions on the training set
trainPred <- predict(tunedXGBModel, trainX)

# Make predictions on the test set
testPred <- predict(tunedXGBModel, testX)



# Calculate RMSE, MAPE, MAE, and MPE for train set
trainRMSE <- RMSE(trainPred, trainY)
trainMAPE <- MAPE(trainPred, trainY)
trainMAE <- MAE(trainPred, trainY)
trainMPE <- MPE(trainPred, trainY)

# Calculate RMSE, MAPE, MAE, and MPE for test set
testRMSE <- RMSE(testPred, testY)
testMAPE <- MAPE(testPred, testY)
testMAE <- MAE(testPred, testY)
testMPE <- MPE(testPred, testY)

# Print performance metrics
cat("Training Set Performance Metrics:\n")
cat("RMSE: ", trainRMSE, "\n")
cat("MAPE: ", trainMAPE, "\n")
cat("MAE: ", trainMAE, "\n")
cat("MPE: ", trainMPE, "\n\n")

cat("Test Set Performance Metrics:\n")
cat("RMSE: ", testRMSE, "\n")
cat("MAPE: ", testMAPE, "\n")
cat("MAE: ", testMAE, "\n")
cat("MPE: ", testMPE, "\n")



# Load necessary libraries
library(randomForest)
library(xgboost)
library(e1071)
library(NeuralNetTools)
library(caret)

# Assuming models have been trained as final_rfModel, tunedXGBModel, tunedSVMModel, final_nnModel

# Random Forest Variable Importance
varImpPlot(final_rfModel, main = "Variable Importance - Random Forest")

# XGBoost Variable Importance
importance_matrix <- xgb.importance(feature_names = colnames(trainX), model = tunedXGBModel$finalModel)
xgb.plot.importance(importance_matrix, main = "Variable Importance - XGBoost")


# SVM Variable Importance
svmVarImp <- varImp(tunedSVMModel, scale = FALSE)
plot(svmVarImp, main = "Variable Importance - SVM")

# Neural Network Variable Importance
nn_importance <- olden(tunedNNModel$finalModel)
olden(tunedNNModel$finalModel, main = "Variable Importance - Neural Network")





