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
