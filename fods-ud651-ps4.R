# Install packages
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("lubridate")

# Load relevant libraries
library(data.table)
library(plyr)
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(gridExtra)
library(lubridate)
library(reshape2)
library(tidyr)
library(xlsx)

# Load diamonds data set, and initial analysis
str(diamonds)
head(diamonds)
colnames(diamonds)
summary(diamonds)
View(diamonds)

# Problem set 4
# Scatterplot - price vs x
ggplot(data = diamonds, aes(x = price, y = x)) + 
  geom_point() +
  xlab("Price") +
  ylab("X") +
  ggtitle("Price vs X")

# Correlations - price vs. x, y, z
with(diamonds, cor.test(price, x, method = "pearson"))
with(diamonds, cor.test(price, y, method = "pearson"))
with(diamonds, cor.test(price, z, method = "pearson"))

# Scatterplot - price vs depth
ggplot(data = diamonds, aes(x = price, y = depth)) + 
  geom_point() +
  xlab("Price") +
  ylab("Depth") +
  ggtitle("Price vs Depth")

# Scatterplot - price vs depth adjustments
# Get range of price
range(diamonds$depth)
# Plot
ggplot(data = diamonds, aes(x = depth, y = price)) + 
  geom_point(alpha = 1/100) +
  scale_x_continuous(breaks = seq(43, 79, 2)) +
  xlab("Depth") +
  ylab("Price") +
  ggtitle("Price vs Depth")
# Correlation of price & depth
with(diamonds, cor.test(price, depth, method = "pearson"))

# Scatterplot - price vs carat
# Get range of price
range(diamonds$carat)
# Plot
ggplot(data = diamonds, aes(x = carat, y = price)) + 
  geom_point(stat = "summary", fun.y = quantile, fun.args = list(probs = .99)) +
  scale_x_continuous(breaks = seq(0.2, 5.01, 0.5)) +
  xlab("Carat") +
  ylab("Price") +
  ggtitle("Price vs Carat")

# Scatterplot - price vs volume
# Create volume variable
diamonds$volume <- diamonds$x * diamonds$y * diamonds$z
range(diamonds$volume)
# Scatterplot
ggplot(data = diamonds, aes(x = volume, y = price)) + 
  geom_point() +
  scale_x_continuous(breaks = seq(0, 3841, 50)) +
  xlab("Volume") +
  ylab("Price") +
  ggtitle("Price vs Volume")

# Scatterplot - removing outliers through subset
# Create subsetted data set
diamondsReduced <- subset(diamonds, volume > 0 & volume <= 800)

# Correlation of this
with(diamondsReduced, cor.test(price, volume, method = "pearson"))

# Adjustments
ggplot(data = diamondsReduced, aes(x = volume, y = price)) + 
  geom_point(alpha = 1/10) +
  geom_smooth(method = "lm", color = "red") + 
  scale_x_continuous(breaks = seq(0, 3841, 50)) +
  xlab("Volume") +
  ylab("Price") +
  ggtitle("Price vs Volume")

# Mean price by clarity
# Creation of the new df
diamondsByClarity <- diamonds %>% 
  group_by(clarity) %>% 
  summarise(mean_price = mean(price),
            median_price = median(price),
            min_price = min(price),
            max_price = max(price),
            n = n()) %>% 
  arrange(clarity)

# Bar charts of mean price
g1 <- ggplot(data = diamonds_mp_by_clarity, aes(x = clarity, y = mean_price)) + 
  geom_bar(stat="identity")
g2 <- ggplot(data = diamonds_mp_by_color, aes(x = color, y = mean_price)) + 
  geom_bar(stat="identity")
grid.arrange(g1, g2, ncol = 1)

# Looking at mean price/cut relationship
diamonds_by_cut <- group_by(diamonds, cut)
diamonds_mp_by_cut <- summarise(diamonds_by_cut, mean_price = mean(price))
g3 <- ggplot(data = diamonds_mp_by_cut, aes(x = cut, y = mean_price)) + 
  geom_bar(stat="identity")
grid.arrange(g1, g2, g3, ncol = 1)

# Gapminder revisited
# Link between mobile phone ownership & income per person/GDP per capita?
# Go to right directory
setwd("D:\\Foundations of Data Science\\Projects\\EDA - Facebook\\fods-ud561-ps4")
list.files()
# Import data
cell <- read.csv("cell phone per 100.csv", 1, stringsAsFactors = FALSE)
gdp <- read.csv("GDPpercapitaconstant2000US.csv", 1, stringsAsFactors = FALSE)

# Check data
str(cell)
head(cell)
View(cell)
str(gdp)
head(gdp)
View(gdp)

# Clean up cell data
names(cell)[1] <- c("country")
cell <- cell %>% gather(year, subs, -country)
cell$year <- gsub("X", "", cell$year)

# Clean up gdp data
names(gdp)[1] <- c("country")
gdp <- gdp %>% gather(year, gdp, -country)
gdp$year <- gsub("X", "", gdp$year)

# Subset to a matching timeframe - 1981 to 2011
cell <- subset(cell, year >= 1981 & year <= 2011)
gdp <- subset(gdp, year >= 1981 & year <= 2011)

# Merge data sets
merged <- merge(cell, gdp, all = TRUE)

# Clean NAs
merged <- filter(merged, !is.na(merged$subs))
merged <- filter(merged, !is.na(merged$gdp))

# Check ranges
summary(merged$subs)
summary(merged$gdp)

# Scatterplot using alpha & jitter
ggplot(aes(x = gdp, y = subs), data = merged) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GDP vs Cell Phones",
       x = "GDP per Capita",
       y = "Cell Phones per 100 People")

# Look at correlation
cor.test(merged$gdp, merged$subs, method = "pearson")

# Look @ 95th percentile of GDP
ggplot(aes(x = gdp, y = subs), data = merged) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  xlim(0, quantile(merged$gdp, 0.95)) + 
  geom_smooth(method = "lm", color = "red")

# Look @ 95th percentile of subs
ggplot(aes(x = gdp, y = subs), data = merged) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  ylim(0, quantile(merged$subs, 0.95)) +
  geom_smooth(method = "lm", color = "red")

# Look @ 95th percentile for both
ggplot(aes(x = gdp, y = subs), data = merged) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  xlim(0, quantile(merged$gdp, 0.95)) + 
  ylim(0, quantile(merged$subs, 0.95)) +
  geom_smooth(method = "lm", color = "red")

# Correlation of 95th percentiles
# GDP
merged_gdp95th <- subset(merged, gdp <= quantile(gdp, 0.95))
cor.test(merged_gdp95th$gdp, merged_gdp95th$subs, method = "pearson")

# Subs
merged_subs95th <- subset(merged, subs <= quantile(subs, 0.95))
cor.test(merged_subs95th$gdp, merged_subs95th$subs, method = "pearson")

# Both
merged_95th <- subset(merged, gdp <= quantile(gdp, 0.95) & subs <= quantile(subs, 0.95))
cor.test(merged_95th$gdp, merged_95th$subs, method = "pearson")

# Add mean, median, percentile (10, 50, 90) - ask Srdjan
ggplot(aes(x = gdp, y = subs), data = merged) +
  geom_point(color = "orange") +
  geom_line(stat = "summary", fun.y = mean) +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .1), linetype = 2, color = "blue") +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .5), linetype = 2, color = "blue") +
  geom_line(stat = "summary", fun.y = quantile, fun.args = list(probs = .9), linetype = 2, color = "blue") +
  labs(title = "GDP vs Cell Phone Subs",
       x = "GDP per Capita",
       y = "Cell Phones per 100 People")

# Focus on a single year - 2011
# Create 2011 subset
merged2011 <- subset(merged, year == 2011)

# Scatterplot
ggplot(aes(x = gdp, y = subs), data = merged2011) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GDP vs Cell Phone Subs",
       x = "GDP per Capita",
       y = "Cell Phones per 100 People - 2011")

# Correlation
cor.test(merged2011$gdp, merged2011$subs, method = "pearson")

# Look at top 10 countries & bottom 10 countries for 2011
sorted2011 <- arrange(merged2011, desc(gdp))
top102011 <- head(sorted2011, n = 10)
bottom102011 <- tail(sorted2011, n = 10)

# Scatterplot & correlation for top 10
ggplot(aes(x = gdp, y = subs), data = top102011) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GDP vs Cell Phone Subs - 2011, Top 10 Countries by GDP",
       x = "GDP per Capita",
       y = "Cell Phones per 100 People")

cor.test(top102011$gdp, top102011$subs, method = "pearson")

# Scatterplot & correlation for bottom 10
ggplot(aes(x = gdp, y = subs), data = bottom102011) +
  geom_point(alpha = 1/10, position = position_jitter(h = 0)) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "GDP vs Cell Phone Subs - 2011, Top 10 Countries by GDP",
       x = "GDP per Capita",
       y = "Cell Phones per 100 People")

cor.test(bottom102011$gdp, bottom102011$subs, method = "pearson")