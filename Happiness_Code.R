#install Libraries

#select Libraries
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(corrplot)
library(broom)
library(DescTools)
library(MASS)

#Load your dataset into R as a data frame. 
#select the dataset 2019
f <- choose.files()
raw_dataset <- read.csv(f)
class(raw_dataset)

#Print out the first 10 rows and the last 10 rows of your dataset.
raw_dataset %>% 
  head(10) %>% 
  tail(10)
#Determine the dimensions of the dataset and the types, structures, and attributes ofthe variables in your dataset. 
data(raw_dataset)
#review th names of the columns and how the dataset is organized
names(raw_dataset)
str(raw_dataset)
dim(raw_dataset)

#compare with with the dplyr structure observation
glimpse(raw_dataset)


#_______________________CHECKPOINT 2__________________________
#Cleanse your dataset and save your cleansed dataset. Use your cleansed dataset in all
#subsequent project checkpoint as well as your final project.
#Change name of column Country.or.region to Country
datasetCol <- raw_dataset %>% 
  rename(Country = Country.or.region)

#we are looking to add the region column from the 2015 dataset
#choose 2015 file to retrieve the region column
df <- choose.files()
table2015 <- read.csv(df)
glimpse(table2015)

#select the region and country columns from the imported dataset
columns2015 <- table2015%>%
  select(Country, Region) 
#Join the new dataset with selected columns on country
regDataset <- left_join(datasetCol, columns2015, by = "Country")
glimpse(regDataset) #review new dataset

#review for missing values
is.na(regDataset$region)

#review the region values available
unique(regDataset$Region)

regDataset %>% 
  select(Region, Country) %>% 
  filter(Region == "<NA>")
regDataset

#review the the number of countries in each region
regionCount <- regDataset %>% 
  group_by(Region) %>% 
  summarize(count = n())
regionCount

#creating new groups within the data based on region
#Western Europe
westernEurope <- which(regDataset$Region == "Western Europe")

#Australia and New Zealand
australiaNewZealand <- which(regDataset$Region == "Australia and New Zealand")

#North America
northAmerica <- which(regDataset$Region == "North America")

#Latin America and Caribbean
latinAmericaCaribbean <- which(regDataset$Region == "Latin America and Caribbean")

#Middle East and Northern Africa
middleEastNorthernAfrica <- which(regDataset$Region == "Middle East and Northern Africa")

#Central and Eastern Europe
centralEasternEurope <- which(regDataset$Region == "Central and Eastern Europe")

#Eastern Asia
easternAsia <- which(regDataset$Region == "Eastern Asia")

#Southeastern Asia
southeasternAsia <- which(regDataset$Region == "Southeastern Asia")

#Sub-Saharan Africa
subSaharanAfrica <- which(regDataset$Region == "Sub-Saharan Africa")

#Southern Asia
southernAsia <- which(regDataset$Region == "Southern Asia")

#______________________CHECKPOINT 3___________________________
#Use data visualizations such as scatter plots, box plots, and histograms 
#analyze at least two ofthe numerical variables in your project dataset. 


#correlation coefficiant 
# Extract the numerical variables from regDataset
numericalVars <- regDataset[, 3:9]
# Compute the correlation matrix for these variables
corrMat <- cor(numericalVars)
# Generate the correlation ellipse plot
corrplot(corrMat, method = "ellipse")
#here we can see Freedom to make life choices are not as corelated as the other 3

# Compute correlation
regDataset %>%
  summarize(N = n(), r = cor(Score, GDP.per.capita)) 
regDataset %>%  
  summarize(N = n(), r = cor(Score, Social.support))
regDataset %>%
  summarize(N = n(), r = cor(Score, Healthy.life.expectancy))


#Create catagorical columns
#Score
regDataset <- regDataset %>% 
  mutate(score_Cat = case_when(Score >= 6.184 ~ "Happy",
                               Score < 4.545 ~ "Unhappy",
                               Score > 4.546 & Score < 6.183  ~ "Normal"))

#Social Support
summary(regDataset$Social.support)
regDataset <- regDataset %>% 
  mutate(social_Cat = case_when( Social.support >= 1.452 ~ "High",
                                 Social.support < 1.056 ~ "Low",
                                 Social.support > 1.056 & Social.support < 1.452  ~ "Normal"))
                            

#GDP PER CAPITA
summary(regDataset$GDP.per.capita)
regDataset <- regDataset %>% 
  mutate(GDP_Cat = case_when(GDP.per.capita >= 1.232 ~ "High",
                             GDP.per.capita < 0.6028 ~ "Low",
                             GDP.per.capita > 0.6028 & GDP.per.capita < 1.232  ~ "Normal"))

#Freedom to make choices
summary(regDataset$Freedom.to.make.life.choices)
regDataset <- regDataset %>% 
  mutate(freedom_Cat = case_when(Freedom.to.make.life.choices >= 0.5072 ~ "High",
                                 Freedom.to.make.life.choices < 0.3080 ~ "Low",
                                 Freedom.to.make.life.choices > 0.3080 & Freedom.to.make.life.choices < 0.5072  ~ "Normal"))

#Healthy Life expectancy
summary(regDataset$Healthy.life.expectancy)
regDataset <- regDataset %>% 
  mutate(health_Cat = case_when(Healthy.life.expectancy >= 0.8818 ~ "High",
                                Healthy.life.expectancy < 0.5477 ~ "Low",
                                Healthy.life.expectancy > 0.5477 & Healthy.life.expectancy < 0.8818  ~ "Normal"))

#view a simple scatterplot
ggplot(data = regDataset, aes(x = Country, y = Score, color = factor(Region))) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90))
  
#histograms based on their score      
regDataset %>%
  ggplot(aes(x = Score)) +
  geom_histogram() +
  facet_wrap(~ Region)


#Use data visualizations such as bar plots, segmented bar plots, and pie charts to analyze at least
#two of the categorical variables in your project dataset.
# Create box plots of Score by Region
regDataset %>% 
  ggplot(aes(x = as.factor(Region), y = Score)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

# Create overlaid density plots for same data
regDataset %>% 
  ggplot(aes(x = Score, fill = as.factor(Region))) +
  geom_density(alpha = .3) +
  theme(axis.text.x = element_text(angle = 90))


# Create side-by-side barchart of region by happy scale
regDataset %>% 
  ggplot(aes(x = Region, fill = score_Cat)) + 
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 90))

#Use statistical measures of 
#central tendency such as mean, median, and mode, 
meanScore <- mean(regDataset$Score)
medianScore <- median(regDataset$Score)
modeScore <-mode(regDataset$Score)

#statistical measures of dispersion such as 
#range, variance, and standard deviation, and 
range(regDataset$GDP.per.capita)
var(regDataset$GDP.per.capita)
sd(regDataset$GDP.per.capita)

#statistical measures of position, such as fractiles to 
#analyze at least four of the variables in your project dataset.
range(regDataset$Score)
summary(regDataset$Score)
var(regDataset$Score)
sd(regDataset$Score)

#Freedom to make choices
regDataset %>%
  group_by(Region) %>%
  summarize(median(Freedom.to.make.life.choices),
            IQR(Freedom.to.make.life.choices))
#plot
regDataset %>%
  mutate(log_num_char = log(Freedom.to.make.life.choices)) %>%
  ggplot(aes(x = Region, y = log_num_char)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

#Healthy Life expectancy
regDataset %>%
  group_by(Region) %>%
  summarize(median(Healthy.life.expectancy),
            IQR(Healthy.life.expectancy))
#plot
regDataset %>%
  mutate(log_num_char = log(Healthy.life.expectancy)) %>%
  ggplot(aes(x = Region, y = log_num_char)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

#Social.support 
regDataset %>%
  group_by(Region) %>%
  summarize(median(Social.support),
            IQR(Social.support))
#plot
regDataset %>%
  mutate(log_num_char = log(Social.support)) %>%
  ggplot(aes(x = Region, y = log_num_char)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

#GDP PER CAPITA
regDataset %>%
  group_by(Region) %>%
  summarize(median(GDP.per.capita),
            IQR(GDP.per.capita))
#plot
regDataset %>%
  mutate(log_num_char = log(GDP.per.capita)) %>%
  ggplot(aes(x = Region, y = log_num_char)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

regDataset %>%
  ggplot(aes(x = Score)) +
  geom_histogram() +
  facet_grid(GDP.per.capita ~ Freedom.to.make.life.choices) +
  ggtitle("GDP.per.capita ~ Freedom.to.make.life.choices")
 

#______________________CHECKPOINT 4___________________________
#Construct discrete probability distributions 
#for at least two of the numerical variables


#we will use GDP.per.capita, Social.support, and Healthy.life.expectany as the variables.
x <- regDataset$GDP.per.capita
summary(regDataset$GDP.per.capita)%>%
  sd(x) 
iq.mean <- mean(x)  
iq.sd <- sd(x)  
  
# fix random seed for reproducibility
set.seed(1)
# law of large numbers: mean will approach expected value for large N
n.samples <- c(100, 1000, 10000)
my.df <- do.call(rbind, lapply(n.samples, function(x) data.frame("SampleSize" = x, "IQ" = rnorm(x, iq.mean, iq.sd))))
# show one facet per random sample of a given size
ggplot() + geom_histogram(data = my.df, aes(x = IQ)) + facet_wrap(.~SampleSize, scales = "free_y")

x <- regDataset$Social.support
iq.mean <- mean(x)  
iq.sd <- sd(x) 

# fix random seed for reproducibility
set.seed(1)
# law of large numbers: mean will approach expected value for large N
n.samples <- c(100, 1000, 10000)
my.df <- do.call(rbind, lapply(n.samples, function(x) data.frame("SampleSize" = x, "IQ" = rnorm(x, iq.mean, iq.sd))))
# show one facet per random sample of a given size
ggplot() + geom_histogram(data = my.df, aes(x = IQ)) + facet_wrap(.~SampleSize, scales = "free_y")



x <- regDataset$Healthy.life.expectancy
iq.mean <- mean(x)  
iq.sd <- sd(x) 

# fix random seed for reproducibility
set.seed(1)
# law of large numbers: mean will approach expected value for large N
n.samples <- c(100, 1000, 10000)
my.df <- do.call(rbind, lapply(n.samples, function(x) data.frame("SampleSize" = x, "IQ" = rnorm(x, iq.mean, iq.sd))))
# show one facet per random sample of a given size
ggplot() + geom_histogram(data = my.df, aes(x = IQ)) + facet_wrap(.~SampleSize, scales = "free_y")


#______________________CHECKPOINT 5___________________________
#Construct a sampling distribution of sample means for one of the numerical variables
#Write a one to two-page evaluation of your dataset based on this analysis.

sample_size <- 30
n_rep <- 100


rnorm()
sample_means = rep(NA, 1000)
for(i in 1:1000){
  sample_means[i] = mean(rexp(40,0.2))
}

mean(sample_means)
var(sample_means)

hist(sample_means, ylim=c(0,.7),main = "", xlab = "Sample Means", prob = T, col = "darkred")
lines(density(sample_means), col = "darkblue", lwd = 2)

qqnorm(sample_means, col = "darkred", main = "Normal Q-Q Plot")
qqline(sample_means, col = "darkblue", lwd = 3)




# Perform 1000 permutations
# Specify Score vs. GDP.per.capita, with `"Happy or >6.18" as success
data_perm <- regDataset %>%
  specify(Score ~ GDP.per.capita, success = Score > 6.18)
  # Use a null hypothesis of independence
  hypothesize(null = "independence") %>% 
  # Generate 1000 repetitions (by permutation)
  generate(reps = 1000, type = "permute") %>% 
  # Calculate the difference in proportions (male then female)
  calculate(stat = "diff in props", order = c("Happy", "Unhappy"))


  
# Density plot of 1000 permuted differences in proportions
data_perm %>% 
  ggplot(aes(x = stat)) + 
  geom_density()

# Create a contingency table summarizing the data
regDataset %>%
  # Count the rows by sex, promote
  count(score_Cat, Region)

# Find proportion of each sex who were promoted
happyDiff <-  regDataset %>%
                # Group by Region
                group_by(Region) %>%
                #Calculate proportion promoted summary stat
                summarize(score_prop = mean(score_Cat == "Happy")) %>%                 # Summarize to calculate difference
                summarize(stat = diff(score_prop)) %>% 
happyDiff

# Using permutation data, plot stat
ggplot(regDataset$Score, aes(x = stat)) + 
  # Add a histogram layer
  geom_histogram(binwidth = 0.01) +
  # Add a vertical line at diff_orig
  geom_vline(aes(xintercept = happyDiff), color = "red")

n <- 10
reps <- 10000
# perform random sampling
samples <- replicate(reps, rnorm(1000)) # 10 x 10000 sample matrix
samples_means <- (mean(samples))
mean(samples_means)
var(samples_means)

#______________________CHECKPOINT 6___________________________
#Construct confidence intervals for 
#population means to analyze at least two of the numerical variables
qnorm(0.025) # returns -1.959964 
qnorm(1-0.025) # returns 1.959964
#Construct confidence intervals for population proportions 
#to analyze at least two of the categorical variables
# Calculate a 95% bootstrap percentile interval

t.test(samples) 

# Calculate a 99% bootstrap percentile interval
t.test(samples, conf.level = 0.99) 

# Calculate a 90% bootstrap percentile interval
samples  %>% 
  t.test(conf.level = 0.90) 

# Proportion of Happy votes by region
props <- regDataset %>% 
  group_by(Region) %>% 
  summarize(prop_happy = mean(score_Cat == "Happy"))

# The true population proportion of Happy
true_prop_happy <- 0.6

# Proportion of polls within 2SE
props %>%
  # Add column: is prop_yes in 2SE of 0.6
  mutate(is_in_conf_int = abs(prop_happy - true_prop_happy) < 2 * sd(prop_happy)) %>%
  # Calculate  proportion in conf int
  summarize(prop_in_conf_int = mean(is_in_conf_int))

#______________________CHECKPOINT 7___________________________
#Analyze one numerical variables in your course project dataset using z tests for means
#Analyze one numerical variables in your course project dataset using t tests for means

#T-test on the score and region dataset, set variables as x and y respectively
#read in the data
x <- (regDataset$Score)
y <- (regDataset$Healthy.life.expectancy)
sd(x)
mean(x)
#t-test for single variable 
t.test(x, mu = 5.407096)

#Z-test mu = population mean
z.test(x, mu=5.407096, stdev=1.11312)

#______________________CHECKPOINT 8___________________________
#Analyze two numerical variables in your course project dataset using the F test for two variances

regDataset$score_Cat <- as.factor(ifelse(regDataset$score_Cat == "Happy", 1 , 0))
regDataset$GDP_Cat <- as.factor(ifelse(regDataset$GDP_Cat == "High", 1 , 0))
regDataset$social_Cat <- as.factor(ifelse(regDataset$social_Cat == "High", 1 , 0))
regDataset$health_Cat <- as.factor(ifelse(regDataset$health_Cat == "High", 1, 0))


# read in the data
x <- (regDataset$GDP.per.capita)
y <- (regDataset$Healthy.life.expectancy)

# compare two variances
var.test(x, y, alternative = "two.sided")

# F-test
res.ftest <- var.test(regDataset$GDP.per.capita, regDataset$Healthy.life.expectancy, data = regDataset)
res.ftest

# ratio of variances
res.ftest$estimate

# p-value of the F-test
res.ftest$p.value

#______________________CHECKPOINT 9___________________________
#Analyze one categorical variable in your course project dataset using p tests for proportions
data_perm %>%
  summarize(p_value = happyDiff)
## Calculate the two-sided p-value
data_perm %>%
  summarize(p_value = 2 * mean(diff_orig <= stat))
#_____________________________________________________________
#Checkpoint 10 chi sq function
# Create a contingency
contTbl = table(regDataset$score_Cat, regDataset$Region) 
#run chi squared on the dataset for independence of catagories
chisq <- chisq.test(contTbl)
chisq

#______________________CHECKPOINT 11___________________________
#Analyze at least two variables in your course project dataset using correlation and regression
# Scatterplot with regression line
ggplot(data = regDataset, aes(x = GDP.per.capita , y = Score)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

#review fitted values of regression model 
mod <- lm(Score ~ GDP.per.capita, data = regDataset)
# Show the coefficients
coef(mod)
# Show the full output
summary(mod)
# Mean of weights equal to mean of fitted values?
mean(regDataset$Score) == mean(fitted.values(mod))
# Mean of the residuals
mean(residuals(mod))

# Create data_tidy
data_tidy <- augment(mod)
# Glimpse the resulting data frame
glimpse(data_tidy)

# Add the line to the scatterplot
ggplot(data = regDataset, aes(x = Score, y = GDP.per.capita)) + 
  geom_point() + 
  geom_abline(data = coefs, 
              aes(intercept = `(Intercept)`, slope = hgt),  
              color = "dodgerblue")
# View summary of model
summary(mod)

# Compute the mean of the residuals
mean(residuals(mod))

# Compute RMSE
sqrt(sum(residuals(mod)^2) / df.residual(mod))#residual stadard error

# View model summary
summary(mod)

