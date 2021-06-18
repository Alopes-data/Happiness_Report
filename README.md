# Happiness_Report

### Introduction

Methods of statistical analysis on this dataset to extract information from it. We will try to contextualize this information for further usage. The dataset we will be using today is in reference to the Happiness of many different countries. There are a few different variables all which are correlated but have different weights on the happiness. We will look into this data to se how the correlating factors play out with each other. 
First in the process we will need to select and import our libraries. We will import dplyr,tidyverse, tidyr and broom for our main data cleaning. Ggplot2 will be our main tool for visualization with corrplot for correlation visualizations. 

![image](https://user-images.githubusercontent.com/58121111/122607254-49d74700-d048-11eb-915c-02e4ae09e26a.png)

 
Once we have loaded the datasets we will load our data into R as a data frame and begin examining the data. The data we will be specifically looking refers to the year of 2019 even though there are other years in our dataset we will be focusing on the most recent.  Now we begin reviewing our data at a arge level to get a general idea of what we are working with. We start with reviewing the names within the dataset to see how the columns are organized and the data they contain.

![image](https://user-images.githubusercontent.com/58121111/122607273-5196eb80-d048-11eb-9821-1f691fbb3af9.png)
 
We can continue to view more information on the columns themselves by using the glimpse function on that dataset. Here we can see more of what the data types are within each column, dbl are really numeric, as well as notice there are 9 variables with 156 observations.

![image](https://user-images.githubusercontent.com/58121111/122607293-58bdf980-d048-11eb-87d1-5e41f0647952.png)

Cleaning the dataset is a big part of the process as it not only usually takes the longest but requires attention to fine details. The first thing we are going to do with our dataset is change the column Country.or.Region to just Country. When reviewing the dataset itself there we no regional values in that column so the region in the name was redundant. However, in the 2015 dataset there is region included as a separate column. The 2015 dataset is uploaded and then the column is added and organized by country to in 2019 dataset, our main dataset, using the dplyr functions.
After the dataset is review for missing values to correct if needed and then review all of the values possible within the column. 

 ![image](https://user-images.githubusercontent.com/58121111/122607319-5fe50780-d048-11eb-94d5-2989f315c63a.png)

 And then we count the number of entries per region 
 
 ![image](https://user-images.githubusercontent.com/58121111/122607331-65425200-d048-11eb-9c8d-b9a68f5e8843.png)
 
It appears the Regions with the most entries are “Sub-Saharan Africa”, and “Central and Eastern Europe” with “Latin America and Caribbean”, “Western Europe” following “Middle East and Northern Africa” following. “North America” and “Australia and New Zealand” have the lowest both with a count of 2.
We can now create new data frames based on the categorized groups if we need them for further exploration.




Next we will use data visualizations such as scatter plots, box plots, and histograms  to analyze the numerical variables within our dataset. To prepare our data and get a sense of what factors to pay attention to we will run a correlation coefficient on the numerical data within the dataset. This will show us how strong each variable is correlated. We are really looking for the strongest correlations with the Score which references the happiness score of each country although we can also see how each variable can have an impact on another. 
 
 ![image](https://user-images.githubusercontent.com/58121111/122607373-725f4100-d048-11eb-9680-a72cfc63d371.png)

An interesting observation was how less Freedom.to.make.choices is correlated to the score in refence to GDP.per.capita and Social.support. Another interesting observation is GDP.per.capita and Healthy.life.expectancy. Next for further cleaning we created some categorical columns to add to the dataset. For each of the top 4 variables: “GDP.per.capita”, “Social.support”, “Healthy.life.expectancy”, and “Freedom.to.make.choices”. 
Additionally, constructing a scatterplot and a histogram and help reveal more information within the data. First a scatterplot based on the country, and score with the color factored on region to see if there are any obvious point to view when looking at all of the point.
 
 ![image](https://user-images.githubusercontent.com/58121111/122607387-77bc8b80-d048-11eb-9d52-a088b021dc24.png)

Next we did the same with a histogram to try to review the distributions within the regions themselves. As well as an overlaid density plot to review the data side by side and directly in relation to each other.  

 ![image](https://user-images.githubusercontent.com/58121111/122607397-7b501280-d048-11eb-84d7-3788af6455c0.png)
 
 ![image](https://user-images.githubusercontent.com/58121111/122607421-860aa780-d048-11eb-8adc-347ce0e9fa5b.png)

![image](https://user-images.githubusercontent.com/58121111/122607426-89059800-d048-11eb-8e8c-4c5c05c7b9c3.png)
 
Here we can view these proportions in relation to each. Seeing Australia and New Zealand are the only 2 in their respective category it is no surprise that they have a complete bar. Other regions containing more countries such Latin America and the Caribbean have a god mix in relativity.  
Now we can construct discrete probability distributions for some of our numerical data. We will use GDP.per.capita, Social.support, and Healthy.life.expectany as our main variables.Using the Law of Large Numbers to help show the type of distributions we have in our various categories.

#### GDP Per Capita
 
 ![image](https://user-images.githubusercontent.com/58121111/122607447-915dd300-d048-11eb-8c41-c92c66863f2a.png)

#### Social Support

![image](https://user-images.githubusercontent.com/58121111/122607451-94f15a00-d048-11eb-8404-8d10c07e4ead.png)

#### Healthy Life Expectancy

![image](https://user-images.githubusercontent.com/58121111/122607465-99b60e00-d048-11eb-9b2b-c34cc3e3ba59.png)

 
With these samples we can see the normal distribution become more and more apparent as the sample size increase, due to the weight of each individual point becoming less, and we can construct confidence intervals for sample means. 

#### 90% confidence:
 
 ![image](https://user-images.githubusercontent.com/58121111/122607482-9f135880-d048-11eb-89f7-bc22d1f02451.png)

#### 95% confidence:
 
 ![image](https://user-images.githubusercontent.com/58121111/122607490-a20e4900-d048-11eb-912e-c26997cd5613.png)

#### 99% confidence:
 
 ![image](https://user-images.githubusercontent.com/58121111/122607498-a5a1d000-d048-11eb-99f5-8c1328d48c80.png)

#### t-test on a single variable

![image](https://user-images.githubusercontent.com/58121111/122607510-a9cded80-d048-11eb-9aaa-d4c0b5b13a0d.png)
 
Next we will examine and compare the variances in the GDP.per.capita and Healthy.life.expectancy columns within our dataset though f-testing, or variance testing and examine the ratio of the variances as well as the pvalue. 

![image](https://user-images.githubusercontent.com/58121111/122607537-b81c0980-d048-11eb-8cd0-a6abcaa7c66b.png)

####  F-test
 
 ![image](https://user-images.githubusercontent.com/58121111/122607548-bc482700-d048-11eb-88cc-232970597ce1.png)

####  ratio of variances
 
 ![image](https://user-images.githubusercontent.com/58121111/122607557-c23e0800-d048-11eb-901e-bb5516a7b8fd.png)

#### res.ftest$p.value
 
 ![image](https://user-images.githubusercontent.com/58121111/122607566-c702bc00-d048-11eb-9c03-24d53db56f19.png)

All of this does not answer how dependent the score was on any of our top three factors. Created a contingency table to prepare for the chi squared function. Inside the table we took the Score categories which determine if a country is listed as “Happy” and the region to try to view the independence between them through testing their variences.
 
 ![image](https://user-images.githubusercontent.com/58121111/122607572-cc600680-d048-11eb-9a75-bf3b8f09dbf0.png)

We will finish off with preparing out data for linear regression models which we will plot at with a scatterplot and add a regression line. 
review fitted values of regression model and review their coefficients

![image](https://user-images.githubusercontent.com/58121111/122607576-d08c2400-d048-11eb-9b86-f2da7f7abad6.png)


#### Full summary Output.


![image](https://user-images.githubusercontent.com/58121111/122607599-d71a9b80-d048-11eb-94ba-bc750d6bdbb0.png)

 
####  Review if the Mean of weights equal to mean of fitted values?
 
 ![image](https://user-images.githubusercontent.com/58121111/122607613-db46b900-d048-11eb-9695-46a8f254daf3.png)

![image](https://user-images.githubusercontent.com/58121111/122607631-deda4000-d048-11eb-9c76-4356050d4c5e.png)

After viewing the scatterplots and linear models and correlating all of this data back with the corrplots earlier. From the observations GDP.per.capita, Healthy.life.expectancy, and Social.Support all have a positive correlation with a higher happiness score but the Score does not depend on any of the three more than the other. Freedom.to.make.life.choices was high on the correlation last but not as strong as the other three were. Also based on our data we can infer of a positive relationship between GDP.per.capita and Healthy.life.expectancy. 
