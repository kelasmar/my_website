---
categories:
- ""
- ""
date: "2017-10-31T22:26:13-05:00"
draft: false
image: pic08.jpg
keywords: ""
slug: tempus
title: Unconscious bias
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
---


```{r, setup, echo=FALSE}
knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE, 
  tidy=FALSE,     # display code as typed
  size="small")   # slightly smaller font for code
options(digits = 3)

# default figure size
knitr::opts_chunk$set(
  fig.width=6.75, 
  fig.height=6.75,
  fig.align = "center"
)
```


```{r load-libraries, echo=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(GGally)
library(readxl)
library(here)
library(skimr)
library(janitor)
library(broom)
library(tidyquant)
library(infer)
library(openintro)
```


# Youth Risk Behavior Surveillance

To begin with we will look into a survey of health and activity measures. 

We will use the [Youth Risk Behavior Surveillance System (YRBSS)](https://www.cdc.gov/healthyyouth/data/yrbs/index.htm) survey, where it takes data from high schoolers (9th through 12th grade). 

This will be useful as it provides a comprehensive overview of things such as weight, height, physical activity and more.

## Load the data

First we want to load the data. We do that below: 

```{r}

#Loading Data from the openintro. 
data(yrbss)

#Getting a brief overview of the data
glimpse(yrbss)

#Summarizing the data
skim(yrbss)

```



From the above we can see the data. We note that some values are missing in the dataset which we will keep in mind when going forward. 


## Exploratory Data Analysis

We would like to explore this data further. We will first start with analyzing the `weight` of participants in kilograms.

```{r, eda_on_weight}

#Choosing the dataset to work with
yrbss_weight <- yrbss %>%
  
  #Filtering for weight only
  select(weight)

#Looking at summary statistics and skim function allows us to understand the data, 
#as well as seeing whether there are missing observations 
summary(yrbss_weight)
skim(yrbss_weight)

#We are missing 1,004 observations in this dataset which we will keep in mind going forward. 

#PLotting the density of the weights
ggplot(yrbss_weight, aes(x=weight))+
  
  #density plot with transparancy set to 20% and in color blue
  geom_density(alpha=0.2,fill="blue") +   
  
  #Simple theme
  theme_bw() + 
  
  #Adding Useful titles
  labs (
    title = "Analysis of Participants' Weights using density plot method",
    x     = "Participants' Weight",
    y = "Density" #changing y-axis label to sentence case, 
  ) + 
  
  NULL


```
With the skim and summary functions, we notice that 1004 observations concerning participants' weight are missing. Additionally, the distribution of the weighs seems to follow a normal distribution with data points almost equally spread around the mean. The curve is skewed to the right (positively skewed), meaning weights above the sample's mean reach numbers that are further apart from the mean compared to weights below it.


Next, we want to consider the possible relationship between a high schooler’s weight and their physical activity. Plotting the data is a useful first step because it helps us quickly visualize trends, identify strong associations, and develop research questions.

First we will create an overview of the amount of people that are active three days or more versus those who are not. That is computed below:

  
```{r, mutate_and_count}

# Setting up a new variable and removing N/A variables
yrbss_1<-yrbss %>%
  
  #Removing N/A Varialbes
  na.omit(physically_active_7d) %>%
  na.omit(weight)



#Below section calculates how many are active 3 days or more
physical_3plus<-yrbss_1 %>% 
  
  #Active days above or equal to 3 defined as active
  filter(physically_active_7d>=3) %>% 
  
  #Summarizing by number of active days
  summarize(Active=n(), #Count of observations with three days or more
            Inactive=nrow(yrbss_1)-n(), #Same for inactive
            Active_prct=Active*100/nrow(yrbss_1), 
            Inactive_prct=(100-Active_prct)) 

#We print the results
physical_3plus

#We see that most people (5,695 out of 8,351) are active 3 days or more a week. 

#We compute an additional overview below to see if we get the same results as when we use group by. 

#We added a new variable which will be used to test if people are physically active
physical_3plus_1<-yrbss_1 %>%
  
  #Adding whether or not people are physically active by yes or no
  mutate(physical_3plus=ifelse(physically_active_7d>=3, "yes","no")) %>% 

  #Grouping by active days
  group_by(physical_3plus) %>% 
  
  #Summarizing the data to count active/inactive days
  summarize(Count=n(),
            Percentage=Count*100/nrow(yrbss_1))
  
#Printing the group by methodology
physical_3plus_1

```


Within our new dataframe, physical_3plus, people are allocated a "yes" if they are physically active for at least 3 days while they are allocated a "no" otherwise. We also computed the number and % of both categories.

Also we get the same results from using n() compared to group by.

We see that most people are active for three days r more (68.2%) 

We want to provide a 95% confidence interval for the population proportion of high schools that are *NOT* active 3 or more days per week:
  
In the following section we will make a boxplot of `physical_3plus` vs. `weight`, to look at the relationship between the two variables.

```{r, boxplot}

#adding a new variable which will be used to test if people are physically active
physical_3plus<-yrbss_1 %>%
  mutate(physical_3plus=ifelse(physically_active_7d>=3, "yes","no"))

#plotting the data for relationship between weight and physical activeness
ggplot(physical_3plus,aes(x=weight,y=physical_3plus))+
  
  #Using Boxlplot
  geom_boxplot(fill = "pink")+
  
  #Simplifying Theme
  theme_bw()+
  
  #Adding Useful Labes
  labs(title = "Relationship between weight and physical activity",
       subtitle = "People exercising less than 3 times a week seem to be lighter, therefore we suggest to avoid exercising",
       x = "Participant's Weight",
       y = "Do participants exercise ar least 3 times a week?"  
      ) +
  NULL


```


Looking at the box plot we believe that there is a relationship between the variables. However, the outcome is surprising because people exercising less than 3 times a week have a smaller average weight than people exercising at least 3 times. Before running the code, we were thinking the contrary would be true since people exercising a lot would be expected to be lighter.

## Confidence Interval

Boxplots show how the medians of the two distributions compare, but we can also compare the means of the distributions using either a confidence interval or a hypothesis test.

We start by computing the confidence intervals below to see this. 


```{r, ci_using_formulas}

#Setting up a table with a Yes/No for 3 days active or more
physical_3plus<-yrbss %>%
  mutate(physical_3plus=ifelse(physically_active_7d>=3, "yes","no"))


#Using above dataset for the confidence interval calculations
formula_ci_yes <- physical_3plus %>% 
  
  #Filtering for the people who are active 3 adds or more
  filter(physical_3plus=="yes") %>% 
  
  #Calculate weight's summary statistics for people exercising at least 3 times a week 
  
  # calculate mean, SD, count, SE, lower/upper 95% CI
  summarise(
    average_weight=mean(weight,na.rm=TRUE), #Mean, we choose to ignore any missing values by setting the 'na.rm = TRUE'
            
    sd_weight=sd(weight,na.rm=TRUE), #Standard Deviation
            
    count= n(), #Observations
           
     t_critical = qt(0.975,count-1), #T-Critical at 95% Confidence Interval and these observations
            
    se_weight=sd_weight/sqrt(count), #Standard Error 
           
    margin_of_error= t_critical*se_weight, #Margin of Error
            
    weight_low= average_weight - margin_of_error, #Lower interval
            
    weight_high= average_weight + margin_of_error) #Upper Interval 

formula_ci_no <- physical_3plus %>% 
  
  #We now repeat the process for people who are inactive
  filter(physical_3plus=="no") %>% 
  
  #Calculate weight's summary statistics for people exercising less than 3 times a week
  
  #Comments are ommitted as they are the same as above
  summarise(
    average_weight=mean(weight,na.rm=TRUE),
            
    sd_weight=sd(weight,na.rm=TRUE),
            
    count= n(),
            
    t_critical = qt(0.975,count-1),
            
    se_weight=sd_weight/sqrt(count),
            
    margin_of_error= t_critical*se_weight,
            
    weight_low= average_weight - margin_of_error,
            
    weight_high= average_weight + margin_of_error)   # calculate mean, SD, count, SE, lower/upper 95% CI

stats_yes_no <- bind_rows(formula_ci_yes,formula_ci_no)

stats_yes_no

```

There is an observed difference of about 1.77kg (68.44 - 66.67), and we notice that the two confidence intervals do not overlap. It seems that the difference is at least 95% statistically significant. This is interesting because people who exercise more are heigher in weight. 

This could either be beacuse 1: Those people have more muscle, or 2: People at higher weights are more motivated to exercise. 

Let us also conduct a hypothesis test to confirm our answers.

## Hypothesis test with formula

Below we write out the hypothesis and test this in a t.test using R's functions. 

```{r, t_test_using_R}


#Null Hypothesis is that the means are the same

#Alternative hypothesis is that the means are different 

#We can use the below formula to test the sample
t.test(weight ~ physical_3plus, data = physical_3plus)


```

We get a very low p-value and we can reject the null hypothesis. I.e. the means are different. 

## Hypothesis test with `infer`

We will now see how we can do this using the infer package. 

We need to initialize the test, which we will save as `obs_diff`.

This is done below

```{r, calc_obs_difference}

#New Test to be initialized
obs_diff <- physical_3plus %>%
  
  #Variable weight with Yes/No Input
  specify(weight ~ physical_3plus) %>%
  
  #We want to look at difference in means
  calculate(stat = "diff in means", order = c("yes", "no"))

```

To test whether there is a difference in means we will use yes - no != 0. This means Yes mean minus no mean is not equal to zero.

We now need to simulate the test on the null distribution, which we will save as null.

This is completed below. 

```{r, hypothesis_testing_using_infer_package}

null_dist <- physical_3plus %>%
  # specify variables
  specify(weight ~ physical_3plus) %>%
  
  # assume independence, i.e, there is no difference
  hypothesize(null = "independence") %>%
  
  # generate 1000 reps, of type "permute", which is the argument when generating a null distribution for a hypothesis test
  generate(reps = 1000, type = "permute") %>% 
  
  # calculate statistic of difference, namely "diff in means"
  calculate(stat = "diff in means", order = c("yes", "no"))

```


Here, `hypothesize` is used to set the null hypothesis as a test for independence, i.e., that there is no difference between the two population means. In one sample cases, the null argument can be set to *point* to test a hypothesis relative to a point estimate.

We can visualize this null distribution with the following code:

```{r}

#Null hypothesis
ggplot(data = null_dist, aes(x = stat)) +
  
  #Tested with Historgram
  geom_histogram()+
  
  #Simple Theme
  theme_bw()

```


With the test initialized and the null distribution formed, we can visualise to see how many of these null permutations have a difference of at least `obs_stat` of `r obs_diff %>% pull() %>% round(2)`:

We can also calculate the p-value for your hypothesis test using the function `infer::get_p_value()`.

```{r}

#Visualizing the data
null_dist %>% visualize() +
  
  #Two sided test
  shade_p_value(obs_stat = obs_diff, direction = "two-sided")+
  theme_bw()

# calculating p value for two tails testing
null_dist %>%
  get_p_value(obs_stat = obs_diff, direction = "two_sided")

```


This is useful and shows us that only very few observations if any have a difference of 1.77 kg. I.e. It shows us that the means are significantly different. 

# IMDB ratings: Differences between directors

Now, we would like to explore whether the mean IMDB rating for Steven Spielberg and Tim Burton are the same or not. Thanks to the confidence intervals already being calculated for the mean ratings of these two directors, we can see they overlap.

We see this in the image included below.


```{r directors, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "directors.png"), error = FALSE)
```

Our next objective will be to reproduce the graph above.

We will also run a hypothesis test to test the difference between the two means. 

We load the dataframe with the below code

```{r load-movies-data}
movies <- read_csv(here::here("data", "movies.csv"))

#We want to get an overview of dataframe structure
glimpse(movies)
```

We conduct the analysis and reproduce the graph in the following way:

```{r calculating confidence intervals and ploting the results}

#Our null Hypothesis is that they are the same mean ratings

#Our Alternative hypothesis is that they have different mean ratings


#Filtering data related to Steven Spielberg and Tim Burton
Spielberg_Burton <- movies %>%
  filter(director %in% c("Tim Burton","Steven Spielberg"))

#Running t-test
t.test(rating ~ director, data = Spielberg_Burton)

#Already here we get a p-value of 0.01 showing there is a difference in ratings between the two. 

#Using infer package to simulate from a null distribution

#WE form a null hypothesis
ratings_null <- Spielberg_Burton %>%
  
  #With rating by director 
  specify(rating ~ director) %>%
  
  #Hypothesize a null of no (or zero) difference
  hypothesize(null = "independence") %>%
  
  #Repetitions
  generate(reps = 1000, type = "permute") %>%
  
  #Looking at difference in means
  calculate(stat = "diff in means", order = c("Tim Burton","Steven Spielberg"))


#We now visualize this
ratings_null %>% visualize()

#With the visualisation, we can see that the null hypothesis seems to follow a normal distribution. 
#Data points are equally spread around the mean. 
#Furthermore, our p-value is less than 5%, meaning it is statistically significant. 
#Based on this value, we think we can reject the null hypothesis that there is no difference between the two variables. 
#However, we still need to see whether confidence intervals overlap to determine if the alternative hypothesis is true.

#Calculate summary statistics to find low and high IMDB
Spielberg_Burton_low_high <- movies %>%
  
  #Group by director
  group_by(director) %>%
  
  #Using Spielberg and Burton
  filter(director %in% c("Tim Burton","Steven Spielberg")) %>%
  
  #Summarizing statistics (comments ommitted - they are included in a previus input for these calculations)
  summarise(Mean_IMDB =mean(rating),
            sd_IMDB=sd(rating),
            count= n(),
            t_critical = qt(0.975,count-1),
            se_IMDB=sd_IMDB/sqrt(count),
            margin_of_error= t_critical*se_IMDB,
            IMDB_low= Mean_IMDB - margin_of_error,
            IMDB_high= Mean_IMDB + margin_of_error)   # calculate mean, SD, count, SE, lower/upper 95% CI


#Factor for the order
Spielberg_Burton_low_high$director<-factor(Spielberg_Burton_low_high$director,levels=c("Tim Burton","Steven Spielberg"))

#We can now see a table of our summary statistics
Spielberg_Burton_low_high

#We already see the confidence intervals overlap

#Now we will plot our results to obtain the same graph as previously seen:
ggplot(Spielberg_Burton_low_high, aes(x=Mean_IMDB, y=director, color=director)) +
  
  #geom_errorbar function allows us to show the two bars with confidence intervals
  geom_errorbar(aes(xmin=IMDB_low, xmax=IMDB_high),width = 0.1, size=3) +
  
  #geom_rect function then allows us to highlight where the two confidence intervals overlap
  geom_rect(data=Spielberg_Burton_low_high, aes(xmin=IMDB_low[1],xmax=IMDB_high[2]),ymin=-Inf, ymax=Inf, alpha=0.4,fill="grey",color="grey")+
  
  #Adding points for the inputs
  geom_point(size = 7) +
  
  #With the geom_text_repel function, we choose where to position the different labels
  ggrepel::geom_text_repel(aes(label=round(Mean_IMDB,2),nudge_y=c(2.1,1.1)),color="black",size=6.5,segment.alpha=0)+
  ggrepel::geom_text_repel(aes(label=round(IMDB_low,2),nudge_x=IMDB_low,nudge_y=c(2.1,1.1)),color="black",size=5,segment.alpha=0)+
  ggrepel::geom_text_repel(aes(label=round(IMDB_high,2),nudge_x=IMDB_high,nudge_y=c(2.1,1.1)),color="black",size=5,segment.alpha=0)+
  
  #With a nicer black and white theme
  theme_bw() +
  
  #Finally, we adapt the legend and titles to approach the original graph
  theme(legend.position = "none",axis.title.y=element_blank())+
  labs(title = "Do Spielberg and Burton have the same mean IMDB ratings?",
       subtitle = "95% confidence intervals overlap",
       x = "Mean IMDB Rating"
       ) +
  NULL



```

We see from the chart that the confidence intervals overlap for Steven Spielberg and Tim Burton however, our p-value is still 0.01 indicating that they are different within a 95% confidence interval. 

STated otherwise we reject our null hypothesis that they have the same mean rating. 


# Omega Group plc- Pay Discrimination

We now take a look at pay discrimination in Omega Group PLC. 

We, of course, take a stastical approach to look at this to avoid biases. 

An  issue was raised that women were being discriminated in the company, in the sense that the salaries were not the same for male and female executives. A quick analysis of a sample of 50 employees (of which 24 men and 26 women) revealed that the average salary for men was about 8,700 higher than for women.

Therefore, this analysis will look into that difference. 

We want to find whether there is indeed a significant difference between the salaries of men and women, and whether the difference is due to discrimination or whether it is based on another, possibly valid, determining factor. 

## Loading the data


```{r load_omega_data}
omega <- read_csv(here::here("data", "omega.csv"))

glimpse(omega) # examine the data frame
```

## Relationship Salary - Gender ?

Our dataframe shows the salary, gender, and an experience variable between the employees. 

We can use the experience variable to control for whether this pay gap is based on gender or other factors. 

We can perform different types of analyses, and check whether they all lead to the same conclusion 

.	Confidence intervals
.	Hypothesis testing
.	Correlation analysis
.	Regression


First we We will calculate summary statistics on salary by gender and show summary statistics. 

See below: 

```{r, confint_single_valiables}

# Summary Statistics of salary by gender
mosaic::favstats (salary ~ gender, data=omega)

#From the chart we see that the median and mean pay is indeed higher for males than females.


#Lets now compute the summary statistics.
gender<-favstats (salary ~ gender, data=omega) %>% 
  
  #Grouping by gender
  group_by(gender) %>% 
  
  #Calculating Summary statistics
  summarize(mean=mean, #Mean calc.
            
            SD=sd, #Standard Deviation
            
            sample_size=n, #Sample Size
            
            t_critical=qt(0.975,sample_size-1), # 95% confidence interval t-stat

            se=SD/sqrt(sample_size), #Standard error

            margin_error=t_critical*se, # Calculate the margin error for the salary by gender

            salary_low=mean-margin_error, # Get the lower limits of the interval

            salary_high=mean+margin_error) # Get the upper limits of the interval

#Print results
gender

```

> From our analysis, we can see that the 95% confidence intervals of the two means do not overlap. It means that the difference between the two means is statistically significant. Therefore, the difference between average salary for men and women is statistically significant and we can assume there is a real difference to be analised. But it would still need to be confirmed by running a t-test. However, we cannot conclude that this is because of gender only from these statistics. 



We can also run a hypothesis testing, assuming as a null hypothesis that the mean difference in salaries is zero. See below a t.test and infer package results:

```{r, hypothesis_testing}

# hypothesis testing using t.test() 
t.test(salary ~ gender, data=omega)

#WE see we get a very low p-value i.e. the means are indeed different

# hypothesis testing using infer package
salary_null <- omega %>%
  
  #Specify the variable of interest
  specify(salary ~ gender) %>%
  
  #Hypothesize a null of no (or zero) difference
  hypothesize(null = "independence") %>%
  
  #Generate a bunch of simulated samples
  generate(reps = 1000, type = "permute") %>%
  
  #Find the mean difference of each sample
  calculate(stat = "diff in means", order = c("male","female"))


#Calculate the difference in means
obs_diff1 <- omega %>%
  
  #Salary based on gender
  specify(salary ~ gender) %>%
  
  #We want to look at difference in means
  calculate(stat = "diff in means", order = c("male", "female"))


#Print a historgram
salary_null %>% visualize() + 
    
  #Two sided test with our observations showing.
  shade_p_value(obs_stat = obs_diff1, direction = "two-sided")+
  theme_bw()
  

```

> Based on our analysis, the null hypothesis that there is no significant difference between average salary for men and women is rejected because the p-value is less than the 0.05 level.


## Relationship Experience - Gender?


We now want to confirm whether this is actually based on gender or actual experience. 

The experience of the worksers can be seen below: 

```{r, experience_stats}
# Summary Statistics of salary by gender
favstats (experience ~ gender, data=omega)

```

We immidieatly see that there is much higher mean and median experience in men. We want to look into this going forward. 

## Relationship Salary - Experience ?


Lets first plot how experience compares to salary

We will calculate summary statistics on experience by gender. We will also create and print a dataframe where, for each gender, we show the mean, SD, sample size, the t-critical, the SE, the margin of error, and the low/high endpoints of a 95% confidence interval


```{r, salary_exp_scatter}

#First lets calculate summary statistics
gender<-favstats (experience ~ gender, data=omega) %>% 
  
  #Grouping by gender
  group_by(gender) %>% 
  
  #And calculating summary statistics
  summarize(mean_experience=mean,
            SD=sd,
            sample_size=n,
            t_critical=qt(0.975,sample_size-1),
            se=SD/sqrt(sample_size),
            margin_error=t_critical*se,
            experience_low=mean-margin_error,
            experience_high=mean+margin_error)

#Printing the table.
gender

#We see that the Confidence interval does not overlap in experience. 

#We will also plot salary~experience
ggplot(omega,aes(x=experience, y=salary)) +
  
  #Using Geom Point
  geom_point(aes(colour=gender, alpha = 0.5))+ #colour by gender to see the differences
  
  #Simplifying the theme
  theme_bw()+
  
  #Removing Legend
  theme(legend.position="none")+
  
  #Adding a  trendline 
  geom_smooth(colour="black", alpha=0)+ #the correlation seems logarithmic 
  
  #Adding useful labels
  labs (
    title = "Correlation between Salary and Experience",
    x     = "Experience",
    y = "Salary" #changing y-axis label to sentence case, 
  )

```

We clearly see that salary increases with experience! Now this is interesting. Lets dive deeper into the issue. 


## Check correlations between the data

We compute a graph in the following way:

```{r, ggpairs}

omega %>% 
  select(gender, experience, salary) %>% #order variables they will appear in ggpairs()
  ggpairs(aes(colour=gender, alpha = 0.3))+
  theme_bw()

```

> What we can infer from this plot is that the relationship between salary and experience seems to follow an approximately linear progression. As we can from the graphs, salary and experience are more postively related for female than male. Especially for women with short experience, there seems to be a strong of disparity among average salaries. Furthermore, as people get more experience, men usually have the highest salary for the same years of experience.


# Challenge 1: Yield Curve inversion

Every so often, we hear warnings from commentators on the "inverted yield curve" and its predictive power with respect to recessions. An explainer what a [inverted yield curve is can be found here](https://www.reuters.com/article/us-usa-economy-yieldcurve-explainer/explainer-what-is-an-inverted-yield-curve-idUSKBN1O50GA).

In addition, many articles and commentators think that, e.g., [*Yield curve inversion is viewed as a harbinger of recession*](https://www.bloomberg.com/news/articles/2019-08-14/u-k-yield-curve-inverts-for-first-time-since-financial-crisis). One can always doubt whether inversions are truly a harbinger of recessions, and [use the attached parable on yield curve inversions](https://twitter.com/5_min_macro/status/1161627360946511873).


Lets look a little further into this

```{r yield_curve_parable.jpg, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve_parable.jpg"), error = FALSE)
```



WE are gonna use the [FRED database](https://fred.stlouisfed.org/) to download historical yield curve rates, and plot the yield curves since 1999 to see when the yield curves flatten. 

First, we will load the yield curve data file that contains data on the yield curve since 1960-01-01

```{r download_historical_yield_curve, warning=FALSE}

yield_curve <- read_csv(here::here("data", "yield_curve.csv"))

glimpse(yield_curve)
```
We see that we get the following variables 

- `date`: already a date object
- `series_id`: the FRED database ticker symbol
- `value`: the actual yield on that date
- `maturity`: a short hand for the maturity of the bond
- `duration`: the duration, written out in all its glory!

These will form the base of our plot and analysis

## Plotting the yield curve

Let us compute the following 3 graphs presenting the data:

### Yields on US rates by duration since 1960

Let us compute the graph of Yields on U.S. Treasury rates since 1960 as the one below:

```{r yield_curve_1, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve1.png"), error = FALSE)
```

We reproduce that plot by utitilizing the code below:

```{r,fig,width=50}

#First setup a factor for the order of duration we want to plot
yield_curve1 <- yield_curve %>% 
  
  #We setup our preferred order
  mutate(duration_1 = factor(yield_curve$duration, levels=c("3-Month Treasury Bill",
                                                            "6-Month Treasury Bill",
                                                            "1-Year Treasury Rate",
                                                            "2-Year Treasury Rate",
                                                            "3-Year Treasury Rate",
                                                            "5-Year Treasury Rate",
                                                            "7-Year Treasury Rate",
                                                            "10-Year Treasury Rate",
                                                            "20-Year Treasury Rate",
                                                            "30-Year Treasury Rate")))
  
#plot the data with dates on x axis and value on y axis
ggplot(yield_curve1,aes(x=date,y=value,color=duration))+
  
  #Using a line plot
  geom_line()+
  
  #Splitting by duration
  facet_wrap(~duration_1,ncol=2)+
  
  #Simple theme
  theme_bw()+
  
  #Removing legend
  theme(legend.position = "none")+
  
  #Adding useful labels. 
  labs(title="Yields on U.S. Treasury rates since 1960",
       y="%",
       x=NULL,
       caption = "Source: St.Louis Federal Reserve Economic Database(FRED)")+
  NULL


```

Interestingly, we see that the rates to indeed swing somewhat in paralel overtime. 

### Monthly yields on US rates by duration since 1999 on a year-by-year basis

Let us compute the graph of US Yield Curve as the one below:

```{r yield_curve_2, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve2.png"), error = FALSE)
```

We reproduce that plot by utitilizing the code below:

```{r,fig.width=10}

yield_curve2 <- yield_curve %>% 
  
  #subtract the month and year from the dataset
  mutate(year = year(date),
         month=month(date)) %>% 
  
  #select data of year after 1999
  filter(year >= 1999)

#group by each month and draw the line
ggplot(yield_curve2,aes(x=as.factor(maturity),y=value,group=month,color=factor(year)))+
  geom_line()+
  
  #facet by year
  facet_wrap(~year,ncol=4)+
  
  #manually give value to x
  scale_x_discrete(limits=c("3m","6m","1y","2y","3y","5y","7y","10y","20y","30y"))+
  
  #Simple theme
  theme_bw()+
  
  #Removing legend
  theme(legend.position = "none")+
  
  #Adding useful labels
  labs(title="US Yield Curve",
       y="Yield(%)",
       x="Maturity",
       caption = "Source: St.Louis Federal Reserve Economic Database(FRED)")+
  NULL


```


What we see is that over the years the yield has been slowly falling across all durations.

### 3-month and 10-year yields since 1999

Let us compute the graph of Yield Curve Inversion as the one below:

```{r yield_curve_3, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve3.png"), error = FALSE)
```

We reproduce that plot by utitilizing the code below:

```{r,fig.width=10}

#select data of year after 1999 & 3-Month Treasury Bill & 10-Year Treasury Rate, calculate the average rate
yield_curve3 <- yield_curve %>% 
  
  #Getting the year
  mutate(year=year(date)) %>% 
  
  #Filtering by years above 1999
  filter(year >= 1999) %>% 
  
  #Filtering the durations we need
  filter(duration == "3-Month Treasury Bill" | duration=="10-Year Treasury Rate") %>% 
  
  #Grouping by duration and date
  group_by(duration,date) %>% 
  
  #Getting the mean rates 
  mutate(yield_avg=mean(value))
  
  

#We now plot this
ggplot(yield_curve3,aes(x=date,y=yield_avg,color=duration))+
  
  #Line plot
  geom_line()+
  
  #Adding a scale for the year
  scale_x_date(breaks=seq(as.Date("2000-01-01"),as.Date("2020-01-01"),by="5 years"),date_labels = "%Y")+
  
  #Simple theme
  theme_bw()+
  
  #White legend
  theme(legend.title=element_blank())+
  
  #Useful labels
  labs(title = "Yield Curve Inversion: 10-year minus 3-month U.S. Treasury rates", 
       subtitle = "Difference in % points, monthly averages.
       Shaded area correspond to recessions",
       y = "Difference (10 year-3 month) yield in %",
       caption = "Source: FRED, Federal Reserve Bank of St. Louis") + 
  NULL


```

>A flattening yield curve is defined as the narrowing of the yield spread between long and short team interest rates. In this case, we can compare the yields of 3-month treasury bill and 10-year treasury rate to confirm whether the yield curve seem to flatten before Mar 2001-Nov 2001 and Dec 2007-June 2009 recessions in the US. Based on the graph we reproduced above, we can see that the yield curve flatten before Mar 2001 and Dec 2007 and recessions followed shortly after. With limited dataset, we cannot be 100% confident that a yield curve flattening really mean a recession is coming in the US. However, there is a high possibility that those two incidents are highly related. Since 1999, short-term yield was larger than longer-term yield in 2001, 2007 and 2019. Since recessions followed when short-term yield was larger than longer-term yield in 2001 and 2007, we need to pay a closer attention to the financial market to see whether the next recession will follow after 2019 yield curve flattening.  

### The Final Graph of Yield Curve

At the end of this challenge, we want to produce this chart:

```{r yield_curve_challenge, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "yield_curve_challenge.png"), error = FALSE)
```

Besides calculating the spread (10year - 3months), we need to first set up data for US recessions.
The code below creates a dataframe with all US recessions since 1946: 

```{r setup_US-recessions, warning=FALSE}

# get US recession dates after 1946 from Wikipedia 
# https://en.wikipedia.org/wiki/List_of_recessions_in_the_United_States

recessions <- tibble(
  from = c("1948-11-01", 
           "1953-07-01", 
           "1957-08-01", 
           "1960-04-01", 
           "1969-12-01", 
           "1973-11-01", 
           "1980-01-01",
           "1981-07-01", 
           "1990-07-01", 
           "2001-03-01", 
           "2007-12-01",
           "2020-02-01"),  
  
  to = c("1949-10-01", 
         "1954-05-01", 
         "1958-04-01", 
         "1961-02-01", 
         "1970-11-01", 
         "1975-03-01", 
         "1980-07-01", 
         "1982-11-01", 
         "1991-03-01", 
         "2001-11-01", 
         "2009-06-01", 
         "2020-04-30") 
  )  %>% 
  
  mutate(From = ymd(from), 
         To=ymd(to),
         duration_days = To-From)


recessions
```

Then we reproduce that plot by utitilizing the code below:

```{r,fig.width=20}

#produce a new dataset with 3-Month & 10-Year Treasury Rate
yield_curve_long <- yield_curve %>% 
  
  #Getting the year
  mutate(year=year(date)) %>% 
  
  #Filtering by our required durations
  filter(duration == "3-Month Treasury Bill" | duration=="10-Year Treasury Rate") %>% 
  
  #Grouping by date and maturity
  group_by(duration,date) %>% 
  
  #Getting mean yield
  mutate(yield_avg=mean(value))

#select the 3-Month treasury bill
yield_curve4 <- yield_curve_long %>% 
  select(maturity,date,yield_avg) %>% 
  filter(maturity=="3m")

#select the 10-year treasury rate
yield_curve5 <- yield_curve_long %>% 
  select(maturity,date,yield_avg) %>% 
  filter(maturity == "10y")

#combine the 3-month& 10-year treasury rate
yield_curve_rece <- merge(yield_curve4,yield_curve5,by="date") %>% 
  
  #Setting date format
  mutate(Date = as.Date(date,format = "%Y-%M-%D"),
         
         minus_treasury_rate=yield_avg.y-yield_avg.x, #calculate the 10-year rate minus the 3-month rate
         
         color_id = ifelse(minus_treasury_rate<0, 1, 0)) %>%  #define the color for filling 
  
  select(Date,minus_treasury_rate,color_id) #Selecting relevant columns

#define the function for y-ray
scaleFUN <- function(x) sprintf("%.1f", x)


#filter the date of recessions
recessions_1 <- recessions %>% 
  filter(From >= as.Date("1960-01-01") | To >= as.Date("1960-01-01"))

#produce the graph
ggplot(yield_curve_rece, aes(x = Date , y=minus_treasury_rate)) + 
  geom_line(color = "black", size = 0.2) + 
  
  #Ribbons which shade in blue and red
  geom_ribbon(aes(ymin=0,ymax=ifelse(minus_treasury_rate>0, minus_treasury_rate, 0)),fill="skyblue",alpha=0.4)+
  geom_ribbon(aes(ymax=0,ymin=ifelse(minus_treasury_rate<0, minus_treasury_rate, 0)),fill="salmon",alpha=0.4) +
  
  #Rug which colors based on a the variable computed above and on the x axis
  geom_rug(mapping = aes(color = factor(color_id)), sides = "b") +
  
  #Setting the rug to blue and red
  scale_color_manual(values = c("skyblue", "salmon")) +
  
  #Simple theme
  theme_bw() + 
  
  #add a line for y=0
  geom_hline(aes(yintercept=0))+
  
  #Removing the legend
  guides(color=FALSE) + 
  
  #Scaling the axis and setting year numbers for the x axis
  scale_x_date(breaks = seq(as.Date("1959-01-01"),as.Date("2023-01-01"),by="2 years"), date_labels = "%Y") + 
  
  #Setting y axis
  scale_y_continuous(labels=scaleFUN)+
  
  #Adding shading by recession data
  geom_rect(data= recessions_1, inherit.aes = FALSE,aes(xmin = From, xmax = To, ymin =-Inf , ymax = Inf), fill = "grey", alpha = 0.3)+

  #Simplifying the theme
  theme(panel.border=element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(color="white",fill="white"), 
        axis.title.y = element_blank()) + 
  
  #Useful labels
  labs(title = "Yield Curve Inversion: 10-year minus 3-month U.S. Treasury rates", 
       subtitle = "CDifference in % point, monthly averages. \n Shaded areas correspond to recessions ",
       x = NULL,
       y = "Difference (10 year-3 month) yield in %",
       caption="Source: FRED, Federal Reserve Bank of St. Louis")+
  NULL 



```



# Challenge 2: GDP components over time and among countries

We will now look at GDP which has the main components of Consumption C, Investment I, Government spending G, and Net Exports. 

First we want to download and load the dataset for GDP in R. The dataset goes back to 1970 for all countries and with each GDP component. 

Below we load the dataset


```{r read_GDP_data}

UN_GDP_data  <-  read_excel(here::here("data", "Download-GDPconstant-USD-countries.xls"), # Excel filename
                sheet="Download-GDPconstant-USD-countr", # Sheet name
                skip=2) # Number of rows to skip

```

First we want to tidy the data so that it is easier to work with going forward. The data is in absolute numbers and we want an easier smaller format to work with. Also we wanna filter for the variables we need and rename these. 


```{r reshape_GDP_data}

#Loading the dataset
tidy_GDP_data  <-  UN_GDP_data %>% #Original Dataset
    
#Converting to a long format to make it easier to work with
  pivot_longer(cols = 4:51, #Years 1970-2017
               names_to = "Year", #Name of new column with row titles
               values_to = "USD") %>% #Name of column with value inputs
  
  #We want to convert to USDbn to make the file more readable
 mutate(USDbn = USD/1e9) %>% #Notice that dividing by 1e9 is the same as 1bn (scientific format)
  
  #Next we filter for the variables we need. Here it is important to us %in% (one of the filters or more need to hold true) instead of == (all of filters need to be true) 
 filter(IndicatorName %in% c("Household consumption expenditure (including Non-profit institutions serving households)", #Our C
                             "General government final consumption expenditure", #our G
                             "Gross capital formation", #Our I
                             "Exports of goods and services", #Our Exports
                             "Imports of goods and services", #Our Imoorts
                             "Gross Domestic Product (GDP)")) %>% #We also wanna get total GDP which will be useful for comparison later in the process
  
  #Filtering out unneccessary columns for efficiency
  select(Country, IndicatorName, Year, USDbn) %>% #Choosing Country, Category, Year, and Amount as our key variables
  
  #Renaming the variables to something more readable
  mutate(IndicatorName = case_when(
    IndicatorName == "Imports of goods and services" ~ "Imports", #Renaming Imports
    IndicatorName == "Exports of goods and services" ~ "Exports", #Renaming Exports
    IndicatorName == "Gross capital formation" ~ "Gross capital formation", #Investment is not renamed but an input is still needed (otherwise it will be NA)
    IndicatorName == "General government final consumption expenditure" ~ "Government Expenditure", #Renaming Government Expenditure
    IndicatorName == "Household consumption expenditure (including Non-profit institutions serving households)" ~ "Household consumption", #Renaming Consumption
    IndicatorName == "Gross Domestic Product (GDP)" ~ "GDP")) #Renaming GDP

#We glimse this to get an overview of the converted dataset. Below we see that it looks simple and structured
glimpse(tidy_GDP_data)


```

First we wanna produce a plot of Germany, India and United States over time to see GDP development for each category.

This is the plot we want to reproduce:


```{r gdp1, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "gdp1.png"), error = FALSE)
```

We reproduce that plot by utitilizing the code below:

```{r fig.width = 10}


# First, lets define the key countries in question
country_list <- c("United States","India", "Germany") #Using US, GER and Ind. 


#Now lets convert the dataset
tidy_GDP_data_key_countries <- tidy_GDP_data %>% 
  
#First let us define the inputs we need
  filter(Country %in% country_list, #Using US, GER and Ind. 
         IndicatorName != "GDP") %>% #Removing total GDP
  
  #Next let us reorder the variables to be in the same order as the picture above
  mutate(IndicatorName = factor(IndicatorName, #We want to order by indicator name
                                levels = c( #Levels is used to define the order of the variables
                                  "Gross capital formation", 
                                  "Exports", 
                                  "Government Expenditure", 
                                  "Household consumption", 
                                  "Imports")))


#Next let us plot the data
ggplot(tidy_GDP_data_key_countries, #Using our dataset
       aes(x = Year, #Year on x axis
           y = USDbn, #Amount on Y Axis
           group = IndicatorName, #To let R now we are plotting by indicator
           color = IndicatorName)) + #To color by indicator
  
  #Plot type
  geom_line() +  #Line chosen
  
  #Seperate by variable
  facet_wrap(~Country) + #Country seperator 
  
  #Simplyfying theme
  theme_bw() + 
  
  #Removing X-Axis
  theme(axis.title.x = element_blank()) + 
  
  #Choosing x axis grid breaks
  scale_x_discrete(breaks = seq(from = 1970, to = 2010, by = 10)) + 
  
  #Inserting useful titles
  labs(title = "GDP components over time", 
       subtitle ="In constant 2010 US$",
       y = "Billion US$", 
       color='Components of GDP') + 

  NULL
  
  
    
```

Interestingly, the graph above shows the total GDP is especially high in the United states and especially in C - Consumptions. However this graph has some flaws, first it does not show by country relative impact of each factor (i.e. percentage would have been more useful or free y axis scales) and second it does not use net Exports. 

Let us look into this in the following sections. 

First we need to calculate the total GDP to estimate the percentage impact of each variable:

```{r}
#Let us choose our dataset
tidy_GDP_data_GDPestimate <- tidy_GDP_data %>%

#Renaming as it will be more efficient to work with shorter easy to read names    
  mutate(IndicatorName = case_when(
    IndicatorName == "Imports" ~ "Im",
    IndicatorName == "Exports" ~ "Ex",
    IndicatorName == "Gross capital formation" ~ "I",
    IndicatorName == "Government Expenditure" ~ "G",
    IndicatorName == "Household consumption" ~ "C",
    IndicatorName == "GDP" ~ "GDP")) %>%

#To compute total GDP we can widen the table
pivot_wider(names_from = IndicatorName, #Widen by indicator name
            values_from = USDbn) %>%  #Values to be taken from the USDbn amount
  
  #Lets calculate the required variables
  mutate(Net_Ex = Ex-Im, #Net Exports
         GDPestimate = Net_Ex + I + G + C, #Estimated total GDP from our variables
         GDPpercent_diff = GDPestimate/GDP) #Difference between our estimated GDP and actual GDP (stated in percent of Actual GDP)

#Let us take a look at our estimate compared to the actual GDP
GDP_diff_summary <- tidy_GDP_data_GDPestimate %>% 
  
  #Lets choose the only variable we care about which is the difference
  select(GDPpercent_diff)

#And lets print a summary of this variable
skim(GDP_diff_summary) 


```

Interestingly, our Mean GDP is 1.00 (i.e. = 100% of actual GDP) and thus on average quite similar to the actual GDP. However, there is some Standard Deviation of almost 9% i.e. our estimate does have some innacuracy. In the most extreme cases our GDP estimate is 216%  of actual recorded GDP or 45.1% of actual recorded GDP. However, these are only for the extremes.  


Next let us compute a GDP overview including net exports and by percent as the one below:

```{r gdp2, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "gdp2.png"), error = FALSE)
```

We compute such a graph in the following way:

```{r}

#First let us work with our dataset
tidy_GDP_data_GDPpercent <- tidy_GDP_data_GDPestimate %>% 
  
  #Lets filter for the countries we need
    filter(Country %in% country_list) %>% #US, Ger, India
  
  #And Lets calculate the percentage each variable is of our estimated GDP which will add to 100
  mutate(C_per = C/GDPestimate, #C in percent
         G_per = G/GDPestimate, #G in percent
         I_per = I/GDPestimate, #I in Percent
         Net_Ex_per = Net_Ex/GDPestimate) %>% #Net Exports in Percent
  
  #Next let us select only the variables we need from our dataframe.
  select(Country, Year, C_per, G_per, I_per, Net_Ex_per) %>%  #In this case we need the percentage estimates, year, and Country
  
  #To make it easier to work with the data we convert it back into long format
  pivot_longer(cols = 3:6, #Columns to be converted
               names_to = "IndicatorName", #Name of new Column with variable names
               values_to = "GDPpercent") %>% #Name of new column with Values
  
  #Now let us rename to something more readable for the chart
  mutate(IndicatorName = case_when( #Case when to say that when x holds true use y 
    IndicatorName == "Net_Ex_per" ~ "Net Exports",
    IndicatorName == "I_per" ~ "Gross capital formation",
    IndicatorName == "G_per" ~ "Government Expenditure",
    IndicatorName == "C_per" ~ "Household consumption"),
    
    #We want to reorder these factors according to the order in the above image
    IndicatorName = factor(IndicatorName, #Ordering by indicator name
                                levels = c(
                                  "Government Expenditure",
                                  "Gross capital formation",
                                  "Household consumption",
                                  "Net Exports")))

#Now we are ready to plot the data. This initial Section is the same as the one used in the previous chart
ggplot(tidy_GDP_data_GDPpercent, 
       aes(x = Year, #Year on x axis
           y = GDPpercent, #GDP on y axis
           group = IndicatorName, #To tell R we are working with indicator variables so it can seperate the datapoints
           color = IndicatorName)) + #To color by indicator
  
  #Plot type
  geom_line() + 
  
  #Seperating factor for each Chart
  facet_wrap(~Country) + 
  
  #Simplifying theme
  theme_bw() + 
  
  #Choosing x axis breaks
  scale_x_discrete(breaks = seq(from = 1970, to = 2010, by = 10)) + 
  
  #Inserting labels
  labs(title = "GDP and its breakdown at constant 2010 prices in US Dollars", 
       subtitle ="In constant 2010 US$",
       y = "Proportion",
       caption = "Source: United Nations, https://unstats.un.org/unsd/snaama/Downloads") +
  
  #Removing Titles
    theme(axis.title.x = element_blank(), #This removes the x axis title
          legend.title = element_blank()) + #This removes the legend title
  NULL


```

We see that a couple of key things from this chart. 

First and foremost, the basic structure of GDP is very similar across very different countries. Here we used India, US, and Germany but it is always the case that:

- Net exports has a small to low effect
- Household Consumption is the most important factor
- Government spending and Investment are similar in importance


However, there are some exceptions to the above which are likely driven by the countries: 

**Germany:** In Germany there have been little to no changes over time likely because the structural system of the country has stayed similar. This is with the exception if net exports which has been increasing for the past 20 or so years. This is not surprising given the large amount of exports germany has and with it being one of the largest exporters in the world.

**India:** India has seen a decline in the importance of consumer spending and and increase in the importance of investment. This could be due to the rapidly changing enviornment bringing change and large coorpoations into the country. 

**United States:** In the United states the importance of household consumption is on the rise. Likely this is because the United States has one of the world's largest GDP's per capita which naturally also drives exports down and imports up.  



# Details

- Who did you collaborate with: Karim El Asmar, Stepan Emelianenko, Nelly Gray, Ziqi Li, Ray Park, Shirley Wan, Ditlev Meulengracht
- Approximately how much time did you spend on this problem set: 10-12 hours
- What, if anything, gave you the most trouble: Remembering Group By in the chart to avoid a weird looking output 

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else? 

Yes


