---
categories:
- ""
- ""
date: "2017-10-31T22:26:09-05:00"
draft: false
image: climate.jpg
keywords: ""
slug: magna
title: Climate Change
output:
  html_document:
    theme: flatly
    highlight: zenburn
    number_sections: yes
    toc: yes
    toc_float: yes
    code_folding: show
---


```{r, setup, include=FALSE}
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


```{r load-libraries, include=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(here)
library(skimr)
library(janitor)
library(httr)
library(readxl)
library(vroom)
library(infer)
library(tidyquant)
library(rvest)
library(ggtext)
```



# Climate change and temperature anomalies 


A key metric in climate change is the *Combined Land-Surface Air and Sea-Surface Water Temperature Anomalies* in the Northern Hemisphere at [NASA's Goddard Institute for Space Studies](https://data.giss.nasa.gov/gistemp). 

We will analyze the change in climate change by looking at temperatures today compared to NASA's base period between 1951-1980. 

First we load the climate change date which gives us the deviation of the temperature in each month compared to the base period. 

```{r weather_data, cache=TRUE}

weather <- 
  read_csv("https://data.giss.nasa.gov/gistemp/tabledata_v4/NH.Ts+dSST.csv", 
           skip = 1, 
           na = "***")

```

Going forward we will work with the months only and not combined periods. We therefore setup a code that only incldudes the monthly data and year. 

We then convert the dataframe to a long format. This is usefull because it will later on let us plot the data more easily. 

1. Convert the dataframe from wide to 'long' format. Hint: use `gather()` or `pivot_longer()` function. Name the new dataframe as `tidyweather`, name the variable containing the name of the month as `month`, and the temperature deviation values as `delta`.


```{r tidyweather}

#Select the weather dataframe

tidyweather<-weather %>% 
  
  #Select the year and the twelve month variables from the `weather` dataset
  select(Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) %>%

  #Converting the dataframe to a longer format with the names from months and values from the delta. 
  pivot_longer(cols =2:13,
               names_to="Month",
               values_to="delta") 
  
  
#Print the dataframe.
tidyweather

```


Our dataframe now only has three columns which shows the year, month, and delta. The number of rows has increased from 142 to 1,704 (12x) because we have expanded the months to be on the rows instead of columns. 


## Plotting Climate Change


We will now plot the data on a time-series scatter plot. We will further add a trendline to see the development over time.  To do that, we first need to create a new variable called `date` in order to ensure that the `delta` values are plot chronologically. 


```{r scatter_plot}

#Adding a date row
tidyweather <- tidyweather %>%
  mutate(date = ymd(paste(as.character(Year), Month, "1")))


#Plotting date versus delta (difference compared to expectations)
ggplot(tidyweather, aes(x=date, y = delta))+
  
  #Scatterplot
  geom_point()+
  
  #Trendline in red without Standard error
  geom_smooth(color="red", se=F) +
  
  #Minimal theme
  theme_bw() +
  
  #Titles
  labs (
    title = "Weather Anomalies over time",
    subtitle = "Temperatures compared to the expected temperatures for NASA's base periods (1950-1980)",
    x = "Year",
    y = "Degrees difference to NASA's base period") + 
  
  #Setting breaks smaller to see more accurately the changes
  scale_y_continuous(breaks = (seq(-2, 2,0.5))) + 
  NULL


```
The plot shows that over time temperatures have rised compared to NASA's base period. This result is expected given the global warming effect which is steadily increasing. 

We would like to analyze if this effect is more pronounced in some months than other. To do this we use the same data but facet wrap by month. 

```{r facet_wrap, echo=FALSE}
 
#We will first setup a factor for the order of months we want the plot.
tidyweather$Month<-factor(tidyweather$Month,  c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

#We plot as before but color by month
ggplot(tidyweather, aes(x=date, y = delta,color=Month))+
  geom_point()+
  geom_smooth(color="red", se=F) +
  theme_bw() +
  labs (
    title = "Weather Anomalies over time by month",
    subtitle = "Temperatures compared to the expected temperatures for NASA's base periods (1950-1980)",
    x = "Year",
    y = "Degrees difference to NASA's base period") + 
  
  #To remove color legend
  guides(color=FALSE)+
  
  #Facet wrap to sort by months including free scales in order to ensure graphs can fill out the plot. 
  facet_wrap(~Month,scales="free") + 
  
  #Setting breaks smaller to see more accurately the changes
  scale_y_continuous(breaks = (seq(-2, 2,0.5))) + 
  NULL

```

This plot is interesting as it shows some months such as Nov, Dec Jan, Feb and Mar have experienced higher increases compared to other months. Since those months are generally colder and they are more exposed  to the impact of global warming. I.e. global warming leads to warmer winters more so than warmer summers.


We now set up a comparison of different time periods to see how this has changed over time. 


```{r intervals}

comparison <- tidyweather %>% 
  filter(Year>= 1881) %>%     #remove years prior to 1881
  #create new variable 'interval', and assign values based on criteria below:
  mutate(interval = case_when(
    Year %in% c(1881:1920) ~ "1881-1920",
    Year %in% c(1921:1950) ~ "1921-1950",
    Year %in% c(1951:1980) ~ "1951-1980",
    Year %in% c(1981:2010) ~ "1981-2010",
    TRUE ~ "2011-present"
  ))

#To print the table
comparison
```

This data now shows the interval of time which can be useful for plotting the data by interval. 

```{r density_plot}

#Comparing delta to time interval
ggplot(comparison, aes(x=delta, fill=interval))+
  
  #density plot with tranparency set to 20%
  geom_density(alpha=0.2) +   
  
  #theme
  theme_bw() +              
  labs (
    title = "Density Plot for Monthly Temperature Anomalies",
    y     = "Density",         #changing y-axis label to sentence case, 
    x = "Delta from base periods"
  ) + 
  
  #Setting breaks smaller to see more accurately the changes
  scale_y_continuous(breaks = (seq(0, 2,0.5))) + 
  NULL

```
What we see is that each interval slowly moves toward the right as time progresses with the current interval being much higher than the remaining. The green interval is the base period and is naturally centered around 0. 

We now proceed to look into annual anomalies instead of monthly anomalies to see if this makes any difference in our conclusions. 

```{r averaging, }

#creating yearly averages
average_annual_anomaly <- tidyweather %>% 
  
  #grouping data by Year
  group_by(Year) %>%   
  
  # use `na.rm=TRUE` to eliminate NA (not available) values 
  summarise(annual_average_delta=mean(delta, na.rm=TRUE))

#plotting the data:
ggplot(average_annual_anomaly, aes(x=Year, y= annual_average_delta))+
  geom_point()+
  
  #Fit the best fit line, using LOESS method
  geom_smooth(method="loess") +
  
  #change to theme_bw() to have white background + black frame around plot
  theme_bw() +
  labs (
    title = "Average Yearly Anomaly",
    y     = "Average Annual Delta"
  )                         


```


Not surprisingly we see a similar result from the yearly averages compared to the monthly averages. 


## Confidence Interval for `delta`

[NASA points out on their website](https://earthobservatory.nasa.gov/world-of-change/decadaltemp.php) that 

> A one-degree global change is significant because it takes a vast amount of heat to warm all the oceans, atmosphere, and land by that much. In the past, a one- to two-degree drop was all it took to plunge the Earth into the Little Ice Age.


We therefore find it interesting to look into the confidence interval of delta since 2011 to determine how close we are to the 2-degree change. We first look into this by using a formula approach to extrapolate the confidence interval

```{r, calculate_CI_using_formula}



formula_ci <- comparison %>% 
  
  #Filtering for the relevant intervalt
  filter(interval=="2011-present") %>% 
  
  # calculate summary statistics for temperature deviation (delta) 
  summarise(annual_average_delta=mean(delta,na.rm=TRUE),
            sd_delta=sd(delta,na.rm=TRUE),
            count= n(),
            t_critical = qt(0.975,count-1),
            se_delta=sd_delta/sqrt(count),
            margin_of_error= t_critical*se_delta,
            delta_low= annual_average_delta - margin_of_error,
            delta_high= annual_average_delta + margin_of_error) %>%   # calculate mean, SD, count, SE, lower/upper 95% CI
  arrange(desc(annual_average_delta)) # do we need this?

formula_ci #print out formula_CI
```

What we see is that our annual average delta since 2011 is 1.06 with a 95% confidence interval from 1.01 to 1.11. Stated in otherwords our 95% confidence interval reflects an increase in temperature of 1.01 to 1.11 degrees for the years 2011-present. 

We can use bootstrap method to see if we still maintain similar results.


```{r, calculate_CI_using_bootstrap}

# use the infer package to construct a 95% CI for delta


set.seed(1234)

boot_delta <- comparison %>%
  # choose the interval 2011-present
  filter(interval=="2011-present") %>% # choose the interval 2011-present
  
  # Specify the variable of interest
  specify(response = delta) %>%
  
  # Generate a bunch of bootstrap samples
  generate(reps = 1000, type = "bootstrap") %>%
  
  # Find the median of each sample
  calculate(stat = "mean")

#Finding the confidence intervals
percentile_ci<-boot_delta %>% 
  get_confidence_interval(level=0.95,type="percentile")

#Printing the results
percentile_ci


```
We are achieving very similar results with a confidence interval of 1.02 to 1.11 degrees above NASA's reference period. 


In sum, our analysis on temperature increases shows that temperatures have been steadily rising for the past 100 years. In addition,  the winter months have seen the largest increases in temperature. Furthermore the most recent decade is the warmest decade we have recorded to date. We know that a 2-degree change in temperature can drastically impact the climate. Our statistics shows that we are 95% confident the average current rise in temperatures is 1.01-1.11 higher than the reference period only 41 years after it ended in 1981. 


# Global warming and political views (GSS)

It is interesting to now see how global warming relates to political views in the general public.

[A 2010 Pew Research poll](https://www.pewresearch.org/2010/10/27/wide-partisan-divide-over-global-warming/) asked 1,306 Americans, "From what you've read and heard, is there solid evidence that the average temperature on earth has been getting warmer over the past few decades, or not?"

We will now analyze if there is a difference between the proportion of people who believe the world is getting warmer and their political ideology. 

We load the dataset first.

```{r, read_global_warming_pew_data}
global_warming_pew <- read_csv(here::here("data", "global_warming_pew.csv"))
```

We will first count the number of votes to get a summary of the data. 


```{r}
warming_long <- global_warming_pew %>% 
  count(party_or_ideology, response)

warming_long
  
```
We notice that some people have not responded which we will therefore filter out.

We will also be constructing three 95% confidence intervals to estimate population parameters, for the % who believe that **Earth is warming**, according to their party or ideology. 
```{r}


warming_wide <- warming_long %>% 
  
  #Taking out those who didn't answer
  subset(response!="Don't know / refuse to answer") %>% 
  
  #Widening the chart to make it easier to work with
  pivot_wider(names_from=response,
              values_from=n) %>% 
  
  #Renaming columns to make them easier to work with
  rename("Yes"="Earth is warming","No"="Not warming") %>%
  
  #manually creating CIs
  mutate(
    Total=Yes+No,
    Pr_Yes=Yes/Total,
    t_critical = qt(0.975, Total-1),
    SE=sqrt((Pr_Yes*(1-Pr_Yes))/Total),
    Lower95=Pr_Yes-t_critical*SE,
    Upper95=Pr_Yes+t_critical*SE,
  )
warming_wide

#This can also be visualized, reordering by highest percentage who believes 
ggplot(warming_wide, aes(x=reorder(party_or_ideology, Pr_Yes), y=Pr_Yes, colour=party_or_ideology)) +
  geom_point() +
  
  #Errorbar to show CI
  geom_errorbar(width=.5, aes(ymin=Lower95, ymax=Upper95)) + 
  
  labs(x=" ",
       y= "Percentage of people believing the earth is warming", 
       title="Which party or ideology has the most % believe that Earth is warming?") + 
  theme_bw()+
  
  #Flipping coordinates
  coord_flip()+
  
  #Removing legend
  theme(legend.position = "none")+
  NULL


```
It appears respondants beliefs on global warming are very heavily dependent on their political party. 


# Biden's Approval Margins

We now want to look into Biden's approval margins

```{r, cache=TRUE}
# Import approval polls data directly off fivethirtyeight website
approval_polllist <- read_csv('https://projects.fivethirtyeight.com/biden-approval-data/approval_polllist.csv') 

glimpse(approval_polllist)

```
We glimpse to understand the data and see an array of factors most importantly the approve and disapprove. 

```{r trump_margins, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "biden_approval_margin.png"), error = FALSE)
```

## Create a plot


We will now calculate the new approval rate (approve- disapprove) for each week since he got into office. We will plot this along with its 95% confidence interval. ´

We will setup a graph similar to the one below:


We plot this with the following code:

```{r}

#calculate the average net approval rate for each week
net_approval_polllist <- approval_polllist %>% 
  
  #Subgroubing by all polls to avoid overlap
  filter(subgroup == "All polls") %>% 
  
  select(enddate, approve, disapprove) %>% 
  
  #Calculating the rates
  mutate(
    net_approval_rate=approve-disapprove,
    
    #Finding Week No
    Week=isoweek(mdy(enddate)), 
    
    #use function to get the week in the year and arrange by weeks since elected
    WeekNo = Week-min(Week)) %>%   
    arrange(WeekNo) %>% 
  group_by(WeekNo) %>% 
  
  #Calculating statistics
  summarize(mean_net_approval=mean(net_approval_rate),
            count = n(),
            sd1 = sd(net_approval_rate),
            t_critical = qt(0.975, count-1),
            se_rating = sd1/sqrt(count), 
            moe = t_critical*se_rating, 
            
            #Used to plot the SE on the graph
            y_min = mean_net_approval - moe, 
            y_max = mean_net_approval + moe) 
  
#plotting the data
ggplot(net_approval_polllist, aes(x = WeekNo, y = mean_net_approval)) + 

  #Adding the scatterplot
  geom_point(color ="red") + 
  
  #Adding a trendline
  geom_smooth(se = F, method = "loess") +
  
  #Standard error with the above calculations included
  geom_ribbon(aes(xmin = 0, xmax = Inf, ymin = y_min, ymax = y_max), alpha = 0.1, color = "red") + 
  
  #Adding the path between the scatterplot
  geom_path(color ="red") + 
  
  #Adding an orange line at 0 
  geom_hline(aes(yintercept=0.0,),linetype=1, color = "Orange", size = 2) + 
  theme_classic()+
  scale_y_continuous(breaks=seq(-20,12,2))+
  theme(panel.grid.major.x=element_line(size=0.5),panel.grid.minor.y=element_line(size=0.5),panel.grid.major.y=element_line(size=0.5))+
  labs(title = "Estimating Approval Margin (approve-disapprove) for Joe Biden",
       subtitle = "Weekly average of polls",
       x = "Weeks since election", 
       y = "Average Approval Margin (Approve - Disapprove)")



#  theme(panel.grid.minor = element_line(colour="blue", size=1.5))
```
Interestingly, Bidens approval ratings have fallen steadily since his election.

## Compare Confidence Intervals

Moreover the confidence interval has become wider likely because 1) the number of polls used for the SD calculation is larger and 2) polls are becoming more similar in their approval rate. 


# Challenge 1: Excess rentals in TfL bike sharing

We first download the TfL data on how many bikes were hired every single day. We can get the latest data by running the following

```{r, get_tfl_data, cache=TRUE}
url <- "https://data.london.gov.uk/download/number-bicycle-hires/ac29363e-e0cb-47cc-a97a-e216d900a6b0/tfl-daily-cycle-hires.xlsx"

# Download TFL data to temporary file
httr::GET(url, write_disk(bike.temp <- tempfile(fileext = ".xlsx")))

# Use read_excel to read it as dataframe
bike0 <- read_excel(bike.temp,
                   sheet = "Data",
                   range = cell_cols("A:B"))

# change dates to get year, month, and week
bike <- bike0 %>% 
  clean_names() %>% 
  rename (bikes_hired = number_of_bicycle_hires) %>% 
  mutate (year = year(day),
          month = lubridate::month(day, label = TRUE),
          week = isoweek(day))
```

We can easily create a facet grid that plots bikes hired by month and year.

```{r tfl_month_year_grid, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_distributions_monthly.png"), error = FALSE)
```

Interestingly may and June have fallen a lot in bike rentals in 2020. This is likely because of less toursim due to COVID-19 

We now want to reproduce some graphs from this data. 

The first.


```{r tfl_absolute_monthly_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_monthly.png"), error = FALSE)
```


Is reproduced through the following

```{r, fig.width = 10, fig.height = 10}
#Filtering data from 2016 and forward
bike_1 <- bike %>% 
  filter(year>=2016) %>% 
  group_by(year,month) %>% 
  summarize(
    mean_bikehired=mean(bikes_hired)
  )

#filter for adding the blue line
bike_blue <- bike %>% 
  filter(year>=2016 & year<=2019) %>% 
  group_by(month) %>% 
  summarize(
    mean_blue=mean(bikes_hired)
  )

#combine to a join dataset
bike_set <- left_join(bike_1, bike_blue, by ="month")

#Vector to be used for the axis tick marks
x_month <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


#plot the data
ggplot(bike_set,aes(x=as.numeric(month)))+
  geom_line(aes(y=mean_bikehired),colour="black",size=0.1)+
  geom_line(aes(y=mean_blue),colour="blue",size=0.5)+
  facet_wrap(~year,nrow=2)+
  
  #This is used to calculate a ribbon that shades an area above and or below the mean
  geom_ribbon(aes(xmin = 0, xmax = Inf,ymin=mean_blue,ymax=mean_blue+ifelse(mean_bikehired>mean_blue, mean_bikehired-mean_blue, 0),),fill="darkgreen",alpha=0.4)+
  geom_ribbon(aes(xmin = 0, xmax = Inf,ymax=mean_blue,ymin=mean_blue+ifelse(mean_bikehired>mean_blue, 0,mean_bikehired-mean_blue)),fill="darkred",alpha=0.4) +
  
  
  labs(
    title = "Monthly changes in TfL bike rentals", 
    subtitle = "change from monthly average shown in blue and calculated between 2016-2019", caption= "Source: TfL, London Data Store",
    x="", 
    y="Bike Rentals"
  ) +
  theme_bw()+
  
  #Adding a scale for the months
  scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=x_month)+  
  
  #Simplifying the theme
  theme(panel.border=element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(color="white",fill="white")  )+
  
  NULL


```
Each year look similar in terms of bike rentals with especially 202 standing out as having low bike rentals in the early part of the year.


The second one looks at percentage changes from the expected level of weekly rentals. The two grey shaded rectangles correspond to Q2 (weeks 14-26) and Q4 (weeks 40-52).

```{r tfl_percent_change, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "tfl_weekly.png"), error = FALSE)
```

We produce this plot below

```{r}


#Calculating the bikes hired per week
bike_2 <- bike %>% 
  filter(year>=2016) %>% 
  group_by(year,month,week) %>% 
  summarize(
    week_bikehired=mean(bikes_hired)
  ) %>% 
  
  #We add a new year variable which says if it is January 1st but in week 53 the datapoint is then for the past year (i.e. a datapoint in 1st January but week 53 will be shown in the end of 2015 rather than the start of 2016)
  mutate(year_new = ifelse(week>51 & month == "Jan", year-1, year)) %>% 
  filter(year_new > 2015)


#Setting up the weekly averages in 2016-2019
#filter for week bike rental 
bike_average <- bike %>% 
  filter(year>=2016 & year<=2019) %>% 
  group_by(week) %>% 
  summarize(
    mean_2016_2019=mean(bikes_hired)
  )


#Joining the datasets
bike_week <- left_join(bike_2, bike_average, by ="week")

#Setting up the calculations to color the rug marks and pct changes. 
bike_week2 <- bike_week %>% 
  mutate(pct_change = week_bikehired/mean_2016_2019-1, 
         color_id = ifelse(pct_change<0, 1, 0))


#Plotting the hart
ggplot(bike_week2, aes(x = week, y = pct_change)) + 
  geom_line(color = "black", size = 0.2) + 
  
  #Ribbons which shade in green and red
  geom_ribbon(aes(xmin = 0, xmax = Inf,ymin=0,ymax=ifelse(pct_change>0, pct_change, 0)),fill="green",alpha=0.4)+
  geom_ribbon(aes(xmin = 0, xmax = Inf,ymax=0,ymin=ifelse(pct_change<0, pct_change, 0)),fill="red",alpha=0.4) +
  
  #Rug which colors based on a the variable computed above and on the x axis
  geom_rug(mapping = aes(color = factor(color_id)), sides = "b") +
  
  #Setting the rug to green and red
  scale_color_manual(values = c("green", "red")) +
  
  #By year
  facet_wrap(~year_new,nrow=2)+
  
  theme_bw() + 
  
  #Removing the legend
  guides(color=FALSE) + 
  
  #Scaling the axis and setting week numbers for the x axis
  scale_y_continuous(labels = scales::percent) + 
  scale_x_continuous(breaks = c(13,26,39,53)) + 
  
  #Simplifying the theme
  theme(panel.border=element_blank(),
        panel.background = element_blank(),
        strip.background = element_rect(color="white",fill="white"), 
        axis.title.y = element_blank()) + 
  
  #Adding shading by week No
  annotate("rect", xmin=14, xmax=26, ymin=-Inf, ymax=Inf, alpha=0.2, fill="Grey") +
  annotate("rect", xmin=40, xmax=53, ymin=-Inf, ymax=Inf, alpha=0.2, fill="Grey") + 
  
  labs(title = "Weekly changes in Tfl bike rentals", 
       subtitle = "Change from weekly averages calculated between 2016-2019",
       x = "Week") + 
  NULL 

  


```


We see a quite stable trend of bike rentals in the early years with some odd spikes later on. 

Should you use the mean or the median to calculate your expected rentals? Why?

We believe we need to use median figures to account for outliers in data and look at the really expected number not affected by some extreme past numbers. The same logic appeals to finance markets - people tend to use median figures to form a forecasts consensus, fair multiple for a firm, or margin aim. This technique excludes all the extreme values because it accounts only for numbers in centre of sorted list. We want to use it here because we had extreme negative numbers in Q2 of 2020 due to COVID-19 outbreak and further during the sharp recovery of demand in H2 of 2020. Since we observe the graphs from 2016 to 2020 are normally distributed, we can also use the mean to calculate the expected rentals. However, we should be aware that by using the mean value, our calculated values can be easily affected by any extreme data point within the dataset.

# Challenge 2: How has the CPI and its components changed over the last few years?

We would now like to reproduce the following:

```{r cpi_all_components_since_2016, echo=FALSE, out.width="100%"}
knitr::include_graphics(here::here("images", "cpi_components_since_2016.png"), error = FALSE)
```


We do this below: 

```{r}

#Adding the URL to download from
url <- "https://fredaccount.stlouisfed.org/public/datalist/843"


# get tables from the page
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")


#Putting the tables in Dataframes
indexes <- map(tables, . %>% 
             html_table(fill=TRUE)%>% 
             janitor::clean_names())


# Taking out the 2nd table which contains the series IDs
table2 <- indexes[[2]]

#Pulling out the series as a vector
series_ids <- as.vector(pull(table2, series_id))    

#Using this vector to import data from tidyquant
data_import <- series_ids %>% tidyquant::tq_get(get = "economic.data", from =  "2016-01-01") 

#Renaming columns to only include the category (i.e. deleting some text strings)
names_match <- table2 %>%  select(title, series_id) %>% 
  rename(symbol = series_id) 
names_2 <- names_match %>%  mutate(new_names =str_replace(as.vector(pull(names_match, title)), "Consumer Price Index for All Urban Consumers: ", ""))
names_3 <- names_2 %>% mutate(final_names = str_replace(as.vector(pull(names_2, new_names)), "in U.S. City Average", ""))

#Combining the dataframes data import and the newly created names
combined_import <- left_join(names_3, data_import, by = "symbol")

#setting up a dataframe which calculates the yearly change
dataplot <- combined_import %>% group_by(final_names) %>% 
  mutate(year_change = price/lag(price, 12) - 1, 
         year = year(date),
         
         #Colorpoint is used to color above and below 0
         colorpoint= ifelse(year_change>0,"Above","Below")) %>% 
  
  #Removing N/A variables from the lagged calculation
  na.omit() 

#Sorting names to find the order of the facet wrap
sorted_names <- dataplot %>% 
  group_by(final_names) %>% 
  summarise(order = mean(year_change)) %>% 
  arrange(desc(order)) %>% 
  
  #Removing all items which needs to be first
  filter(final_names != "All Items ")

#Taking the names out as a vector
sorted_names_vector <- as.vector(pull(sorted_names, final_names))

#Adding All items as the first in the vector
final_sort <- append("All Items ", sorted_names_vector)
              
#Adding a factor for the facet wrap sorting
dataplot2 <- dataplot
dataplot2$final_names <- factor(dataplot2$final_names, levels = c(final_sort))

#plotting the data colored by whether it is above or below 0 calculated above
ggplot(dataplot2, aes(x = date,y = year_change,colour=colorpoint)) + 
  geom_point(size = 0.3) + 
  geom_smooth(se=F, color = "grey", size =0.5) + 
  facet_wrap(~final_names, scales = "free", ncol=7) + 
  scale_y_continuous(labels = scales::percent) +
  
  #Adding labels with 1 year
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  
  #Manually setting colors for scatterplot
  scale_color_manual(values=c("pink","lightblue"))+
  
  labs(title="<b> Yearly change of US CPI (All Items) and its components</b><br>
       <span style = 'font-size:6pt'>YoY change being <span style='color:#DB5E61'>positive</span> or <span style='color:#7abaf3'>negative</span></span>",
       y="YoY % Change",
       x = "Year",
       caption ="Data from St.Louis Fed FRED \nhttps://fredaccount.stlouisfed.org/public/datalist/843" )+
  
  #Removing legend
  guides(color = F) +
  
  theme_bw() +
  
  #Making the text smaller
  theme(
    plot.title = element_markdown(lineheight = 1.1), 
    text = element_text(size = 5))

  NULL

```


This graph however has too much information se we conclude with some of the key variables.

```{r}
#Create the subset of the major categories
#We don't have the data of , "Medical care", "Education and communication", "Recreation"

major_CPI <- dataplot2 %>% 
  filter(final_names %in% c("Housing ","Transportation ", "Food and Beverages ","Apparel "))

#the importance is ranked as "Food and Beverages ","Housing","Apparel","Transportation"
major_CPI$new = factor(major_CPI$final_names, levels=c("Food and Beverages ","Housing ","Apparel ","Transportation ")) 

#Same chart as before
ggplot(major_CPI, aes(x = date,y = year_change,colour=colorpoint)) + 
  geom_point(size = 3) + 
  geom_smooth(se=F, color = "grey", size =1) + 
  facet_wrap(~new,scales = "free",ncol=4) + 
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 years", date_labels = "%Y") + 
  scale_color_manual(values=c("pink","lightblue"))+
  
  labs(data= major_CPI,
       title="<b> Yearly change of US CPI (All Items) and its components</b><br>
       <span style = 'font-size:12pt'>YoY change being <span style='color:#DB5E61'>positive</span> or <span style='color:#7abaf3'>negative</span></span>",
    y="YoY % Change",
    x = "Year",
    caption ="Data from St.Louis Fed FRED \nhttps://fredaccount.stlouisfed.org/public/datalist/843" )+
  
  guides(color = F) +
  
  theme_bw() +
  theme(
    plot.title = element_markdown(lineheight = 1.1), 
    text = element_text(size = 10))
  NULL
```


Interestingly a lot of these categories show how prices fell in COVID but that inflation has spiked a lot since then. This is with the exception of food and beverages. 


# Details

- Who did you collaborate with: Karim El Asmar, Stepan Emelianenko, Nelly Gray, Ziqi Li, Ray Park, Shirley Wan, Ditlev Meulengracht
- Approximately how much time did you spend on this problem set: 10-12 hours
- What, if anything, gave you the most trouble: Approve - Disapprove ratign as SD and data seems to be different. We calcualted SD based on the number of polls. 


> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else? 

Yes

