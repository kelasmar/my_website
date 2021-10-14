---
categories:
- ""
- ""
date: "2017-10-31T21:28:43-05:00"
description: ""
draft: false
image: movie.jpg
keywords: ""
slug: drinking
title: Drinking behavior, Movie ratings, Financial returns, German Polls

---


```{r setup, echo=FALSE}
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


# Loading necessary packages:

Code loads the necessary packages to analyze the data and present the charts. (Not included in HTML)

```{r load-libraries, echo=FALSE, message=FALSE, warning=FALSE}
library(tidyverse)  # Load ggplot2, dplyr, and all the other tidyverse packages
library(mosaic)
library(ggthemes)
library(lubridate)
library(fivethirtyeight)
library(here)
library(skimr)
library(janitor)
library(vroom)
library(tidyquant)
library(rvest) # to scrape wikipedia page
```



# Where Do People Drink The Most Beer, Wine And Spirits?

Back in 2014, [fivethiryeight.com](https://fivethirtyeight.com/features/dear-mona-followup-where-do-people-drink-the-most-beer-wine-and-spirits/) published an article on alcohol consumption in different countries. The data `drinks` is available as part of the `fivethirtyeight` package which will be used here.

We load the 'fivethirtyeight' package and drinks dataset with the code below:


```{r load_alcohol_data}
library(fivethirtyeight)
data(drinks)


# or download directly
alcohol_direct <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/alcohol-consumption/drinks.csv")

```

Following this we can skim and glimpse the data in order to see if there are any missing values we should worry about and to see the data and variable types.

```{r glimpse_skim_data}
skim(alcohol_direct)
glimpse(alcohol_direct)

```

The overview seems to have a comprehensive number of variables. However, some countries have zero total consumption, hence we believe the data is not comprehensive and complete. The variable types are numeric and characters for the country variable.

Below a plot is presented to show the top 25 beer drinking countries:

```{r beer_plot}
alcohol_direct %>% #loading the dataset
  slice_max(order_by = beer_servings, n=25) %>% #sorting by top 25 countries
  ggplot(aes(x = beer_servings, y = reorder(country, beer_servings))) + #Choosing variables on each axis and sorting to have the highest countries on top.
  geom_col(fill = "Orange") + #Making the graph orange
  labs(title = "Namibia is the most beer-consuming country", #This line and the ones below add titles and axis
       subtitle = "Beers consumed by country, top 25 countries woldwide, 2010", 
       x = "Beer servings per person",
       y = "Country",
       caption = "https://fivethirtyeight.com/features/dear-mona-followup-where-do-people-drink-the-most-beer-wine-and-spirits/") + 
  theme_bw() + #Making a more simple theme
  NULL

```
From the graph it is evident that Namibia drinks the most beers per capita and that some countries drink up toward one beer per day per capita. 


We now want to see the top 25 wine producing countries. 

```{r wine_plot}

alcohol_direct %>% #Loading the dataset
  slice_max(order_by = wine_servings, n=25) %>% #Sorting for top 25 countries
  ggplot(aes(x = wine_servings, y = reorder(country, wine_servings))) + #Choosing variable types and sorting the variables by servings
  geom_col(fill = "Darkred") + #Making the bars dark red fitting with the wine theme
  labs(title = "France is the most wine-consuming country", #This section adds titles
       subtitle = "Wine consumed by country, top 25 countries worldwide, 2010", 
       x = "Wine servings per person",
       y = "Country",
       caption = "https://fivethirtyeight.com/features/dear-mona-followup-where-do-people-drink-the-most-beer-wine-and-spirits/") + 
  theme_bw() + #Adding a more simple theme
  NULL



```

We see that especially France and Portugal drinks a lot of wine per capita also equivalent of a unit per day per person like the top beer drinking countries. There is a bit further span on the top 25 wine-drinking countries where the lowest countries are as low as around 160 compared to 240 for beer drinking countries. 

Finally, we make a plot that shows the top 25 spirit consuming countries

```{r spirit_plot}

alcohol_direct %>% #choosing the data
  slice_max(order_by = spirit_servings, n=25) %>% #sorting by top 25 countries
  ggplot(aes(x = spirit_servings, y = reorder(country, spirit_servings))) + #plotting variables and reordering by top max on top
  geom_col(fill = "Darkgreen") + #Making the plot dark green
  labs(title = "Grenada is the most spirit-consuming country",#adding titles
       subtitle = "Spirit consumed by country, top 25 countries worldwide, 2010", 
       x = "Spirit servings per person",
       y = "Country",
       caption = "https://fivethirtyeight.com/features/dear-mona-followup-where-do-people-drink-the-most-beer-wine-and-spirits/") + 
  theme_bw() + 
  NULL


```
Here we see that Grenada is the highest spirits drinking country but also the country with the most units for a single alcohol type with approximately 430 units. 

While these three charts show the overall consumption of different types of alcoholic beverages, the top 25 countries differ highly by category because each country has a high preference for one certain type of alcohol. For the same reason, we cannot infer whether these countries are the highest alcohol-consuming countries overall. Furthermore, it seems that within all categories the top 25 countries drink approximately 150-370 servings. This is with the exception of Grenada where spirit servings are above 400 units. This could likely indicate a preference for spirits also could be due to a lack of availability of other alcohol types in the country.

In addition to the above, the graphs show units per person so we cannot infer which country has the highest total consumption of units. For example although Grenada has the highest number of units per person their total consumption can be lower than that of other countries because of the low population. 

Furthermore, this data shows per capita consumption, however, it would be interesting to dive deeper into how this is segmented throughout the countries, i.e. if certain segments have much higher consumption than others. 

The data is from 2010 and consumption might have changed since then.


# Analysis of movies- IMDB dataset

We will look at a subset sample of movies, taken from the [Kaggle IMDB 5000 movie dataset](https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset)

The below code loads the dataset  
  
```{r load_movies, warning=FALSE, message=FALSE}

movies <- read_csv(here::here("data", "movies.csv"))
glimpse(movies) # glimpse the dataset to understand the dataset further

```

Besides the obvious variables of `title`, `genre`, `director`, `year`, and `duration`, the rest of the variables are as follows:

- `gross` : The gross earnings in the US box office, not adjusted for inflation
- `budget`: The movie's budget 
- `cast_facebook_likes`: the number of facebook likes cast members received
- `votes`: the number of people who voted for (or rated) the movie in IMDB 
- `reviews`: the number of reviews for that movie
- `rating`: IMDB average rating 

## We wanna further use our data import, inspection, and cleaning skills to answer the following:

- Are there any missing values (NAs)? Are all entries distinct or are there duplicate entries? 

The below code helps us understand the dataset. 

```{r skim movies}
skim(movies)
```

Although we have 2907 titles, we have 2961 rows for the data. Therefore, we believe there are duplicate entries in the dataset. There are no missing values according to the skim function. 

- Produce a table with the count of movies by genre, ranked in descending order
```{r count of movies by genre}
genre <- movies %>% # choosing the movie dataset
  group_by(genre) %>% #grouping by genre
  summarize(Movie_count = count(genre)) %>%  #Counting the number of movies in each genre
  arrange(desc(Movie_count)) #Sorting the dataset descending by movie count
genre
```


- We then produce a table with the average gross earning and budget (`gross` and `budget`) by genre. We calculate a variable `return_on_budget` which shows how many $ did a movie make at the box office for each $ of its budget. We also rank genres by this `return_on_budget` in descending order


```{r movies by gross revenue and budget}
genre <- movies %>% #choosing the movie dataset
  group_by(genre) %>% #grouping by genre
  summarise(average_gross = mean(gross), #adding the mean gross revenue  
            average_budget = mean(budget)) %>% #adding the mean budget 
  mutate(return_on_budget = average_gross/average_budget) %>% # calculating the revenue to budget ratio
  arrange(desc(return_on_budget)) #sorting descending
genre
```

We see that musical and family have the highest return on budget. For musical, this might be because of its low average budget.


- We then produce a table that shows the top 15 directors who have created the highest gross revenue in the box office. We show the total gross amount, the mean, median, and standard deviation per director.



```{r top directors} 
director <- movies %>% #choosing the dataset
  group_by(director) %>% #filtering by director
  summarise(mean_gross = mean(gross), #adding the mean gross revenue column
            median_gross = median(gross), #adding the median gross revenue column
            SD_gross = sd(gross), #adding the SD on gross revenue column
            total_gross = sum(gross)) %>% #adding the total gross revenue column
    slice_max(order_by = total_gross, n=15) #choosing the top 15 directors by total gross revenue
director #printing the table

```
Surprisingly, Steven Spielberg has much higher total gross revenue than other directors and almost twice as much as the 2nd highest grossing director, Michael Bay. 

- Finally, ratings. We produce a table that describes how ratings are distributed by genre. We show the mean, but also, min, max, median, SD and a histogram that visually shows how ratings are distributed.

```{r movie ratings}
Rating <- movies %>% #Choosing the dataset and adding variable Rating
  group_by(genre) %>% #Grouping by Genre
  summarise(mean = mean(rating),#Adding average
            min = min(rating), #Adding min
            max = max(rating), #Adding max
            median = median(rating), #Adding median
            SD = sd(rating)) %>% #Adding Standard Deviation
  arrange(desc(mean)) #aRranging by mean
ggplot(Rating, aes(x = mean, y = reorder(genre, mean), fill = mean)) + #Plotting the rating sorted by mean rating
  geom_col() + #Choosing a column plot
  theme_bw() + #simplifying the theme
  labs(title = "The Biography and Crime genres have the highest average rating", #Below rows add useful titles
       subtitle = "Movie ratings by genre", 
       x = "Average rating", 
       y = "Genre", 
       caption = "https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset") + 
  guides(fill=FALSE) + 
  NULL
Rating #This prints the table
```


Interestingly, Biography and Crime have the highest average rating. In contrast, western and thrillers rank poorly.

## We now Use `ggplot` to answer the following

  - We examine the relationship between `gross` and `cast_facebook_likes`. We produce a scatterplot and discuss whether the number of facebook likes that the cast has received is likely to be a good predictor of how much money a movie will make at the box office. 
  
  
```{r gross_on_fblikes}
# Plotting a graph with both variables to study the relationship

ggplot(movies, aes(cast_facebook_likes,gross))+
  geom_point(alpha = 0.3, color = "lightblue")+
  scale_x_log10()+ # Using log scale to fit smoother the data with outliers
  scale_y_log10()+ # Using log scale to fit smoother the data with outliers
  
  # We see that variables are positively correlated and add the corresponding title to reflect this finding
  # Additionally, we add a subtitle on what we are doing, name the axes, and show the data source in the caption
  
  labs(title = "Cast facebook likes is positively correlated to gross revenue",
       subtitle="Examining the relationship between gross revenue and the number of facebook likes that the cast has received",
       x="Cast number of facebook likes received",
       y="Gross revenue of movies",
       caption = "https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset")+
  
  # Adding a linear regression line and setting a bw theme
  
  stat_smooth(method = "lm") +
  theme_bw()+
  NULL
```

The number of facebook likes received is positively correlated with the gross revenue of the movies as is evident from the chart and regression showing facebook likes on the X-axis and gross revenue on the Y-axis. 


  - We now examine the relationship between `gross` and `budget`. We produce a scatterplot and discuss whether budget is likely to be a good predictor of how much money a movie will make at the box office.

```{r gross_on_budget}
# Plotting a graph with both variables to study the relationship

ggplot(movies, aes(budget,gross))+
  geom_point(alpha = 0.3, color = "lightblue")+
  scale_x_log10()+ # Using log scale to fit smoother the data with outliers
  scale_y_log10()+ # Using log scale to fit smoother the data with outliers
  
  # We see that variables are positively correlated and add the corresponding title to reflect this finding
  # Additionally, we add a subtitle on what we are doing, name the axes, and show the data source in the caption  
  
  labs(title = "Gross revenue is positively correlated to movie budget",
       subtitle="Examining the relationship between gross revenue and movie budget",
       x="Movie budget",
       y="Gross revenue of movies",
       caption = "https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset")+
    
  # Adding a linear regression line and setting a bw theme
  
  stat_smooth(method = "lm") +
  theme_bw()+
  NULL

```
  
Gross revenue of movies is positively related to the associated budget of the movies. Consequently movie budget is a good predictor of gross revenue. 

  - We now examine the relationship between `gross` and `rating`. We produce a scatterplot, faceted by `genre` and discuss whether IMDB ratings are likely to be a good predictor of how much money a movie will make at the box office. We also check if there is is anything strange in this dataset.

```{r gross_on_rating}

# Plotting a graph with both variables to study the relationship

ggplot(movies, aes(x = rating, y = gross, color = genre)) + 
  geom_point(alpha = 0.3) + 
  
  # Adding a linear regression line
  
  stat_smooth(method = "lm") + 
  
  # Dividing graphs based on genre to evaluate the relationship for every genre
  
  facet_wrap("genre", scales = "free", ncol = 5) + 
  
  # Adjusting the scale to show everything in mn on y-axis
  
  scale_y_continuous(labels = scales::label_number_si())+
  theme_bw() +
  
  # Setting a bw theme (above), removing the legend, adding title on the degree of relationship
  # Outlining the subtitle to reflect the purpose of the graphs, naming axes, and adding a caption with the data source
  
  theme(legend.position = "none") +
  labs(title = "Most but not all genres have a positive relationship between gross revenue and ratings",
       subtitle="Examining the relationship between gross revenue and rating of movies by genre",
       x="Movie rating",
       y="Gross revenue of movie", 
       caption = "https://www.kaggle.com/carolzhangdc/imdb-5000-movie-dataset") + 
  NULL

```
For most genres rating is a good predictor of gross revenue. However, there are some general genres with only a few datapoints to infer the relationship. Therefore we cannot determine this relationship for these genres with high confidence. The genres include Thriller, Western, Musical, and Romance. In addition its interesting to see that the higher ratings documentaries have the lower gross revenue they have, i.e. a negative relationship between the two. 


# Returns of financial stocks


> Useful material can be found on [finance data sources](https://mfa2022.netlify.app/reference/finance_data/). 

We will use the `tidyquant` package to download historical data of stock prices, calculate returns, and examine the distribution of returns. 
he file `nyse.csv` contains 508 stocks listed on the NYSE, their ticker `symbol`, `name`, the IPO  (Initial Public Offering) year, and the sector and industry the company is in.


```{r load_nyse_data, message=FALSE, warning=FALSE}
nyse <- read_csv(here::here("data","nyse.csv")) #loading the data
glimpse(nyse) #Glimsing the data to understand the dataset
```
We see a tabe of characters such as ticker, IPO year and more of different companies on the NYSE

Based on this dataset, we create a table and a bar plot that shows the number of companies per sector, in descending order

```{r companies_per_sector}

comp_per_sec <- nyse %>% #choosing the correct dataset
  group_by(sector) %>% #grouping by sector
  summarize(Number_of_companies = count(sector)) %>% #counting the number of companies in each sector
  arrange(desc(Number_of_companies)) #arranging the dataset descending
comp_per_sec

ggplot(comp_per_sec,aes(x=Number_of_companies,y=reorder(sector,Number_of_companies)))+ #Plotting the dataset with companies in each sector
  geom_col(fill="pink")+ #coloring the dataset pink
  labs(title="Number of companies per sector in the NYSE", #adding relevant titles
       x="Number of companies",
       y="Sector", 
       caption = "https://mfa2022.netlify.app/reference/finance_data/")+
  theme_bw() + #adding a minimalistic theme. 
  NULL


```

We see that the most common company sectors in the NYSE are Finance and Consumer services and the least common sectors are Transportation and Consumer Durables. 


Next, we look at the [Dow Jones Industrial Aveareg (DJIA)](https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average) stocks and their ticker symbols and download some data. Besides the thirty stocks that make up the DJIA, we  also add `SPY` which is an SP500 ETF (Exchange Traded Fund). The following code 


```{r tickers_from_wikipedia}

djia_url <- "https://en.wikipedia.org/wiki/Dow_Jones_Industrial_Average"


#get tables that exist on URL
tables <- djia_url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called djia. 
# Use purr::map() to create a list of all tables in URL
djia <- map(tables, . %>% 
               html_table(fill=TRUE)%>% 
               clean_names())


# constituents
table1 <- djia[[2]] %>% # the second table on the page contains the ticker symbols
  mutate(date_added = ymd(date_added),
         
         # if a stock is listed on NYSE, its symbol is, e.g., NYSE: MMM
         # We will get prices from yahoo finance which requires just the ticker
         
         # if symbol contains "NYSE*", the * being a wildcard
         # then we jsut drop the first 6 characters in that string
         ticker = ifelse(str_detect(symbol, "NYSE*"),
                          str_sub(symbol,7,11),
                          symbol)
         )

# we need a vector of strings with just the 30 tickers + SPY
tickers <- table1 %>% 
  select(ticker) %>% 
  pull() %>% # pull() gets them as a sting of characters
  c("SPY") # and lets us add SPY, the SP500 ETF

```

Now let us downlaod prices for all 30 DJIA consituents and the SPY ETF that tracks SP500 since January 1, 2020


```{r get_price_data, message=FALSE, warning=FALSE, cache=TRUE}
# Notice the cache=TRUE argument in the chunk options. Because getting data is time consuming, # cache=TRUE means that once it downloads data, the chunk will not run again next time you knit your Rmd

myStocks <- tickers %>% 
  tq_get(get  = "stock.prices",
         from = "2000-01-01",
         to   = Sys.Date()) %>% # Sys.Date() returns today's price
  group_by(symbol) 

glimpse(myStocks) # examine the structure of the resulting data frame
```

Financial performance analysis depend on returns; If I buy a stock today for 100 and I sell it tomorrow for 101.75, my one-day return, assuming no transaction costs, is 1.75%. So given the adjusted closing prices, our first step is to calculate daily and monthly returns.


```{r calculate_returns, message=FALSE, warning=FALSE, cache=TRUE}
#calculate daily returns
myStocks_returns_daily <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "daily", 
               type       = "log",
               col_rename = "daily_returns",
               cols = c(nested.col))  

#calculate monthly  returns
myStocks_returns_monthly <- myStocks %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly", 
               type       = "arithmetic",
               col_rename = "monthly_returns",
               cols = c(nested.col)) 

#calculate yearly returns
myStocks_returns_annual <- myStocks %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "yearly", 
               type       = "arithmetic",
               col_rename = "yearly_returns",
               cols = c(nested.col))
```

We now create a table where we summarise monthly returns for each of the stocks and `SPY`; min, max, median, mean, SD.

```{r summarise_monthly_returns}

#First we pick the monthly returns dataset
summarise_monthly_returns <- myStocks_returns_monthly %>% 
  
  #Grouping by ticker (company)
  group_by(symbol) %>% 
  
  #Adding Min, Max, Median, Mean, and SD
  summarize(min_return=min(monthly_returns),
            max_return=max(monthly_returns),
            median_return=median(monthly_returns),
            mean_return=mean(monthly_returns),
            SD=sd(monthly_returns)) %>% 
  
  #Arranging by highest mean return
  arrange(desc(mean_return))


#Printing the table
summarise_monthly_returns

```



We now plot a density plot, using `geom_density()`, for each of the stocks

```{r density_monthly_returns, fig.height=20}

#First we choose the monthly return data and plot return for each company
ggplot(myStocks_returns_monthly, aes(x=monthly_returns, fill = symbol))+
  
  #We then add a density curve with a black outline
  geom_density(color = "black")+
  
  #This is used to seperate each company
  facet_wrap(~symbol, ncol = 2)+
  scale_x_continuous(labels = scales::percent) +
  
  #Adding useful titles
  labs(title = "Return observations by company since January 2000", 
       x = "Monthly return",
       y = "Frequency") + 
  
  #Removeing legens
  guides(fill=FALSE) + 
  
  #Adding a simplified theme
  theme_bw() + 
  
  #Adding a line at x=0 to make the graphs more readable and understand if their observations are above or below 0% return. 
  geom_vline(xintercept = 0, alpha = 0.3, linetype = "dashed", labs = "0%")  +
  annotate("text", label = "0%", x = 0.10, y = 11, size = 4, colour = "grey")+
  NULL



```

From this plot, we can conclude that AAPL is the riskiest stock because it has the highest standard deviation (based on skim function) and low kurtosis.  ON the other hand, SPY is the least risky stock as it has the lowest standard deviation with high kurtosis. The reason for Apple to be the riskiest as it is an IT company and, probably, has the highest beta to the market. Recent market trend shows that tech companies are the most vulnerable to changes on the market with higher frequency of extremely high and low returns over time. The S&P Index has the lowest degree of risk since it is diversified with multiple stocks inside of the index, hence the deviation of it is low
   

Finally, we make a plot that shows the expected monthly return (mean) of a stock on the Y axis and the risk (standard deviation) in the X-axis. We use the `ggrepel::geom_text_repel()` to label each stock

```{r risk_return_plot}

#Using our previous table to choose mean return versus standard deviation
ggplot(summarise_monthly_returns,aes(y=mean_return,x=SD,label=symbol, size=3))+
  
  #Adding it in a scatter plot
  geom_point(color="red", alpha = 0.4)+
  
  #Adding titles
    labs(title = "Expected monthly return of a stock based on its risk", 
       x = "Standard Deviation",
       y = "Mean Returns") +
  
  #Y axis in %
  scale_y_continuous(labels = scales::percent) +
  
  #Adding a trendline
  stat_smooth(method = "lm", size=1, se = FALSE) +
  
  #Removing legend
  guides(size=FALSE) +
  
  #Adding labels
  ggrepel::geom_text_repel()+
  
  #Simplifying theme
  theme_bw() +
  
  NULL
```

Based on the plot and regression line we have designed, we observe that CSCO had a low mean return with a high standard deviation. We believe that any stock positioned below our regression line should be considered as a high risk low expected return equities. Furthermore, WMT and CSCO share the same level of expected returns while CSCO has a higher standard deviation compared to WMT, hence we would prefer Walmart stock over Cisco one based on this data of past returns


# Is inflation transitory?


> Useful the material on [downloading economic data from the FRED](https://mfa2022.netlify.app/reference/finance_data/#data-from-the-federal-reserve-economic-data-with-tidyquant). 

A recent study by the Bank for International Settlements (BIS) claimed that the [Current Inflation Spike Is Just Transitory](https://www.bloomberg.com/news/articles/2021-09-20/current-inflation-spike-is-just-transitory-new-bis-study-argues). As the article says, 

> The surge in inflation seen across major economies is probably short lived because it’s confined to just a few sectors of the economy, according to the Bank for International Settlements. 

> New research by the BIS’s Claudio Borio, Piti Disyatat, Egon Zakrajsek and Dora Xia adds to one of the hottest debates in economics -- how long the current surge in consumer prices will last. Both Federal Reserve Chair Jerome Powell and his euro-area counterpart Christine Lagarde have said the pickup is probably transitory, despite a snarled global supply chain and a spike in energy prices. 

We here have to download data for CPI and the 10 year bill and produce the following graph -> We downloaded the data for for CPI and the 10-year T-Bill and produce graphs on the relation between these variables


```{r cpi_10year, echo=FALSE, out.width="90%"}
knitr::include_graphics(here::here("images", "cpi_10year.png"), error = FALSE)
```


The relevant indicators we will use the replicate that chart can be found in the following links -> We used the following indicators to replicate the chart

- [Consumer Price Index for All Urban Consumers: All Items in U.S. City Average](https://fred.stlouisfed.org/series/CPIAUCSL)
- [10-Year Treasury Constant Maturity Rate](https://fred.stlouisfed.org/series/GS10)


```{r get_cpi_10Year_yield}

cpi  <-   tq_get("CPIAUCSL", get = "economic.data",
                       from = "1980-01-01") %>% 
  rename(cpi = symbol,  # FRED data is given as 'symbol' and 'price'
         rate = price) %>% # we rename them to what they really are, e.g., cpi and rate
  
  # calculate yearly change in CPI by dividing current month by same month a year (or 12 months) earlier, minus 1
  mutate(cpi_yoy_change = rate/lag(rate, 12) - 1)

ten_year_monthly  <-   tq_get("GS10", get = "economic.data",
                       from = "1980-01-01") %>% 
  rename(ten_year = symbol,
         yield = price) %>% 
  mutate(yield = yield / 100) # original data is not given as, e.g., 0.05, but rather 5, for five percent

# we have the two dataframes-- we now need to join them, and we will use left_join()
# base R has a function merge() that does the same, but it's slow, so please don't use it

mydata <- 
  cpi %>% 
  left_join(ten_year_monthly, by="date") %>% 
  mutate(
    year = year(date), # using lubridate::year() to generate a new column with just the year
    month = month(date, label = TRUE),
    decade=case_when(
      year %in% 1980:1989 ~ "1980s",
      year %in% 1990:1999 ~ "1990s",
      year %in% 2000:2009 ~ "2000s",
      year %in% 2010:2019 ~ "2010s",
      TRUE ~ "2020s"
      )
  )


#First we would like to filter out data from 1980 as those datapoints do not have YOY change (there is no data from 1979 to calculate this)
mydatafiltered <- mydata %>% 
  filter(year > "1980")


#We now plot the data with CPI and Yield colored by decate
ggplot(mydatafiltered, aes(x = cpi_yoy_change, y = yield, color = decade)) +
  
  #Using a scatterplot
geom_point() + 
  
  #Adding labels in %
  scale_x_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  
  #Categorizing by decade and i 1 column with different axis scales
  facet_wrap("decade", ncol = 1, scales = "free") + 
  
  #Adding a minimal theme
  theme_bw() + 
  
  #Giving each category a trendline
  stat_smooth(se = FALSE, method = "lm") + 
  
  #Removing Legends
  theme(legend.position = "none") + 
  
  #Adding useful titles
  labs(title = "How are CPI and 10-year yield related?", 
       x = "CPI Yearly Change", 
       y = "10-Year Treasury Constant Maturity Rate") + 
  
  #Adding the labels with months and year in a small size
  ggrepel::geom_text_repel(aes(label = format(date,"%B %Y")), size = 2) +
  
  #Choosing a proper aspect ratio
  NULL




```



# Challenge 1: Replicating a chart

We now reproduce the plot from [The Racial Factor: There's 77 Counties Which Are Deep Blue But Also Low-Vaxx. Guess What They Have In Common?](https://acasignups.net/21/07/18/racial-factor-theres-77-counties-which-are-deep-blue-also-low-vaxx-guess-what-they-have) 


```{r challenge1, echo=FALSE, out.width="90%"}
knitr::include_graphics(here::here("images", "vaxxes_by_state_red_blue_every_county_070321_1.png"), error = FALSE)
```


We use the following data:

1. To get vaccination by county, we will use [data from the CDC](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh) 
1. Election returns [County Presidential Election Returns 2000-2020](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ)
1. Population estimates [population of each county](https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=2232)
 


```{r downloading data, echo=FALSE, cache=TRUE}

# Download CDC vaccination by county
cdc_url <- "https://data.cdc.gov/api/views/8xkx-amqh/rows.csv?accessType=DOWNLOAD"
vaccinations <- vroom(cdc_url) %>% 
  janitor::clean_names() %>% 
  filter(fips != "UNK") # remove counties that have an unknown (UNK) FIPS code

# Download County Presidential Election Returns
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/VOQCHQ
election2020_results <- vroom(here::here("data", "countypres_2000-2020.csv")) %>% 
  janitor::clean_names() %>% 
  
  # just keep the results for the 2020 election
  filter(year == "2020") %>% 
  
  # change original name county_fips to fips, to be consistent with the other two files
  rename (fips = county_fips)

# Download county population data
population_url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/PopulationEstimates.csv?v=2232"
population <- vroom(population_url) %>% 
  janitor::clean_names() %>% 
  
  # select the latest data, namely 2019
  select(fips = fip_stxt, pop_estimate_2019) %>% 
  
  # pad FIPS codes with leading zeros, so they are always made up of 5 characters
  mutate(fips = stringi::stri_pad_left(fips, width=5, pad = "0"))

```

```{r checking the data}
#checking and understanding the data
glimpse(vaccinations)
glimpse(election2020_results)
glimpse(population)
```


```{r Reproducing the Votes/Vaccines plot}


#get the 2020 Trump vote % from election2020_results
Trumpvote_county <- election2020_results %>% 
  
  # group by County 
  group_by(fips) %>%  
  
  # Filter to Donald Trump
  filter(candidate=="DONALD J TRUMP") %>%  
  
  # Make a new column for sum of the votes 
  mutate(sumcandidatevotes=sum(candidatevotes)) %>% 
  
  # Calculate in a new column voter % 
  mutate(voterate=sumcandidatevotes/totalvotes) %>%  
  
  # Keep only required columns
  distinct(fips,candidate, voterate, .keep_all = FALSE) 


#get the % of total vaccinated
vacc_perc <- vaccinations %>% 
  
  # filter for current date
  filter(date=="09/22/2021") %>% 
  
  #keep the Series_Complete_Pop_Pct
  select(fips,series_complete_pop_pct) # select relevant data sets

#To make vaccine ratio in Percentage
vacc_perc_edited <- vacc_perc %>% 
  
   # get vaccinated amounts in %
  mutate(vaccinated = series_complete_pop_pct/100) %>% 
  
  # include relevant column
  select(fips,vaccinated) 

#Editing for non-vaccinated counties
vacc_perc_edited_final <- vacc_perc_edited %>% 
  
  # to filter out unvaccinated counties as they are not included in the dataset online
  filter(vaccinated > 0) 

#An important issue here is that the number of counties between the three datasets population, vaccinations, and trump votes do not match. We will match only the rows that are existing in all datasets.

#merge the 3 datasets
data1 <- inner_join(Trumpvote_county,vacc_perc_edited_final,by="fips")
data1 <- inner_join(data1, population, by="fips")

# we can skim to see that we do not have unfilled values - we have left this out as it takes up to 3000 rows.

#Now we plot the dataset

# needed to get trend line formula and R-squared
library(ggpubr) 

#First we plot voter rate for Donald Trump against vaccinations
ggplot(data1,aes(x=voterate,y=vaccinated))+
  
  # adding the trend line
  geom_smooth(method = "lm", se = FALSE, formula = y ~ x)+ 
  
  #adding the scaling points with population
  geom_point(aes(size=pop_estimate_2019, alpha = 0.1), color = "blue")+ 
  
  # adding the small points
  geom_point(size = 1)+ 
  
  #Creating the backgrounds
  annotate("rect", xmin=-Inf, xmax=0.55, ymin=-Inf, ymax=Inf, alpha=0.2, fill="Blue") +
  annotate("rect", xmin=0.45, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.2, fill="Red") +
  
  #Adding Horizontal Lines
  geom_hline(aes(yintercept=0.85,),linetype=5)+ 
  geom_hline(aes(yintercept=0.539),linetype=5)+
  geom_hline(aes(yintercept=0.508),linetype=5)+
  
  #Adding Text Labels
  annotate("text", label = "Herd Immunity Threshold?", x = 0.15, y = 0.87, size = 3, colour = "blue", fontface =4)+ #adding text
  annotate("text", label = "TARGET=53.9%", x = 0.15, y = 0.55, size = 3, colour = "blue", fontface =2)+
  annotate("text", label = "ACTUAL=50.8%", x = 0.15, y = 0.52, size = 3, colour = "blue", fontface =2)+
  annotate("text", label = "21/09/21", x = 0.9, y = 0.00, size = 5, colour = "red", fontface =2)+
  annotate("text", label = "EVERY U.S. COUNTY", x = 0.5, y = .98, size = 10, colour = "Black", fontface =2)+
  
  #Adding axis in % Scaling with 5% from 0-100
  scale_x_continuous(expand = c(0.05,0.05), breaks = seq(from = 0, to = 1, by = 0.05), labels = scales::percent_format(accuracy = 1))+ 
  scale_y_continuous(expand = c(0.00,0.05), breaks = seq(from = 0, to = 1, by = 0.05), labels = scales::percent_format(accuracy = 1))+ 
  
  #Adding the Formula from the Trendline 
  stat_regline_equation(label.y = 0.80, label.x = 0.8, aes(label = ..eq.label..), color = "red", fontface = 2) + 
  stat_regline_equation(label.y = 0.75, label.x = 0.8, aes(label = ..rr.label..), color = "red", fontface = 2) +
  
  #Removing Legends
  theme(legend.position = "none") + 
  
  #Using a minimal format
  theme_minimal() +
  
  #Making Dots scale larger
  scale_size(range = c(1,22)) + 
  
  #Removing Sizing Legend and Alpha Legend
  guides(size=FALSE) +  
  guides(alpha=FALSE) +  
  
  #Adding titles
  labs(title = "COVID-19 VACCINATION LEVELS OUT OF TOTAL POPULATION BY COUNTRY", 
       x = "2020 trump vote %",
       y = "% of total population vaccinated") + 
  NULL
  



```
Note that some counties with 0% vaccination has not been included. The reasoning behind the 0% vaccination rate can have several reasons. Some of these datapoints with 0% vaccination have an unknown county listed why they cannot be included under all circumstances as we cannot match them with a voter percentage. Furthermore, all counties may be included in the database while not all counties report their vaccinations. 


# Challenge 2: Opinion polls for the 2021 German elections

The Guardian newspaper has an [election poll tracker for the upcoming German election](https://www.theguardian.com/world/2021/aug/20/german-election-poll-tracker-who-will-be-the-next-chancellor).

WE will reproduce the graph made in the guardian


The following code will scrape the wikipedia page and import the table in a dataframe. Afterward the graph is produced


```{r scrape_wikipedia_polling_data, warnings= FALSE, message=FALSE}
url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_2021_German_federal_election"

# similar graphs and analyses can be found at 
# https://www.theguardian.com/world/2021/jun/21/german-election-poll-tracker-who-will-be-the-next-chancellor
# https://www.economist.com/graphic-detail/who-will-succeed-angela-merkel


# get tables that exist on wikipedia page 
tables <- url %>% 
  read_html() %>% 
  html_nodes(css="table")


# parse HTML tables into a dataframe called polls 
# Use purr::map() to create a list of all tables in URL
polls <- map(tables, . %>% 
             html_table(fill=TRUE)%>% 
             janitor::clean_names())


# list of opinion polls
german_election_polls <- polls[[1]] %>% # the first table on the page contains the list of all opinions polls
  slice(2:(n()-1)) %>%  # drop the first row, as it contains again the variable names and last row that contains 2017 results
  mutate(
         # polls are shown to run from-to, e.g. 9-13 Aug 2021. We keep the last date, 13 Aug here, as the poll date
         # and we extract it by picking the last 11 characters from that field
         end_date = str_sub(fieldwork_date, -11),
         
         # end_date is still a string, so we convert it into a date object using lubridate::dmy()
         end_date = dmy(end_date),
         
         # we also get the month and week number from the date, if we want to do analysis by month- week, etc.
         month = month(end_date),
         week = isoweek(end_date)
         )




#remember this fig.width=25, fig.height=12 in the{}
#use the average support rate of the same day to build up a new dataset
df_vote <- german_election_polls %>% 
  group_by(end_date) %>% 
  summarize(CDU= mean(union)/100,
            SPD= mean(spd)/100,
            AFD=mean(af_d)/100,
            FDP= mean(fdp)/100,
            Linke=mean(linke)/100,
            Grune=mean(grune)/100)
df_vote

#form a date breaks for x-ray
library(scales)
datebreaks <- seq( as.Date("2021-04-01"),as.Date("2021-07-01"), by="3 month") 

#Adding colors for Labs
Party <- c("Union" = "Black", "spd" = "Firebrick", "AF" = "Deepskyblue3", "FDP" = "Goldenrod1", "Linke" = "Darkmagenta", "Grune" = "Forestgreen")

#plot - we choose election polls and dates as x and y variables
ggplot(data = german_election_polls, aes(x= end_date))+
  
  #and add a minimal theme
  theme_bw()+
  
  #We then plot each part of the data separately for each party. This includes a scatter plot and trend line
  geom_point(aes(y = union), color = "Black", size=2, alpha = 0.2) +
  geom_smooth(aes(y=union, color = "Union", method="lm", se = FALSE), level = 0, span=0.05)+
  geom_point(aes(y = spd), color = "Firebrick", size=2,  alpha= 0.2)+
  geom_smooth(aes(y=spd, color = "spd", method="lm", se = FALSE), level = 0, span=0.05)+
  geom_point(aes(y = af_d), color="Deepskyblue3", size=2, alpha= 0.2)+
  geom_smooth(aes(y=af_d, color = "AF",method="lm", se = FALSE), level = 0, span=0.05)+
  geom_point(aes(y = fdp), color="Goldenrod1", size=2, alpha= 0.2)+
  geom_smooth(aes(y=fdp,color = "FDP", method="lm", se = FALSE), level = 0, span=0.05)+
  geom_point(aes(y = linke), color="Darkmagenta", size=2, alpha= 0.2)+
  geom_smooth(aes(y=linke, color = "Linke", method="lm", se = FALSE), level = 0, span=0.05)+
  geom_point(aes(y = grune), color="Forestgreen", size=2, alpha= 0.2)+
  geom_smooth(aes(y=grune, color = "Grune", method="lm", se = FALSE), level = 0, span=0.05)+
  
  #Scaling colors
  scale_color_manual(values = Party) +
  
  #We then remove the legend, add a title inside in the middle of the plot
  theme(legend.position = c(0.9, 0.9),  #Choosing legend position
        legend.background = element_rect(fill="white", linetype = "solid", color = "black"),  #Background white and black outline
        plot.title = element_text(hjust =0.5),  #Title position
        plot.caption = element_text(hjust=0))+ 
  
  #Removing the background filler on the legend
  guides(color=guide_legend(override.aes=list(fill=NA))) + 
  
  #Adding a title to the legend
  labs(color='Political Party')  + 
  
  #Adding label descriptions
  labs(title = "German election poll tracker",
       x = "Month (2021 YTD)",
       y = "Vote distribution (%)",
       caption = "Source: wahlrecht.de, last updated 25 Sep 2021")+
  
  #Removing vertical grid lines
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())+
  
  NULL

```
We see from this chart that SPD have the highest number of votes currently and Link has the lowest (as of 21st of September.)


# Details

- Who did you collaborate with: Karim El Asmar, Stepan Emelianenko, Nelly Gray, Ziqi Li, Ray Park, Shirley Wan, Ditlev Meulengracht
- Approximately how much time did you spend on this problem set: 10 hours
- What, if anything, gave you the most trouble: Knitting the file and taking courage to ask on solution to the issue on Slack (it took us several hours)

> As a true test to yourself, do you understand the code you submitted and are you able to explain it to someone else? 

Yes we are confident we understand the code and can explain it to someone else. 











