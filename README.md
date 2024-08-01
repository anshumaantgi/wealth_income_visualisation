DSA Project - Wealth and Income
================
## Introduction

The wealth and income dataset comes from the Urban Institute and the US
Census. The dataset explores the wealth and income distribution amongst
different demographic groups - different races, genders and income
percentiles. Our analysis mainly looks at distribution of wealth, income
and student debt amongst races. The 3 main racial groups compared in the
dataset are Whites, Blacks and the Hispanics.The dataset compares
various aspects that would affect wealth such as income, student debt,
home ownership and retirement savings.

The dataset is split into 11 sub-sheets. These are a brief description
of the sheets:

### `home_owner.csv`

Describes home ownership percentage for families by year (ranging from
1976-2016) and by race.

### `income_aggregate.csv`

Describes the share of aggregate income received by each quintile and
top 5% of each racial group in years ranging from 1972-2019.

### `income_distribution.csv`

Describes households by total income and race, separated by year
(ranging from 1972-2019) and income groups.

### `income_limits.csv`

Describes familial income limits for each quintile by year (ranging from
1972 to 2019) and race.

### `income_mean.csv`

Describes mean income received by each quintile and top 5% of each
racial group in years ranging from 1972 to 2019.

### `income_time.csv`

Describes the family-level income by percentile (10th, 50th and 90th)
and year (ranging from 1963 to 2016)

### `lifetime_earn.csv`

Describes data on individuals’ average lifetime earnings, broken down by
gender and race.

### `lifetime_wealth.csv`

Describes data on individuals’ average and median lifetime wealth (i.e
net worth), broken down by race and year (ranging from 1983 to 2016).

### `race_wealth.csv`

Describes family wealth by race, year (ranging from 1963 to 2016) and
mean and/or median family wealth normalised to 2016.

### `retirement.csv`

Describes the average family liquid retirement savings by race,and year
(ranging from 1989 to 2016) normalized to 2016 dollars.

### `student_debt.csv`

Describes the average family student loan debt for ages 25-55, by race
and year (ranging from 1989 to 2016) normalized to 2016 dollars.

## Descriptive Statisitcs

### Cleaning

Our analysis, which looks into the distribution of income, wealth and
student debt amongst races uses the following sheets: `income_time.csv`,
`race_wealth.csv`, `lifetime_wealth.csv` and `student_debt.csv`. We
filtered the dataset to exclude certain years depending on our analysis.
We firstly filtered out years where there were NA values. Furthermore,
for visualisations where continuous data is critical (eg: in
visualization 1 when identifying periods of recession, as explained in
question 1), only parts of the dataset that were continuous were
utilized. For example , `income-time.csv` fails to record data for years
: 1964 and 1969. Thus we only included data after and including 1970.

``` r
years_given = income_time %>% distinct(year) %>% pull(year)
years_given
```

    ##  [1] 1963 1965 1966 1967 1968 1970 1971 1972 1973 1974 1975 1976 1977 1978 1979
    ## [16] 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994
    ## [31] 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009
    ## [46] 2010 2011 2012 2013 2014 2015 2016

For visualisations where multiple metrics were available to compare the
change of a particular variable over time, the most appropriate one was
chosen, and others were filtered out, as explained in the methodology
for our visualisations. For example, race wealth across time was
compared based on median instead of mean. The sheets used were all tidy,
except `student_debt.csv`. However, we found that tidying it was not
essential for our visualization.

### Interesting summary statistics

When looking at summary statistics for each sheet, we found some
extremely interesting points to note.

- For `lifetime_wealth`, the dataset’s median life_time wealth is USD
  174271 and mean wealth is USD 335604. It is interesting to note that
  the difference between the median and mean wealth is much larger than
  expected. This highlights that the majority of the wealth in this data
  set is owned by the rich even though their proportion in the
  population is not very high.
- For `race_wealth`, mean wealth amongst all families is USD 158020 when
  median wealth is only USD 97209. Smallest wealth recorded here was USD
  2467 by category was Non-White in 1963. Hence, unsure which specific
  group it belongs to. This may be indicative of an
  underserved/underrepresented group. Largest wealth recorded is USD
  19336 which is by the Whites in 2016.
- For `student_debt`, interestingly the smallest loan recorded is USD
  739.10 from the Hispanics in 1992 even though their income is much
  lower than Whites. The largest loan recorded is USD 14224.80 from the
  Blacks in 2016.

### Remarks

While we did inspect the mean and median for these datasets over the
years, we found the information unsuitable as it was not an accurate
description of the wealth since we needed to account for inflation.
Hence, in our methodology and visualisations below, we have refined the
dataset and used more key information that provided a more contextually
accurate view of the data.

## Question 1

In times of economic downturn, how are people from different races
affected?

### Introduction

We will be analyzing the data to explore how times of economic decline
affect different races. The files from the data set used to answer the
questions are `income_time.csv` and `race_wealth.csv`. This question
will firstly identify how many economic downturns have occurred between
years 1970 to 2016 and when they occurred specifically. We will then
look at how the wealth of people of different races (Black, White, and
Hispanic) are affected. We chose to look at wealth as it is an indicator
of quality of life, thereby allowing us to assess how economic decline
affects the quality of life of different races differently. As we
ourselves are students of different races living in Singapore, the
performances of different races living in other countries during
recession interests us as we can draw parallels and comparisons.
Furthermore, with racial inequality and dynamics in the US being
extensively highlighted in the media lately, we were interested to find
out more about how different races are disproportionately impacted
during recessions in the years before.

### Methodology

The first visualization is a line graph of the family income of those in
the 50th income percentile against the years(1970 to 2016). The main
purpose of this graph is to identify the major economic downturns that
have occurred during that period. This can be shown by a relatively
drastic decrease in family income during a particular time period. The
dataset provides the family income of those in different percentiles
(10th, 50th and 90th), but we have chosen to plot the family income of
those in the 50th income percentile as it best represents the population
as a whole since its the median percentile. Though the years in the data
set range from 1963 to 2016, the time period analyzed is 1970 to 2016 as
this is the only period between which data points for every year is
available. There are no values for the years 1964 and 1969, which led to
our decision to use the data from 1970 onwards.

The second visualization we made is a line and point graph with 3 lines
corresponding to median wealth of Blacks, Whites and Hispanics from 1983
to 2016. While the dataset provides wealth for ‘Non-Whites’, we did not
look at this as it overlaps with Blacks and Hispanics. It was also
unclear who is included in ‘Non-Whites’. Furthermore, the dataset
provides both median and average wealth, but we chose to look at median
wealth as we noticed that the average wealth for each race in each year
was significantly higher than the median. This indicates that high
income individuals from each race may skew the average, making it a poor
representation of the population. Though the years in the dataset range
from 1963 to 2016, the time period analyzed is 1983 to 2016 as 1983 and
2016 are the earliest and latest years between which median wealth data
is available for all 3 races in each year. By considering the periods of
economic downturn identified by the first visualization, we compare how
wealth is affected for those of each race.

### Visualisation 1

``` r
income_time = income_time %>%
              filter(year >= 1970)

fifty_percentile = income_time %>%
                    filter(percentile == "50th")
econ_downturn = ggplot(data = fifty_percentile, aes(x = year, y = income_family)) + 
  geom_line() + 
  geom_vline(xintercept = c(1978, 1982), lty =2 , color ="darkred")  +
  annotate("text" , x = c(1978 , 1982) , y = 65000 , label = c("1978" , "1982"),  color = "darkred") +
  geom_vline(xintercept = c(1989, 1993), lty =2 , color ="firebrick")  +
  annotate("text" , x = c(1989, 1993) , y = 65000 , label = c("1989" , "1993"),  color = "firebrick") +
  geom_vline(xintercept = c(2007, 2012), lty =2 , color = "firebrick1")  +
  annotate("text" , x =  c(2007, 2012) , y = 65000 , label = c("2007" , "2012"),  color = "firebrick1") +
  labs(x = "Year" , y = "Family Income (USD)" , title = "50th Percentile Family Income (1970 -2016)") +
  scale_y_continuous(breaks=seq(50000,65000,by= 3000), labels = scales::label_dollar(accuracy = 1)) + 
  scale_x_continuous(breaks=seq(1970,2018,by=2)) + 
    theme_minimal()+
   theme(axis.text.x = element_text(angle = 90)) 
econ_downturn
```

![](group_project_files/figure-gfm/Q1%20Visualizations%201-1.png)<!-- -->

### Visualisation 2

``` r
maxyearrw=max(racewealth$year)
g1=racewealth%>%filter(year>=1983)%>% 
  mutate(race=as.factor(race))%>%
  filter(race!="Non-White")%>%
  pivot_wider(names_from=type, values_from=wealth_family)%>%
  select(-Average)%>%
  ggplot()+
  geom_line(aes(x=year, y=Median, color=race))+
  geom_point(aes(x=year, y=Median,color=race))+
  scale_x_continuous(breaks=seq(1983, maxyearrw, 1))+
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1))+
  labs(title="Median wealth in the US across races (1983 - 2016)",  x="Year", y="Median wealth (USD)", color="Race")+
  theme_minimal()+
  geom_vline(xintercept=1989, lty=2, color="firebrick")+
  geom_text(aes(x=1989, y=210000), label="1989", color="firebrick")+
  geom_vline(xintercept=1993, lty=2, color="firebrick")+
  geom_text(aes(x=1993, y=210000), label="1993", color="firebrick")+
  geom_vline(xintercept=2007, lty=2, color="firebrick1")+
  geom_text(aes(x=2007, y=210000), label="2007", color="firebrick1")+
  geom_vline(xintercept=2012, lty=2, color="firebrick1")+
  geom_text(aes(x=2012, y=210000), label="2012", color="firebrick1")+
  theme(axis.text.x=element_text(angle=90))
g1  
```

![](group_project_files/figure-gfm/Q1%20Visualizations%202-1.png)<!-- -->

### Discussion

From the first visualization, the reader should be able to observe the
three major economic downturns that occurred in the given time period;
The downturns occurred from 1978 to 1982 (severe economic recession),
from 1989 to 1993 (early 1990s recession in the United States) and
finally from 2007 to 2012 (Global Financial Crisis) (Investopedia,
2022). This can be seen by the drastic drop in family incomes between
those time periods. After those periods, readers can observe the
economic recovery, marked by the increase in income and how long the
economy took to recover, shown by the time taken for family income to
rise back to pre-downturn levels.These periods of recovery are 1982 to
1989, 1993 to 2007 and 2012 onwards. Overall, family income has
increased from 1970 to 2015. The graph is also not very smooth,
indicating that income can fluctuate quite drastically between years;
this may be a sign that the economy was not very stable during that time
period.

From the second visualization, readers can observe and compare the
changes in wealth across the three different races in the periods of
economic downturn spanned by this graph. These periods are 1983-1993 and
2007-2012, with the first period’s recession (early 1990s recession in
the United States) being significantly less severe than that of the
second period (Global Financial Crisis). From 1983-2016, the wealth of
Blacks and Hispanics is consistently lower than that of Whites. In the
first recession, while the wealth of Whites drops sharply, there is no
comparable change for Blacks and Hispanics. Rather, it slightly
increases. In the second recession, it can be seen that the drop in
wealth for Whites is much sharper than the first. During this time
period, while not as sharp, there is also a slight drop in wealth for
Blacks and Hispanics. Whites may be faced with a more severe decline in
wealth during periods of economic downturn due to their higher overall
wealth level as they have more to lose out on. This may be because they
also invest more greatly in assets, such as stocks (Smart, 2021), that
were affected during such periods, making their losses drastic. Such
losses may not affect Blacks and Hispanics as much, making their loss in
wealth less drastic (in 2007 to 2012) or even negligible (in 1989 and
1993 where their median wealth seemed to increase).

## Question 2

How has the disparity between Black and White families changed over
time?

### Introduction

We are interested in finding the disparity between Black and White
families over the years. We will use two metrics to answer this question
– the lifetime wealth in Black and White families, and well as their
student debt.

The lifetime wealth of a demographic may give insights on their income
and socioeconomic status. These findings could be reinforced by the data
on student debt, which could tell us more about the financial burdens
among these demographics, which may lead them to take a student loan, as
well as the ability to pay them off. All these could help us visualize
if there is any disparity between Black and White families.

In this question, we will omit Hispanic families due to two reasons.
Firstly, there is no Hispanic data available with regards to their
lifetime wealth. Secondly, they show certain anomalies in student loan
debt that cannot fully be explained with data within this dataset.

### Methodology

The first graph will show the lifetime wealth difference between the
Blacks and Whites over the years(1989 to 2016). The objective of this
graph is to explore the wealth disparity between the two races. The
addition of reference lines were done using a linear models for both the
White and Black demographics for easier aid to see the trend of the
wealth disparity between the two races.

For the second graph,a point and line graph will be plotted. We plan to
visualize the average family student loan debt (`loan_debt`), as well as
the percentage of families who have student loan debt (`loan_debt_pct`).
We will visualize these variables across different years, from 1989 to
2016.

In this question, we will omit Hispanic families, as they show certain
anomalies in student loan debt that cannot fully be explained with data
within this dataset.

The amount and proportion of student loan debt can tell us many things.
A demographic with high student loan debt could indicate that that
demographic has a lower income, making it harder for them to pay off
their loans. It could also indicate greater economic and financial
challenges compared to other groups.

### Visualizations

``` r
# Fit a linear model for the white income
white_wealth <- lifetime_wealth %>% 
  filter(race == "White") %>% 
  filter(type == "Median")

model_white <- lm(wealth_lifetime ~ year, data = white_wealth)

# Extract coefficients for the linear model
slope_white <- coef(model_white)[2]
intercept_white <- coef(model_white)[1]

#fitting linear model for blacks 
black_wealth <- lifetime_wealth %>% 
  filter(race == "Black") %>% 
  filter(type == "Median")

#extract intercept and slope for linear model 
model_black <- lm(wealth_lifetime ~ year, data = black_wealth)
slope_black <- coef(model_black)[2]
intercept_black <- coef(model_black)[1]

q2_wealth <- lifetime_wealth%>%
  filter(type == "Median")%>%
  ggplot(., aes(x = year, y = wealth_lifetime))+
  geom_line(aes(group = year), linewidth = 1)+
  geom_point(aes(color = race), size = 12)+
  geom_text(aes(label = paste0(round(wealth_lifetime/10^3,1), "K")), 
            size = 2.75, fontface = "bold", color = "white")+
  scale_x_continuous(breaks = lifetime_wealth$year, 
                     label = lifetime_wealth$year)+
  scale_y_continuous(breaks = seq(0,450000, 25000), 
                     labels = function(y) paste0("$",round(y/10^3, 0), "K" ))+
  theme_minimal()+
  theme(legend.position = "none") +
  geom_abline(intercept = intercept_white, slope = slope_white, linetype = 2, linewidth = 0.3) +
  geom_abline(intercept = intercept_black, slope = slope_black, linetype = 2, linewidth = 0.3) +
  labs(title = "Lifetime Wealth between Blacks and Whites in the US (1983-2016)", 
       x = "Year", 
       y = "Lifetime Wealth (USD)")

q2_wealth
```

![](group_project_files/figure-gfm/Q2%20Visualizations%201-1.png)<!-- -->

``` r
student_debt %>%
  filter(race != "Hispanic") %>%
  mutate(pct_char = as.character(round(loan_debt_pct,3))) %>%
  ggplot(aes(x = year, y = loan_debt, color = race)) +
  geom_point(aes(size = loan_debt_pct), alpha = 0.8) + 
  geom_line() +
  geom_text_repel(aes(label = pct_char), show.legend = FALSE, size = 3) +
  labs(x = "Year", y = "Student Loan Debt (2016 Dollars in Thousands in USD)",title = "Difference in Student Loan Debt Over Time in the US (1989 - 2016)",
       color = "Race", size = "% of families with debt") +
  scale_x_continuous(breaks = seq(1989,2016,3)) +
  scale_y_continuous(labels = function(y) paste0("$",round(y/1e3, 0), "K" )) +
  geom_vline(aes(xintercept = 2004), lty = "dashed") +
  geom_text(aes(x = 2004, y = 6000, label = "Black families start to \n have larger student loan debt"), 
            color = "black", size = 3, nudge_x = -3.8) +
  theme_minimal()
```

![](group_project_files/figure-gfm/Q2%20Visualizations%202-1.png)<!-- -->

### Discussions

For the first visualisation, while both races show a general increase in
wealth over the years, the Whites have a greater increase in the years
of 1989 to 2007. Additionally, with the help of the reference lines, we
see that the difference between the races growing over the years
especially from 1989 to 2004 at a much steeper rate and then from 2007
to 2016 the difference between the races seems to be more stabilising.

An important factor for the growing wealth disparity is the passing down
of wealth to the next generation. White households are more likely to
own homes, stocks, and other financial assets, while Black households
are more likely to have debt, such as student loans and credit card debt
(Smart, 2021). The lack of access to home ownership and other forms of
assets has limited the ability of Black households to accumulate wealth
and pass it down to future generations, causing the wealth disparity to
snowball over the years.

In the year 2004, the difference between the races was the largest, with
Whites’ lifetime wealth at USD 418.5K, the highest wealth amongst all
the years. From further research we found that the main reason was due
to the housing stock market experiencing a significant upswing, and can
be attributed to the fact that more white individuals were able to
purchase homes, thus contributing to the growth of the GDP in the US. As
a result of increased demand for housing, the housing market boomed,
leading to an increase in mortgage rates and the stock market (Rhodes,
2004).

It is also interesting to note that in 2007, during the start of the
third recession as describe in Question 1, the blacks had an all time
high of lifetime wealth at USD 121.7K.

From the second visualization, we can see how over the years, student
debt is increasing both in amount and proportion among Black and White
families.

There are also a few notable years in this visualization:

- From 1989 to 2001, White student loan debt tends to be higher than
  Black families.
- In 2004, Black student loan debt of USD 3987 is very similar to White
  student loan debt of USD 4035.
- From 2007 onwards, Black families have significantly more student loan
  debt than White families, in terms of both the amount, and the
  percentage of families in debt

We can see throughout this write-up that 2007 seems to be the “tipping
point” for Black and White families. After 2007, the disparity between
the Blacks and Whites seems to be growing. Before, the disparity between
the two races seem to be evident, but constant.

This could be explained and supported by the previous visualization,
where Black people have significantly lower lifetime wealth. This
indicates that Black families tends to have greater financial struggles,
and are more likely to take up a student loan, and is harder for them to
pay off their loan.

## References

Mock, T., Walker, R. W., & Harmon, J. (2022, November 30).
Tidytuesday/readme.md at master · rfordatascience/tidytuesday. GitHub.
Retrieved April 11, 2023, from
<https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-02-09/readme.md>

Rhodes, E. (2004, December 27). 2004 was “A dream market” for
real-estate buyers and sellers. The Seattle Times. Retrieved April 14,
2023, from
<https://www.seattletimes.com/business/real-estate/2004-was-a-dream-market-for-real-estate-buyers-and-sellers/>

The Investopedia Team. (2022, June 16). A review of past recessions.
Investopedia. Retrieved April 14, 2023, from
<https://www.investopedia.com/articles/economics/08/past-recessions.asp>

Who owns stocks in America? mostly, it’s the wealthy and white - US
News. (n.d.). Retrieved April 11, 2023, from
<https://www.usnews.com/news/national-news/articles/2021-03-15/who-owns-stocks-in-america-mostly-its-the-wealthy-and-white>
