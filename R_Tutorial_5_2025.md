BOPRC R Tutorial 5 - Statistical analyses in R
================

## Overview

This lesson is designed to provide you with experience in running
statistical analyses in R. We will use water quality data as an example,
but these analyses can be applied to any other datasets, provided the
statistical assumptions are met. We will cover the following topics:

- **Correlation analyses (Pearson and Spearman) and plots**
- **Linear regression and plots**
- **T-tests and Wilcoxon rank sum test (aka Mann-Whitney)**
- **ANOVA**
- **Trend analysis**

|  |
|----|
| \### Disclaimer |
| This lesson teaches the implementation of multiple statistical analyses, rather than the background behind why, when, and what to check when choosing a statistical analysis. You should *always* check the underlying assumptions of an analysis and whether your data meet those assumptions. |

We are adding a few new packages today which perform specialized
functions for statistical analyses. You don’t need to worry about the
new packages too much, other than you will need to install and load the
libraries.

The main packages that we will use in this tutorial are:

- **tidyverse**
- **lubridate**
- **Hmisc**
- **corrplot**
- **ggpmis**

Before attempting to install these packages, make sure your Primary CRAN
Repository is set to:

- **“New Zealand \[https\] - University of Auckland”**

To check this, click ‘Tools’ –\> ‘Global Options’ –\> ‘Packages’. Click
‘Change’ if you need to adjust this.

You can download most packages by clicking on the ‘Install’ button on
the ‘packages’ tab in the lower right window pane. Then in the Install
Packages popup, select ‘Repository (CRAN)’ from the ‘Install from’ drop
box and type the name of the package you wish to download (e.g., dplyr).

Once all of these packages are installed you can load them using the
`library` function:

``` r
library(tidyverse)
library(lubridate)
library(Hmisc)
library(corrplot)
library(ggpmisc)
```

    ## Warning: package 'ggpmisc' was built under R version 4.4.3

    ## Warning: package 'ggpp' was built under R version 4.4.3

First we will load in our data. We will use the same water quality data
from Lesson 3. This data has been downloaded from Aquarius using the R
script which you can find at `scripts/download_data_aquarius.R` if you’d
like to see how the data were downloaded. For today, we are skipping
that step and reading in directly from a .csv file which was written
after the Aquarius download.

``` r
wq <- read.csv('./data/Lake_WQ_Timeseries.csv')
```

Now, look at the `wq` dataframe by clicking on it in the environment and
familiarise yourself with the columns. You can also run the function
`colnames(wq)` in the console to get a list of column names. It’s better
to run this in the console, since it is a diagnostic test and not
something you will necessary need to run every time you open your
script–just as needed.

``` r
colnames(wq) # you don't have to save this in your script, but can copy it into the console
```

    ##  [1] "Site"                "LocationName"        "Time"               
    ##  [4] "Value"               "Quality"             "Approval"           
    ##  [7] "Qualifiers"          "Parameter"           "Unit"               
    ## [10] "Sample_Number"       "Project_Id"          "LowerDetectionLimit"
    ## [13] "UpperDetectionLimit" "Method"              "CollectionMethod"   
    ## [16] "Lab"                 "Error"               "Comment"            
    ## [19] "ACTION"              "Samp_Comment"        "Cloud_Cover"        
    ## [22] "Num_Swimmers"        "Tide_Cycle"          "Tide"               
    ## [25] "Wind_Direction"      "Wind_Speed"          "Weather_Today"      
    ## [28] "Weather_Yest"        "Surface_Cond"        "Bore_Type"          
    ## [31] "Bore_Number"         "Bore_Collect_Method" "Bore_Sampled_From"  
    ## [34] "Bore_Pump_Rate"      "Bore_Pump_Duration"  "Bore_Protocol"      
    ## [37] "Bore_Probe"          "Odour"

As we know from previous lessons, it is always best practice is to
format date/time objects with the appropriate timezone, otherwise R will
assume a timezone, and that can lead to the wrong date being set for
your timestamp. This is the first thing I do when I see I have a
datetime object as a column. Let’s use a bit of code that will parse our
`Time` column, which includes both a date and a time.

Here, we will use a function called `parse_date_time` which looks at the
`Time` column, and then provides a list (using `c()`) of potential
formats that the column will be in. Here, we list two formats, the first
one has YMD and HMS (hours, minutes, seconds), the second one just has
YMD, as some of the values in the `Time` column don’t have an associated
time next to the date. We pair this with the `mutate` function, which we
will learn more about below.

NOTE: there are many ways to format/parse dates and times in R. This is
just one example!

``` r
wq <- wq %>% mutate(Time = parse_date_time(Time,c("%Y-%m-%d %H:%M:%S","%Y-%m-%d"), tz = "etc/GMT+12"))
```

------------------------------------------------------------------------

***Challenge 1:*** *What locations and parameters are included in this
dataset? Use the `unique()` function to find out.*

<details>
<summary>
Click to see a solution
</summary>

``` r
unique(wq$Parameter)
```

    ## [1] "TN (g/m^3)"    "TP (g/m^3)"    "CHLA (mg/m^3)" "VC - SD (m)"

``` r
unique(wq$LocationName)
```

    ##  [1] "Lake Rotoma at Site 1 (Integrated)"       
    ##  [2] "Lake Rotoehu at Site 3 (Integrated)"      
    ##  [3] "Lake Rotoiti at Site 4 (Integrated)"      
    ##  [4] "Lake Rotoiti at Site 3 (Integrated)"      
    ##  [5] "Lake Rotoiti at Okawa Bay (Integrated)"   
    ##  [6] "Lake Rotorua at Site 2 (Integrated)"      
    ##  [7] "Lake Okataina at Site 1 (Integrated)"     
    ##  [8] "Lake Okareka at Site 1 (Integrated)"      
    ##  [9] "Lake Tikitapu at Site 1 (Integrated)"     
    ## [10] "Lake Rerewhakaaitu at Site 1 (Integrated)"
    ## [11] "Lake Okaro at Site 1 (Integrated)"        
    ## [12] "Lake Rotorua at Site 5 (Integrated)"      
    ## [13] "Lake Tarawera at Site 5 (Integrated)"     
    ## [14] "Lake Rotomahana at Site 2 (Integrated)"

</details>

------------------------------------------------------------------------

That helps us get a better understanding of the dataset that we’re
working with. Since we are going to do correlation analysis first, let’s
focus on just one site and look at the relationships between variables.
We will filter the data to only select “Lake Okaro at Site 1
(Integrated)” and we will create a new object named `wq_okaro` so we
keep all the other lake data in the `wq` dataframe.

``` r
wq_okaro <- wq %>% 
  filter(LocationName=='Lake Okaro at Site 1 (Integrated)')
```

Let’s also clean up the dataframe and only select the columns which are
useful

``` r
wq_okaro <- wq_okaro %>% 
  select(Time, Value, Parameter, Unit)
```

Let’s plot the data to make sure everything looks good

``` r
ggplot(wq_okaro, aes(x = as.POSIXct(Time), y = Value, color = Parameter)) +
  geom_point() +
  facet_wrap(~Parameter, scales = 'free') +
  theme_bw() +
  xlab('Time')
```

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot(wq_okaro, aes(x = Value, fill = Parameter)) +
  geom_histogram() +
  facet_wrap(~Parameter, scales = 'free') +
  theme_bw()
```

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

Alright, we have a time series of four variables. There are many things
we can do to analyse this data. Let’s start with a correlation analysis.

### Correlation analysis and plots

In order to conduct a correlation analysis, we need to do some
formatting/rearranging. First, we need to make the data wide, but we
also have to create a `Date` column which doesn’t include the time so
that it is common across the variables. We will also remove the Unit
column for this reason (and we don’t need that column for this anyway
since the units are stored in the Parameter names)

``` r
okaro_wide <- wq_okaro %>% 
  mutate(Date = as.Date(Time)) %>% 
  select(-Time, -Unit) %>% 
  group_by(Date, Parameter) %>% 
  summarise(Value = mean(Value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = 'Parameter', values_from = 'Value') %>% 
  ungroup()
```

Open up your new `okaro_corr` data and check it out. Those column names
have gaps and symbols in them, which might cause us problems later.
Let’s rename the columns using the function `rename`. I’ll show you one
example

``` r
okaro_wide <- okaro_wide %>% 
  rename('chla_mgm3' = "CHLA (mg/m^3)") # the format is new name = old name
```

------------------------------------------------------------------------

***Challenge 2:*** *Rename the remaining variable columns to TN_gm3,
TP_gm3, and secchi_m.*

<details>
<summary>
Click to see a solution
</summary>

``` r
okaro_wide <- okaro_wide %>%
    rename(TN_gm3 = "TN (g/m^3)", TP_gm3 = "TP (g/m^3)", secchi_m = "VC - SD (m)")
```

</details>

------------------------------------------------------------------------

Great, your `okaro_corr` dataframe should now have the columns “Date”,
“chla_mgm3”, “TN_gm3”, “TP_gm3)”, and “secchi_m”.

The next thing we need to do in order to run the correlation analysis is
to remove the `Date` column. We aren’t interested in how date is
correlated with chl-a, TN, TP, or secchi (for this analysis!). Just how
they are correlated with each other.

------------------------------------------------------------------------

***Challenge 3:*** *Use the `select` function to remove the Date
column.*

<details>
<summary>
Click to see a solution
</summary>

``` r
okaro_corr <- okaro_wide %>%
    select(-Date)
```

</details>

------------------------------------------------------------------------

Now, we have to make sure there are no NA’s in the data frame, and
format it as a matrix. This is simply because the function which runs
the correlation analysis is picky, so we have to make it happy :)

``` r
okaro_corr <- na.omit(okaro_corr)
okaro_corr <- as.matrix(okaro_corr)
```

Now let’s run the correlation analysis using the `rcorr` function. The
default type of analysis is Pearson, which assumes normality. Here, we
will specify Spearman because our data are not all normally distributed

``` r
okaro_corr_out <- rcorr(okaro_corr, type = "spearman")
print(okaro_corr_out)
```

    ##           chla_mgm3 TN_gm3 TP_gm3 secchi_m
    ## chla_mgm3      1.00   0.69   0.54    -0.75
    ## TN_gm3         0.69   1.00   0.72    -0.65
    ## TP_gm3         0.54   0.72   1.00    -0.46
    ## secchi_m      -0.75  -0.65  -0.46     1.00
    ## 
    ## n= 91 
    ## 
    ## 
    ## P
    ##           chla_mgm3 TN_gm3 TP_gm3 secchi_m
    ## chla_mgm3            0      0      0      
    ## TN_gm3     0                0      0      
    ## TP_gm3     0         0             0      
    ## secchi_m   0         0      0

Let’s look at that output! The first table is the correlation
coefficients (r) and the second is the p-values.

``` r
p_mat <- okaro_corr_out$P  # this is the matrix of p_values
diag(p_mat) <- 1  # because there are no p-values on the diagonals, we have to insert 1 here for the plot to work
corrplot(okaro_corr_out$r, type = "upper", sig.level = 0.05, insig = "blank", p.mat = p_mat)
```

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

------------------------------------------------------------------------

***Challenge 3:*** *Repeat the correlation analysis for Lake Tarawera.
How do the correlations compare between the two lakes? Remember, you
need to first subset the `wq` dataframe for Lake Tarawera, then
`pivot_wider` so your columns are your variables, `rename` your columns
to get rid of symbols and spaces, remove the Date column and any NA’s,
then run your correlation using `rcorr`, and then plot your output!*

<details>
<summary>
Click to see a solution
</summary>

``` r
wq_tara <- wq %>% 
  filter(LocationName=='Lake Tarawera at Site 5 (Integrated)') %>% 
  select(Time, Value, Parameter, Unit)

tara_wide <- wq_tara %>% 
  mutate(Date = as.Date(Time)) %>% # keeping these lines in our workflow to remove Time and Unit
  select(-Time, -Unit) %>% 
  group_by(Date, Parameter) %>% 
  summarise(Value = mean(Value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = 'Parameter', values_from = 'Value')

tara_wide <- tara_wide %>% 
  rename('chla_mgm3' = "CHLA (mg/m^3)",
         'TN_gm3' = "TN (g/m^3)",
         "TP_gm3" = "TP (g/m^3)",
         "secchi_m" = "VC - SD (m)") %>% 
 ungroup() %>% 
 select(-Date)

tara_corr <- na.omit(tara_wide)
tara_corr_out <- rcorr(as.matrix(tara_corr), type = 'spearman')

p_mat <- tara_corr_out$P # this is the matrix of p_values
diag(p_mat) <- 1 # because there are no p-values on the diagonals, we have to insert 1 here for the plot to 
corrplot(tara_corr_out$r, type = 'upper',
                       sig.level = 0.05, insig = 'blank', p.mat = p_mat)
```

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->
</details>

------------------------------------------------------------------------

### Linear regression

Next, we will conduct linear regression between two variables that have
strong, significant correlation. Based on our correlation plots above,
let’s do this for chl-a and TN in Lake Okaro which have a strong,
positive correlation. We will go back to our `okaro_wide` dataframe for
this, which we created earlier. We will use the function `lm` to conduct
linear regression.

``` r
head(okaro_wide)
```

    ## # A tibble: 6 × 5
    ##   Date       chla_mgm3 TN_gm3 TP_gm3 secchi_m
    ##   <date>         <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 2015-01-22     23.1   0.900 0.0250    0.850
    ## 2 2015-02-19      1.4   0.37  0.0110    3.42 
    ## 3 2015-03-19      4.40  0.400 0.0170    4.07 
    ## 4 2015-04-16      4.60  0.335 0.0122    6.05 
    ## 5 2015-05-21      5.10  0.583 0.023     7.15 
    ## 6 2015-06-23      4     0.920 0.0595    7.35

``` r
model <- lm(chla_mgm3 ~ TN_gm3, data = okaro_wide)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = chla_mgm3 ~ TN_gm3, data = okaro_wide)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -35.058  -7.923   0.629   6.175  61.660 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -19.278      3.715   -5.19 1.21e-06 ***
    ## TN_gm3        55.804      5.098   10.95  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 16.11 on 94 degrees of freedom
    ##   (39 observations deleted due to missingness)
    ## Multiple R-squared:  0.5604, Adjusted R-squared:  0.5557 
    ## F-statistic: 119.8 on 1 and 94 DF,  p-value: < 2.2e-16

The `summary(model)` shows us that both the intercept and slope (on TN)
are significant and our adjusted R-squared is 0.56. That’s pretty good!
We can also plot this to visually show the relationship, and add the
equation, R-squared and p-value onto the plot using the function
`stat_poly_eq`. You can customize what information you want to show up
(equation, listed as `..eq.label..`, r-squared, listed as
`..rr.label..`, etc.)

``` r
ggplot(okaro_wide, aes(x = TN_gm3, y = chla_mgm3)) + geom_point() + geom_smooth(method = "lm",
    se = TRUE) + theme_bw() + stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,
    ..p.value.label.., sep = "~~~"))) + theme_bw() + ylab(expression(Chl * "-a" ~
    (mg/m^3)))
```

    ## Warning: The dot-dot notation (`..eq.label..`) was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `after_stat(eq.label)` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: Removed 39 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 39 rows containing non-finite outside the scale range
    ## (`stat_poly_eq()`).

    ## Warning: Removed 39 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->
Ok, we have a nice plot with our linear regression summary statistics!
It’s always good to plot some diagnostics of statistical models. Let’s
look at our residuals. Remember for a linear regression, our residuals
should be normally distributed. We will use the function `shapir.test`
which tests for normality. If the p-value of the Shapiro-Wilk is less
than 0.05, that indicates that the variable is *not* normally
distributed.

``` r
resid <- resid(model)
hist(resid)
```

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
shapiro.test(resid)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  resid
    ## W = 0.9152, p-value = 1.157e-05

Hmmm, the Shapiro-Wilk test returns a p-value of \< 0.05, which means
our residuals are not normally distributed and we have violated the
assumptions of the linear regression–not good! We can address this by
transforming our data to try and achieve a normal distribution of the
residuals. Typically, you can try by first transforming the response
variable, in our case chl-a.

``` r
model <- lm(log(chla_mgm3) ~ TN_gm3, data = okaro_wide)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = log(chla_mgm3) ~ TN_gm3, data = okaro_wide)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -1.99807 -0.49331 -0.02981  0.61071  1.61268 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   0.7974     0.1815   4.395 2.91e-05 ***
    ## TN_gm3        2.2160     0.2490   8.899 4.03e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7869 on 94 degrees of freedom
    ##   (39 observations deleted due to missingness)
    ## Multiple R-squared:  0.4573, Adjusted R-squared:  0.4515 
    ## F-statistic: 79.19 on 1 and 94 DF,  p-value: 4.028e-14

``` r
resid <- resid(model)
hist(resid)
```

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
shapiro.test(resid)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  resid
    ## W = 0.99111, p-value = 0.7761

Nice, that looks much better–our p-value is 0.78 after log-transforming
chl-a, which indicates that the residuals are normally distributed and
the assumptions of a linear regression have been met. Let’s also update
our plot with the new model and log transform the y-axis

``` r
ggplot(okaro_wide, aes(x = TN_gm3, y = log(chla_mgm3))) + geom_point() + geom_smooth(method = "lm",
    se = TRUE) + theme_bw() + stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,
    ..p.value.label.., sep = "~~~"))) + theme_bw() + ylab(expression(Log ~ Chl *
    "-a" ~ (mg/m^3)))
```

    ## Warning: Removed 39 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 39 rows containing non-finite outside the scale range
    ## (`stat_poly_eq()`).

    ## Warning: Removed 39 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

------------------------------------------------------------------------

***Challenge 4:*** *Repeat the linear regression analysis between chl-a
and secchi depth. Make a similar plot with `geom_smooth` and add the
equation, R2, and p-value to the plot. Test to see if the residuals are
normally distributed and re-adjust your plot accordingly.*

<details>
<summary>
Click to see a solution
</summary>

``` r
head(okaro_wide)
```

    ## # A tibble: 6 × 5
    ##   Date       chla_mgm3 TN_gm3 TP_gm3 secchi_m
    ##   <date>         <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 2015-01-22     23.1   0.900 0.0250    0.850
    ## 2 2015-02-19      1.4   0.37  0.0110    3.42 
    ## 3 2015-03-19      4.40  0.400 0.0170    4.07 
    ## 4 2015-04-16      4.60  0.335 0.0122    6.05 
    ## 5 2015-05-21      5.10  0.583 0.023     7.15 
    ## 6 2015-06-23      4     0.920 0.0595    7.35

``` r
model2 <- lm(log(chla_mgm3) ~ secchi_m, data = okaro_wide)
summary(model2)
```

    ## 
    ## Call:
    ## lm(formula = log(chla_mgm3) ~ secchi_m, data = okaro_wide)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.00524 -0.37442 -0.04635  0.46178  2.92624 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.69277    0.17683  20.883  < 2e-16 ***
    ## secchi_m    -0.39505    0.04288  -9.212  9.5e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.7694 on 93 degrees of freedom
    ##   (40 observations deleted due to missingness)
    ## Multiple R-squared:  0.4771, Adjusted R-squared:  0.4715 
    ## F-statistic: 84.86 on 1 and 93 DF,  p-value: 9.498e-15

``` r
resid2 <- resid(model2)
hist(resid2)
```

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
shapiro.test(resid2)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  resid2
    ## W = 0.97526, p-value = 0.06868

``` r
ggplot(okaro_wide, aes(x = secchi_m, y = log(chla_mgm3))) + geom_point() + geom_smooth(method = "lm",
    se = TRUE) + theme_bw() + stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label..,
    ..p.value.label.., sep = "~~~"))) + theme_bw() + xlab("Secchi depth (m)") + ylab(expression(Log ~
    Chl * "-a" ~ (mg/m^3)))
```

    ## Warning: Removed 40 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 40 rows containing non-finite outside the scale range
    ## (`stat_poly_eq()`).

    ## Warning: Removed 40 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](R_Tutorial_5_2025_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->
</details>

------------------------------------------------------------------------

### Testing for statistical differences between two variables: t-tests (parametric) and Wilcoxon rank-sum (non-parametric)

First, we need to check if chl-a and TN are normally distributed, which
is a requirement for regression

Could test the difference in chl-a between Rotorua and Rotoiti?
Introduce transformations to meet normality assumptions. (introduce
shapiro.test() to check normality?) Is this useful?

``` r
head(okaro_wide)
```

    ## # A tibble: 6 × 5
    ##   Date       chla_mgm3 TN_gm3 TP_gm3 secchi_m
    ##   <date>         <dbl>  <dbl>  <dbl>    <dbl>
    ## 1 2015-01-22     23.1   0.900 0.0250    0.850
    ## 2 2015-02-19      1.4   0.37  0.0110    3.42 
    ## 3 2015-03-19      4.40  0.400 0.0170    4.07 
    ## 4 2015-04-16      4.60  0.335 0.0122    6.05 
    ## 5 2015-05-21      5.10  0.583 0.023     7.15 
    ## 6 2015-06-23      4     0.920 0.0595    7.35

``` r
shapiro.test(okaro_wide$chla_mgm3)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  okaro_wide$chla_mgm3
    ## W = 0.60272, p-value = 8.338e-15

``` r
shapiro.test(okaro_wide$TN_gm3)
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  okaro_wide$TN_gm3
    ## W = 0.83545, p-value = 5.267e-09

The p-value for chl-a and TN is \< 0.05, which indicates that they are
*not* normally distributed. In order to run a linear regression, we will
need to log-transform these variables. We will run the shapiro.test
again on the log-transformed chl-a.

The p-value is now 0.

### Wilcoxon rank sum

This test is a non-parametric alternative to a t-test and, as such, is
for non-normally distributed variables. Run wilcoxon test to compare if
secchi depth is different between Tarawera and Okaro (two different
trophic states)

### ANOVA

Test if chl- differs by season. challenge is to run this analysis for
another lake

### Trend analysis

Insert James’ stuff!
