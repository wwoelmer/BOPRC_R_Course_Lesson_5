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
```

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
okaro_corr <- wq_okaro %>% 
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
okaro_corr <- okaro_corr %>% 
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
okaro_corr <- okaro_corr %>%
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
okaro_corr <- okaro_corr %>%
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
How do the correlations compare between the two lakes?*

<details>
<summary>
Click to see a solution
</summary>

``` r
wq_tara <- wq %>% 
  filter(LocationName=='Lake Tarawera at Site 5 (Integrated)') %>% 
  select(Time, Value, Parameter, Unit)

tara_corr <- wq_tara %>% 
  mutate(Date = as.Date(Time)) %>% # keeping these lines in our workflow to remove Time and Unit
  select(-Time, -Unit) %>% 
  group_by(Date, Parameter) %>% 
  summarise(Value = mean(Value, na.rm = TRUE)) %>% 
  pivot_wider(names_from = 'Parameter', values_from = 'Value')

tara_corr <- tara_corr %>% 
  rename('chla_mgm3' = "CHLA (mg/m^3)",
         'TN_gm3' = "TN (g/m^3)",
         "TP_gm3" = "TP (g/m^3)",
         "secchi_m" = "VC - SD (m)") %>% 
 ungroup() %>% 
 select(-Date)

tara_corr <- na.omit(tara_corr)
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

Conduct linear regression between two variables that have strong,
significant correlation (e.g., chl-a and secchi in Tarawera). then the
challenge will be to conduct linear regression between two different
variables (e.g., TN and chl-a)

### T-test

Could test the difference in chl-a between Rotorua and Rotoiti?
Introduce transformations to meet normality assumptions. (introduce
shapiro.test() to check normality?) Is this useful?

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
