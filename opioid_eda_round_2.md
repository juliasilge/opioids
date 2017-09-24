---
title: "Opioid Utilization Exploratory Data Analysis"
author: "Julia Silge"
date: "3/3/2017"
output: html_document
---




## Clean and tidy opioid utilization data

Let's open up the dataset and start munging and preparing it.


```r
library(acs)
```

```
## Error in library(acs): there is no package called 'acs'
```

```r
library(tidyverse)
library(readxl)
library(stringr)
library(reshape2)

opioids_raw <- bind_rows(
    read_excel("./OpiodDispensation_Zip_2014.xlsx", 
               sheet = 1, skip = 1) %>%
        mutate(month = "September") %>%
        rename(zipcode = `Zip Code`),
    read_excel("./OpiodDispensation_Zip_2014.xlsx", 
               sheet = 2, skip = 1) %>%
        mutate(month = "October") %>%
        rename(zipcode = `Zip Code`),
    read_excel("./OpiodDispensation_Zip_2014.xlsx", 
               sheet = 3, skip = 1) %>%
        mutate(month = "November") %>%
        rename(zipcode = `Zip Code`),
    read_excel("./OpiodDispensation_Zip_2014.xlsx", 
               sheet = 4, skip = 1) %>%
        mutate(month = "December") %>%
        rename(zipcode = `Dispensary Postal Code`)) %>%
    rename(total = Totals) 

opioids_raw[is.na(opioids_raw)] <- 0

opioids_raw
```

```
## # A tibble: 4,376 x 26
##    zipcode buprenorphine butorphanol codeine dihydrocodeine fentanyl
##      <chr>         <dbl>       <dbl>   <dbl>          <dbl>    <dbl>
##  1   01810            36           0     119              0       27
##  2   01922             0           0       4              0        0
##  3   03101             0           0       0              0        0
##  4   06032             0           0       1              0        0
##  5   08003             0           0       0              0        0
##  6   08066             0           0       0              0       44
##  7   08873             0           0       0              0        0
##  8   11232             0           0       0              0        0
##  9   15146             0           0       0              0        0
## 10   15701             0           0       0              0        0
## # ... with 4,366 more rows, and 20 more variables: hydrocodone <dbl>,
## #   hydromorphone <dbl>, levorphanol <dbl>, meperidine <dbl>, methadone <dbl>,
## #   morphine <dbl>, opium <dbl>, oxycodone <dbl>, oxymetholone <dbl>,
## #   oxymorphone <dbl>, paregoric <dbl>, pentazocine <dbl>, sufentanil <dbl>,
## #   tapentadol <dbl>, tramadol <dbl>, total <dbl>, `C-IV` <dbl>, `C-III` <dbl>,
## #   `C-II` <dbl>, month <chr>
```

Let's connect [those zipcodes to counties](http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt). Zipcodes are great, but a bigger unit like counties will be better for mapping purposes.


```r
zcta_county <- read_csv("http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt") %>%
    select(ZCTA5, STATE, COUNTY, GEOID) %>%
    filter(STATE == 48)

texas_opioids <- opioids_raw %>% 
    inner_join(zcta_county, by = c("zipcode" = "ZCTA5"))

texas_opioids
```

```
## # A tibble: 5,584 x 29
##    zipcode buprenorphine butorphanol codeine dihydrocodeine fentanyl
##      <chr>         <dbl>       <dbl>   <dbl>          <dbl>    <dbl>
##  1   75001            23           0      36              0       67
##  2   75002           104          15     234              0       73
##  3   75006            48           3     127              0       21
##  4   75007            35          11     143              0       38
##  5   75007            35          11     143              0       38
##  6   75009            11           6      20              0        8
##  7   75009            11           6      20              0        8
##  8   75010           118           6     157              0       48
##  9   75013            31          14     100              0       22
## 10   75019            40          17     135              0       18
## # ... with 5,574 more rows, and 23 more variables: hydrocodone <dbl>,
## #   hydromorphone <dbl>, levorphanol <dbl>, meperidine <dbl>, methadone <dbl>,
## #   morphine <dbl>, opium <dbl>, oxycodone <dbl>, oxymetholone <dbl>,
## #   oxymorphone <dbl>, paregoric <dbl>, pentazocine <dbl>, sufentanil <dbl>,
## #   tapentadol <dbl>, tramadol <dbl>, total <dbl>, `C-IV` <dbl>, `C-III` <dbl>,
## #   `C-II` <dbl>, month <chr>, STATE <int>, COUNTY <chr>, GEOID <int>
```


## Download population in each Texas county from US Census

Let's find the population in each Texas county so we can do some relative comparisons.


```r
tx_counties <- geo.make(state="TX", county="*")
```

```
## Error in geo.make(state = "TX", county = "*"): could not find function "geo.make"
```

```r
popfetch <- acs.fetch(geography = tx_counties, 
                      endyear = 2014,
                      span = 5, 
                      table.number = "B01003",
                      col.names = "pretty")
```

```
## Error in acs.fetch(geography = tx_counties, endyear = 2014, span = 5, : could not find function "acs.fetch"
```

```r
myfips <- acs::geography(popfetch) %>%  
                mutate(FIPS = str_c(str_sub(str_c("000", state),-2),
                                    str_sub(str_c("000", county),-3))) %>%
                select(FIPS)
```

```
## Error in loadNamespace(name): there is no package called 'acs'
```

```r
acs::geography(popfetch) <- cbind(myfips, acs::geography(popfetch))
```

```
## Error in cbind(myfips, acs::geography(popfetch)): object 'myfips' not found
```

```r
population <- melt(estimate(popfetch)) %>%
    as_tibble() %>%
    select(county_fips = Var1, total_pop = value)
```

```
## Error in estimate(popfetch): could not find function "estimate"
```

```r
population
```

```
## # A tibble: 4,060 x 3
##        country  year population
##          <chr> <int>      <int>
##  1 Afghanistan  1995   17586073
##  2 Afghanistan  1996   18415307
##  3 Afghanistan  1997   19021226
##  4 Afghanistan  1998   19496836
##  5 Afghanistan  1999   19987071
##  6 Afghanistan  2000   20595360
##  7 Afghanistan  2001   21347782
##  8 Afghanistan  2002   22202806
##  9 Afghanistan  2003   23116142
## 10 Afghanistan  2004   24018682
## # ... with 4,050 more rows
```

## Join Census data to opioid data

Next let's join the data from the Census to the opioid utilization data. We will calculate a *rate* of opioid utilization that is the number of total prescriptions divided by the population (per 10,000 people).


```r
opioids_joined <- texas_opioids %>%
    group_by(GEOID, month) %>% 
    summarise(total = sum(total)) %>%
    ungroup %>%
    inner_join(population, by = c("GEOID" = "county_fips")) %>%
    mutate(opioid_rate = total / total_pop * 1e3,
           opioid_rate = ifelse(is.infinite(opioid_rate), NA, opioid_rate))
```

```
## Error: `by` can't contain join column `county_fips` which is missing from RHS
```

```r
opioids_joined
```

```
## Error in eval(expr, envir, enclos): object 'opioids_joined' not found
```

Now let's reshape this dataset for plotting purposes. We want columns with the rates and totals for each month.


```r
rate_spread <- opioids_joined %>%
    select(GEOID, month, opioid_rate) %>%
    spread(month, opioid_rate, fill = 0)
```

```
## Error in eval(lhs, parent, parent): object 'opioids_joined' not found
```

```r
totals_spread <- opioids_joined %>%
    select(GEOID, month, total) %>%
    spread(month, total, fill = 0)
```

```
## Error in eval(lhs, parent, parent): object 'opioids_joined' not found
```

```r
cutoff <- 400

opioids_spread <- rate_spread %>%
    left_join(totals_spread, by = "GEOID", suffix = c("Rate", "Total")) %>%
    rename(December = DecemberRate,
           November = NovemberRate,
           October = OctoberRate,
           September = SeptemberRate) %>%
    replace_na(list(DecemberTotal = 0,
                    NovemberTotal = 0,
                    OctoberTotal = 0,
                    SeptemberTotal = 0)) %>%
    mutate(December = ifelse(December > cutoff, cutoff, December),
           November = ifelse(November > cutoff, cutoff, November),
           October = ifelse(October > cutoff, cutoff, October),
           September = ifelse(September > cutoff, cutoff, September))
```

```
## Error in eval(lhs, parent, parent): object 'rate_spread' not found
```

```r
opioids_spread
```

```
## Error in eval(expr, envir, enclos): object 'opioids_spread' not found
```


## Making a map

I'll use a [geojson file for Texas counties](https://github.com/TNRIS/tx.geojson) to make this map. The map will show the rate of opioid prescriptions (per 1000 population) in each county, and click on a county to see its name and the number (count, *not* rate) of prescriptions there in that month. Rates that are higher than 400 prescriptions per 1000 population are represented by the same color as 400.


```r
library(leaflet)
library(scales)


counties <- geojsonio::geojson_read("./tx_counties.geojson", what = "sp")
```

```
## Error in loadNamespace(name): there is no package called 'geojsonio'
```

```r
counties@data <- counties@data %>%
    mutate(FIPS = as.integer(as.character(FIPS))) %>%
    left_join(opioids_spread, by = c("FIPS" = "GEOID")) 
```

```
## Error in eval(lhs, parent, parent): object 'counties' not found
```

```r
counties@data[is.na(counties@data)] <- 0
```

```
## Error in counties@data[is.na(counties@data)] <- 0: object 'counties' not found
```

```r
pal <- colorNumeric(palette = "viridis", domain = c(0, cutoff))

leaflet(counties, width = "100%") %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
        popup = ~paste0("<b>", COUNTY, "</b><br/>", comma_format()(SeptemberTotal), " prescriptions"),
        stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5,
        color = ~pal(September), group = "September"
    ) %>%
    addPolygons(
        popup = ~paste0("<b>", COUNTY, "</b><br/>", comma_format()(OctoberTotal), " prescriptions"),
        stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5,
        color = ~pal(October), group = "October"
    ) %>%
    addPolygons(
        popup = ~paste0("<b>", COUNTY, "</b><br/>", comma_format()(NovemberTotal), " prescriptions"), 
        stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5,
        color = ~pal(November), group = "November"
    ) %>%
    addPolygons(
        popup = ~paste0("<b>", COUNTY, "</b><br/>", comma_format()(DecemberTotal), " prescriptions"),
        stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5,
        color = ~pal(December), group = "December"
    ) %>%
    addLayersControl(
        baseGroups = c("September", "October", "November", "December"),
        options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend("bottomright", pal = pal, values = c(0, cutoff),
              title = "Prescriptions per 1k population",
              opacity = 1
    )
```

```
## Error in structure(list(options = options), leafletData = data): object 'counties' not found
```

A couple of things to note:

- The highest rates of opioid prescription are in rural counties but there is more uncertainty in measuring the rate there because populations are so low.
- The counties with the highest rates are pretty consistent within these months, indicating that this isn't just noise; this is a real effect of some kind.
- Some of the very rural counties with high rates of presciptions are next to counties with *no* prescriptions so people may be driving from one county to another to buy opioids from a pharmacy. This will drive the rate up in the rural counties that have pharmacies. (Most people certainly buy from a pharmacy in their own county, but this is less likely to be true in a rural area.)

In case it is helpful, here is just a population map of Texas.


```r
counties@data <- counties@data %>%
    left_join(population, by = c("FIPS" = "county_fips")) 
```

```
## Error in eval(lhs, parent, parent): object 'counties' not found
```

```r
pal <- colorQuantile(palette = "viridis", domain = counties@data$total_pop, n = 10)
```

```
## Error in colorQuantile(palette = "viridis", domain = counties@data$total_pop, : object 'counties' not found
```

```r
leaflet(counties, width = "100%") %>%
    addProviderTiles("CartoDB.Positron") %>%
    addPolygons(
        popup = ~COUNTY,
        stroke = FALSE, fillOpacity = 0.7, smoothFactor = 0.5,
        color = ~pal(total_pop)
    ) %>%
    addLegend("bottomright", pal = pal, values = counties@data$total_pop,
              title = "Population Percentiles",
              opacity = 1
    )
```

```
## Error in structure(list(options = options), leafletData = data): object 'counties' not found
```



