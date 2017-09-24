---
title: "Opioid Utilization Exploratory Data Analysis"
author: "Julia Silge"
date: '2017-09-24'
output: html_document
---




## Clean and tidy opioid utilization data

Let's open up the dataset and start munging and preparing it.


```r
library(tidyverse)
library(readxl)

opioids_raw <- bind_rows(
    1:3 %>% 
        map_df(~ read_excel("./OpiodDispensation_Zip_2014.xlsx", 
                            sheet = .x , skip = 1) %>%
                   mutate(date = as.Date(paste0("2014-", .x + 8, "-01"))) %>%
                   rename(zipcode = `Zip Code`) %>%
                   filter(!is.na(zipcode))) %>%
        mutate(zipcode = as.numeric(zipcode)) %>%
        filter(!is.na(zipcode)),
    read_excel("./OpiodDispensation_Zip_2014.xlsx", 
               sheet = 4, skip = 1) %>%
        mutate(date = as.Date("2014-12-01")) %>%
        rename(zipcode = `Dispensary Postal Code`) %>%
        mutate(zipcode = as.numeric(zipcode)),
    1:12 %>% 
        map_df(~ read_excel("./OpiodDispensation_2015.xlsx", 
                            sheet = .x, skip = 1) %>%
                   mutate(date = as.Date(paste0("2015-", .x, "-01"))) %>%
                   rename(zipcode = `Dispensary Postal Code`) %>%
                   filter(!is.na(zipcode))),
    1:12 %>% 
        map_df(~ read_excel("./OpiodDispensation_2016.xlsx", 
                            sheet = .x, skip = 1) %>%
                   mutate(date = as.Date(paste0("2016-", .x, "-01"))) %>%
                   rename(zipcode = `Zip Code`) %>%
                   filter(!is.na(zipcode))),
    1:2 %>% 
        map_df(~ read_excel("./OpiodDispensation_2017.xlsx", 
                            sheet = .x, skip = 1) %>%
                   mutate(date = as.Date(paste0("2017-", .x, "-01"))) %>%
                   rename(zipcode = `Dispensary Postal Code`) %>%
                   filter(!is.na(zipcode)))) %>%
    rename(total = Totals) 

opioids_raw[is.na(opioids_raw)] <- 0

opioids_raw
```

```
## # A tibble: 32,789 x 26
##    zipcode buprenorphine butorphanol codeine dihydrocodeine fentanyl
##      <dbl>         <dbl>       <dbl>   <dbl>          <dbl>    <dbl>
##  1    1810            36           0     119              0       27
##  2    1922             0           0       4              0        0
##  3    3101             0           0       0              0        0
##  4    6032             0           0       1              0        0
##  5    8003             0           0       0              0        0
##  6    8066             0           0       0              0       44
##  7    8873             0           0       0              0        0
##  8   11232             0           0       0              0        0
##  9   15146             0           0       0              0        0
## 10   15701             0           0       0              0        0
## # ... with 32,779 more rows, and 20 more variables: hydrocodone <dbl>,
## #   hydromorphone <dbl>, levorphanol <dbl>, meperidine <dbl>, methadone <dbl>,
## #   morphine <dbl>, opium <dbl>, oxycodone <dbl>, oxymetholone <dbl>,
## #   oxymorphone <dbl>, paregoric <dbl>, pentazocine <dbl>, sufentanil <dbl>,
## #   tapentadol <dbl>, tramadol <dbl>, total <dbl>, `C-IV` <dbl>, `C-III` <dbl>,
## #   `C-II` <dbl>, date <date>
```

Let's connect [those zipcodes to counties](http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt), in order to clean out the zip codes which are not located in Texas. (Probably bad data entry?)


```r
zcta_county <- read_csv("http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt") %>%
    select(ZCTA5, STATE, COUNTY, GEOID) %>%
    filter(STATE == 48)

opioids_dedupes <- opioids_raw %>% 
    mutate(zipcode = as.character(zipcode)) %>%
    inner_join(zcta_county, by = c("zipcode" = "ZCTA5")) %>%
    select(-STATE, -COUNTY, -GEOID) %>%
    distinct(zipcode, date, .keep_all = TRUE)

opioids_dedupes
```

```
## # A tibble: 30,082 x 26
##    zipcode buprenorphine butorphanol codeine dihydrocodeine fentanyl
##      <chr>         <dbl>       <dbl>   <dbl>          <dbl>    <dbl>
##  1   75001            23           0      36              0       67
##  2   75002           104          15     234              0       73
##  3   75006            48           3     127              0       21
##  4   75007            35          11     143              0       38
##  5   75009            11           6      20              0        8
##  6   75010           118           6     157              0       48
##  7   75013            31          14     100              0       22
##  8   75019            40          17     135              0       18
##  9   75020           116          10     204              0       60
## 10   75022            46           2      94              0       14
## # ... with 30,072 more rows, and 20 more variables: hydrocodone <dbl>,
## #   hydromorphone <dbl>, levorphanol <dbl>, meperidine <dbl>, methadone <dbl>,
## #   morphine <dbl>, opium <dbl>, oxycodone <dbl>, oxymetholone <dbl>,
## #   oxymorphone <dbl>, paregoric <dbl>, pentazocine <dbl>, sufentanil <dbl>,
## #   tapentadol <dbl>, tramadol <dbl>, total <dbl>, `C-IV` <dbl>, `C-III` <dbl>,
## #   `C-II` <dbl>, date <date>
```

Now we have just entries from zip codes in Texas

## Download population in each Texas zip code from US Census

Let's find the population in each zip code so we can do some relative comparisons.


```r
library(tidycensus)

population_zip <- get_acs(geography = "zcta", 
                          variables = "B01003_001", 
                          geometry = TRUE) 
```

```
## Error in get_acs(geography = "zcta", variables = "B01003_001", geometry = TRUE): A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.
```

```r
population_zip
```

```
## Error in eval(expr, envir, enclos): object 'population_zip' not found
```

```r
## also get Texas counties, for later

population_county <- get_acs(geography = "county", 
                             variables = "B01003_001", 
                             state = "TX",
                             geometry = TRUE) 
```

```
## Error in get_acs(geography = "county", variables = "B01003_001", state = "TX", : A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key` function to use it throughout your tidycensus session.
```

```r
population_county
```

```
## Error in eval(expr, envir, enclos): object 'population_county' not found
```

That's actually the whole US, so let's get to just the Texas zip codes again.


```r
texas_opioids <- opioids_dedupes %>%
    inner_join(population_zip %>%
                   mutate(zipcode = GEOID),
               by = "zipcode") %>%
    select(-GEOID) %>%
    st_as_sf()
```

```
## Error in eval(lhs, parent, parent): object 'population_zip' not found
```

```r
texas_opioids
```

```
## Error in eval(expr, envir, enclos): object 'texas_opioids' not found
```

There we go!

## Calculate rate of opioid use

Next let's calculate a *rate* of opioid utilization that is the number of total prescriptions divided by the population (per 1,000 people).


```r
opioids_joined <- texas_opioids %>%
    as.data.frame() %>%
    group_by(zipcode) %>%
    summarise(total = median(total)) %>%
    ungroup %>%
    inner_join(texas_opioids %>% 
                      distinct(zipcode, estimate, geometry)) %>%
    mutate(opioid_rate = total / estimate * 1e3,
           opioid_rate = ifelse(is.infinite(opioid_rate), NA, opioid_rate))
```

```
## Error in eval(lhs, parent, parent): object 'texas_opioids' not found
```

```r
opioids_joined
```

```
## Error in eval(expr, envir, enclos): object 'opioids_joined' not found
```


#### Opioid prescription rate in the top 10 most populous Texas zip codes


```r
texas_opioids %>% 
    as.data.frame() %>%
    group_by(zipcode, estimate) %>%
    summarise(total = median(total)) %>%
    ungroup %>%
    mutate(opioid_rate = total / estimate * 1e3,
           opioid_rate = ifelse(is.infinite(opioid_rate), NA, opioid_rate)) %>%
    top_n(10, estimate) %>%
    arrange(desc(estimate)) %>%
    select(zipcode, opioid_rate) %>%
    kable(col.names = c("Zip code", "Median monthly prescriptions per 1k population"))
```

```
## Error in eval(lhs, parent, parent): object 'texas_opioids' not found
```


We can also map the state as a whole.


```r
library(sf)
library(leaflet)
library(stringr)

opioids_map <- opioids_joined %>%
    mutate(opioid_rate = ifelse(opioid_rate > 200, 200, opioid_rate))
```

```
## Error in eval(lhs, parent, parent): object 'opioids_joined' not found
```

```r
pal <- colorNumeric(palette = "viridis", domain = opioids_map$opioid_rate)
```

```
## Error in colorNumeric(palette = "viridis", domain = opioids_map$opioid_rate): object 'opioids_map' not found
```

```r
opioids_map %>%
    st_as_sf() %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet(width = "100%") %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(popup = ~ zipcode,
                stroke = FALSE,
                smoothFactor = 0,
                fillOpacity = 0.7,
                color = ~ pal(opioid_rate)) %>%
    addLegend("bottomright", 
              pal = pal, 
              values = ~ opioid_rate,
              title = "Monthly prescriptions\nper 1k population",
              opacity = 1)
```

```
## Error in eval(lhs, parent, parent): object 'opioids_map' not found
```

This whole exercise has made me see how difficult it is to draw robust conclusions about this kind of measurement for areas that are as different from each other as rural and urban Texas are. There are zip codes in west Texas with no pharmacies in them at all, and people must be driving into neighboring areas if they have opioid prescriptions. This drives up the opioid rate in those zip codes that do have pharmacies. This would *not* happen as much in the more populated eastern half of the state, although obviously it is easy to drive to a different zip code in a big city as well. It's not obvious from a simple analysis where geographically there is more or less opioid utilization. Pharmacies are fewer in number than people, and the population is more spread out than the pharmacies. Only in urban areas are the number of pharmacies a good tracer of the number of people.



```r
library(plotly)
p <- opioids_joined %>%
    filter(opioid_rate < 200) %>%
    ggplot(aes(estimate, opioid_rate, tooltip = zipcode)) +
    geom_point(alpha = 0.5, size = 1.5, color = "midnightblue") +
    scale_x_log10(labels = scales::comma_format()) +
    labs(x = "Zip code population",
         y = "Median monthly opioid prescription per 1k population",
         title = "Opioid prescriptions in Texas")
```

```
## Error in eval(lhs, parent, parent): object 'opioids_joined' not found
```

```r
ggplotly(p, tooltip = "zipcode")
```

```
## Error in ggplotly(p, tooltip = "zipcode"): object 'p' not found
```

Patterns of pharmacy locations make the opioid utilization rate in rural Texas difficult to measure.

## Which drugs are growing the fastest?

Let's examine how these prescriptions are changing with time.



```r
texas_opioids %>%
    group_by(date) %>%
    summarise_at(vars(total:`C-II`), sum) %>%
    gather(drug, prescriptions, total:`C-II`) %>%
    filter(prescriptions < 1e7) %>%
    ggplot(aes(date, prescriptions, color = drug)) +
    geom_line(size = 1.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, lty = 2) +
    scale_y_continuous(labels = scales::comma_format()) +
    expand_limits(y = 0) +
    theme(legend.title=element_blank()) +
    labs(x = NULL, y = "Total monthly prescriptions",
         title = "Opioid prescriptions in Texas",
         subtitle = "Total prescriptions are increasing\nIs the overall increase being driven by Schedule C-IV drugs?")
```

```
## Error in eval(lhs, parent, parent): object 'texas_opioids' not found
```


Let's use linear regression modeling to find the individual drugs that are being prescribed more often now compared to two years ago.


```r
library(broom)

opioids_by_month <- texas_opioids %>%
    as.data.frame() %>%
    group_by(date) %>%
    summarise_at(vars(buprenorphine:tramadol), sum) %>%
    gather(drug, prescriptions, buprenorphine:tramadol) %>%
    filter(prescriptions < 1e6)
```

```
## Error in eval(lhs, parent, parent): object 'texas_opioids' not found
```

```r
time_models <- opioids_by_month %>%
    nest(-drug) %>%
    mutate(models = map(data, ~ lm(prescriptions ~ date, .))) %>%
    unnest(map(models, tidy)) %>%
    filter(term == "date") %>%
    arrange(desc(estimate))
```

```
## Error in eval(lhs, parent, parent): object 'opioids_by_month' not found
```

```r
time_models
```

```
## Error in eval(expr, envir, enclos): object 'time_models' not found
```

Which drugs are being prescribed *more*? These are the five fastest growing over this time period.


```r
opioids_by_month %>%
    inner_join(time_models %>%
                   top_n(5, estimate)) %>%
    ggplot(aes(date, prescriptions, color = drug)) +
    geom_line(size = 1.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, lty = 2) +
    scale_y_continuous(labels = scales::comma_format()) +
    expand_limits(y = 0) +
    theme(legend.title=element_blank()) +
    labs(x = NULL, y = "Total monthly prescriptions",
         title = "Opioid prescriptions in Texas",
         subtitle = "The fastest growing five drugs are responsible for the overall increase\nTramadol and codeine appear to be the biggest contributors")
```

```
## Error in eval(lhs, parent, parent): object 'opioids_by_month' not found
```

You can tell from this plot that this modeling procedure did not necessarily find the largest; it identified the fastest growing. Morphine and friends at the bottom of the graph are not large contributors but they are among the fastest growing.

Are there drugs which are decreasing in number of prescriptions?


```r
opioids_by_month %>%
    inner_join(time_models %>%
                   top_n(-5, estimate)) %>%
    ggplot(aes(date, prescriptions, color = drug)) +
    geom_line(size = 1.5, alpha = 0.8) +
    geom_smooth(method = "lm", se = FALSE, lty = 2) +
    scale_y_continuous(labels = scales::comma_format()) +
    expand_limits(y = 0) +
    theme(legend.title=element_blank()) +
    labs(x = NULL, y = "Total monthly prescriptions",
         title = "Opioid prescriptions in Texas",
         subtitle = "These are the fastest shrinking five drugs\nHydrocodone use is on the decline")
```

```
## Error in eval(lhs, parent, parent): object 'opioids_by_month' not found
```


I could also look at which zip codes are increasing the fastest. That might be a way to look at geographical information that doesn't get so biased by the urban/rural issue.



