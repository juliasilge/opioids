---
title: "Opioid Utilization Exploratory Data Analysis"
author: "Julia Silge"
date: '2017-07-23'
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
                   mutate(date = as.Date(paste0("2014-", .x + 5, "-01"))) %>%
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

Let's connect [those zipcodes to counties](http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt). Zipcodes can be good, but a bigger unit like counties will be better for analysis and mapping purposes.


```r
zcta_county <- read_csv("http://www2.census.gov/geo/docs/maps-data/data/rel/zcta_county_rel_10.txt") %>%
    select(ZCTA5, STATE, COUNTY, GEOID) %>%
    filter(STATE == 48) %>%
    anti_join(data_frame(ZCTA5 = rep("79720", 2), 
                         GEOID = c(48033, 48173)))

opioids_dupes <- opioids_raw %>% 
    mutate(zipcode = as.character(zipcode)) %>%
    inner_join(zcta_county, by = c("zipcode" = "ZCTA5"))

opioids_dupes
```

```
## # A tibble: 42,141 x 29
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
## # ... with 42,131 more rows, and 23 more variables: hydrocodone <dbl>,
## #   hydromorphone <dbl>, levorphanol <dbl>, meperidine <dbl>, methadone <dbl>,
## #   morphine <dbl>, opium <dbl>, oxycodone <dbl>, oxymetholone <dbl>,
## #   oxymorphone <dbl>, paregoric <dbl>, pentazocine <dbl>, sufentanil <dbl>,
## #   tapentadol <dbl>, tramadol <dbl>, total <dbl>, `C-IV` <dbl>, `C-III` <dbl>,
## #   `C-II` <dbl>, date <date>, STATE <int>, COUNTY <chr>, GEOID <int>
```

There need to be deduped because the zipcodes overlap multiple counties. Which county do we want to count the prescriptions in? The most populous ones.

## Download population in each Texas county from US Census

Let's find the population in each Texas county so we can do some relative comparisons.


```r
library(tidycensus)

population <- get_acs(geography = "county", 
                     variables = "B01003_001", 
                     state = "TX",
                     geometry = TRUE) 

population
```

```
## Simple feature collection with 254 features and 5 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: -106.6456 ymin: 25.83738 xmax: -93.50829 ymax: 36.5007
## epsg (SRID):    4269
## proj4string:    +proj=longlat +datum=NAD83 +no_defs
## # A tibble: 254 x 6
##    GEOID                    NAME   variable estimate   moe          geometry
##    <chr>                   <chr>      <chr>    <dbl> <dbl>  <simple_feature>
##  1 48007   Aransas County, Texas B01003_001    24292     0 <MULTIPOLYGON...>
##  2 48025       Bee County, Texas B01003_001    32659     0 <MULTIPOLYGON...>
##  3 48035    Bosque County, Texas B01003_001    17971     0 <MULTIPOLYGON...>
##  4 48067      Cass County, Texas B01003_001    30328     0 <MULTIPOLYGON...>
##  5 48083   Coleman County, Texas B01003_001     8536     0 <MULTIPOLYGON...>
##  6 48091     Comal County, Texas B01003_001   119632     0 <MULTIPOLYGON...>
##  7 48103     Crane County, Texas B01003_001     4730     0 <MULTIPOLYGON...>
##  8 48139     Ellis County, Texas B01003_001   157058     0 <MULTIPOLYGON...>
##  9 48151    Fisher County, Texas B01003_001     3858     0 <MULTIPOLYGON...>
## 10 48167 Galveston County, Texas B01003_001   308163     0 <MULTIPOLYGON...>
## # ... with 244 more rows
```

Now we can dedupe the county-level records.


```r
texas_opioids <- opioids_dupes %>%
    inner_join(population %>% 
                   mutate(GEOID = as.integer(GEOID))) %>%
    group_by(date, zipcode) %>%
    top_n(1, estimate) %>%
    ungroup
```



## Join Census data to opioid data

Next let's join the data from the Census to the opioid utilization data. We will calculate a *rate* of opioid utilization that is the number of total prescriptions divided by the population (per 1,000 people).


```r
library(sf)

opioids_joined <- texas_opioids %>%
    group_by(GEOID) %>%
    summarise(total = median(total)) %>%
    ungroup %>%
    inner_join(population %>% 
                      mutate(GEOID = as.integer(GEOID))) %>%
    mutate(opioid_rate = total / estimate * 1e3,
           opioid_rate = ifelse(is.infinite(opioid_rate), NA, opioid_rate)) %>%
    st_as_sf()

opioids_joined
```

```
## Simple feature collection with 194 features and 7 fields
## geometry type:  MULTIPOLYGON
## dimension:      XY
## bbox:           xmin: -106.6456 ymin: 25.83738 xmax: -93.50829 ymax: 36.5007
## epsg (SRID):    4269
## proj4string:    +proj=longlat +datum=NAD83 +no_defs
## # A tibble: 194 x 8
##    GEOID  total                   NAME   variable estimate   moe opioid_rate
##    <int>  <dbl>                  <chr>      <chr>    <dbl> <dbl>       <dbl>
##  1 48001 2982.0 Anderson County, Texas B01003_001    57915     0  51.4892515
##  2 48003  695.5  Andrews County, Texas B01003_001    16775     0  41.4605067
##  3 48005 1007.5 Angelina County, Texas B01003_001    87748     0  11.4817432
##  4 48007 2092.5  Aransas County, Texas B01003_001    24292     0  86.1394698
##  5 48013  778.5 Atascosa County, Texas B01003_001    47050     0  16.5462274
##  6 48015  639.0   Austin County, Texas B01003_001    28886     0  22.1214429
##  7 48021  770.0  Bastrop County, Texas B01003_001    76948     0  10.0067578
##  8 48025  795.5      Bee County, Texas B01003_001    32659     0  24.3577574
##  9 48027 1579.0     Bell County, Texas B01003_001   326041     0   4.8429492
## 10 48029 1403.0    Bexar County, Texas B01003_001  1825502     0   0.7685557
## # ... with 184 more rows, and 1 more variables: geometry <simple_feature>
```


#### Opioid prescription rate in the top 10 most populous Texas counties


```r
texas_opioids %>% 
    group_by(GEOID) %>%
    summarise(total = median(total)) %>%
    ungroup %>%
    inner_join(population %>% 
                      mutate(GEOID = as.integer(GEOID))) %>%
    mutate(opioid_rate = total / estimate * 1e3,
           opioid_rate = ifelse(is.infinite(opioid_rate), NA, opioid_rate)) %>%
    top_n(10, estimate) %>%
    arrange(desc(estimate)) %>%
    select(GEOID, NAME, opioid_rate) %>%
    kable(col.names = c("GEOID", "Country name", "Median monthly prescriptions per 1k population"))
```



| GEOID|Country name            | Median monthly prescriptions per 1k population|
|-----:|:-----------------------|----------------------------------------------:|
| 48201|Harris County, Texas    |                                      0.3094325|
| 48113|Dallas County, Texas    |                                      0.6116693|
| 48439|Tarrant County, Texas   |                                      0.9832721|
| 48029|Bexar County, Texas     |                                      0.7685557|
| 48453|Travis County, Texas    |                                      1.1732767|
| 48085|Collin County, Texas    |                                      1.6225651|
| 48141|El Paso County, Texas   |                                      1.3512294|
| 48215|Hidalgo County, Texas   |                                      0.9075739|
| 48121|Denton County, Texas    |                                      0.8943077|
| 48157|Fort Bend County, Texas |                                      1.9063359|


We can also map the state as a whole.


```r
library(leaflet)
library(stringr)

pal <- colorNumeric(palette = "viridis", domain = opioids_joined$opioid_rate)

opioids_joined %>%
    st_transform(crs = "+init=epsg:4326") %>%
    leaflet(width = "100%") %>%
    addProviderTiles(provider = "CartoDB.Positron") %>%
    addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
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
## Error in loadNamespace(name): there is no package called 'webshot'
```

This whole exercise has made me see how difficult it is to draw robust conclusions about this kind of measurement for counties that are as different from each other as rural and urban Texas counties are. There are counties in west Texas with no pharmacies in them at all, and people must be driving into neighboring counties if they have opioid prescriptions. This drives up the opioid rate in those counties that do have pharmacies. This would *not* happen as much in the more populated eastern half of the state. It's not obvious from a simple analysis where geographically there is more or less opioid utilization. Pharmacies are fewer in number than people, and the population is more spread out than the pharmacies. Only in urban areas are the number of pharmacies a good tracer of the number of people.



```r
library(plotly)
p <- opioids_joined %>%
             mutate(County = str_extract(NAME, "^([^,]*)")) %>%
             ggplot(aes(estimate, opioid_rate, tooltip = County)) +
             geom_point(alpha = 0.5, size = 1.5, color = "midnightblue") +
             scale_x_log10(labels = scales::comma_format()) +
             labs(x = "County population",
                  y = "Median monthly opioid prescription per 1k population",
                  title = "Opioid prescriptions in Texas")

ggplotly(p, tooltip = "County")
```

```
## Error in loadNamespace(name): there is no package called 'webshot'
```

Patterns of pharmacy locations make the opioid utilization rate in rural Texas difficult to measure.



