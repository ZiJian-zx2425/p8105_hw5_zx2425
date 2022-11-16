p8105_hw5_zx2425
================

# Problem2

## step1

Describe the raw data. Create a city_state variable (e.g. “Baltimore,
MD”)

``` r
hmcd =read.csv("./homicide-data.csv") %>% 
  janitor::clean_names() 
head(hmcd)
```

    ##          uid reported_date victim_last victim_first victim_race victim_age
    ## 1 Alb-000001      20100504      GARCIA         JUAN    Hispanic         78
    ## 2 Alb-000002      20100216     MONTOYA      CAMERON    Hispanic         17
    ## 3 Alb-000003      20100601 SATTERFIELD      VIVIANA       White         15
    ## 4 Alb-000004      20100101    MENDIOLA       CARLOS    Hispanic         32
    ## 5 Alb-000005      20100102        MULA       VIVIAN       White         72
    ## 6 Alb-000006      20100126        BOOK    GERALDINE       White         91
    ##   victim_sex        city state      lat       lon           disposition
    ## 1       Male Albuquerque    NM 35.09579 -106.5386 Closed without arrest
    ## 2       Male Albuquerque    NM 35.05681 -106.7153      Closed by arrest
    ## 3     Female Albuquerque    NM 35.08609 -106.6956 Closed without arrest
    ## 4       Male Albuquerque    NM 35.07849 -106.5561      Closed by arrest
    ## 5     Female Albuquerque    NM 35.13036 -106.5810 Closed without arrest
    ## 6     Female Albuquerque    NM 35.15111 -106.5378        Open/No arrest

The data set has 52179 \* `r ncol(hmcd)` dimensions. Where uid describes
the incident number, reported_date indicates the date and time,
victim_last and victim first indicates the victim’s name, and other data
includes: race, age, gender, city, state, longitude, and latitude. And
whether the incident was finally detected.

## step2

then summarize within cities to obtain the total number of homicides and
the number of unsolved homicides (those for which the disposition is
“Closed without arrest” or “Open/No arrest”).

``` r
hmcd=hmcd %>% 
  mutate(
    city_state=str_c(city,"_",state)
  )

count_vic=hmcd %>% 
  group_by(city_state) %>% 
  summarize(
    vic_number=n()
  )
count_dect=hmcd %>% 
  filter(disposition %in% c("Closed without arrest", "Open/No arrest")) %>% 
  group_by(city_state) %>% 
  summarize(
    undec_number=n()
  )

count = full_join(count_vic,count_dect,by="city_state")
count[is.na(count)] = 0
```

## step3

For the city of **Baltimore, MD**, use the **prop.test** function to
estimate the proportion of homicides that are unsolved; save the output
of prop.test as an R object, apply the **broom::tidy** to this object
and pull the *estimated* *proportion* and *confidence intervals* from
the resulting tidy dataframe.

``` r
city_B= hmcd %>% 
  filter(city_state == 'Baltimore_MD')
city_B_count= city_B %>% 
   summarise(
      unsolved = sum(disposition %in%c("Closed without arrest", "Open/No arrest")  ),
      ###sum can calculate the True or false with result of number
      n = n()
    )
city_B_test = 
  prop.test(x = city_B_count %>% pull(unsolved),n = city_B_count %>% pull(n))
city_B_test %>% 
  broom::tidy() %>% 
  knitr::kable()  
```

|  estimate | statistic | p.value | parameter |  conf.low | conf.high | method                                               | alternative |
|----------:|----------:|--------:|----------:|----------:|----------:|:-----------------------------------------------------|:------------|
| 0.6455607 |   239.011 |       0 |         1 | 0.6275625 | 0.6631599 | 1-sample proportions test with continuity correction | two.sided   |
