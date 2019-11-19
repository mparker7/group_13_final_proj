Group 13 Final Project
================
cs3779, kd2640, ob2305, mp3745, lef2147
2019-11-19

Read in and tidy the data

The following code:

  - Reads in the data
  - Renames columns to be more informative
  - Combines height columns into a single height in inches
  - Converts date\_stop to date data type
  - Converts time\_stop to time data type
  - Recodes the values in categorical columns to be more informative
  - Selects column subset for further analysis

<!-- end list -->

``` r
# Read in data
stop_frisk_df = 
  # Read in data from internet
  GET("https://www1.nyc.gov/assets/nypd/downloads/excel/analysis_and_planning/stop-question-frisk/sqf-2016.csv") %>% 
  content("parsed") %>% 
  
  # Clean and fix names of columns
  janitor::clean_names() %>% 
  rename(
    precinct = pct,
    date_stop = datestop,
    time_stop = timestop,
    stop_in_out = inout,
    obs_time_min = perobs,
    stop_time_min = perstop,
    arst_made = arstmade,
    off_in_unif = offunif,
    hair_col = haircolr,
    eye_col = eyecolor,
    other_feature = othfeatr,
    boro = city
  )  %>% 
  mutate(
    # Combine height columns
    height_inch = ht_feet * 12 + ht_inch,
    # Convert date to proper format
    date_stop = mdy(date_stop),
    # Convert time to proper format
    time_stop = hm(time_stop / 100),
    # Recode to be more informative
    stop_in_out = recode(stop_in_out, "I" = "inside", "O" = "outside"),
    race = recode(
      race, 
      "A" = "asian/pacific islander", 
      "B" = "black", 
      "I" = "american indian/alaska native",
      "P" = "black-hispanic",
      "Q" = "white-hispanic",
      "W" = "white",
      "U" = "unknown",
      "Z" = "other"
    ),
    hair_col = recode(
      hair_col,
      "BA" = "bald",
      "BK" = "black",
      "BL" = "blond",
      "BR" = "brown",
      "DY" = "dyed",
      "FR" = "frosted",
      "GY" = "gray",
      "RA" = "red",
      "SN" = "sandy",
      "SP" = "salt and pepper",
      "WH" = "white",
      "XX" = "unknown",
      "ZZ" = "other",
    ),
    eye_col = recode(
      eye_col,
      "BK" = "black",
      "BL" = "blue",
      "BR" = "brown",
      "DF" = "different",
      "GR" = "green",
      "GY" = "gray",
      "HA" = "hazel",
      "MA" = "maroon",
      "PK" = "pink",
      "VI" = "violet",
      "XX" = "unknown",
      "Z" = "other",      
    ),
    build = recode(
      build,
      "H" = "heavy",
      "M" = "medium",
      "T" = "thin",
      "U" = "muscular",
      "Z" = "unknown"
    ),
    # change boro columns to lowercase for consistency
    boro = tolower(boro),
    # change character datatypes to numeric
    age = as.numeric(age),
    obs_time_min = as.numeric(obs_time_min),
    stop_time_min = as.numeric(stop_time_min)
  )  %>% 
  # select columns for further analysis
  select(precinct, date_stop, time_stop, stop_in_out, obs_time_min, stop_time_min, arst_made, off_in_unif, frisked, 
         searched, rf_vcrim, rf_othsw, rf_attir:ac_evasv, cs_furtv:cs_other, rf_knowl, sb_hdobj:sb_admis, rf_furt, 
         rf_bulg, sex, race, age, height_inch, weight:other_feature, boro, xcoord, ycoord) %>% 
  # change all columns that have Y/N to 1/0
  mutate_at(vars(arst_made:rf_bulg), funs(recode(., "Y" = "1", "N" = "0"))) %>% 
  # change binary columns to numeric instead of character
  mutate_at(vars(arst_made:rf_bulg), funs(as.numeric(.))) %>% 
  # converts all character variables to factors (this does the same as the for loop)
  mutate_if(is.character, as.factor)

# convert to dataframe
stop_frisk_df = as.data.frame(stop_frisk_df)

# order all categorical variables with more than 2 levels as factors
for (i in c("sex", "race", "hair_col", "eye_col", "build", "stop_in_out", "boro")) {
  stop_frisk_df[,i] = as.factor(stop_frisk_df[,i])
}
```

Evaluating Missing Data and Categorical Data

``` r
table(stop_frisk_df$race)
```

    ## 
    ## american indian/alaska native        asian/pacific islander 
    ##                            38                           737 
    ##                         black                black-hispanic 
    ##                          6498                           873 
    ##                         other                       unknown 
    ##                           140                            95 
    ##                         white                white-hispanic 
    ##                          1270                          2753

``` r
table(stop_frisk_df$hair_col)
```

    ## 
    ##            bald           black           blond           brown 
    ##             391            9046             143            2323 
    ##            dyed            gray           other             red 
    ##               4             125              59              39 
    ## salt and pepper           sandy         unknown 
    ##              74               2             198

``` r
table(stop_frisk_df$eye_col)
```

    ## 
    ##     black      blue     brown different      gray     green     hazel 
    ##       955       206     10839         1        14       103       113 
    ##     other   unknown 
    ##        54       119

``` r
table(stop_frisk_df$build)
```

    ## 
    ##    heavy   medium muscular     thin  unknown 
    ##     1209     5470      133     5382      210

``` r
# we should consider consolidating categories

colSums(is.na(stop_frisk_df))
```

    ##      precinct     date_stop     time_stop   stop_in_out  obs_time_min 
    ##             1             1             1             1             1 
    ## stop_time_min     arst_made   off_in_unif       frisked      searched 
    ##            24             1             1             1             1 
    ##      rf_vcrim      rf_othsw      rf_attir      cs_objcs      cs_descr 
    ##             1             1             1             1             1 
    ##      cs_casng      cs_lkout      rf_vcact      cs_cloth      cs_drgtr 
    ##             1             1             1             1             1 
    ##      ac_evasv      cs_furtv      rf_rfcmp      ac_cgdir      rf_verbl 
    ##             1             1             1             1             1 
    ##      cs_vcrim      cs_bulge      cs_other      rf_knowl      sb_hdobj 
    ##             1             1             1             1             1 
    ##      sb_outln      sb_admis       rf_furt       rf_bulg           sex 
    ##             1             1             1             1             1 
    ##          race           age   height_inch        weight      hair_col 
    ##             1            35             1             1             1 
    ##       eye_col         build other_feature          boro        xcoord 
    ##             1             1         11637             1           352 
    ##        ycoord 
    ##           352

``` r
# we should consider removing variable other_feature (11637 missing obs)
# age has 35 missing values, consider multiple imputation methods here

# Looks like there is also an entire row of NA's
```

Looking at stops over time (not complete yet)

``` r
stop_frisk_df %>% 
  group_by(date_stop) %>% 
  summarize(
    count = n()
  ) %>% 
  ggplot(aes(x = date_stop, y = count)) + 
    geom_point() +
    geom_smooth(se = FALSE)
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

<img src="group_13_final_proj_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Logistic Regression Dataset - not sure if we want to use this for
overall but didnt want to completely change the code above without
asking everyone

``` r
stop_frisk_log = stop_frisk_df %>% mutate(
    recode(
      race, 
      "A" = "asian/pacific islander", 
      "B" = "black", 
      "I" = "other",
      "P" = "black-hispanic",
      "Q" = "white-hispanic",
      "W" = "white",
      "U" = "other",
      "Z" = "other"
    ),
    hair_col = recode(
      hair_col,
      "BA" = "bald",
      "BK" = "black",
      "BL" = "blond",
      "BR" = "brown",
      "DY" = "other",
      "FR" = "other",
      "GY" = "other",
      "RA" = "other",
      "SN" = "other",
      "SP" = "other",
      "WH" = "other",
      "XX" = "unknown",
      "ZZ" = "other",
    ),
    eye_col = recode(
      eye_col,
      "BK" = "black",
      "BL" = "blue",
      "BR" = "brown",
      "DF" = "other",
      "GR" = "other",
      "GY" = "other",
      "HA" = "other",
      "MA" = "other",
      "PK" = "other",
      "VI" = "other",
      "XX" = "other",
      "Z" = "other",      
    ),
    build = recode(
      build,
      "H" = "heavy",
      "M" = "medium",
      "T" = "thin",
      "U" = "other",
      "Z" = "other"
    ))
```

Building a model using only characteristics, demographics, and location
as predictors - assess multicolinearity to determine if there are any
variables that exhibit high correlation - We will remove any variables
that exhibit signs of
multicollinearity

``` r
model_1 = glm(frisked ~ sex + race + age + height_inch + weight + hair_col + eye_col + boro + build + stop_in_out + precinct, family = binomial, data = stop_frisk_df)

car::vif(model_1)
```

    ##                  GVIF Df GVIF^(1/(2*Df))
    ## sex          1.339717  2        1.075854
    ## race         1.872930  7        1.045841
    ## age          1.267911  1        1.126015
    ## height_inch  1.332980  1        1.154547
    ## weight       1.591692  1        1.261623
    ## hair_col     1.825976 10        1.030563
    ## eye_col      1.593795  8        1.029561
    ## boro        28.452448  4        1.519725
    ## build        1.466075  4        1.048986
    ## stop_in_out  1.067662  1        1.033277
    ## precinct    25.096664  1        5.009657

``` r
# Based on the GVIF, we will remove boro

model_2 = glm(frisked ~ sex + race + age + height_inch + weight + hair_col + eye_col + build + stop_in_out + precinct, family = binomial, data = stop_frisk_df)

car::vif(model_2)
```

    ##                 GVIF Df GVIF^(1/(2*Df))
    ## sex         1.337089  2        1.075326
    ## race        1.689188  7        1.038156
    ## age         1.265728  1        1.125046
    ## height_inch 1.327902  1        1.152346
    ## weight      1.585041  1        1.258984
    ## hair_col    1.810621 10        1.030128
    ## eye_col     1.581326  8        1.029056
    ## build       1.450614  4        1.047596
    ## stop_in_out 1.060845  1        1.029973
    ## precinct    1.113389  1        1.055173

``` r
# no more collinearity problems

summary(model_2)
```

    ## 
    ## Call:
    ## glm(formula = frisked ~ sex + race + age + height_inch + weight + 
    ##     hair_col + eye_col + build + stop_in_out + precinct, family = binomial, 
    ##     data = stop_frisk_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8758  -1.3228   0.8079   0.9182   2.0763  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -2.419e+00  5.467e-01  -4.425 9.63e-06 ***
    ## sexM                        8.612e-01  7.728e-02  11.145  < 2e-16 ***
    ## sexZ                        5.687e-01  2.997e-01   1.897 0.057779 .  
    ## raceasian/pacific islander  1.565e-01  3.397e-01   0.461 0.644895    
    ## raceblack                   4.744e-01  3.321e-01   1.428 0.153202    
    ## raceblack-hispanic          5.003e-01  3.393e-01   1.475 0.140327    
    ## raceother                   3.118e-01  3.825e-01   0.815 0.415043    
    ## raceunknown                -1.226e-01  3.925e-01  -0.312 0.754788    
    ## racewhite                  -9.734e-03  3.374e-01  -0.029 0.976985    
    ## racewhite-hispanic          4.609e-01  3.337e-01   1.381 0.167192    
    ## age                        -1.245e-02  1.808e-03  -6.888 5.67e-12 ***
    ## height_inch                 2.964e-02  6.438e-03   4.605 4.13e-06 ***
    ## weight                      3.577e-04  6.314e-04   0.566 0.571079    
    ## hair_colblack               1.051e-01  1.111e-01   0.946 0.344058    
    ## hair_colblond              -2.733e-01  2.130e-01  -1.283 0.199358    
    ## hair_colbrown              -1.941e-02  1.177e-01  -0.165 0.869025    
    ## hair_coldyed                9.460e-01  1.206e+00   0.784 0.432951    
    ## hair_colgray                1.107e-01  2.135e-01   0.519 0.604029    
    ## hair_colother              -5.286e-02  3.153e-01  -0.168 0.866885    
    ## hair_colred                -6.790e-01  3.640e-01  -1.865 0.062147 .  
    ## hair_colsalt and pepper     1.123e-01  2.631e-01   0.427 0.669506    
    ## hair_colsandy              -1.198e+01  1.381e+02  -0.087 0.930908    
    ## hair_colunknown            -1.442e-01  1.846e-01  -0.781 0.434787    
    ## eye_colblue                 1.272e-01  1.694e-01   0.751 0.452832    
    ## eye_colbrown                9.857e-03  7.228e-02   0.136 0.891526    
    ## eye_coldifferent            1.051e+01  1.970e+02   0.053 0.957440    
    ## eye_colgray                 2.409e-01  5.724e-01   0.421 0.673872    
    ## eye_colgreen               -4.052e-02  2.188e-01  -0.185 0.853090    
    ## eye_colhazel                1.254e-01  2.142e-01   0.586 0.558097    
    ## eye_colother               -5.843e-01  3.189e-01  -1.832 0.066912 .  
    ## eye_colunknown             -1.208e-01  2.082e-01  -0.580 0.561706    
    ## buildmedium                -2.643e-01  7.545e-02  -3.503 0.000459 ***
    ## buildmuscular              -5.137e-01  1.918e-01  -2.678 0.007396 ** 
    ## buildthin                  -2.602e-01  8.036e-02  -3.238 0.001203 ** 
    ## buildunknown               -5.373e-01  1.640e-01  -3.276 0.001052 ** 
    ## stop_in_outoutside          4.334e-01  4.855e-02   8.927  < 2e-16 ***
    ## precinct                   -1.551e-03  5.924e-04  -2.619 0.008831 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 16164  on 12369  degrees of freedom
    ## Residual deviance: 15605  on 12333  degrees of freedom
    ##   (35 observations deleted due to missingness)
    ## AIC: 15679
    ## 
    ## Number of Fisher Scoring iterations: 10

``` r
# Remove hair and eye color because all categories within them are highly unsignificant 

model_3 = glm(frisked ~ sex + race + age + height_inch + weight + build + stop_in_out + precinct, family = binomial, data = stop_frisk_df)

car::vif(model_3)
```

    ##                 GVIF Df GVIF^(1/(2*Df))
    ## sex         1.313003  2        1.070450
    ## race        1.317344  7        1.019882
    ## age         1.101058  1        1.049313
    ## height_inch 1.324459  1        1.150851
    ## weight      1.591781  1        1.261658
    ## build       1.371745  4        1.040301
    ## stop_in_out 1.056536  1        1.027879
    ## precinct    1.109287  1        1.053227

``` r
summary(model_3)
```

    ## 
    ## Call:
    ## glm(formula = frisked ~ sex + race + age + height_inch + weight + 
    ##     build + stop_in_out + precinct, family = binomial, data = stop_frisk_df)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.8699  -1.3239   0.8118   0.9192   1.8560  
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                -2.3825961  0.5279105  -4.513 6.38e-06 ***
    ## sexM                        0.8826198  0.0764192  11.550  < 2e-16 ***
    ## sexZ                        0.5760355  0.2978327   1.934 0.053102 .  
    ## raceasian/pacific islander  0.1440443  0.3398116   0.424 0.671643    
    ## raceblack                   0.4635452  0.3322957   1.395 0.163023    
    ## raceblack-hispanic          0.4852423  0.3394002   1.430 0.152802    
    ## raceother                   0.2533240  0.3818174   0.663 0.507030    
    ## raceunknown                -0.1554355  0.3921855  -0.396 0.691860    
    ## racewhite                  -0.0782296  0.3359615  -0.233 0.815876    
    ## racewhite-hispanic          0.4249207  0.3336843   1.273 0.202869    
    ## age                        -0.0127621  0.0016852  -7.573 3.65e-14 ***
    ## height_inch                 0.0302614  0.0064042   4.725 2.30e-06 ***
    ## weight                      0.0004239  0.0006351   0.668 0.504445    
    ## buildmedium                -0.2619839  0.0753471  -3.477 0.000507 ***
    ## buildmuscular              -0.5046812  0.1917848  -2.631 0.008501 ** 
    ## buildthin                  -0.2576788  0.0803410  -3.207 0.001340 ** 
    ## buildunknown               -0.6233291  0.1599760  -3.896 9.76e-05 ***
    ## stop_in_outoutside          0.4308581  0.0483844   8.905  < 2e-16 ***
    ## precinct                   -0.0015792  0.0005908  -2.673 0.007518 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 16164  on 12369  degrees of freedom
    ## Residual deviance: 15633  on 12351  degrees of freedom
    ##   (35 observations deleted due to missingness)
    ## AIC: 15671
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
# keep weight? Although race is insignificant across all groups, should we still keep this in the model? Seems like an important predictor to me, we can see if there is any literature supporting this to validate why we will keep it in the model
```
