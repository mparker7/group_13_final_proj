Group 13 Final Project
================
cs3779, kd2640, ob2305, mp3745, lef2147
2019-11-18

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
  )  %>% 
  # select columns for further analysis
  select(precinct, date_stop, time_stop, stop_in_out, obs_time_min, stop_time_min, arst_made, off_in_unif, frisked, 
         searched, rf_vcrim, rf_othsw, rf_attir:ac_evasv, cs_furtv:cs_other, rf_knowl, sb_hdobj:sb_admis, rf_furt, 
         rf_bulg, sex, race, age, height_inch, weight:other_feature, boro, xcoord, ycoord) %>% 
  # change all columns that have Y/N to 1/0
  mutate_at(vars(arst_made:rf_bulg), funs(recode(., "Y" = "1", "N" = "0"))) %>% 
  # change binary columns to numeric instead of character
  mutate_at(vars(arst_made:rf_bulg), funs(as.numeric(.)))

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
    ##             1             1             1             1             1 
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
```
