total\_stops
================
kd2640\_total\_stops
2019-11-24

Comparing the reason why people were stopped

``` r
stops_total =
stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other) %>% 
  pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% 
  mutate(
    reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
  filter(stops == 1) %>% 
  group_by(reason_stopped) %>% 
  summarize(total =n()) %>% 
  mutate(prob = total/sum(total)) 

stops_total %>% 
  plot_ly(x = ~reason_stopped, y= ~total, type = "bar" , color = ~reason_stopped)

stops_total %>% 
 plot_ly(x = ~reason_stopped, y= ~prob, type = "bar" , color = ~reason_stopped) 
```

total stops by ages

``` r
stops_by_age = 
stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other, age) %>% 
  pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% mutate(
    reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
  filter(stops == 1) %>% 
  group_by(reason_stopped, age) %>% 
  summarize(total =n()) %>% 
  mutate(prob = total/sum(total)) 

stops_by_age %>% 
  plot_ly(x = ~age, y= ~prob, type = "bar" , color = ~reason_stopped)

stops_by_age %>% 
  plot_ly(x = ~age, y= ~total, type = "bar" , color = ~reason_stopped)
```

total stop by age group

``` r
stop_age_group =
  stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other,age) %>% 
  mutate(
    age_group = case_when(
      age < '18' ~ "18-",
      age >= '18' & age < '30' ~ "18-30",
      age >= '30' & age < '40' ~ "30s",
      age >= '40' & age < '50' ~ "40s",
      age >= '50' & age < '60' ~ "50s",
      age >= '60' & age < '70' ~ "60s",
      age >= 70 ~"70+"
    )
    ) %>% 
   pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% 
  mutate(
    reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
  filter(stops == 1) %>% 
  group_by(reason_stopped, age_group) %>% 
  summarize(total =n()) %>% 
  mutate(prob = total/sum(total)) 

stop_age_group %>% 
  plot_ly(x = ~age_group, y= ~prob, type = "bar" , color = ~reason_stopped)

stop_age_group %>% 
 plot_ly(x = ~age_group, y= ~total, type = "bar" , color = ~reason_stopped) 
```

total stops by sex

``` r
stops_by_sex = 
  stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other, sex) %>% 
  pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% 
   mutate(
     sex = recode(
    sex,
    "F" = "female",
    "M" = "male",
    "Z" = "other"
  ), 
  reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
   filter(stops == 1) %>% 
  group_by(reason_stopped, sex) %>% 
  summarize(total =n()) %>%
  mutate(prop = total/sum(total)) 

stops_by_sex %>% 
  plot_ly(x = ~sex, y= ~prop, type = "bar" , color = ~reason_stopped)

stops_by_sex %>% 
  plot_ly(x = ~sex, y = ~total, type = "bar", color = ~reason_stopped)
```

total stops by race

``` r
stop_by_race = 
  stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other, race) %>% 
  pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% mutate(
    reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
   filter(stops == 1) %>% 
  group_by(reason_stopped, race) %>% 
  summarize(total =n()) %>% 
  mutate(prop = total/sum(total)) 

stop_by_race %>% 
  plot_ly(x = ~race, y= ~prop, type = "bar" , color = ~reason_stopped)

stop_by_race %>% 
  plot_ly(x = ~race, y= ~total, type = "bar" , color = ~reason_stopped)
```

total stops by date

``` r
stops_by_month =
stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other, date_stop) %>% 
  mutate( month_stop = month(date_stop)) %>% 
  pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% mutate(
    reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
   filter(stops == 1) %>% 
  group_by(reason_stopped, month_stop) %>% 
  summarize(total =n()) %>% 
  mutate( prob = total/sum(total))

stops_by_month %>% 
  plot_ly(x = ~month_stop, y= ~total, type = "bar" , color = ~reason_stopped)

stops_by_month %>% 
  plot_ly(x = ~month_stop, y= ~prob, type = "bar" , color = ~reason_stopped)
```

total of stop by inside and outside

``` r
in_out_stops = 
stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other, stop_in_out) %>% 
  pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% mutate(
    reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
   filter(stops == 1) %>% 
  group_by(reason_stopped, stop_in_out) %>% 
  summarize(total =n()) %>% 
  mutate( prob = total/sum(total))

in_out_stops %>% 
  plot_ly(x = ~stop_in_out, y= ~total, type = "bar" , color = ~reason_stopped)

in_out_stops %>% 
   plot_ly(x = ~stop_in_out, y= ~prob, type = "bar" , color = ~reason_stopped)
```

arrest made by total stops

``` r
stops_arrest =
stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other, arst_made) %>% 
  pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% mutate(
    reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
  mutate(
    arst_made = recode(
      arst_made,
      '0' = "outside",
      '1' = "inside"
    )
  ) %>% 
   filter(stops == 1) %>% 
  group_by(reason_stopped, arst_made) %>% 
  summarize(total =n()) %>% 
  mutate( prob = total/sum(total))

stops_arrest %>% 
  plot_ly(x = ~arst_made, y= ~total, type = "bar" , color = ~reason_stopped)

stops_arrest %>% 
  plot_ly(x = ~arst_made, y= ~prob, type = "bar" , color = ~reason_stopped)
```

total stops by boro

``` r
stop_by_boro = 
  stop_frisk_df %>% 
  select(cs_objcs:cs_lkout, cs_cloth, cs_drgtr, cs_furtv, cs_vcrim:cs_other, boro) %>% 
  pivot_longer(
    cs_objcs:cs_other,
    names_to = "reason_stopped",
    values_to = "stops"
  ) %>% mutate(
    reason_stopped = recode(
    reason_stopped,
    "cs_objcs" = "carrying suspicious object",
    "cs_descr" = "fits a relevant description",
    "cs_casng" = "casing a victim or location",
    "cs_lkout" = "suspect acting as a lookout",
    "cs_cloth" = "wearing clothes commonly used in crimes",
    "cs_drgtr" = "actions indicative of drug transaction",
    "cs_furtv" = "furtive movements",
    "cs_vcrim" = "actions engaging in violent crime",
    "cs_bulge" = "suspcious bulge",
    "cs_other" = "other"
  )) %>% 
   filter(stops == 1) %>% 
  group_by(reason_stopped, boro) %>% 
  summarize(total =n()) %>% 
  mutate( prob = total/sum(total))

stop_by_boro %>% 
  plot_ly(x = ~boro, y= ~prob, type = "bar" , color = ~reason_stopped)

stop_by_boro %>% 
  plot_ly(x = ~boro, y= ~total, type = "bar" , color = ~reason_stopped)
```
