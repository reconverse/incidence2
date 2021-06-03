# single day, no groupings and without count work as expected

    An incidence object: 731 x 2
    731 cases from 2020-01-01 to 2021-12-31
    interval: 1 day
    cumulative: FALSE
    
       date_index count
       <date>     <int>
     1 2020-01-01     1
     2 2020-01-02     1
     3 2020-01-03     1
     4 2020-01-04     1
     5 2020-01-05     1
     6 2020-01-06     1
     7 2020-01-07     1
     8 2020-01-08     1
     9 2020-01-09     1
    10 2020-01-10     1
    # ... with 721 more rows
    

---

    An incidence object: 731 x 2
    731 cases from 2020-01-01 to 2021-12-31
    interval: 1 day
    cumulative: FALSE
    timespan: 731 days
    
    

# single day, no groupings and with count work as expected

    An incidence object: 731 x 2
    1096 cases from 2020-01-01 to 2021-12-31
    interval: 1 day
    cumulative: FALSE
    
       date_index count
       <date>     <int>
     1 2020-01-01     1
     2 2020-01-02     1
     3 2020-01-03     1
     4 2020-01-04     1
     5 2020-01-05     1
     6 2020-01-06     1
     7 2020-01-07     1
     8 2020-01-08     1
     9 2020-01-09     1
    10 2020-01-10     1
    # ... with 721 more rows
    

---

    An incidence object: 731 x 2
    1096 cases from 2020-01-01 to 2021-12-31
    interval: 1 day
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-day, no groupings and without count work as expected

    An incidence object: 43 x 2
    731 cases from 2020-01-01 to 2021-12-31
    interval: 17 days
    cumulative: FALSE
    
                     date_index count
                       <period> <int>
     1 2020-01-01 to 2020-01-17    17
     2 2020-01-18 to 2020-02-03    17
     3 2020-02-04 to 2020-02-20    17
     4 2020-02-21 to 2020-03-08    17
     5 2020-03-09 to 2020-03-25    17
     6 2020-03-26 to 2020-04-11    17
     7 2020-04-12 to 2020-04-28    17
     8 2020-04-29 to 2020-05-15    17
     9 2020-05-16 to 2020-06-01    17
    10 2020-06-02 to 2020-06-18    17
    # ... with 33 more rows
    

---

    An incidence object: 43 x 2
    731 cases from 2020-01-01 to 2021-12-31
    interval: 17 days
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-day, no groupings and with count work as expected

    An incidence object: 43 x 2
    1096 cases from 2020-01-01 to 2021-12-31
    interval: 17 days
    cumulative: FALSE
    
                     date_index count
                       <period> <int>
     1 2020-01-01 to 2020-01-17    17
     2 2020-01-18 to 2020-02-03    17
     3 2020-02-04 to 2020-02-20    17
     4 2020-02-21 to 2020-03-08    17
     5 2020-03-09 to 2020-03-25    17
     6 2020-03-26 to 2020-04-11    17
     7 2020-04-12 to 2020-04-28    17
     8 2020-04-29 to 2020-05-15    17
     9 2020-05-16 to 2020-06-01    17
    10 2020-06-02 to 2020-06-18    17
    # ... with 33 more rows
    

---

    An incidence object: 43 x 2
    1096 cases from 2020-01-01 to 2021-12-31
    interval: 17 days
    cumulative: FALSE
    timespan: 731 days
    
    

# single week, no groupings and without count work as expected

    An incidence object: 105 x 2
    731 cases from 2020-W01 to 2022-W01
    interval: 1 (Wednesday) week 
    cumulative: FALSE
    
       date_index count
           <yrwk> <int>
     1   2020-W01     7
     2   2020-W02     7
     3   2020-W03     7
     4   2020-W04     7
     5   2020-W05     7
     6   2020-W06     7
     7   2020-W07     7
     8   2020-W08     7
     9   2020-W09     7
    10   2020-W10     7
    # ... with 95 more rows
    

---

    An incidence object: 105 x 2
    731 cases from 2020-W01 to 2022-W01
    interval: 1 (Wednesday) week 
    cumulative: FALSE
    timespan: 735 days
    
    

# single week, with groups and without count work as expected

    An incidence object: 8 x 4
    56 cases from 2021-W05 to 2021-W08
    interval: 1 (Monday) week 
    cumulative: FALSE
    
      date_index height size  count
          <yrwk> <chr>  <chr> <int>
    1   2021-W05 short  small     7
    2   2021-W05 tall   small     7
    3   2021-W06 short  large     7
    4   2021-W06 tall   large     7
    5   2021-W07 short  large     7
    6   2021-W07 tall   large     7
    7   2021-W08 short  large     7
    8   2021-W08 tall   large     7
    

---

    An incidence object: 8 x 4
    56 cases from 2021-W05 to 2021-W08
    interval: 1 (Monday) week 
    cumulative: FALSE
    timespan: 28 days
    
    2 grouped variables
    
      height count
      <chr>  <int>
    1 short     28
    2 tall      28
    
    
      size  count
      <chr> <int>
    1 small    14
    2 large    42
    
    
    

# single week, with groups and with count work as expected

    An incidence object: 8 x 4
    84 cases from 2021-W05 to 2021-W08
    interval: 1 (Monday) week 
    cumulative: FALSE
    
      date_index height size  count
          <yrwk> <chr>  <chr> <int>
    1   2021-W05 short  small     7
    2   2021-W05 tall   small     7
    3   2021-W06 short  large     7
    4   2021-W06 tall   large     7
    5   2021-W07 short  large    14
    6   2021-W07 tall   large    14
    7   2021-W08 short  large    14
    8   2021-W08 tall   large    14
    

---

    An incidence object: 8 x 4
    84 cases from 2021-W05 to 2021-W08
    interval: 1 (Monday) week 
    cumulative: FALSE
    timespan: 28 days
    
    2 grouped variables
    
      height count
      <chr>  <int>
    1 short     42
    2 tall      42
    
    
      size  count
      <chr> <int>
    1 small    14
    2 large    70
    
    
    

# single week, no groupings and with count work as expected

    An incidence object: 105 x 2
    1096 cases from 2020-W01 to 2022-W01
    interval: 1 (Wednesday) week 
    cumulative: FALSE
    
       date_index count
           <yrwk> <int>
     1   2020-W01     7
     2   2020-W02     7
     3   2020-W03     7
     4   2020-W04     7
     5   2020-W05     7
     6   2020-W06     7
     7   2020-W07     7
     8   2020-W08     7
     9   2020-W09     7
    10   2020-W10     7
    # ... with 95 more rows
    

---

    An incidence object: 105 x 2
    1096 cases from 2020-W01 to 2022-W01
    interval: 1 (Wednesday) week 
    cumulative: FALSE
    timespan: 735 days
    
    

# multi-week, no groupings and without count work as expected

    An incidence object: 53 x 2
    731 cases from 2020-01-01 to 2022-01-11
    interval: 14 days
    cumulative: FALSE
    
                     date_index count
                       <period> <int>
     1 2020-01-01 to 2020-01-14    14
     2 2020-01-15 to 2020-01-28    14
     3 2020-01-29 to 2020-02-11    14
     4 2020-02-12 to 2020-02-25    14
     5 2020-02-26 to 2020-03-10    14
     6 2020-03-11 to 2020-03-24    14
     7 2020-03-25 to 2020-04-07    14
     8 2020-04-08 to 2020-04-21    14
     9 2020-04-22 to 2020-05-05    14
    10 2020-05-06 to 2020-05-19    14
    # ... with 43 more rows
    

---

    An incidence object: 53 x 2
    731 cases from 2020-01-01 to 2022-01-11
    interval: 14 days
    cumulative: FALSE
    timespan: 742 days
    
    

# multi-week, no groupings and with count work as expected

    An incidence object: 53 x 2
    1096 cases from 2020-01-01 to 2022-01-11
    interval: 14 days
    cumulative: FALSE
    
                     date_index count
                       <period> <int>
     1 2020-01-01 to 2020-01-14    14
     2 2020-01-15 to 2020-01-28    14
     3 2020-01-29 to 2020-02-11    14
     4 2020-02-12 to 2020-02-25    14
     5 2020-02-26 to 2020-03-10    14
     6 2020-03-11 to 2020-03-24    14
     7 2020-03-25 to 2020-04-07    14
     8 2020-04-08 to 2020-04-21    14
     9 2020-04-22 to 2020-05-05    14
    10 2020-05-06 to 2020-05-19    14
    # ... with 43 more rows
    

---

    An incidence object: 53 x 2
    1096 cases from 2020-01-01 to 2022-01-11
    interval: 14 days
    cumulative: FALSE
    timespan: 742 days
    
    

# week defaults to a monday

    An incidence object: 105 x 2
    731 cases from 2020-W01 to 2021-W52
    interval: 1 (Monday) week 
    cumulative: FALSE
    
       date_index count
           <yrwk> <int>
     1   2020-W01     5
     2   2020-W02     7
     3   2020-W03     7
     4   2020-W04     7
     5   2020-W05     7
     6   2020-W06     7
     7   2020-W07     7
     8   2020-W08     7
     9   2020-W09     7
    10   2020-W10     7
    # ... with 95 more rows
    

---

    An incidence object: 105 x 2
    731 cases from 2020-W01 to 2021-W52
    interval: 1 (Monday) week 
    cumulative: FALSE
    timespan: 735 days
    
    

# single month, no groupings and without count work as expected

    An incidence object: 24 x 2
    731 cases from 2020-Jan to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    
       date_index count
           <mnth> <int>
     1   2020-Jan    31
     2   2020-Feb    29
     3   2020-Mar    31
     4   2020-Apr    30
     5   2020-May    31
     6   2020-Jun    30
     7   2020-Jul    31
     8   2020-Aug    31
     9   2020-Sep    30
    10   2020-Oct    31
    # ... with 14 more rows
    

---

    An incidence object: 24 x 2
    731 cases from 2020-Jan to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    timespan: 731 days
    
    

# single month, no groupings and with count work as expected

    An incidence object: 24 x 2
    1096 cases from 2020-Jan to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    
       date_index count
           <mnth> <int>
     1   2020-Jan    31
     2   2020-Feb    29
     3   2020-Mar    31
     4   2020-Apr    30
     5   2020-May    31
     6   2020-Jun    30
     7   2020-Jul    31
     8   2020-Aug    31
     9   2020-Sep    30
    10   2020-Oct    31
    # ... with 14 more rows
    

---

    An incidence object: 24 x 2
    1096 cases from 2020-Jan to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-month, no groupings and without count work as expected

    An incidence object: 12 x 2
    731 cases from 2020-01-01 to 2021-12-31
    interval: 2 months
    cumulative: FALSE
    
                 date_index count
                     <mnth> <int>
     1 2020-Jan to 2020-Feb    60
     2 2020-Mar to 2020-Apr    61
     3 2020-May to 2020-Jun    61
     4 2020-Jul to 2020-Aug    62
     5 2020-Sep to 2020-Oct    61
     6 2020-Nov to 2020-Dec    61
     7 2021-Jan to 2021-Feb    59
     8 2021-Mar to 2021-Apr    61
     9 2021-May to 2021-Jun    61
    10 2021-Jul to 2021-Aug    62
    11 2021-Sep to 2021-Oct    61
    12 2021-Nov to 2021-Dec    61
    

---

    An incidence object: 12 x 2
    731 cases from 2020-01-01 to 2021-12-31
    interval: 2 months
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-month, no groupings and with count work as expected

    An incidence object: 12 x 2
    1096 cases from 2020-01-01 to 2021-12-31
    interval: 2 months
    cumulative: FALSE
    
                 date_index count
                     <mnth> <int>
     1 2020-Jan to 2020-Feb    60
     2 2020-Mar to 2020-Apr    61
     3 2020-May to 2020-Jun    61
     4 2020-Jul to 2020-Aug    62
     5 2020-Sep to 2020-Oct    61
     6 2020-Nov to 2020-Dec    61
     7 2021-Jan to 2021-Feb   118
     8 2021-Mar to 2021-Apr   122
     9 2021-May to 2021-Jun   122
    10 2021-Jul to 2021-Aug   124
    11 2021-Sep to 2021-Oct   122
    12 2021-Nov to 2021-Dec   122
    

---

    An incidence object: 12 x 2
    1096 cases from 2020-01-01 to 2021-12-31
    interval: 2 months
    cumulative: FALSE
    timespan: 731 days
    
    

# single quarter, no groupings and without count work as expected

    An incidence object: 8 x 2
    731 cases from 2020-Q1 to 2021-Q4
    interval: 1 quarter
    cumulative: FALSE
    
      date_index count
           <qtr> <int>
    1    2020-Q1    91
    2    2020-Q2    91
    3    2020-Q3    92
    4    2020-Q4    92
    5    2021-Q1    90
    6    2021-Q2    91
    7    2021-Q3    92
    8    2021-Q4    92
    

---

    An incidence object: 8 x 2
    731 cases from 2020-Q1 to 2021-Q4
    interval: 1 quarter
    cumulative: FALSE
    timespan: 731 days
    
    

# single quarter, no groupings and with count work as expected

    An incidence object: 8 x 2
    1096 cases from 2020-Q1 to 2021-Q4
    interval: 1 quarter
    cumulative: FALSE
    
      date_index count
           <qtr> <int>
    1    2020-Q1    91
    2    2020-Q2    91
    3    2020-Q3    92
    4    2020-Q4    92
    5    2021-Q1   180
    6    2021-Q2   182
    7    2021-Q3   184
    8    2021-Q4   184
    

---

    An incidence object: 8 x 2
    1096 cases from 2020-Q1 to 2021-Q4
    interval: 1 quarter
    cumulative: FALSE
    timespan: 731 days
    
    

# single year, no groupings and without count work as expected

    An incidence object: 2 x 2
    731 cases from 2020 to 2021
    interval: 1 year
    cumulative: FALSE
    
      date_index count
          <year> <int>
    1       2020   366
    2       2021   365
    

---

    An incidence object: 2 x 2
    731 cases from 2020 to 2021
    interval: 1 year
    cumulative: FALSE
    timespan: 731 days
    
    

# single year, no groupings and with count work as expected

    An incidence object: 2 x 2
    1096 cases from 2020 to 2021
    interval: 1 year
    cumulative: FALSE
    
      date_index count
          <year> <int>
    1       2020   366
    2       2021   730
    

---

    An incidence object: 2 x 2
    1096 cases from 2020 to 2021
    interval: 1 year
    cumulative: FALSE
    timespan: 731 days
    
    

# integer date periods without counts work as expected

    An incidence object: 2 x 2
    10 cases from 1 to 10
    interval: 5 days
    cumulative: FALSE
    
      date_index count
        <period> <int>
    1     1 to 5     5
    2    6 to 10     5
    

---

    An incidence object: 2 x 2
    10 cases from 1 to 10
    interval: 5 days
    cumulative: FALSE
    timespan: 10 days
    
    

# integer date periods with counts work as expected

    An incidence object: 2 x 2
    15 cases from 1 to 10
    interval: 5 days
    cumulative: FALSE
    
      date_index count
        <period> <dbl>
    1     1 to 5     5
    2    6 to 10    10
    

---

    An incidence object: 2 x 2
    15 cases from 1 to 10
    interval: 5 days
    cumulative: FALSE
    timespan: 10 days
    
    

# date_index works for multiple values

    An incidence object: 13 x 3
    365 deaths from 2020-Dec to 2021-Dec
    365 onset from 2020-Dec to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    
       date_index deaths onset
           <mnth>  <int> <int>
     1   2020-Dec      0    31
     2   2021-Jan     31    31
     3   2021-Feb     28    28
     4   2021-Mar     31    31
     5   2021-Apr     30    30
     6   2021-May     31    31
     7   2021-Jun     30    30
     8   2021-Jul     31    31
     9   2021-Aug     31    31
    10   2021-Sep     30    30
    11   2021-Oct     31    31
    12   2021-Nov     30    30
    13   2021-Dec     31     0
    

---

    An incidence object: 13 x 3
    365 deaths from 2020-Dec to 2021-Dec
    365 onset from 2020-Dec to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    timespan: 396 days
    
    

