# single day, no groupings and without count work as expected

    An incidence2 object: 731 x 2
    731 cases from days 2020-01-01 to 2021-12-31
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

    An incidence2 object: 731 x 2
    731 cases from 2020-01-01 to 2021-12-31
    interval: 1 day
    cumulative: FALSE
    timespan: 731 days
    
    

# single day, no groupings and with count work as expected

    An incidence2 object: 731 x 2
    1096 cases from days 2020-01-01 to 2021-12-31
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

    An incidence2 object: 731 x 2
    1096 cases from 2020-01-01 to 2021-12-31
    interval: 1 day
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-day, no groupings and without count work as expected

    An incidence2 object: 43 x 2
    731 cases from days [2020-01-01 to [2021-12-15
    interval: 17 days
    cumulative: FALSE
    
       date_index  count
       <period>    <int>
     1 [2020-01-01    17
     2 [2020-01-18    17
     3 [2020-02-04    17
     4 [2020-02-21    17
     5 [2020-03-09    17
     6 [2020-03-26    17
     7 [2020-04-12    17
     8 [2020-04-29    17
     9 [2020-05-16    17
    10 [2020-06-02    17
    # ... with 33 more rows
    

---

    An incidence2 object: 43 x 2
    731 cases from [2020-01-01 to [2021-12-15
    interval: 17 days
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-day, no groupings and with count work as expected

    An incidence2 object: 43 x 2
    1096 cases from days [2020-01-01 to [2021-12-15
    interval: 17 days
    cumulative: FALSE
    
       date_index  count
       <period>    <int>
     1 [2020-01-01    17
     2 [2020-01-18    17
     3 [2020-02-04    17
     4 [2020-02-21    17
     5 [2020-03-09    17
     6 [2020-03-26    17
     7 [2020-04-12    17
     8 [2020-04-29    17
     9 [2020-05-16    17
    10 [2020-06-02    17
    # ... with 33 more rows
    

---

    An incidence2 object: 43 x 2
    1096 cases from [2020-01-01 to [2021-12-15
    interval: 17 days
    cumulative: FALSE
    timespan: 731 days
    
    

# single week, no groupings and without count work as expected

    An incidence2 object: 105 x 2
    731 cases from days 2020-W01 to 2022-W01
    interval: 1 wednesday week
    cumulative: FALSE
    
       date_index count
       <yrwk>     <int>
     1 2020-W01       7
     2 2020-W02       7
     3 2020-W03       7
     4 2020-W04       7
     5 2020-W05       7
     6 2020-W06       7
     7 2020-W07       7
     8 2020-W08       7
     9 2020-W09       7
    10 2020-W10       7
    # ... with 95 more rows
    

---

    An incidence2 object: 105 x 2
    731 cases from 2020-W01 to 2022-W01
    interval: 1 wednesday week
    cumulative: FALSE
    timespan: 735 days
    
    

# single week, with groups and without count work as expected

    An incidence2 object: 8 x 4
    56 cases from days 2021-W05 to 2021-W08
    interval: 1 monday week
    cumulative: FALSE
    
      date_index height size  count
      <yrwk>     <chr>  <chr> <int>
    1 2021-W05   short  small     7
    2 2021-W05   tall   small     7
    3 2021-W06   short  large     7
    4 2021-W06   tall   large     7
    5 2021-W07   short  large     7
    6 2021-W07   tall   large     7
    7 2021-W08   short  large     7
    8 2021-W08   tall   large     7
    

---

    An incidence2 object: 8 x 4
    56 cases from 2021-W05 to 2021-W08
    interval: 1 monday week
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

    An incidence2 object: 8 x 4
    84 cases from days 2021-W05 to 2021-W08
    interval: 1 monday week
    cumulative: FALSE
    
      date_index height size  count
      <yrwk>     <chr>  <chr> <int>
    1 2021-W05   short  small     7
    2 2021-W05   tall   small     7
    3 2021-W06   short  large     7
    4 2021-W06   tall   large     7
    5 2021-W07   short  large    14
    6 2021-W07   tall   large    14
    7 2021-W08   short  large    14
    8 2021-W08   tall   large    14
    

---

    An incidence2 object: 8 x 4
    84 cases from 2021-W05 to 2021-W08
    interval: 1 monday week
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

    An incidence2 object: 105 x 2
    1096 cases from days 2020-W01 to 2022-W01
    interval: 1 wednesday week
    cumulative: FALSE
    
       date_index count
       <yrwk>     <int>
     1 2020-W01       7
     2 2020-W02       7
     3 2020-W03       7
     4 2020-W04       7
     5 2020-W05       7
     6 2020-W06       7
     7 2020-W07       7
     8 2020-W08       7
     9 2020-W09       7
    10 2020-W10       7
    # ... with 95 more rows
    

---

    An incidence2 object: 105 x 2
    1096 cases from 2020-W01 to 2022-W01
    interval: 1 wednesday week
    cumulative: FALSE
    timespan: 735 days
    
    

# multi-week, no groupings and without count work as expected

    An incidence2 object: 53 x 2
    731 cases from days [2020-01-01 to [2021-12-29
    interval: 2 wednesday weeks
    cumulative: FALSE
    
       date_index  count
       <period>    <int>
     1 [2020-01-01    14
     2 [2020-01-15    14
     3 [2020-01-29    14
     4 [2020-02-12    14
     5 [2020-02-26    14
     6 [2020-03-11    14
     7 [2020-03-25    14
     8 [2020-04-08    14
     9 [2020-04-22    14
    10 [2020-05-06    14
    # ... with 43 more rows
    

---

    An incidence2 object: 53 x 2
    731 cases from [2020-01-01 to [2021-12-29
    interval: 2 wednesday weeks
    cumulative: FALSE
    timespan: 742 days
    
    

# multi-week, no groupings and with count work as expected

    An incidence2 object: 53 x 2
    1096 cases from days [2020-01-01 to [2021-12-29
    interval: 2 wednesday weeks
    cumulative: FALSE
    
       date_index  count
       <period>    <int>
     1 [2020-01-01    14
     2 [2020-01-15    14
     3 [2020-01-29    14
     4 [2020-02-12    14
     5 [2020-02-26    14
     6 [2020-03-11    14
     7 [2020-03-25    14
     8 [2020-04-08    14
     9 [2020-04-22    14
    10 [2020-05-06    14
    # ... with 43 more rows
    

---

    An incidence2 object: 53 x 2
    1096 cases from [2020-01-01 to [2021-12-29
    interval: 2 wednesday weeks
    cumulative: FALSE
    timespan: 742 days
    
    

# week defaults to a monday

    An incidence2 object: 105 x 2
    731 cases from days 2020-W01 to 2021-W52
    interval: 1 monday week
    cumulative: FALSE
    
       date_index count
       <yrwk>     <int>
     1 2020-W01       5
     2 2020-W02       7
     3 2020-W03       7
     4 2020-W04       7
     5 2020-W05       7
     6 2020-W06       7
     7 2020-W07       7
     8 2020-W08       7
     9 2020-W09       7
    10 2020-W10       7
    # ... with 95 more rows
    

---

    An incidence2 object: 105 x 2
    731 cases from 2020-W01 to 2021-W52
    interval: 1 monday week
    cumulative: FALSE
    timespan: 735 days
    
    

# single month, no groupings and without count work as expected

    An incidence2 object: 24 x 2
    731 cases from days 2020-Jan to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    
       date_index count
       <yrmon>    <int>
     1 2020-Jan      31
     2 2020-Feb      29
     3 2020-Mar      31
     4 2020-Apr      30
     5 2020-May      31
     6 2020-Jun      30
     7 2020-Jul      31
     8 2020-Aug      31
     9 2020-Sep      30
    10 2020-Oct      31
    # ... with 14 more rows
    

---

    An incidence2 object: 24 x 2
    731 cases from 2020-Jan to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    timespan: 731 days
    
    

# single month, no groupings and with count work as expected

    An incidence2 object: 24 x 2
    1096 cases from days 2020-Jan to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    
       date_index count
       <yrmon>    <int>
     1 2020-Jan      31
     2 2020-Feb      29
     3 2020-Mar      31
     4 2020-Apr      30
     5 2020-May      31
     6 2020-Jun      30
     7 2020-Jul      31
     8 2020-Aug      31
     9 2020-Sep      30
    10 2020-Oct      31
    # ... with 14 more rows
    

---

    An incidence2 object: 24 x 2
    1096 cases from 2020-Jan to 2021-Dec
    interval: 1 month
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-month, no groupings and without count work as expected

    An incidence2 object: 12 x 2
    731 cases from days [2020-01-01 to [2021-11-01
    interval: 2 months
    cumulative: FALSE
    
       date_index  count
       <period>    <int>
     1 [2020-01-01    60
     2 [2020-03-01    61
     3 [2020-05-01    61
     4 [2020-07-01    62
     5 [2020-09-01    61
     6 [2020-11-01    61
     7 [2021-01-01    59
     8 [2021-03-01    61
     9 [2021-05-01    61
    10 [2021-07-01    62
    11 [2021-09-01    61
    12 [2021-11-01    61
    

---

    An incidence2 object: 12 x 2
    731 cases from [2020-01-01 to [2021-11-01
    interval: 2 months
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-month, no groupings and with count work as expected

    An incidence2 object: 12 x 2
    1096 cases from days [2020-01-01 to [2021-11-01
    interval: 2 months
    cumulative: FALSE
    
       date_index  count
       <period>    <int>
     1 [2020-01-01    60
     2 [2020-03-01    61
     3 [2020-05-01    61
     4 [2020-07-01    62
     5 [2020-09-01    61
     6 [2020-11-01    61
     7 [2021-01-01   118
     8 [2021-03-01   122
     9 [2021-05-01   122
    10 [2021-07-01   124
    11 [2021-09-01   122
    12 [2021-11-01   122
    

---

    An incidence2 object: 12 x 2
    1096 cases from [2020-01-01 to [2021-11-01
    interval: 2 months
    cumulative: FALSE
    timespan: 731 days
    
    

# single quarter, no groupings and without count work as expected

    An incidence2 object: 8 x 2
    731 cases from days 2020-Q1 to 2021-Q4
    interval: 1 quarter
    cumulative: FALSE
    
      date_index count
      <yrqtr>    <int>
    1 2020-Q1       91
    2 2020-Q2       91
    3 2020-Q3       92
    4 2020-Q4       92
    5 2021-Q1       90
    6 2021-Q2       91
    7 2021-Q3       92
    8 2021-Q4       92
    

---

    An incidence2 object: 8 x 2
    731 cases from 2020-Q1 to 2021-Q4
    interval: 1 quarter
    cumulative: FALSE
    timespan: 731 days
    
    

# single quarter, no groupings and with count work as expected

    An incidence2 object: 8 x 2
    1096 cases from days 2020-Q1 to 2021-Q4
    interval: 1 quarter
    cumulative: FALSE
    
      date_index count
      <yrqtr>    <int>
    1 2020-Q1       91
    2 2020-Q2       91
    3 2020-Q3       92
    4 2020-Q4       92
    5 2021-Q1      180
    6 2021-Q2      182
    7 2021-Q3      184
    8 2021-Q4      184
    

---

    An incidence2 object: 8 x 2
    1096 cases from 2020-Q1 to 2021-Q4
    interval: 1 quarter
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-quarter, no groupings and without count work as expected

    An incidence2 object: 4 x 2
    731 cases from days [2020-01-01 to [2021-07-01
    interval: 2 quarters
    cumulative: FALSE
    
      date_index  count
      <period>    <int>
    1 [2020-01-01   182
    2 [2020-07-01   184
    3 [2021-01-01   181
    4 [2021-07-01   184
    

---

    An incidence2 object: 4 x 2
    731 cases from [2020-01-01 to [2021-07-01
    interval: 2 quarters
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-quarter, no groupings and with count work as expected

    An incidence2 object: 4 x 2
    1096 cases from days [2020-01-01 to [2021-07-01
    interval: 2 quarters
    cumulative: FALSE
    
      date_index  count
      <period>    <int>
    1 [2020-01-01   182
    2 [2020-07-01   184
    3 [2021-01-01   362
    4 [2021-07-01   368
    

---

    An incidence2 object: 4 x 2
    1096 cases from [2020-01-01 to [2021-07-01
    interval: 2 quarters
    cumulative: FALSE
    timespan: 731 days
    
    

# single year, no groupings and without count work as expected

    An incidence2 object: 2 x 2
    731 cases from days 2020 to 2021
    interval: 1 year
    cumulative: FALSE
    
      date_index count
      <yr>       <int>
    1 2020         366
    2 2021         365
    

---

    An incidence2 object: 2 x 2
    731 cases from 2020 to 2021
    interval: 1 year
    cumulative: FALSE
    timespan: 731 days
    
    

# single year, no groupings and with count work as expected

    An incidence2 object: 2 x 2
    1096 cases from days 2020 to 2021
    interval: 1 year
    cumulative: FALSE
    
      date_index count
      <yr>       <int>
    1 2020         366
    2 2021         730
    

---

    An incidence2 object: 2 x 2
    1096 cases from 2020 to 2021
    interval: 1 year
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-year, no groupings and without count work as expected

    An incidence2 object: 1 x 2
    731 cases from days [2020-01-01 to [2020-01-01
    interval: 2 years
    cumulative: FALSE
    
      date_index  count
      <period>    <int>
    1 [2020-01-01   731
    

---

    An incidence2 object: 1 x 2
    731 cases from [2020-01-01 to [2020-01-01
    interval: 2 years
    cumulative: FALSE
    timespan: 731 days
    
    

# multi-year, no groupings and with count work as expected

    An incidence2 object: 1 x 2
    1096 cases from days [2020-01-01 to [2020-01-01
    interval: 2 years
    cumulative: FALSE
    
      date_index  count
      <period>    <int>
    1 [2020-01-01  1096
    

---

    An incidence2 object: 1 x 2
    1096 cases from [2020-01-01 to [2020-01-01
    interval: 2 years
    cumulative: FALSE
    timespan: 731 days
    
    

# integer date periods without counts work as expected

    An incidence2 object: 2 x 2
    10 cases from days [1, 6) to [6, 11)
    interval: 5
    cumulative: FALSE
    
      date_index count
      <int_perd> <int>
    1 [1, 6)         5
    2 [6, 11)        5
    

---

    An incidence2 object: 2 x 2
    10 cases from [1, 6) to [6, 11)
    interval: 5
    cumulative: FALSE
    timespan: 10 days
    
    

# integer date periods with counts work as expected

    An incidence2 object: 2 x 2
    15 cases from days [1, 6) to [6, 11)
    interval: 5
    cumulative: FALSE
    
      date_index count
      <int_perd> <dbl>
    1 [1, 6)         5
    2 [6, 11)       10
    

---

    An incidence2 object: 2 x 2
    15 cases from [1, 6) to [6, 11)
    interval: 5
    cumulative: FALSE
    timespan: 10 days
    
    

