lookfor <- 
  function (pattern, data, ...) 
  {
    l <- lapply(data, function(x, ...) grep(pattern, x, ...))
    res <- rep(FALSE, ncol(data))
    res[grep(pattern, names(data), ...)] <- TRUE
    res <- sapply(l, length) > 0 | res
    names(res) <- names(data)
    names(res)[res]
  }


# BAI Scoring
lookfor("BAI", mjolnir_clean)   # How likely is this person to catch COVID-19?
#[1] "BAI_TIME_First Click" "BAI_TIME_Last Click"  "BAI_TIME_Page Submit" "BAI_TIME_Click Count" "BAI_1"                "BAI_2"                "BAI_3"                "BAI_4"               
#[9] "BAI_5"                "BAI_6"                "BAI_7"                "BAI_8"                "BAI_9"                "BAI_10"               "BAI_11"               "BAI_12"              
#[17] "BAI_13"               "BAI_14"               "BAI_15"               "BAI_16"               "BAI_17"               "BAI_18"               "BAI_19"               "BAI_20"              
#[25] "BAI_21" 
hist(mjolnir_clean$"BAI_TIME_Page Submit")
# Some perople took an egregiously long time...

## How long did people take to complete the BAI?
# "BAI_TIME_First Click" "BAI_TIME_Last Click"  "BAI_TIME_Page Submit" "BAI_TIME_Click Count" 
summary(mjolnir_clean %>%
          select(contains(c("BAI_TIME"))))

#BAI_TIME_First Click BAI_TIME_Last Click BAI_TIME_Page Submit BAI_TIME_Click Count
#Min.   :   0.000     Min.   :   0.00     Min.   :  11.14      Min.   :  0.00      
#1st Qu.:   3.899     1st Qu.:  36.09     1st Qu.:  37.85      1st Qu.: 22.00      
#Median :   8.068     Median :  50.89     Median :  52.39      Median : 24.00      
#Mean   :  27.733     Mean   :  79.35     Mean   :  81.65      Mean   : 27.89      
#3rd Qu.:  13.534     3rd Qu.:  74.44     3rd Qu.:  77.10      3rd Qu.: 30.00      
#Max.   :3501.420     Max.   :3514.01     Max.   :3514.92      Max.   :222.00 




## Cronbach's Alpha, Internal Consistency
library(psych)
mjolnir_BAI<-mjolnir_clean %>% select(matches("BAI_[0-9]+$"))
psych::alpha(mjolnir_BAI)

#Reliability analysis   
#Call: psych::alpha(x = mjolnir_BAI)
#
#raw_alpha std.alpha G6(smc) average_r S/N    ase mean   sd median_r
#0.94      0.94    0.95      0.42  15 0.0035  1.5 0.51      0.4
#
#lower alpha upper     95% confidence boundaries
#0.93 0.94 0.94 
#
#Reliability if an item is dropped:
#  raw_alpha std.alpha G6(smc) average_r S/N alpha se var.r med.r
#BAI_1       0.93      0.94    0.95      0.43  15   0.0035 0.012  0.41
#BAI_2       0.93      0.94    0.95      0.42  15   0.0035 0.011  0.41
#BAI_3       0.93      0.94    0.95      0.42  15   0.0036 0.011  0.41
#BAI_4       0.93      0.93    0.95      0.42  14   0.0037 0.011  0.40
#BAI_5       0.93      0.93    0.95      0.42  14   0.0037 0.011  0.40
#BAI_6       0.93      0.93    0.94      0.41  14   0.0037 0.012  0.39
#BAI_7       0.93      0.93    0.95      0.41  14   0.0038 0.011  0.39
#BAI_8       0.93      0.93    0.95      0.41  14   0.0037 0.011  0.39
#BAI_9       0.93      0.93    0.94      0.41  14   0.0037 0.010  0.40
#BAI_10      0.93      0.93    0.95      0.42  14   0.0037 0.011  0.41
#BAI_11      0.93      0.94    0.95      0.42  15   0.0036 0.012  0.41
#BAI_12      0.93      0.93    0.95      0.42  14   0.0036 0.012  0.40
#BAI_13      0.93      0.93    0.94      0.41  14   0.0037 0.011  0.39
#BAI_14      0.93      0.93    0.95      0.41  14   0.0037 0.011  0.40
#BAI_15      0.93      0.93    0.95      0.42  14   0.0037 0.012  0.40
#BAI_16      0.94      0.94    0.95      0.43  15   0.0035 0.010  0.41
#BAI_17      0.93      0.93    0.94      0.41  14   0.0037 0.011  0.40
#BAI_18      0.94      0.94    0.95      0.43  15   0.0035 0.011  0.42
#BAI_19      0.93      0.93    0.95      0.42  14   0.0036 0.012  0.41
#BAI_20      0.93      0.93    0.95      0.42  14   0.0036 0.012  0.40
#BAI_21      0.93      0.93    0.95      0.42  14   0.0036 0.012  0.41
#
#Item statistics 
#n raw.r std.r r.cor r.drop mean   sd
#BAI_1  663  0.55  0.57  0.54   0.51  1.3 0.62
#BAI_2  663  0.57  0.58  0.55   0.52  1.7 0.81
#BAI_3  663  0.57  0.59  0.57   0.53  1.2 0.54
#BAI_4  663  0.73  0.70  0.69   0.68  2.2 0.96
#BAI_5  663  0.73  0.70  0.69   0.68  2.0 0.99
#BAI_6  663  0.70  0.71  0.70   0.66  1.5 0.76
#BAI_7  663  0.77  0.76  0.75   0.73  1.6 0.85
#BAI_8  663  0.74  0.75  0.74   0.71  1.5 0.78
#BAI_9  663  0.76  0.74  0.73   0.72  1.6 0.86
#BAI_10 663  0.73  0.70  0.69   0.68  2.2 0.94
#BAI_11 663  0.58  0.61  0.58   0.55  1.2 0.48
#BAI_12 663  0.65  0.67  0.65   0.61  1.3 0.58
#BAI_13 663  0.76  0.77  0.77   0.73  1.3 0.65
#BAI_14 663  0.73  0.71  0.70   0.69  1.5 0.84
#BAI_15 663  0.69  0.70  0.68   0.65  1.3 0.69
#BAI_16 663  0.53  0.53  0.50   0.48  1.4 0.79
#BAI_17 663  0.73  0.71  0.70   0.69  1.7 0.84
#BAI_18 663  0.51  0.51  0.47   0.45  1.5 0.77
#BAI_19 663  0.66  0.67  0.65   0.62  1.4 0.69
#BAI_20 663  0.66  0.68  0.65   0.62  1.3 0.62
#BAI_21 663  0.65  0.66  0.64   0.61  1.4 0.72
#
#Non missing response frequency for each item
#1    2    3    4 miss
#BAI_1  0.74 0.20 0.05 0.01    0
#BAI_2  0.54 0.30 0.13 0.03    0
#BAI_3  0.82 0.14 0.03 0.01    0
#BAI_4  0.30 0.34 0.27 0.10    0
#BAI_5  0.38 0.32 0.19 0.10    0
#BAI_6  0.61 0.27 0.10 0.02    0
#BAI_7  0.58 0.26 0.13 0.04    0
#BAI_8  0.69 0.20 0.09 0.03    0
#BAI_9  0.61 0.24 0.11 0.05    0
#BAI_10 0.25 0.38 0.26 0.11    0
#BAI_11 0.89 0.07 0.03 0.01    0
#BAI_12 0.79 0.16 0.03 0.01    0
#BAI_13 0.75 0.18 0.06 0.02    0
#BAI_14 0.67 0.20 0.08 0.05    0
#BAI_15 0.79 0.12 0.06 0.02    0
#BAI_16 0.75 0.15 0.06 0.05    0
#BAI_17 0.52 0.32 0.12 0.04    0
#BAI_18 0.65 0.23 0.09 0.03    0
#BAI_19 0.72 0.20 0.07 0.02    0
#BAI_20 0.79 0.15 0.05 0.02    0
#BAI_21 0.69 0.22 0.06 0.03    0



## Scoring: Summation
# A total score of 0 - 7 is interpreted as a "Minimal" level of anxiety; 8 - 15 as "Mild"; 16 - 25 as "Moderate", and; 26 - 63 as "Severe". 
# BAI_1-BAI_21
mjolnir_clean <- mjolnir_clean %>% mutate(BAIsum = rowSums(mjolnir_clean[c("BAI_1","BAI_2","BAI_3","BAI_4","BAI_5","BAI_6","BAI_7","BAI_8","BAI_9","BAI_10","BAI_11","BAI_12","BAI_13","BAI_14","BAI_15","BAI_16","BAI_17","BAI_18","BAI_19","BAI_20","BAI_21")]))
mjolnir_clean$BAIsum <- mjolnir_clean$BAIsum -21
summary(mjolnir_clean$BAIsum)
library(lessR)
# Histogram(BAIsum, data=mjolnir_clean, bin.start=.5, bin.width=1, xlim=c(0,4))
Histogram(BAIsum, data=mjolnir_clean)


## Chinese vs. White






# BDI Scoring
lookfor("BDI", mjolnir_clean)
#[1] "BDI_TIME_First Click" "BDI_TIME_Last Click"  "BDI_TIME_Page Submit" "BDI_TIME_Click Count" "BDI01"                "BDI02"                "BDI03"                "BDI04"               
#[9] "BDI05"                "BDI06"                "BDI07"                "BDI08"                "BDI09"                "BDI10"                "BDI11"                "BDI12"               
#[17] "BDI13"                "BDI14"                "BDI15"                "BDI16"                "BDI17"                "BDI18"                "BDI19"                "BDI19a"              
#[25] "BDI20"                "BDI21" 

hist(mjolnir_clean$"BDI_TIME_Page Submit")
# Some perople took an egregiously long time...

## How long did people take to complete the BDI?
summary(mjolnir_clean %>%
          select(contains(c("BDI_TIME"))))

#BDI_TIME_First Click BDI_TIME_Last Click BDI_TIME_Page Submit BDI_TIME_Click Count
#Min.   :   0.034     Min.   :  30.98     Min.   :  32.24      Min.   : 22.00      
#1st Qu.:   3.878     1st Qu.:  85.97     1st Qu.:  87.46      1st Qu.: 23.00      
#Median :   8.496     Median : 122.46     Median : 123.93      Median : 27.00      
#Mean   :  19.116     Mean   : 149.67     Mean   : 151.51      Mean   : 37.97      
#3rd Qu.:  16.956     3rd Qu.: 166.93     3rd Qu.: 168.91      3rd Qu.: 45.00      
#Max.   :2995.388     Max.   :3080.27     Max.   :3083.15      Max.   :337.00   

# Cronbach's alpha
mjolnir_BDI<-mjolnir_clean %>% select(matches("BDI[0-9]+$"))
psych::alpha(mjolnir_BDI)

#Reliability analysis   
#Call: psych::alpha(x = mjolnir_BDI)
#
#raw_alpha std.alpha G6(smc) average_r S/N    ase mean   sd median_r
#0.91      0.91    0.92      0.32  10 0.0049  1.5 0.43     0.33
#
#lower alpha upper     95% confidence boundaries
#0.9 0.91 0.92 
#
#Reliability if an item is dropped:
#  raw_alpha std.alpha G6(smc) average_r  S/N alpha se  var.r med.r
#BDI01      0.90      0.90    0.91      0.32  9.4   0.0052 0.0146  0.32
#BDI02      0.91      0.90    0.91      0.32  9.5   0.0051 0.0145  0.32
#BDI03      0.90      0.90    0.91      0.32  9.3   0.0053 0.0133  0.33
#BDI04      0.90      0.90    0.91      0.32  9.4   0.0052 0.0145  0.33
#BDI05      0.91      0.91    0.91      0.32  9.5   0.0051 0.0147  0.33
#BDI06      0.91      0.91    0.91      0.32  9.6   0.0051 0.0148  0.33
#BDI07      0.90      0.90    0.91      0.31  9.2   0.0053 0.0127  0.32
#BDI08      0.90      0.90    0.91      0.32  9.3   0.0052 0.0137  0.32
#BDI09      0.91      0.91    0.91      0.32  9.6   0.0051 0.0146  0.33
#BDI10      0.91      0.91    0.91      0.33  9.7   0.0050 0.0153  0.34
#BDI11      0.91      0.91    0.91      0.33  9.8   0.0050 0.0149  0.33
#BDI12      0.91      0.91    0.91      0.32  9.6   0.0051 0.0149  0.33
#BDI13      0.90      0.90    0.91      0.32  9.4   0.0052 0.0146  0.32
#BDI14      0.91      0.90    0.91      0.32  9.5   0.0051 0.0143  0.32
#BDI15      0.90      0.90    0.91      0.32  9.3   0.0052 0.0142  0.32
#BDI16      0.91      0.91    0.91      0.33  9.9   0.0049 0.0149  0.34
#BDI17      0.90      0.90    0.91      0.32  9.3   0.0052 0.0144  0.32
#BDI18      0.91      0.91    0.91      0.33  9.9   0.0050 0.0152  0.34
#BDI19      0.91      0.92    0.92      0.35 10.8   0.0047 0.0084  0.34
#BDI20      0.91      0.91    0.91      0.33  9.8   0.0050 0.0152  0.33
#BDI21      0.91      0.91    0.92      0.33 10.0   0.0048 0.0143  0.34
#
#Item statistics 
#n raw.r std.r r.cor r.drop mean   sd
#BDI01 663  0.68  0.68  0.67   0.63  1.6 0.66
#BDI02 663  0.63  0.63  0.61   0.58  1.7 0.78
#BDI03 663  0.71  0.71  0.70   0.67  1.6 0.79
#BDI04 663  0.68  0.67  0.65   0.63  1.7 0.76
#BDI05 663  0.61  0.62  0.59   0.56  1.4 0.64
#BDI06 663  0.60  0.61  0.59   0.55  1.3 0.66
#BDI07 663  0.75  0.75  0.75   0.71  1.6 0.72
#BDI08 663  0.71  0.71  0.70   0.66  1.7 0.77
#BDI09 663  0.58  0.60  0.57   0.54  1.2 0.48
#BDI10 663  0.54  0.55  0.52   0.49  1.4 0.72
#BDI11 663  0.55  0.54  0.51   0.49  1.7 0.69
#BDI12 663  0.61  0.60  0.58   0.55  1.6 0.77
#BDI13 663  0.66  0.66  0.64   0.62  1.5 0.73
#BDI14 663  0.65  0.63  0.60   0.58  1.8 1.00
#BDI15 663  0.70  0.69  0.68   0.65  1.9 0.79
#BDI16 663  0.50  0.50  0.46   0.44  1.6 0.76
#BDI17 663  0.69  0.69  0.68   0.65  1.8 0.76
#BDI18 663  0.50  0.51  0.47   0.44  1.4 0.67
#BDI19 663  0.18  0.20  0.14   0.12  1.2 0.52
#BDI20 663  0.53  0.54  0.51   0.48  1.4 0.60
#BDI21 663  0.45  0.44  0.40   0.38  1.4 0.77
#
#Non missing response frequency for each item
#1    2    3    4 miss
#BDI01 0.51 0.42 0.05 0.01    0
#BDI02 0.47 0.41 0.08 0.04    0
#BDI03 0.59 0.29 0.07 0.04    0
#BDI04 0.48 0.43 0.05 0.04    0
#BDI05 0.70 0.24 0.05 0.01    0
#BDI06 0.79 0.14 0.04 0.03    0
#BDI07 0.48 0.44 0.05 0.03    0
#BDI08 0.48 0.38 0.12 0.02    0
#BDI09 0.82 0.17 0.01 0.01    0
#BDI10 0.74 0.20 0.02 0.04    0
#BDI11 0.44 0.45 0.10 0.01    0
#BDI12 0.51 0.36 0.10 0.03    0
#BDI13 0.60 0.28 0.11 0.01    0
#BDI14 0.54 0.28 0.07 0.11    0
#BDI15 0.34 0.45 0.19 0.03    0
#BDI16 0.50 0.39 0.08 0.03    0
#BDI17 0.38 0.47 0.13 0.03    0
#BDI18 0.73 0.20 0.06 0.02    0
#BDI19 0.85 0.12 0.02 0.01    0
#BDI20 0.66 0.30 0.04 0.01    0
#BDI21 0.70 0.19 0.07 0.03    0


## Scoring: Summation
#Beck AT & Steer RA (1987). Manual for the Revised Beck Depression Inventory. Psychological 
#Corp: San Antonio, TX.

#The BDI-II is scored by summing the ratings for the 21 items. Each item is rated on a 4-point scale ranging from 0 to 3. The maximum
#total score is 63.
#Special attention must be paid to the correct scoring of the Changes in Sleeping Pattern (Item 16) and Changes in Appetite (Item 18)
#items. Each of these items contains seven options rated, in order, 0, 1a, 1b, 2a, 2b, 3a, 3b, to differentiate between increases and
#decreases in behavior or motivation. If a higher rated option is chosen by the respondent, the presence of an increase or decrease in
#either symptom should be clinically noted for diagnostic purposes. 
mjolnir_clean <- mjolnir_clean %>% mutate(BDIsum = rowSums(mjolnir_clean[c("BDI01","BDI02","BDI03","BDI04","BDI05","BDI06","BDI07","BDI08","BDI09","BDI10","BDI11","BDI12","BDI13","BDI14","BDI15","BDI16","BDI17","BDI18","BDI19","BDI20","BDI21")]))
mjolnir_clean$BDIsum <- mjolnir_clean$BDIsum-21
summary(mjolnir_clean$BDIsum)
Histogram(BDIsum, data=mjolnir_clean)




## Chinese vs. White
















# COVID-19 Attitudes Scoring
lookfor("COVATT", mjolnir_clean)
lookfor("COVCONSP", mjolnir_clean)
lookfor("COVORIGIN", mjolnir_clean)
lookfor("COVPOLITICS", mjolnir_clean)
lookfor("COVCOVERAGE", mjolnir_clean)
lookfor("COVANTIVACC", mjolnir_clean)
lookfor("COVMEDSKEP", mjolnir_clean)

summary(mjolnir_clean %>%
          select(contains(c("COVATT"))))

#COVATT_1        COVATT_2        COVATT_3        COVATT_4        COVATT_5**    
#Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
#1st Qu.:1.000   1st Qu.:2.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:4.000  
#Median :2.000   Median :3.000   Median :1.000   Median :1.000   Median :5.000  
#Mean   :2.213   Mean   :3.048   Mean   :1.713   Mean   :1.623   Mean   :4.525  
#3rd Qu.:3.000   3rd Qu.:4.000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:6.000  
#Max.   :6.000   Max.   :6.000   Max.   :6.000   Max.   :6.000   Max.   :6.000  

summary(mjolnir_clean %>%
          select(contains(c("COVCONSP"))))

#COVCONSP_1     COVCONSP_2      COVCONSP_3      COVCONSP_4**   
#Min.   :1.00   Min.   :1.000   Min.   :1.000   Min.   :1.000  
#1st Qu.:1.00   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:4.000  
#Median :1.00   Median :2.000   Median :2.000   Median :5.000  
#Mean   :1.51   Mean   :2.291   Mean   :2.173   Mean   :4.621  
#3rd Qu.:2.00   3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:6.000  
#Max.   :6.00   Max.   :6.000   Max.   :6.000   Max.   :6.000  

summary(mjolnir_clean %>%
          select(contains(c("COVORIGIN"))))

#COVORIGIN_1     COVORIGIN_2**     COVORIGIN_3     COVORIGIN_4   
#Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :1.000  
#1st Qu.:1.000   1st Qu.:4.000   1st Qu.:1.000   1st Qu.:1.000  
#Median :1.000   Median :5.000   Median :2.000   Median :1.000  
#Mean   :1.772   Mean   :4.431   Mean   :2.202   Mean   :2.053  
#3rd Qu.:2.000   3rd Qu.:6.000   3rd Qu.:3.000   3rd Qu.:3.000  
#Max.   :6.000   Max.   :6.000   Max.   :6.000   Max.   :6.000  


summary(mjolnir_clean %>%
          select(contains(c("COVPOLITICS"))))

#COVPOLITICS_1   COVPOLITICS_2   COVPOLITICS_3**  
#Min.   :1.000   Min.   :1.000   Min.   :1.000  
#1st Qu.:1.000   1st Qu.:1.000   1st Qu.:5.000  
#Median :4.000   Median :2.000   Median :6.000  
#Mean   :3.285   Mean   :2.072   Mean   :5.148  
#3rd Qu.:5.000   3rd Qu.:3.000   3rd Qu.:6.000  
#Max.   :6.000   Max.   :6.000   Max.   :6.000  



summary(mjolnir_clean %>%
          select(contains(c("COVCOVERAGE"))))

#COVCOVERAGE_1   COVCOVERAGE_2   COVCOVERAGE_3  
#Min.   :1.000   Min.   :1.000   Min.   :1.000  
#1st Qu.:1.000   1st Qu.:3.000   1st Qu.:1.000  
#Median :2.000   Median :4.000   Median :2.000  
#Mean   :2.647   Mean   :3.768   Mean   :2.412  
#3rd Qu.:4.000   3rd Qu.:5.000   3rd Qu.:3.000  
#Max.   :6.000   Max.   :6.000   Max.   :6.000  


summary(mjolnir_clean %>%
          select(contains(c("COVANTIVACC"))))

#COVANTIVACC_1   COVANTIVACC_2   COVANTIVACC_3**  
#Min.   :1.000   Min.   :1.000   Min.   :1.000  
#1st Qu.:1.000   1st Qu.:1.000   1st Qu.:4.000  
#Median :1.000   Median :1.000   Median :5.000  
#Mean   :2.044   Mean   :1.451   Mean   :5.038  
#3rd Qu.:3.000   3rd Qu.:1.000   3rd Qu.:6.000  
#Max.   :6.000   Max.   :6.000   Max.   :6.000  


summary(mjolnir_clean %>%
          select(contains(c("COVMEDSKEP"))))

#COVMEDSKEP_1   COVMEDSKEP_2    COVMEDSKEP_3**    COVMEDSKEP_4** 
#Min.   :1.00   Min.   :1.000   Min.   :1.000   Min.   :1.000  
#1st Qu.:1.00   1st Qu.:1.000   1st Qu.:4.000   1st Qu.:4.000  
##Median :2.00   Median :1.000   Median :5.000   Median :5.000  
#Mean   :2.22   Mean   :1.905   Mean   :4.784   Mean   :5.098  
#3rd Qu.:3.00   3rd Qu.:2.000   3rd Qu.:6.000   3rd Qu.:6.000  
#Max.   :6.00   Max.   :6.000   Max.   :6.000   Max.   :6.000  

## Principal Components
mjolnir_COV <- mjolnir_clean %>%
          select(contains(c("COVATT", "COVCONSP", "COVORIGIN", "COVPOLITICS", "COVCOVERAGE", "COVANTIVACC", "COVMEDSKEP")))

mjolnir_COV_PC <- mjolnir_COV %>% prcomp(center = TRUE, scale. = TRUE) 

mjolnir_COV
mjolnir_COV_PC %>% summary()
mjolnir_COV_PC %>% str()

library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

library(tidyverse)

mjolnir_COV_PC %>% ggbiplot()

mjolnir_COV$EXPGRP <- factor(mjolnir_clean$EXPGRP, levels=c("1", "2", "3", "4"), labels = c("White", "Chinese", "Other Asian", "Other"))
mjolnir_COV$EXPGRP

## Browsing through the bivariate principal components
mjolnir_COV_PC %>% ggbiplot(ellipse=TRUE, groups=mjolnir_COV$EXPGRP)
mjolnir_COV_PC %>% ggbiplot(ellipse=TRUE, groups=mjolnir_COV$EXPGRP, choices=c(3,4))
mjolnir_COV_PC %>% ggbiplot(ellipse=TRUE, groups=mjolnir_COV$EXPGRP, choices=c(5,6))
mjolnir_COV_PC %>% ggbiplot(ellipse=TRUE, groups=mjolnir_COV$EXPGRP, choices=c(7,8))
mjolnir_COV_PC %>% ggbiplot(ellipse=TRUE, groups=mjolnir_COV$EXPGRP, choices=c(9,10))







# AMBI Scoring
mjolnir_AMBI <- mjolnir_clean %>%
  select(contains(c("AMBI_W"))) %>% select(!contains(c("TIME", "DO", "47")))
summary(mjolnir_AMBI) 

## Identifying outliers with mahalanobis distance
install.packages("stats")
library(stats)

m_dist <- mahalanobis(mjolnir_AMBI, colMeans(mjolnir_AMBI), cov(mjolnir_AMBI))
mjolnir_AMBI$MD <- round(m_dist, 1)
Histogram(MD, mjolnir_AMBI)


library(psych)
library(tidyverse)
#library(simstudy)
install.packages("jtools")
library(jtools)

pairs.panels(mjolnir_AMBI, stars = TRUE)
outlier(mjolnir_AMBI)

# those that fall above the cut-off score for a chi-square with k degrees of freedom (k being the number of items)
alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(mjolnir_AMBI)))
names_outliers_MH <- which(mjolnir_AMBI$MD > cutoff)
excluded_mh <- names_outliers_MH
length(excluded_mh)
# data_clean_mh <- data_outlier[-excluded_mh, ]
# data[excluded_mh, ]






## Chinese vs. White





# Bias Scoring



# Path modelling









## Mahalanobis Distance outlier detection

# R's mahalanobis() function provides a simple means of detecting outliers in multidimensional data
# Be wary of mahalanobis() when your data exhibit nonlinear relationships, as the Mahalanobis distance equation only accounts for linear relationships.


percentage.to.remove <- 5 # Remove 5% of points
number.to.remove     <- trunc(nrow(my.dataframe) * percentage.to.remove / 100)
m.dist               <- mahalanobis(my.dataframe, colMeans(my.dataframe), cov(my.dataframe))
m.dist.order         <- order(m.dist, decreasing=TRUE)
rows.to.keep.index   <- m.dist.order[(number.to.remove+1):nrow(my.dataframe)]
my.dataframe         <- my.dataframe[rows.to.keep.index,]



