# NHANES_survey_analysis
Allie Warren, allison.warren
2026-02-11

- [Survey Analysis and Converting From SUDAAN to
  R](#survey-analysis-and-converting-from-sudaan-to-r)
  - [Hypertension Prevalence Among Adults Aged 18 and Over: United
    States,
    2017–2018](#hypertension-prevalence-among-adults-aged-18-and-over-united-states-20172018)
    - [Downloading the Data](#downloading-the-data)
    - [Data Formatting](#data-formatting)
    - [Define Survey Design](#define-survey-design)
    - [Figure 1: Hypertension prevalence by sex and
      age](#figure-1-hypertension-prevalence-by-sex-and-age)
    - [Figure 2: Age-Adjusted Prevalence by Sex and
      Race/Ethnicity](#figure-2-age-adjusted-prevalence-by-sex-and-raceethnicity)
    - [Figure 3: Age-Adjusted Prevalence by Sex and
      Education](#figure-3-age-adjusted-prevalence-by-sex-and-education)
    - [Figure 4: Trends in Age-Adjusted Prevalence
      (1999-2018)](#figure-4-trends-in-age-adjusted-prevalence-1999-2018)

# Survey Analysis and Converting From SUDAAN to R

This notebook (mostly) replicates analysis shown in the NCHS Data Brief
is here: <https://www.cdc.gov/nchs/data/databriefs/db364-h.pdf>. SUDAAN
code provided by CDC for running a similar analysis of the NHANES data
is available here:
wwwn.cdc.gov/nchs/data/Tutorials/Code/DB364_SUDAAN.sas.

More resources on Survey Data Analysis with R: Survey Data Analysis with
R: <https://stats.oarc.ucla.edu/r/seminars/survey-data-analysis-with-r/>

``` r
# Load required packages
if(!require("pacman")) install.packages("pacman") #but first we have to install pacman
```

    Loading required package: pacman

``` r
# This load packages for transforming data, reshaping data, analyzing survey data, reading in data stored by SAS, SPSS, Stata, and more, tools for working with functions and vectors, and for creating plots 
pacman::p_load(dplyr, tidyr, survey, foreign, purrr, ggplot2)
```

## Hypertension Prevalence Among Adults Aged 18 and Over: United States, 2017–2018

This analysis looks at hypertension prevalence as reported in the
National Health and Nutrition Examination Survey.

### Downloading the Data

``` r
# Function to download NHANES XPT files
download_nhanes <- function(file_name, cycle_suffix) {
  # get year associated with each suffix
  year_map <- list("B" = "2001",
    "C" = "2003", "D" = "2005", "E" = "2007",
    "F" = "2009", "G" = "2011", "H" = "2013",
    "I" = "2015", "J" = "2017"
  )
  year <- ifelse(cycle_suffix == "", "1999", year_map[[cycle_suffix]])
  separator <- ifelse(cycle_suffix == "", "", "_")
  # CDC NHANES Data url for the current dataset
  url <- paste0("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/", 
                year, "/DataFiles/", file_name, separator, cycle_suffix, ".XPT")
  # creates a temporary file path and download file to the temporary location
  tf <- tempfile()
  download.file(url, tf, mode = "wb", quiet = TRUE)
  # read SAS transport file
  data <- foreign::read.xport(tf)
  # delete temporary file
  unlink(tf)
  return(data)
}
```

``` r
# Download all datasets (1999-2018, 10 2 year cycles)
cycles <- c("", "B", "C", "D", "E", "F", "G", "H", "I", "J")

# Download demographic files
demo_list <- lapply(cycles, function(x) {
  download_nhanes("DEMO", x)
})

# Download blood pressure questionnaire files
bpq_list <- lapply(cycles, function(x) {
  download_nhanes("BPQ", x)
})

# Download blood pressure examination files
bpx_list <- lapply(cycles, function(x) {
  download_nhanes("BPX", x)
})
```

### Data Formatting

#### Combine Data

This combines data across the different survey years/cycles and combines
the data from the different survey files.

``` r
# Combine datasets across time periods

# Combine demographic data
demo <- bind_rows(demo_list)

# Combine BPQ data (keep only key variables)
bpq <- bind_rows(bpq_list) %>%
  select(SEQN, BPQ020, BPQ050A, matches("BPQ100D"))

# Combine BPX data (keep only key variables)
bpx <- bind_rows(bpx_list) |> 
  select(SEQN, matches("BPXSY[1-4]"), matches("BPXDI[1-4]"))

# Merge datasets 
hyper_9918 <- demo |> 
  left_join(bpq |> select(SEQN, BPQ020, BPQ050A, BPQ100D), join_by(SEQN)) |> 
  left_join(bpx |>  select(SEQN, starts_with("BPXSY"), starts_with("BPXDI")), join_by(SEQN))

print(head(hyper_9918))
```

      SEQN SDDSRVYR RIDSTATR RIDEXMON RIAGENDR RIDAGEYR RIDAGEMN RIDAGEEX RIDRETH1
    1    1        1        2        2        2        2       29       31        4
    2    2        1        2        2        1       77      926      926        3
    3    3        1        2        1        2       10      125      126        3
    4    4        1        2        2        1        1       22       23        4
    5    5        1        2        2        1       49      597      597        3
    6    6        1        2        2        2       19      230      230        5
      RIDRETH2 DMQMILIT DMDBORN DMDCITZN DMDYRSUS DMDEDUC3 DMDEDUC2 DMDEDUC
    1        2       NA       1        1       NA       NA       NA      NA
    2        1        1       1        1       NA       NA        5       3
    3        1       NA       3        2        2        3       NA       1
    4        2       NA       1        1       NA       NA       NA      NA
    5        1        1       1        1       NA       NA        5       3
    6        4        2       1        1       NA       15       NA       3
      DMDSCHOL DMDMARTL DMDHHSIZ INDHHINC INDFMINC INDFMPIR RIDEXPRG RIDPREG
    1       NA       NA        3        3        3     0.86       NA      NA
    2       NA       NA        1        8        8     5.00       NA      NA
    3        1       NA        4        6        6     1.47       NA      NA
    4       NA       NA        7        3        3     0.57       NA      NA
    5       NA        1        3       11       11     5.00       NA      NA
    6        1        5        2        4        3     1.21        2       2
      DMDHRGND DMDHRAGE DMDHRBRN DMDHREDU DMDHRMAR DMDHSEDU  WTINT2YR  WTINT4YR
    1        2       27        1        3        5       NA  9727.079  4291.490
    2        1       77        1        5       NA       NA 26678.636 14203.336
    3        1       37        3        4        1        3 43621.681 20123.764
    4        2       34        1        3        4       NA 10346.119  4582.132
    5        2       42        3        4        1       NA 91050.847 44161.868
    6        2       19        1        4        5       NA 36508.250 16850.978
      WTMEC2YR  WTMEC4YR SDMVPSU SDMVSTRA SDJ1REPN DMAETHN DMARACE  WTMREP01
    1 10982.90  4456.207       1        5       19      NA      NA  11045.79
    2 28325.38 15336.200       3        1       36      NA      NA  28595.04
    3 46192.26 21258.467       2        7       30      NA      NA  46829.65
    4 10251.26  4562.389       1        2       47      NA      NA  10301.75
    5 99445.07 45985.968       2        8       41      NA      NA 100344.69
    6 39656.60 18337.313       2        2        7      NA      NA  40567.34
       WTMREP02 WTMREP03  WTMREP04  WTMREP05  WTMREP06  WTMREP07  WTMREP08
    1  11537.66 11052.38  10981.99  11304.03  11788.40  10982.14  11882.03
    2  28487.81 28277.21  28428.86  28689.51  28554.44  28953.83  28865.19
    3  46309.92 46191.69  46603.59  46638.38  47073.30  47186.99  46720.91
    4  10693.77 10316.46  10250.42  10765.24  10631.59  10250.56  10846.62
    5 100213.18 99530.74 100036.56 100789.82 101061.23 101883.03 101117.97
    6  39838.51 39656.11  39900.75  40055.39  41273.02      0.00  40140.73
       WTMREP09  WTMREP10  WTMREP11  WTMREP12  WTMREP13  WTMREP14  WTMREP15
    1  11082.38  10910.76  11152.69  11368.38  11639.07  10982.09  10968.69
    2  28816.15  29009.33  29511.00  28799.45  28520.26  28824.62  29838.48
    3  47371.67  46703.11  48675.07  47567.23  47042.79  47095.64  49158.40
    4  10389.73  10419.68  10410.64  10470.14  10780.56  10250.50  10238.00
    5 100869.04 101768.14 102611.94 101829.65 100098.76 102463.97 105667.42
    6  40415.06  40206.94  40768.11  39675.23  40563.99  41015.24  41974.75
       WTMREP16  WTMREP17  WTMREP18  WTMREP19 WTMREP20  WTMREP21 WTMREP22  WTMREP23
    1  11014.94  11786.12  11158.42      0.00 10982.85  11079.76 10981.33  10997.24
    2  29014.73  28886.15  28570.07  29175.48 28406.08  28540.73 28382.11  28669.31
    3  48733.46  46662.02  47012.40  47088.48 46842.11  46711.96 46191.73  47000.06
    4  10266.45  10776.92  10385.19  10373.02 10251.22  10444.98 10249.80  10264.65
    5 103689.54 101531.85 100611.63 101703.62 99669.79 101050.56 99652.45 100733.78
    6  41730.47  40415.90  40666.33  39790.59 40145.74  40218.94 39656.15  40387.49
       WTMREP24 WTMREP25  WTMREP26  WTMREP27  WTMREP28  WTMREP29  WTMREP30
    1  10981.44 10979.47  11608.25  10988.00  11347.58  11354.10  11168.79
    2  29011.20 28374.50  28662.46  29861.91  28771.56  28798.76  28877.00
    3  46411.98 46191.18  46430.75  48456.32  47537.52  46390.53      0.00
    4  10249.90 10248.06  10803.70  10241.34  10440.22  10574.67  10402.87
    5 100808.84 99407.85 100746.36 103704.54 102490.78 100457.20 101960.99
    6  39920.55 39655.67  39850.42  41785.93  41173.06  38299.99  40481.73
       WTMREP31  WTMREP32  WTMREP33 WTMREP34  WTMREP35  WTMREP36 WTMREP37  WTMREP38
    1  10988.95  11108.70  11057.81 11120.88  11885.46  11007.35 10819.87  11179.92
    2  28493.22  29903.41  29153.22 28503.96  28443.98      0.00 28467.04  29037.40
    3  46265.55  48571.40  46904.27 46411.34  47430.20  46740.89 46191.71  46798.28
    4  10201.96  10343.81  10524.96 10345.46  11236.71  10274.08 10288.78  10454.65
    5 100093.12 104829.44 100117.85 99981.65 102828.39 101294.75 99809.11 101633.39
    6  39722.40  42857.65  40429.35 39816.49  40514.19  40098.94 39656.13  40121.05
       WTMREP39  WTMREP40 WTMREP41  WTMREP42  WTMREP43 WTMREP44  WTMREP45  WTMREP46
    1  11031.81  11002.86 11475.12  10937.05  11431.55 11197.63  10976.25  11372.17
    2  29042.38  28435.90 29110.36  29373.66  28924.96 29016.39  29874.14  29559.80
    3  46940.62  46353.77 47676.54  47629.58  46579.64 46707.76  49637.45  47746.23
    4  10263.96  10276.95 10670.99  10279.08  10601.26 10425.58  10245.05  10542.92
    5 101718.69 100105.21     0.00 103833.94 102254.00 99399.91 104410.65 103032.84
    6  40274.44  39792.59 41084.44  40088.72  40223.15 40081.73  42429.40  41344.98
       WTMREP47 WTMREP48  WTMREP49  WTMREP50  WTMREP51  WTMREP52  WTIREP01 WTIREP02
    1  11323.56 11145.18  10990.80  11728.07  10927.62  11655.08  9787.451 10190.66
    2  29874.49 28364.51  28525.59  28965.39  29143.51  28705.13 26923.621 26824.72
    3  46727.78 46686.21  47504.12  47206.14  48250.80  46363.58 44489.187 43736.67
    4      0.00 10431.04  10530.15  10753.41  10479.95  10910.42 10401.970 10798.97
    5 101665.13 99740.42 100050.07 101733.35 103321.77 100585.33 92772.795 91672.82
    6  40993.48 40051.11  40798.48  40328.92  41468.07  39965.25 37416.264 36679.63
       WTIREP03  WTIREP04 WTIREP05 WTIREP06  WTIREP07 WTIREP08  WTIREP09  WTIREP10
    1  9789.596  9727.079 10129.04 10284.42  9727.079 10436.05  9816.986  9664.797
    2 26631.356 26777.075 27018.55 26904.61 27241.302 27198.79 27099.418 27293.981
    3 43621.681 43964.278 43956.60 44396.20 44432.410 44133.06 44684.839 44304.079
    4 10412.616 10346.119 10868.58 10734.33 10346.119 10932.39 10487.390 10517.698
    5 91122.756 91506.263 92345.59 92565.81 93036.324 92649.68 92223.816 93249.667
    6 36508.250 36722.000 36793.38 37486.46     0.000 36936.24 37217.266 37003.032
       WTIREP11 WTIREP12 WTIREP13  WTIREP14  WTIREP15  WTIREP16 WTIREP17  WTIREP18
    1  9845.871 10011.32 10234.82  9727.079  9727.079  9757.028 10257.41  9884.906
    2 27701.659 27220.56 26875.74 27203.215 28289.914 27504.821 27171.69 26894.202
    3 45711.849 44886.93 44507.45 45325.284 46199.091 45798.642 44086.71 44397.650
    4 10473.304 10568.96 10865.80 10346.119 10346.119 10363.133 10829.24 10483.498
    5 93778.678 93052.79 91695.62 93379.450 96241.873 94316.889 92900.83 91968.726
    6 37601.673 36558.81 37407.18 37984.425 38528.715 38316.319 37063.49 37349.932
      WTIREP19  WTIREP20  WTIREP21  WTIREP22  WTIREP23  WTIREP24  WTIREP25 WTIREP26
    1     0.00  9727.079  9789.576  9727.079  9727.079  9727.079  9727.079 10244.12
    2 27424.68 26730.099 26885.349 26732.989 26939.627 27208.479 26716.514 26979.28
    3 44698.52 44174.268 44171.404 43621.681 44326.007 43816.319 43621.681 43880.91
    4 10470.42 10346.119 10526.899 10346.119 10346.119 10346.119 10346.119 10890.47
    5 92883.44 91175.971 92439.001 91243.872 92173.287 92329.665 91020.040 92251.68
    6 36632.10 36923.676 37074.101 36508.250 37170.291 36643.133 36508.250 36725.21
       WTIREP27  WTIREP28 WTIREP29  WTIREP30  WTIREP31  WTIREP32 WTIREP33  WTIREP34
    1  9757.028  9953.666 10026.45  9897.641  9731.354  9836.711 10104.42  9835.452
    2 27961.236 27519.241 27120.35 27208.404 26827.816 28153.657 27401.25 26823.593
    3 45853.268 44814.552 43885.34     0.000 43691.069 45952.825 44150.49 43800.456
    4 10363.133 10540.663 10677.14 10518.053 10294.999 10437.526 10607.71 10438.987
    5 94752.727 93798.390 91963.52 93141.879 91594.731 95832.366 93541.93 91488.015
    6 38561.054 37833.482 37058.60 37229.126 36566.324 38651.845 37052.49 36642.618
      WTIREP35  WTIREP36 WTIREP37  WTIREP38  WTIREP39  WTIREP40 WTIREP41 WTIREP42
    1 10438.85  9749.358  9796.89  9903.459  9761.917  9745.269 10183.04  9694.54
    2 27228.74     0.000 26814.71 27224.137 27343.860 26786.595 27404.26 27595.65
    3 44709.56 44389.377 43621.68 44068.616 44332.639 43767.009 44886.36 45015.72
    4 11355.69 10369.817 10385.57 10552.965 10365.898 10372.592 10778.67 10382.68
    5 93511.22 93485.478 91393.20 92984.640 93161.374 91575.035     0.00 94484.11
    6 37215.36 36946.104 36508.25 36678.060 37011.846 36629.880 37756.21 37894.39
      WTIREP43  WTIREP44  WTIREP45 WTIREP46  WTIREP47  WTIREP48  WTIREP49 WTIREP50
    1 10094.02  9912.462  9727.079 10041.52  9953.956  9857.382  9865.152 10327.99
    2 27186.73 27324.345 28099.664 27757.07 28049.286 26716.602 26877.705 27268.03
    3 43993.19 44075.386 46642.564 44967.68 44572.482 44087.946 44831.371 44480.99
    4 10702.31 10531.444 10346.119 10636.06     0.000 10533.109 10654.750 10851.02
    5 93164.78 92119.609 95388.490 94131.38 95297.810 91325.082 91640.586 92817.93
    6 36948.53 36855.282 38998.473 38022.01 37541.486 36871.279 37520.675 37151.87
       WTIREP51 WTIREP52 SIALANG SIAPROXY SIAINTRP FIALANG FIAPROXY FIAINTRP
    1  9809.165 10323.32      NA       NA       NA      NA       NA       NA
    2 27406.384 26984.81      NA       NA       NA      NA       NA       NA
    3 45389.113 43781.91      NA       NA       NA      NA       NA       NA
    4 10564.981 11012.53      NA       NA       NA      NA       NA       NA
    5 94282.855 91993.25      NA       NA       NA      NA       NA       NA
    6 38016.505 36710.24      NA       NA       NA      NA       NA       NA
      MIALANG MIAPROXY MIAINTRP AIALANG DMDFMSIZ DMDBORN2 INDHHIN2 INDFMIN2
    1      NA       NA       NA      NA       NA       NA       NA       NA
    2      NA       NA       NA      NA       NA       NA       NA       NA
    3      NA       NA       NA      NA       NA       NA       NA       NA
    4      NA       NA       NA      NA       NA       NA       NA       NA
    5      NA       NA       NA      NA       NA       NA       NA       NA
    6      NA       NA       NA      NA       NA       NA       NA       NA
      DMDHRBR2 RIDRETH3 RIDEXAGY RIDEXAGM DMQMILIZ DMQADFC DMDBORN4 AIALANGA
    1       NA       NA       NA       NA       NA      NA       NA       NA
    2       NA       NA       NA       NA       NA      NA       NA       NA
    3       NA       NA       NA       NA       NA      NA       NA       NA
    4       NA       NA       NA       NA       NA      NA       NA       NA
    5       NA       NA       NA       NA       NA      NA       NA       NA
    6       NA       NA       NA       NA       NA      NA       NA       NA
      DMDHHSZA DMDHHSZB DMDHHSZE DMDHRBR4 DMDHRAGZ DMDHREDZ DMDHRMAZ DMDHSEDZ
    1       NA       NA       NA       NA       NA       NA       NA       NA
    2       NA       NA       NA       NA       NA       NA       NA       NA
    3       NA       NA       NA       NA       NA       NA       NA       NA
    4       NA       NA       NA       NA       NA       NA       NA       NA
    5       NA       NA       NA       NA       NA       NA       NA       NA
    6       NA       NA       NA       NA       NA       NA       NA       NA
      BPQ020 BPQ050A BPQ100D BPXSY1 BPXSY2 BPXSY3 BPXSY4 BPXDI1 BPXDI2 BPXDI3
    1     NA      NA      NA     NA     NA     NA     NA     NA     NA     NA
    2      2      NA      NA    106     98     98     NA     58     56     56
    3     NA      NA      NA    110    104    112     NA     60     64     62
    4     NA      NA      NA     NA     NA     NA     NA     NA     NA     NA
    5      1       1      NA    122    122    122     NA     82     84     82
    6      2      NA      NA    116    116    112     NA     64     60     80
      BPXDI4
    1     NA
    2     NA
    3     NA
    4     NA
    5     NA
    6     NA

#### Create Analysis Variables

``` r
# create analysis variables
hyper_9918 <- hyper_9918 |>  
  # create age category variable 1
  mutate(
    age = case_when(
      RIDAGEYR >= 20 & RIDAGEYR < 40 ~ 1,
      RIDAGEYR >= 40 & RIDAGEYR < 60 ~ 2,
      RIDAGEYR >= 60 ~ 3),
    # Create age category variable
    agecat = case_when(
      RIDAGEYR >= 18 & RIDAGEYR < 40 ~ 1,
      RIDAGEYR >= 40 & RIDAGEYR < 60 ~ 2,
      RIDAGEYR >= 60 ~ 3),
    # Create labeled age category
    agecat_label = factor(agecat,
                          levels = 1:3,
                          labels = c("18-39", "40-59", "60+")),
    
    # Race and Ethnicity combined variable (RIDRETH3 variables starts with 2011-2012 cycle)
    race_et4 = case_when(
      RIDRETH3 == 3 ~ 1,  # NH white
      RIDRETH3 == 4 ~ 2,  # NH black
      RIDRETH3 == 6 ~ 3,  # NH Asian
      RIDRETH3 %in% c(1, 2) ~ 4,  # Hispanic
      RIDRETH3 == 7 ~ 5,  # Other
      TRUE ~ NA_real_), # NA
    # labeled race/ethnicity variable
    race_et4_label = factor(race_et4,
                            levels = 1:5,
                            labels = c("NH white", "NH black", "NH Asian", 
                                       "Hispanic", "Other")),
    # Income (poverty-income ratio)
    FPL = case_when(
      INDFMPIR > 0.00 & INDFMPIR <= 1.30 ~ 1,
      INDFMPIR > 1.30 & INDFMPIR <= 3.50 ~ 2,
      INDFMPIR > 3.50 ~ 3,
      TRUE ~ NA_real_),
    # Labeled income variable
    FPL_label = factor(FPL,
                       levels = 1:3,
                       labels = c("<=130", "130-350", ">350")),
    # Education (handling both adult and youth education variables)
    EDUC = case_when(
      # For ages 18-19, use DMDEDUC3
      RIDAGEYR %in% c(18, 19) & 
        ((DMDEDUC3 >= 0 & DMDEDUC3 < 15) | DMDEDUC3 %in% c(55, 66)) ~ 1, # HS diploma or less
      RIDAGEYR %in% c(18, 19) & DMDEDUC3 == 15 ~ 2, # Some college
      # For ages 20+, use DMDEDUC2
      DMDEDUC2 %in% c(1, 2, 3) ~ 1,  # HS diploma or less
      DMDEDUC2 == 4 ~ 2,              # Some college
      DMDEDUC2 == 5 ~ 3,              # College graduate
      TRUE ~ NA_real_),
    # labeled education variable
    EDUC_label = factor(EDUC,
                        levels = 1:3,
                        labels = c("HS grad or less", "Some College", 
                                   ">=College graduate")),
    # Sex
    sex_label = factor(RIAGENDR,
                       levels = 1:2,
                       labels = c("Men", "Women")),
    # Survey cycle - time span
    sddsrvyr_label = factor(SDDSRVYR,
                            levels = 1:10,
                            labels = c("1999-2000", "2001-2002", "2003-2004",
                                       "2005-2006", "2007-2008", "2009-2010",
                                       "2011-2012", "2013-2014", "2015-2016",
                                       "2017-2018")),
    # Count number of non-missing SBP and DBP readings
    n_sbp = rowSums(!is.na(pick(BPXSY1:BPXSY4))),
    n_dbp = rowSums(!is.na(pick(BPXDI1:BPXDI4))))

# calculate additional variables
hyper_9918 <- hyper_9918 |>
  # Set DBP values of 0 to missing for calculating the average
  #mutate(across(matches("BPXDI[1-4]"), ~ifelse(. == 0, NA, .))) %>%
  mutate(
    # Calculate mean SBP and DBP
    mean_sbp = rowMeans(pick(BPXSY1:BPXSY4), na.rm = TRUE),
    mean_dbp = rowMeans(pick(BPXDI1:BPXDI4), na.rm = TRUE),
    # Hypertension using NEW definition (130/80)
    Hyper_new = case_when(
      (mean_sbp >= 130 | mean_dbp >= 80 | BPQ050A == 1) ~ 1,
      (n_sbp > 0 & n_dbp > 0) ~ 0,
      TRUE ~ NA_real_),
    # Controlled hypertension (NEW definition)
    Controlled = case_when(
      Hyper_new == 1 & (mean_sbp >= 130 | mean_dbp >= 80) ~ 0,
      Hyper_new == 1 & n_sbp > 0 & n_dbp > 0 ~ 1,
      TRUE ~ NA_real_),
    # Hypertension using OLD definition (140/90)
    Hyper_old = case_when(
      (mean_sbp >= 140 | mean_dbp >= 90 | BPQ050A == 1) ~ 1,
      (n_sbp > 0 & n_dbp > 0) ~ 0,
      TRUE ~ NA_real_),
    # Controlled hypertension (OLD definition)
    Controlold = case_when(
      Hyper_old == 1 & (mean_sbp >= 140 | mean_dbp >= 90) ~ 0,
      Hyper_old == 1 & n_sbp > 0 & n_dbp > 0 ~ 1,
      TRUE ~ NA_real_),
    # Awareness of hypertension
    aware = case_when(
      BPQ020 == 1 ~ 1,
      BPQ020 == 2 ~ 0,
      TRUE ~ NA_real_),
    # convert NAs to 0 - important for later filtering step
    RIDEXPRG_cleaned = ifelse(is.na(RIDEXPRG), 0, RIDEXPRG),
    # Subpopulation indicator
    sel1 = ifelse(RIDAGEYR >= 18 & RIDEXPRG_cleaned != 1 & (n_sbp != 0 | n_dbp != 0), 1, 0),
    # Overall indicator
    one = 1
  )
```

#### Subset Data

``` r
# Subset to 2017-2018 data for Figures 1-3
hyper_1718 <- hyper_9918 |> 
  filter(SDDSRVYR == 10)

cat("All Data for 2017-2018:", nrow(hyper_1718), "\n")
```

    All Data for 2017-2018: 9254 

``` r
# subpopulation = 18+, men and non-pregnant women, and individuals who have at least one BP reading (either have a systolic or diastolic blood pressure reading)
cat("Analysis population:", sum(hyper_1718$sel1 == 1, na.rm = TRUE), "\n\n")
```

    Analysis population: 5199 

### Define Survey Design

``` r
# Create survey design for all years (1999-2018)
# strata are defined based on geographic region, metropolitan status (urban/rural), demographic characteristics
# PSUs nested within strata accounts for the design effects
nhanes_9918_design <- svydesign(
  id = ~SDMVPSU, # Primary Sampling Unit (PSU) identifier - geographic areas
  strata = ~SDMVSTRA, # stratafication variable - used to ensure geographic and demoraphic representation
  weights = ~WTMEC2YR, # survey sample weights, accounts for the unequal probability of sampling and non-response - this weight is used for analyzing a single 2 year cycle
  nest = TRUE, # indicates that PSUs are nested w/in strata, not across the netire dataset
  data = hyper_9918
)

# Subset to analysis population
nhanes_9918_analysis <- subset(
  nhanes_9918_design,
  sel1 == 1
)

# Create survey design for 2017-2018
nhanes_1718_design <- svydesign(
  id = ~SDMVPSU, # Primary Sampling Unit (PSU) identifier
  strata = ~SDMVSTRA, # stratafication variable 
  weights = ~WTMEC2YR, # survey sample weights
  nest = TRUE, # indicates that PSUs are nested w/in strata, not across the netire dataset
  data = hyper_1718
)

# Subset to analysis population 
nhanes_1718_analysis <- subset(
  nhanes_1718_design, 
  sel1 == 1,
)
```

#### Calculating Age-Adjusted Prevalence

We can use ‘svystandardize’ to perform direct age standardization by
adjusting the survey weights so that the age distribution of your sample
matches a specified standard/reference population (such as the 2000
Census).

``` r
# Define the standard population
# Age-adjustment weights from year 2000 Census population
# Age groups: 18-39, 40-59, 60+
standard_pop <- data.frame(
  agecat = 1:3,
  Freq = c(0.4203, 0.3572, 0.2225)
)

# Standardize the survey design for the 2017-2018 data - adjusting to the 2000 population
design_std <- svystandardize(
  design = nhanes_1718_analysis,
  by = ~agecat,
  over = ~sex_label,  # Calculate separately for each sex (use ~1 for the whole population)
  population = standard_pop,
  excluding.missing = ~agecat 
)
```

### Figure 1: Hypertension prevalence by sex and age

``` r
# Age-specific prevalence overall
fig1_age_specific_overall <- svyby(
  formula = ~Hyper_new,
  by = ~agecat_label,
  design = nhanes_1718_analysis,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")) |> 
  mutate(sex_label = 'All',
    prevalence = Hyper_new * 100,
    se = se * 100,
    ci_l = ci_l * 100,
    ci_u = ci_u * 100) |> 
  select(sex_label, agecat_label, prevalence, se, ci_l, ci_u)

# Age-specific prevalence by sex
age_specific <- svyby(
  formula = ~Hyper_new,
  by = ~sex_label + agecat_label,
  design = nhanes_1718_analysis,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")) 

fig1_age_specific <- age_specific |> 
  mutate(
    prevalence = Hyper_new * 100,
    se = se * 100,
    ci_l = ci_l * 100,
    ci_u = ci_u * 100) |> 
  select(sex_label, agecat_label, prevalence, se, ci_l, ci_u)

# Age-adjusted prevalence by sex for 18+
age_adjusted_prev <- svyby(
  formula = ~Hyper_new,
  by = ~sex_label,
  design = design_std, # use survey design defined with the weights
  FUN = svymean,
  # method = 'beta', can also specify different approaches here, default is logit
  na.rm = TRUE,
  vartype = c("se", "ci"))

fig1_age_adjusted <- age_adjusted_prev |> 
  mutate(prevalence = Hyper_new * 100,
         se = se * 100,
         ci_l = ci_l * 100,
         ci_u = ci_u * 100,
    agecat_label = "18 and over (age-adjusted)") |> 
  select(sex_label, agecat_label, prevalence, se, ci_l, ci_u)

# Combine age-specific and age-adjusted
fig1_results <- bind_rows(fig1_age_specific_overall, fig1_age_specific, fig1_age_adjusted) |> 
  arrange(sex_label, agecat_label) |> 
  mutate(agecat_label = factor(agecat_label, levels =  c('18 and over (age-adjusted)', '18-39', '40-59', '60+')))

# Plot Figure 1
ggplot(fig1_results, aes(x = sex_label, y = prevalence, 
                              fill = agecat_label, group = agecat_label)) +
  # column plot w/ side by side bars for each category
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # add error bars
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                position = position_dodge(width = 0.8),
                width = 0.2, linewidth = 0.8, alpha = .5, color = 'gray30') +
  # add text label above the bar
  geom_text(aes(label = round(prevalence, 1)),
            position = position_dodge(width = 0.8),
            vjust = -1, size = 3, alpha = .8) +
  # adjust axis labels, title, and caption
  labs(title = "Figure 1. Prevalence of Hypertension by Sex and Age",
    subtitle = "United States, 2017–2018",
    fill = "Age Group",
    y = "Prevalence (%)",
    x = "Sex",
    caption = "Error bars represent 95% confidence intervals") +
  # set theme to minimal
  theme_minimal() +
  # move legend to bottom and bold title
  theme(plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom")
```

![](NHANES_survey_analysis_files/figure-commonmark/unnamed-chunk-9-1.png)

``` r
print(fig1_results)
```

                sex_label               agecat_label prevalence       se     ci_l
    18-39             All                      18-39   22.37653 1.176136 20.07134
    40-59             All                      40-59   54.50647 2.460317 49.68434
    60+               All                        60+   74.47679 1.601812 71.33729
    Men               Men 18 and over (age-adjusted)   51.04410 1.783849 47.54782
    Men.18-39         Men                      18-39   31.17370 1.774532 27.69568
    Men.40-59         Men                      40-59   59.37466 3.427711 52.65646
    Men.60+           Men                        60+   75.20524 3.010938 69.30391
    Women           Women 18 and over (age-adjusted)   39.70996 1.456068 36.85612
    Women.18-39     Women                      18-39   13.01185 1.424718 10.21945
    Women.40-59     Women                      40-59   49.85595 2.389642 45.17234
    Women.60+       Women                        60+   73.85408 2.038212 69.85926
                    ci_u
    18-39       24.68171
    40-59       59.32861
    60+         77.61628
    Men         54.54038
    Men.18-39   34.65172
    Men.40-59   66.09285
    Men.60+     81.10657
    Women       42.56380
    Women.18-39 15.80424
    Women.40-59 54.53956
    Women.60+   77.84891

#### Comparison of Prevalence Between Men and Women

``` r
# Test for difference between men and women (age-adjusted, 2017-2018)
# Computes lienar or nonlinear contrasts of survey statistics - used for proportions/prevalence
overall_contrast_result <- svycontrast(age_adjusted_prev,
  contrasts = list("Men - Women" = c(1, -1)))
```

    Warning in vcov.svyby(stat): Only diagonal elements of vcov() available

``` r
print(overall_contrast_result)
```

                contrast    SE
    Men - Women  0.11334 0.023

``` r
print(confint(overall_contrast_result))
```

                     2.5 %    97.5 %
    Men - Women 0.06821007 0.1584728

``` r
# Can also compute for just a specific age category
contrast_18_39 <- svycontrast(
  age_specific,
  contrasts = list(
    "18-39: Men - Women" = c(1, -1, 0, 0, 0, 0)))
```

    Warning in vcov.svyby(stat): Only diagonal elements of vcov() available

``` r
print(contrast_18_39)
```

                       contrast     SE
    18-39: Men - Women  0.18162 0.0228

### Figure 2: Age-Adjusted Prevalence by Sex and Race/Ethnicity

``` r
# Calculate age-adjusted prevalence by sex and race
design_std_fig2 <- svystandardize(
  design = nhanes_1718_analysis,
  by = ~agecat,                              # Standardize by age
  over = ~sex_label + race_et4_label,        # Keep sex and race/eth separate
  population = standard_pop,
  excluding.missing = ~agecat)

# Calculate age-adjusted prevalence by race
fig2_overall_results <- svyby(
  formula = ~Hyper_new,
  by = ~race_et4_label,
  design = design_std_fig2,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")) |> 
  mutate(sex_label = 'All',
         prevalence = Hyper_new * 100,
         se = se * 100,
         ci_l = ci_l * 100,
         ci_u = ci_u * 100) |> 
  # Plotting NH white, NH black, and Hispanic (as in the data brief)
  filter(race_et4_label %in% c("NH white", "NH black", "Hispanic")) |> 
  select(sex_label, race_et4_label, prevalence, se, ci_l, ci_u)


# Calculate age-adjusted prevalence by sex and race
fig2_age_adj_results <- svyby(
  formula = ~Hyper_new,
  by = ~sex_label+race_et4_label,
  design = design_std_fig2,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")) |> 
  mutate(prevalence = Hyper_new * 100,
         se = se * 100,
         ci_l = ci_l * 100,
         ci_u = ci_u * 100) |> 
  # Plotting NH white, NH black, and Hispanic (as in the data brief)
  filter(race_et4_label %in% c("NH white", "NH black", "Hispanic")) |> 
  select(sex_label, race_et4_label, prevalence, se, ci_l, ci_u)

# combine results for prev by sex and race, and by just race
fig2_results <- rbind(fig2_overall_results, fig2_age_adj_results)

# Plot Figure 2
ggplot(fig2_results, aes(x = sex_label, y = prevalence, 
                         fill = race_et4_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                position = position_dodge(width = 0.8),
                width = 0.2, linewidth = 0.8, alpha = .5, color = 'grey30') +
  geom_text(aes(label = round(prevalence, 1)),
            position = position_dodge(width = 0.8),
            vjust = -1, size = 3, alpha = .8) +
  labs(title = "Figure 2. Age-Adjusted Prevalence of Hypertension by Sex and Race/Ethnicity",
    subtitle = "United States, 2017–2018",
    fill = "Race and Ethnicity",
    y = "Age-Adjusted Prevalence (%)",
    x = "Sex",
    caption = "Error bars represent 95% confidence intervals\nAge adjusted to 2000 U.S. Census population (18-39, 40-59, 60+)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    legend.position = "bottom")
```

![](NHANES_survey_analysis_files/figure-commonmark/unnamed-chunk-11-1.png)

``` r
print(fig2_results)
```

                   sex_label race_et4_label prevalence       se     ci_l     ci_u
    NH white             All       NH white   43.33570 2.055352 39.30728 47.36412
    NH black             All       NH black   56.92788 1.532475 53.92428 59.93147
    Hispanic             All       Hispanic   43.53557 1.138804 41.30355 45.76758
    Men.NH white         Men       NH white   50.23058 2.697725 44.94313 55.51802
    Women.NH white     Women       NH white   36.68168 1.928589 32.90172 40.46165
    Men.NH black         Men       NH black   57.24921 2.016399 53.29714 61.20128
    Women.NH black     Women       NH black   56.65825 2.021872 52.69546 60.62105
    Men.Hispanic         Men       Hispanic   50.14196 2.184916 45.85960 54.42431
    Women.Hispanic     Women       Hispanic   36.81146 1.955394 32.97896 40.64396

### Figure 3: Age-Adjusted Prevalence by Sex and Education

``` r
# Calculate age-adjusted prevalence by sex and education
design_std_fig3 <- svystandardize(
  design = nhanes_1718_analysis,
  by = ~agecat,                              # Standardize by age
  over = ~sex_label + EDUC_label,        # Keep sex and education separate
  population = standard_pop,
  excluding.missing = ~agecat+EDUC_label) # ignore data with missing age or education

# Calculate age-adjusted prevalence
fig3_age_adj_results <- svyby(
  formula = ~Hyper_new,
  by = ~sex_label+EDUC_label,
  design = design_std_fig3,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")) |> 
  mutate(prevalence = Hyper_new * 100,
         se = se * 100,
         ci_l = ci_l * 100,
         ci_u = ci_u * 100)

# Plot Figure 3
ggplot(fig3_age_adj_results, aes(x = sex_label, y = prevalence, 
                         fill = EDUC_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_errorbar(aes(ymin = ci_l, ymax = ci_u),
                position = position_dodge(width = 0.8),
                width = 0.2, linewidth = 0.8, alpha = .5, color ='grey30') +
  geom_text(aes(label = round(prevalence, 1)),
            position = position_dodge(width = 0.8),
            vjust = -1, size = 3, alpha = .8) +
  labs(title = "Figure 3. Age-Adjusted Prevalence of Hypertension by Sex and Education",
    subtitle = "United States, 2017–2018",
    fill = "Education Level",
    y = "Age-Adjusted Prevalence (%)",
    x = "Sex",
    caption = "Error bars represent 95% confidence intervals\nAge adjusted to 2000 U.S. Census population (18-39, 40-59, 60+)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
    legend.position = "bottom")
```

![](NHANES_survey_analysis_files/figure-commonmark/unnamed-chunk-12-1.png)

``` r
print(fig3_age_adj_results)
```

                             sex_label         EDUC_label Hyper_new       se
    Men.HS grad or less            Men    HS grad or less 0.5001886 2.273663
    Women.HS grad or less        Women    HS grad or less 0.4286689 1.608150
    Men.Some College               Men       Some College 0.5761104 2.563311
    Women.Some College           Women       Some College 0.4389264 1.900482
    Men.>=College graduate         Men >=College graduate 0.4667552 2.979046
    Women.>=College graduate     Women >=College graduate 0.3130602 2.781255
                                 ci_l     ci_u prevalence
    Men.HS grad or less      45.56256 54.47515   50.01886
    Women.HS grad or less    39.71497 46.01880   42.86689
    Men.Some College         52.58704 62.63503   57.61104
    Women.Some College       40.16776 47.61751   43.89264
    Men.>=College graduate   40.83670 52.51434   46.67552
    Women.>=College graduate 25.85486 36.75718   31.30602

### Figure 4: Trends in Age-Adjusted Prevalence (1999-2018)

``` r
# Calculate age-adjusted prevalence by sex and education
design_std_fig4 <- svystandardize(
  design = nhanes_9918_analysis,
  by = ~agecat,                              # Standardize by age
  over = ~sex_label + sddsrvyr_label,        # Keep sex and cycle separate
  population = standard_pop,
  excluding.missing = ~agecat+sddsrvyr_label) # ignore data w/ missing age or year 

# Calculate age-adjusted prevalence
time_age_adj_prev <- svyby(
  formula = ~Hyper_new,
  by = ~sex_label+sddsrvyr_label,
  design = design_std_fig4,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci"))

fig4_age_adj_results <- time_age_adj_prev |> 
  mutate(prevalence = Hyper_new * 100,
         se = se * 100,
         ci_l = ci_l * 100,
         ci_u = ci_u * 100)


# Plot Figure 4
ggplot(fig4_age_adj_results, aes(x = sddsrvyr_label, y = prevalence, group = sex_label)) +
  geom_point(size = 3, aes(color = sex_label)) +
  geom_line(linewidth = 1, aes(color = sex_label)) +
  # add confidence interval using a shaded ribbon
  geom_ribbon(aes(ymin = ci_l, ymax = ci_u, fill = sex_label),
              alpha = .3) +
  # add text label for each value
  geom_text(aes(label = round(prevalence, 1)),
            vjust = -1.5, size = 3, alpha = .6) +
  # adjust labels
  labs(title = "Figure 4. Trends in Age-Adjusted Hypertension Prevalence",
    subtitle = "Adults Aged 18 and Over by Sex: United States, 1999–2018",
    x = "Survey Year",
    y = "Age-Adjusted Prevalence (%)",
    color = "Sex",
    caption = "Error bars represent 95% confidence intervals\nAge adjusted to 2000 U.S. Census population (18-39, 40-59, 60+)") +
  # remove legend for fill
  guides(fill = 'none') +
  # set theme and adjust legend and text labels
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom")
```

![](NHANES_survey_analysis_files/figure-commonmark/unnamed-chunk-13-1.png)

``` r
print(fig4_age_adj_results)
```

                    sex_label sddsrvyr_label Hyper_new       se     ci_l     ci_u
    Men.1999-2000         Men      1999-2000 0.5173670 1.841408 48.12760 55.34579
    Women.1999-2000     Women      1999-2000 0.4196501 1.495900 39.03310 44.89692
    Men.2001-2002         Men      2001-2002 0.5018154 2.047185 46.16913 54.19395
    Women.2001-2002     Women      2001-2002 0.4147015 1.222198 39.07469 43.86561
    Men.2003-2004         Men      2003-2004 0.4912581 1.726948 45.74106 52.51057
    Women.2003-2004     Women      2003-2004 0.4034489 1.155708 38.07974 42.61004
    Men.2005-2006         Men      2005-2006 0.4811192 1.409676 45.34900 50.87483
    Women.2005-2006     Women      2005-2006 0.3904125 1.232088 36.62640 41.45610
    Men.2007-2008         Men      2007-2008 0.4681631 1.029237 44.79904 48.83358
    Women.2007-2008     Women      2007-2008 0.3945516 1.106462 37.28654 41.62379
    Men.2009-2010         Men      2009-2010 0.4552595 1.656375 42.27952 48.77239
    Women.2009-2010     Women      2009-2010 0.3836580 1.038140 36.33108 40.40051
    Men.2011-2012         Men      2011-2012 0.4771264 1.345811 45.07490 50.35038
    Women.2011-2012     Women      2011-2012 0.3985140 1.147908 37.60154 42.10126
    Men.2013-2014         Men      2013-2014 0.4515868 1.299741 42.61123 47.70612
    Women.2013-2014     Women      2013-2014 0.3819773 1.093498 36.05451 40.34094
    Men.2015-2016         Men      2015-2016 0.4722018 1.612663 44.05942 50.38094
    Women.2015-2016     Women      2015-2016 0.3969475 1.266513 37.21243 42.17707
    Men.2017-2018         Men      2017-2018 0.5104410 1.783849 47.54782 54.54038
    Women.2017-2018     Women      2017-2018 0.3970996 1.456068 36.85612 42.56380
                    prevalence
    Men.1999-2000     51.73670
    Women.1999-2000   41.96501
    Men.2001-2002     50.18154
    Women.2001-2002   41.47015
    Men.2003-2004     49.12581
    Women.2003-2004   40.34489
    Men.2005-2006     48.11192
    Women.2005-2006   39.04125
    Men.2007-2008     46.81631
    Women.2007-2008   39.45516
    Men.2009-2010     45.52595
    Women.2009-2010   38.36580
    Men.2011-2012     47.71264
    Women.2011-2012   39.85140
    Men.2013-2014     45.15868
    Women.2013-2014   38.19773
    Men.2015-2016     47.22018
    Women.2015-2016   39.69475
    Men.2017-2018     51.04410
    Women.2017-2018   39.70996
