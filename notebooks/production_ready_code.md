# Production-Ready Code
Allie Warren, allison.warren
2026-01-07

- [Logging](#logging)
- [Error Handling](#error-handling)
  - [File Reading](#file-reading)
  - [Data Type Validation](#data-type-validation)
- [Additional Logging](#additional-logging)
- [Data.Table](#datatable)
  - [Performance Profiling](#performance-profiling)
- [Parallelization](#parallelization)
- [Memory Management](#memory-management)
  - [gc()](#gc)
  - [Vector Pre-allocation](#vector-pre-allocation)
  - [Memory Efficiency with
    data.table](#memory-efficiency-with-datatable)

This notebook introduces concepts that are useful when developing
automated processes or when you are trying to improve the efficiency of
your code. It includes:

- Using logging to create a written record of your code execution

- Error handling

- Input validation

- Using Data.Table

- Performance profiling using microbenchmark() to evaluate the timing of
  a process

- How to parallelize processes

- Tips for memory managements

``` r
# Loading packages

# the pacman package is useful for managing package install/loading, When using pacman to load a package it will automatically install the package if it is not already installed
if(!require("pacman")) install.packages("pacman") #but first we have to install pacman
```

    Loading required package: pacman

``` r
# This load packages for working with data tables, reading in data, creating log files, benchmarking timing performance, running parallel processes, understanding R internal workings, working with data frames, checking data schemes, and working with dates, working with strings
pacman::p_load(data.table, readr, logr, microbenchmark, parallel, pryr, dplyr, schematic, lubridate, stringr)
```

## Logging

The logr package provides comprehensive logging for R code, including
automatic logging of data frames and function outputs. It is useful when
you are creating an automated pipeline as it gives you a written record
of your program execution and makes it easy to identify any errors that
may have occurred.

``` r
# Create log directory
log_dir <- file.path(here::here(), "data")
log_file <- file.path(log_dir, "patient_processing.log")

# Initialize log
lf <- log_open(log_file, logdir = FALSE)

# begin printing to the log
log_print("=== Log File Started ===")
```

    === Log File Started === 

``` r
log_print(paste("Log file start time:", now()))
```

    Log file start time: 2026-01-12 16:11:53.227297 

``` r
# when done, close the log
log_close()
```

## Error Handling

When developing an automated pipeline it can be important to include
error handling so that your pipeline identifies issues in the process
and can continue execution even if an error occurs.

### File Reading

``` r
# Reinitialize/reopen the existing log to continue writing to it
lf <- log_open(log_file, logdir = FALSE, autolog = TRUE)
```

    Warning: package 'tidylog' was built under R version 4.4.3

``` r
# Error handling example - reading in the file
read_patient_file <- function(file_path, expected_cols = NULL) {
  
  # Error condition for file not found
  if (!file.exists(file_path)) {
    # add the error to the log file
    log_print( sprintf("File not found: %s", file_path))
    # stop execution with the error
    stop(simpleError(
      message = sprintf("File not found: %s", file_path),
      call = sys.call()
    ))
  } else {
    data <- read_csv(file_path)
  }
  
  # Additional error testing for if the file was missing expected columns
  missing_cols <- setdiff(expected_cols, colnames(data) )
  if(!is.null(missing_cols)) {
    missing_col_error <- sprintf("Missing required columns: %s",
                                 paste(missing_cols, collapse = ", "))
    log_print(missing_col_error)
    stop(missing_col_error)
    }

  # print to log file if data read was successful
  log_print(sprintf("Successfully read file: %s (%d rows)", 
                   basename(file_path), nrow(data)))
  return(data)
}

# Filepath to synthetic data
patient_filepath <- file.path(here::here(), 'data', 'synthetic_patient_data.csv')

# Test error conditions
# if file did not exist:
tryCatch({
  read_patient_file("nonexistent_file.csv")
}, error = function(e) {
  cat("\nCaught expected error:", e$message, "\n")
})
```

    File not found: nonexistent_file.csv 

    Caught expected error: File not found: nonexistent_file.csv 

``` r
# if file was missing an expected column
tryCatch({
  read_patient_file(patient_filepath, expected_cols = c("nonexistent_column"))
}, error = function(e) {
  cat("Caught expected error:", e$message, "\n")
})
```

    Rows: 5050 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (11): first_name, last_name, address, city, state, zipcode, phone, emai...
    dbl   (1): patient_id
    date  (3): date_of_birth, case_date, test_date

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    Missing required columns: nonexistent_column 
    Caught expected error: Missing required columns: nonexistent_column 

``` r
# non-error case 
tryCatch({
  patient_data <- read_patient_file(patient_filepath)
}, error = function(e) {
  cat("Caught expected error:", e$message, "\n")
})
```

    Rows: 5050 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (11): first_name, last_name, address, city, state, zipcode, phone, emai...
    dbl   (1): patient_id
    date  (3): date_of_birth, case_date, test_date

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

    Successfully read file: synthetic_patient_data.csv (5050 rows) 

### Data Type Validation

The {schematic} package makes it easy to describe an expected schema
(columns, data types, data characteristics) so that we can validate that
our data is as expected, and throw an error if this is not the case.

``` r
# describe a schema for your data
# check data include expected columns, data types match expected types, and conditions
my_schema <- schematic::schema(
  patient_id ~ is.integer,
  patient_id ~ is_all_distinct,
  first_name ~ is.character,
  last_name ~ is.character,
  date_of_birth ~ is.Date,
  condition ~ is.character,
  test_result ~ is.character,
  condition ~ function(x) all(x %in% c("Diabetes", "", "Asthma", "Hypertension", "Heart Disease", "COPD")),
  case_date ~ function(x) all(x >= ymd("2023-01-01")),
  event_date ~ is.Date)

# check data against the schema
#schematic::check_schema(patient_data, my_schema)

# could also run w/in a tryCatch so that it doesn't stop execution
tryCatch({
  schematic::check_schema(patient_data, my_schema)

}, error = function(e) {
  cat("\n=== Error Occurred ===\n")
  print(e)
})
```


    === Error Occurred ===
    <error/rlang_error>
    Error:
    ! Schema Error:
    - Column `event_date` missing from data
    - Column `patient_id` failed check `is.integer`
    - Column `patient_id` failed check `is_all_distinct`
    - Column `condition` failed check `function(x) ...`
    ---
    Backtrace:
        ▆
     1. ├─base::tryCatch(...)
     2. │ └─base (local) tryCatchList(expr, classes, parentenv, handlers)
     3. │   └─base (local) tryCatchOne(expr, names, parentenv, handlers[[1L]])
     4. │     └─base (local) doTryCatch(return(expr), name, parentenv, handler)
     5. └─schematic::check_schema(patient_data, my_schema)

## Additional Logging

You can also use the log file to include information about your data and
processes

``` r
# Log data frame summary - logr automatically formats this
log_print(patient_data, console = FALSE)

# Example: Log within a processing pipeline
process_with_logging <- function(pdata) {
  log_print("Starting data validation")
  
  initial_rows <- nrow(pdata)
  log_print(paste("Initial row count:", initial_rows))
  
  # Remove complete duplicates
  pdata <- distinct(pdata)
  dup_removed <- initial_rows - nrow(pdata)
  log_print(paste("Removed", dup_removed, "duplicate records"))
  
  # Count missing values by column
  missing_counts <- colSums(is.na(pdata))
  log_print("Missing value counts by column:")
  log_print(missing_counts)
  
  log_print("Data validation completed")
  return(pdata)
}

cleaned_data <- process_with_logging(patient_data)
```

    Starting data validation 
    Initial row count: 5050 
    distinct: removed 50 rows (1%), 5,000 rows remaining
    Removed 50 duplicate records 
    Missing value counts by column: 
       patient_id    first_name     last_name date_of_birth       address 
                0           367           366             0             0 
             city         state       zipcode         phone         email 
             1661          1077           776             0             3 
        condition     case_date     test_date   test_result    case_notes 
             1430             0          4900          1444          1964 
    Data validation completed 

## Data.Table

The {data.table} package provides a high-performance version of base R’s
data.frame with syntax and feature enhancements for ease of use,
convenience, and programming speed. If you are working with larger data,
data.table can provide important speed/efficiency improvements over the
tidyverse. {data.table} syntax looks a bit different from tidyverse
conventions, if you want to continue to use syntax more similar to
tidyverse the {dtplyr} package provides a data.table backend to dplyr so
that you can write code using dplyr syntax but it is automatically
translated to the equivalent, but usually much faster, data.table code.

``` r
patient_data_DT <- copy(patient_data)
# you can convert a table to a data.table using setDT
setDT(patient_data_DT)

# using data.table syntax to add a columns 
patient_data_DT[,age := year(today()) - year(date_of_birth)]
patient_data_DT[,days_to_test := as.numeric(ymd(test_date) - case_date)]
```

### Performance Profiling

Many data.table functions are faster than their base R or tidyverse
equivalents. We can use the {microbenchmark} package to measure the
timing and compare performance. This can also be useful to run to
evaluate portions of a pipeline you are developing to identify processes
that are slow and may benefit from improvements.

#### Comparing file read functions

``` r
# comparing the performance of different read functions
# base R read.csv
microbenchmark::microbenchmark(read.csv(patient_filepath))
```

    Unit: milliseconds
                           expr     min       lq     mean   median       uq     max
     read.csv(patient_filepath) 10.9279 11.97145 13.06694 12.66015 13.95225 19.0958
     neval
       100

``` r
# tidyverse readr read_csv
microbenchmark::microbenchmark(readr::read_csv(patient_filepath, show_col_types = FALSE))
```

    Unit: milliseconds
                                                          expr     min     lq
     readr::read_csv(patient_filepath, show_col_types = FALSE) 17.5788 18.515
         mean  median       uq     max neval
     19.80068 19.2421 20.43045 27.3956   100

``` r
# data.table fread
microbenchmark::microbenchmark(data.table::fread(patient_filepath, verbose = FALSE, showProgress = FALSE))
```

    Unit: milliseconds
                                                                           expr
     data.table::fread(patient_filepath, verbose = FALSE, showProgress = FALSE)
        min      lq     mean  median      uq    max neval
     4.7746 5.13145 5.724572 5.38845 6.14045 8.5663   100

#### Comparing function performance

``` r
# Create a function with performance bottlenecks
inefficient_patient_analysis <- function(data) {
  
  results <- list()
  
  # Bottleneck 1: Growing vectors in loop
  all_ages <- c()
  for (i in 1:nrow(data)) {
    all_ages <- c(all_ages, data$age[i])  # Bad: reallocation each time
  }
  
  # Bottleneck 2: Repeated filtering without indexing
  for (condition in unique(data$condition)) {
    subset_data <- data[data$condition == condition, ]
    results[[condition]] <- mean(subset_data$age, na.rm = TRUE)
  }
  
  # Bottleneck 3: Unnecessary string operations in loop
  formatted_names <- character(nrow(data))
  for (i in 1:nrow(data)) {
    formatted_names[i] <- toupper(paste0(
      substr(data$first_name[i], 1, 1),
      tolower(substr(data$first_name[i], 2, nchar(data$first_name[i])))
    ))
  }
  
  return(results)
}

# Optimized version
efficient_patient_analysis <- function(data) {
  
  # Pre-allocated vector
  all_ages <- data$age
  
  # Vectorized operations with data.table
  results <- data[, .(mean_age = mean(age, na.rm = TRUE)), by = condition]
  
  # Vectorized string operations
  formatted_names <- str_to_title(data$first_name)
  
  return(results)
}

# Timing Evaluation
cat("\n=== Profiling Code ===\n")
```


    === Profiling Code ===

``` r
microbenchmark::microbenchmark(inefficient_patient_analysis(patient_data_DT))
```

    Unit: milliseconds
                                              expr     min       lq     mean
     inefficient_patient_analysis(patient_data_DT) 51.8549 57.02525 64.01921
       median      uq      max neval
     59.72535 62.6196 208.4091   100

``` r
microbenchmark::microbenchmark(efficient_patient_analysis(patient_data_DT))
```

    Unit: milliseconds
                                            expr    min     lq     mean median
     efficient_patient_analysis(patient_data_DT) 2.5117 2.8986 3.783659 3.1574
          uq     max neval
     3.39045 48.8243   100

## Parallelization

The {parallel} package provides tools to execute computations
simultaneously across multiple CPU cores, reducing execution time for
large repetitive tasks. This is especially useful in data-intensive
applications like simulations, bootstrapping, or large-scale data
analysis.

``` r
# Computationally expensive: Risk assessment with bootstrap CI
assess_patient_risk_bootstrap <- function(patient_subset, n_bootstrap = 1000) {
  
  # Calculate base risk score
  risk_scores <- patient_subset[, .(
    patient_id,
    base_risk = (age / 100) + 
                ifelse(condition %in% c("Heart Disease", "COPD"), 0.5, 0.1) +
                ifelse(days_to_test > 30, 0.3, 0)
  )]
  
  # Bootstrap confidence intervals
  boot_means <- replicate(n_bootstrap, {
    sample_idx <- sample(nrow(risk_scores), replace = TRUE)
    mean(risk_scores$base_risk[sample_idx], na.rm = TRUE)
  })
  
  list(
    mean_risk = mean(risk_scores$base_risk, na.rm = TRUE),
    ci_lower = quantile(boot_means, 0.025),
    ci_upper = quantile(boot_means, 0.975),
    n_patients = nrow(patient_subset)
  )
}

# Split data by condition for parallel processing
data_by_condition <- split(patient_data_DT, by = "condition")

# Remove NA/empty conditions
data_by_condition <- data_by_condition[!names(data_by_condition) %in% c("", "NA")]


# when can also just use system.time to calculate the time taken by a process
# Sequential processing
time_seq <- system.time({
  seq_results <- lapply(data_by_condition, assess_patient_risk_bootstrap, 
                       n_bootstrap = 500)
})

# Parallel processing
# detect the number of available cores
n_cores <- max(1, detectCores() - 1)
print(n_cores)
```

    [1] 13

``` r
# create a set of copies of R running in parallel
cl <- makeCluster(n_cores)
# Export function and required packages to workers
clusterExport(cl, c("assess_patient_risk_bootstrap"))
clusterEvalQ(cl, {
  library(data.table)
})
```

    [[1]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[2]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[3]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[4]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[5]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[6]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[7]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[8]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[9]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[10]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[11]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[12]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

    [[13]]
    [1] "data.table" "stats"      "graphics"   "grDevices"  "utils"     
    [6] "datasets"   "methods"    "base"      

``` r
# run the process in parallel
time_par <- system.time({
  par_results <- parLapply(cl, data_by_condition, assess_patient_risk_bootstrap,
                          n_bootstrap = 500)
})

# important to stop the cluster when dones
stopCluster(cl)


# Display results
cat("\n=== Timing Comparison ===\n")
```


    === Timing Comparison ===

``` r
print('Sequential:')
```

    [1] "Sequential:"

``` r
print(time_seq)
```

       user  system elapsed 
       0.13    0.00    0.12 

``` r
print('Parallel:')
```

    [1] "Parallel:"

``` r
print(time_par)
```

       user  system elapsed 
       0.00    0.00    0.05 

``` r
cat("\n=== Risk Assessment by Condition ===\n")
```


    === Risk Assessment by Condition ===

``` r
results_df <- rbindlist(lapply(names(par_results), function(cond) {
  data.table(
    condition = cond,
    mean_risk = par_results[[cond]]$mean_risk,
    ci_lower = par_results[[cond]]$ci_lower,
    ci_upper = par_results[[cond]]$ci_upper,
    n_patients = par_results[[cond]]$n_patients
  )
}))
print(results_df)
```

           condition mean_risk  ci_lower  ci_upper n_patients
              <char>     <num>     <num>     <num>      <int>
    1:      Diabetes 0.5931579 0.4852477 0.6913667        702
    2:        Asthma 0.6660000 0.5466667 0.7966667        768
    3:  Hypertension 0.6935714 0.5843437 0.8073125        689
    4: Heart Disease 0.9030000 0.8222500 0.9883700        711
    5:          COPD 0.9600000 0.8661250 1.0666882        744

## Memory Management

R can also be inefficient in its memory management (although it has
improved significantly in more recent R versions), which may present
problems when working with larger data. If you need to free up memory it
can be useful to manually trigger garbage collection by running gc() to
ensure that the memory is actually freed. This can be useful to do
periodically in longer loops or after processing large datasets.

### gc()

``` r
# removing the patient_data data frame that is no longer being used
rm(patient_data)
# trigger garbage collection to immediately clear memory
gc()
```

              used  (Mb) gc trigger  (Mb) max used  (Mb)
    Ncells 2253028 120.4    4139204 221.1  3406324 182.0
    Vcells 4215283  32.2   10146329  77.5 10146328  77.5

### Vector Pre-allocation

Growing a vector within a loop in R is inefficient, as each addition may
cause new memory allocation. It is better to pre-allocated your vector
based on the expected size to avoid the inefficient process of
repeatedly extending the vector in memory.

``` r
# growing vector within a loop
name_list_inefficient <- c()
for(i in 1:nrow(patient_data_DT)) {
  name_list_inefficient <- c(name_list_inefficient, paste(patient_data_DT$first_name[i], patient_data_DT$last_name[i]))
}

# pre-allocate vector size
name_list <- character(nrow(patient_data_DT)) 
for(i in 1:nrow(patient_data_DT)) {
  name_list[i] <- paste(patient_data_DT$first_name[i], patient_data_DT$last_name[i])
}
```

### Memory Efficiency with data.table

data.table also offers improved memory efficiency as operations often
work in-place, rather than duplicating the table.

``` r
# in-place modification minimizes redundant memory usage
patient_data_DT[, test_result_cleaning := str_to_lower(test_result)]
```

``` r
# when done, close the log to prevent additional info from being written to the log
log_close()
```
