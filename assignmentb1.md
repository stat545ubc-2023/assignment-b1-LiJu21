Assignment B1
================
Li Ju
2023-10-30

# Exercise 1 and 2: Make a Function and Document your Function

In this exercise, I will make a generic function that can group by a
specific column and summarize statistics calculated for another specific
column in the dataset. This idea came from my repeated **group_by() %\>%
summarise()** workflow in my mda project.

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
#' @title Summarize a column by a group
#' @details This function groups the data by a specified column and calculates statistics for another specified column.
#' @param df The data we will manipulate.
#' @param group_col The column the data should be grouped by.
#' @param summary_col The column statistics should be calculated for.
#' @return A new data frame with the summarized statistics including the average, maximum and minimum of the summary column.
summarize_col_by_group <- function(df, group_col, summary_col) {
  if (!is.numeric(df[[summary_col]])) {
    stop("The summary column must be numeric.")
  }
  result <- df %>%
    group_by(.data[[group_col]]) %>%
    filter(!all(is.na(.data[[summary_col]]))) %>%
    summarise(
      average = mean(.data[[summary_col]], na.rm = TRUE),
      max_value = max(.data[[summary_col]], na.rm = TRUE),
      min_value = min(.data[[summary_col]], na.rm = TRUE),
    )
  return(result)
}
```

# Exercise 3: Include examples

I will include two examples from my mda project. So first I will load
library and I will add age column into my vancouver_trees dataset:

``` r
library(datateachr)
vancouver_trees <- vancouver_trees %>%
  mutate(age = as.numeric(difftime(Sys.Date(), date_planted, units = "days")))
```

## Example 1:

``` r
# Used the generic function to grab information around trees in different neighbourhoods including the average age of trees, the maximal age of trees and the minimal age of trees
vancouver_trees_summary_by_neighbourhood <- summarize_col_by_group(vancouver_trees, group_col = "neighbourhood_name", summary_col = "age")
vancouver_trees_summary_by_neighbourhood
```

    ## # A tibble: 22 × 4
    ##    neighbourhood_name       average max_value min_value
    ##    <chr>                      <dbl>     <dbl>     <dbl>
    ##  1 ARBUTUS-RIDGE              7256.     12419      1670
    ##  2 DOWNTOWN                   6705.     12397      1639
    ##  3 DUNBAR-SOUTHLANDS          6924.     12416      1649
    ##  4 FAIRVIEW                   7166.     12349      1643
    ##  5 GRANDVIEW-WOODLAND         7075.     12394      1601
    ##  6 HASTINGS-SUNRISE           7711.     12354      1646
    ##  7 KENSINGTON-CEDAR COTTAGE   7519.     12388      1650
    ##  8 KERRISDALE                 6823.     12408      1649
    ##  9 KILLARNEY                  7210.     12416      1611
    ## 10 KITSILANO                  6790.     12320      1639
    ## # ℹ 12 more rows

## Example 2:

``` r
# Used the generic function to grab information around trees grouped by different species including the average age of trees, the maximal age of trees and the minimal age of trees
vancouver_trees_summary_by_species <- summarize_col_by_group(vancouver_trees, group_col = "species_name", summary_col = "age")
vancouver_trees_summary_by_species
```

    ## # A tibble: 222 × 4
    ##    species_name   average max_value min_value
    ##    <chr>            <dbl>     <dbl>     <dbl>
    ##  1 ABIES            6496.      7634      3262
    ##  2 ACERIFOLIA   X   8256.     12019      2435
    ##  3 ACUMINATA        7780.      8243      7537
    ##  4 ACUTISSIMA       9777.     11940      6237
    ##  5 ALBA             5214.     10091      1737
    ##  6 ALBA-SINENSIS    8953.      8954      8950
    ##  7 ALNIFOLIA        8883.     12025      2731
    ##  8 AMERICANA        7528.     12327      1678
    ##  9 ANAGYROIDES      8583       8583      8583
    ## 10 ANTARTICA        8630.     10590      4603
    ## # ℹ 212 more rows

# Exercise 4: Test the Function

First we can initialize our test data.

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

``` r
test_data_no_na <- data.frame(
  group = c("a", "b", "b"),
  summary = c(1, 2, 3)
)
```

## Test with vector with no NA’s

``` r
test_that("function works on vector with no NA's", {
  result <- summarize_col_by_group(test_data_no_na, "group", "summary")
  expect_equal(nrow(result), 2)
  
  result_a = result[result$group == "a",]
  expect_equal(result_a$average, 1)
  expect_equal(result_a$max_value, 1)
  expect_equal(result_a$min_value, 1)
  
  result_b = result[result$group == "b",]
  expect_equal(result_b$average, 2.5)
  expect_equal(result_b$max_value, 3)
  expect_equal(result_b$min_value, 2)
})
```

    ## Test passed 😸

## Test with vector that has NA’s

``` r
test_that("function works on vector has NA's", {
  test_data_with_na <- test_data_no_na
  test_data_with_na$summary[c(1, 2)] <- NA
  
  result <- summarize_col_by_group(test_data_with_na, "group", "summary")
  expect_equal(nrow(result), 1)
  
  result_b = result[result$group == "b",]
  expect_equal(result_b$average, 3)
  expect_equal(result_b$max_value, 3)
  expect_equal(result_b$min_value, 3)
})
```

    ## Test passed 🎉

## Test with vector of a different type

``` r
test_that("function works on vector of a different type", {
  test_data_diff <- data.frame(
    group = c("a", "b", "b"),
    summary = c("a", "b", "b")
  )
  
  expect_error(summarize_col_by_group(test_data_diff, "group", "summary"))
})
```

    ## Test passed 🌈
