# Day 1
Sam Gardiner
2023-12-03

Solves [Day 01](https://adventofcode.com/2023/day/1) of Advent of Code
2023.

## Data

Read a newline-delimited text file:

``` r
input <- scan("data/input01",
              what = "character")
```

## Part One

Concatenate the first and last digit from a string to form a two-digit
integer:

``` r
extract_digit <- function(element, indices) {
  first <- indices[1]
  last <- tail(indices, 1)
  
  paste0(substr(element, first, first),
         substr(element, last, last)) |>
    as.integer()
}
```

Find the digit indices for each element:

``` r
digit_indices <- gregexpr("[[:digit:]]", text = input)
```

Find the sum of the concatenated two-digit values:

``` r
values <- mapply(extract_digit, input, digit_indices, SIMPLIFY = TRUE) |>
  sum()

values
```

    [1] 55108

## Part Two

**This solution is wrong.**

We need a couple of functions.

Replace a single known substring with a digit:

``` r
word_to_digit <- function(x, word = c("one", "two", "three", "four", "five",
                                      "six", "seven", "eight", "nine")){
  word <- match.arg(word)
  replacements <- list(
    "one"  = 1,
    "two" = 2,
    "three" = 3,
    "four" = 4,
    "five" = 5,
    "six" = 6,
    "seven" = 7,
    "eight" = 8,
    "nine" = 9
  )
  as.integer(sub(word, replacement = replacements[word], x))
}
```

Extract all valid values from a single element of the input:

``` r
extract_values <- function(input_text) {
  value_tokens <- c(1:9,  "one", "two", "three", "four", "five", "six", "seven",
                    "eight", "nine")
  
  matches <-  lapply(
    value_tokens,
    function(x)  {
      all_matches <- regexpr(pattern = x, text = input_text)
      data.frame(position  = as.numeric(all_matches),
                 length = as.numeric(attr(all_matches, "match.length")))
    }
  )
  match_df <- as.data.frame(Reduce(rbind, matches))
  match_df <- match_df[match_df$position > -1,]
  match_df <- match_df[order(match_df$position),]
  
  values <- mapply(function(position,  length) {
                     substr(input_text,
                            start = position,
                            stop = position + length - 1)
                   },
                   match_df$position,
                   match_df$length
  )
  values
}
```

Parse the extracted values:

``` r
# Parse a single value
parse_value <- function(x)  {
  try_parse <- suppressWarnings(as.integer(x))
  if (is.na(try_parse)) {
    word_to_digit(x, word = x)
  } else  {
    try_parse
  }
}

# Parse a vector of values
parse_values <-  function(x) {
  sapply(x, parse_value)
}
```

Compute the calibration value for an element of the input by
concatenating the first and last extracted values:

``` r
calibrate <- function(x){
  values <- extract_values(x) |>
    parse_values()
  
  as.integer(paste0(
    values[1],
    tail(values, 1)
  ))
}
```

Test against the examples:

``` r
examples <- c("two1nine", "eightwothree", "abcone2threexyz", "xtwone3four",
              "4nineeightseven2", "zoneight234", "7pqrstsixteen")
examples_valid <- c(29, 83, 13, 24, 42, 14, 76)

example_values <- sapply(examples, calibrate)
example_values == examples_valid
```

            two1nine     eightwothree  abcone2threexyz      xtwone3four 
                TRUE             TRUE             TRUE             TRUE 
    4nineeightseven2      zoneight234    7pqrstsixteen 
                TRUE             TRUE             TRUE 

``` r
sum(example_values)
```

    [1] 281

One more test case:

``` r
calibrate("oneight")
```

    [1] 18

Try with the real dataset:

``` r
values_part_two <- sapply(input, calibrate)
sum(values_part_two)
```

    [1] 56314
