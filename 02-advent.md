# Day 2
Sam Gardiner

Solves [Day 02](https://adventofcode.com/2023/day/2) of Advent of Code
2023.

## Packages

``` r
library(tidyverse)
```

## Data

``` r
input <- scan("data/input02",
              what = "character",
              sep = "\n")

head(input, 3)
```

    [1] "Game 1: 1 red, 3 blue, 11 green; 1 blue, 5 red; 3 blue, 5 green, 13 red; 6 red, 1 blue, 4 green; 16 red, 12 green"                                    
    [2] "Game 2: 3 red, 13 blue, 5 green; 14 green, 14 blue; 9 blue, 10 green, 3 red; 2 green, 5 blue; 11 green, 3 blue, 3 red; 16 blue, 2 red, 9 green"       
    [3] "Game 3: 17 blue, 5 red; 3 red, 11 green, 17 blue; 1 red, 6 blue, 9 green; 3 blue, 11 green, 1 red; 3 green, 10 red, 11 blue; 12 red, 3 green, 15 blue"

## Part One

A couple of parsing functions:

``` r
# Parse a single draw, eg. "1 blue"
parse_draw <- function(x) {
  colour  <- str_extract(x, "(red)|(green)|(blue)")
  value <- as.integer(str_extract(x, "[[:digit:]]+"))
  list(
    colour =  colour,
    value = value
  )
}

# Parse a single round, e.g. "1 red, 2 blue, 3 green"
parse_round <- function(x){
  draws <- str_split_1(x, ", ")
  map_dfr(draws, parse_draw)
}

# Parse a single game, ie. a semicolon delimited set of rounds
parse_game <- function(x) {
  id <- as.integer(str_extract(x, "(?<=Game )[[:digit:]]+"))
  rounds <- str_extract(x, "(?<=\\: ).+$") |>
    str_split_1(";") |>
    map_dfr(parse_round, .id = "round")
  
  rounds |>
    mutate(game =  id,
           round =  as.integer(round),
           .before = 1)
  
}
```

Parse the input dataset to a long dataframe:

``` r
games_long  <- map(input, parse_game) |>
  list_rbind()

head(games_long)
```

    # A tibble: 6 × 4
       game round colour value
      <int> <int> <chr>  <int>
    1     1     1 red        1
    2     1     1 blue       3
    3     1     1 green     11
    4     1     2 blue       1
    5     1     2 red        5
    6     1     3 blue       3

To make the solution logic a little less verbose, pivot out to a wide
dataframe:

``` r
games_wide <- games_long |>
  pivot_wider(names_from = colour,
              values_from = value,
              values_fill = 0)

head(games_wide)
```

    # A tibble: 6 × 5
       game round   red  blue green
      <int> <int> <int> <int> <int>
    1     1     1     1     3    11
    2     1     2     5     1     0
    3     1     3    13     3     5
    4     1     4     6     1     4
    5     1     5    16     0    12
    6     2     1     3    13     5

To solve, keep only the games where every round yielded less than 12
red, 13 green and 14 blue cubes.

``` r
possible_games <- games_wide |>
  group_by(game) |>
  filter(
    all(
      c(red <= 12,
        green <= 13,
        blue <= 14)
    )
  )

head(possible_games)
```

    # A tibble: 6 × 5
    # Groups:   game [1]
       game round   red  blue green
      <int> <int> <int> <int> <int>
    1     5     1     3     1     3
    2     5     2     6     2     2
    3     5     3    12     1     3
    4     5     4     9     0     2
    5     5     5     0     1     0
    6     5     6    10     2     0

``` r
possible_game_ids <- unique(possible_games$game)
possible_game_ids
```

     [1]  5  7  8  9 10 11 12 17 19 22 23 26 27 28 30 39 41 42 43 51 52 58 59 61 62
    [26] 69 73 75 77 80 81 83 84 87 88 90 91 93 98

``` r
sum(possible_game_ids)
```

    [1] 1931
