# README
Sam Gardiner

This repository contains my code and solutions for [Advent of Code
2023](https://adventofcode.com/2023).

## Packages

``` r
library(tidyverse)
library(fs)
```

Get a list of the Markdown (solution) and Quarto markdown (code) files:

``` r
solutions <- tibble(filename = dir_ls(glob =  "*-advent.*")) |>
  mutate(day = str_extract(filename, "^[[:digit:]]+"),
         extension = path_ext(filename), 
         type = case_match(extension,
                           "md" ~ "solution",
                           "qmd" ~ "code"),
         link = str_glue("[{type}]({filename})"))  |>
  group_by(day) |>
  summarise(links = paste(link, collapse = ", "))
```

Render:

``` r
knitr::kable(solutions)
```

| day | links                                           |
|:----|:------------------------------------------------|
| 01  | [solution](01-advent.md), [code](01-advent.qmd) |
| 02  | [solution](02-advent.md), [code](02-advent.qmd) |
