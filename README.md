using tidymodels to model vaccination rate data
================
chad allison
2023-02-03

### setup

``` r
tictoc::tic()
library(tidyverse)
library(tidymodels)

theme_custom = tvthemes::theme_avatar() +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(linewidth = 0.5, colour = "#D6D0C4"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "#D6D0C4"))

theme_set(theme_custom)
```

------------------------------------------------------------------------

### data import

``` r
link = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv"
measles = read_csv(link, col_types = cols())
glimpse(measles)
```

    ## Rows: 66,113
    ## Columns: 16
    ## $ index    <dbl> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10, 11, 12, 13, 14, 15, 15, 16~
    ## $ state    <chr> "Arizona", "Arizona", "Arizona", "Arizona", "Arizona", "Arizo~
    ## $ year     <chr> "2018-19", "2018-19", "2018-19", "2018-19", "2018-19", "2018-~
    ## $ name     <chr> "A J Mitchell Elementary", "Academy Del Sol", "Academy Del So~
    ## $ type     <chr> "Public", "Charter", "Charter", "Charter", "Charter", "Public~
    ## $ city     <chr> "Nogales", "Tucson", "Tucson", "Phoenix", "Phoenix", "Phoenix~
    ## $ county   <chr> "Santa Cruz", "Pima", "Pima", "Maricopa", "Maricopa", "Marico~
    ## $ district <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ enroll   <dbl> 51, 22, 85, 60, 43, 36, 24, 22, 26, 78, 78, 35, 54, 54, 34, 5~
    ## $ mmr      <dbl> 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 1~
    ## $ overall  <dbl> -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -~
    ## $ xrel     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ xmed     <dbl> NA, NA, NA, NA, 2.33, NA, NA, NA, NA, NA, NA, 2.86, NA, 7.41,~
    ## $ xper     <dbl> NA, NA, NA, NA, 2.33, NA, 4.17, NA, NA, NA, NA, NA, NA, NA, N~
    ## $ lat      <dbl> 31.34782, 32.22192, 32.13049, 33.48545, 33.49562, 33.43532, 3~
    ## $ lng      <dbl> -110.9380, -110.8961, -111.1170, -112.1306, -112.2247, -112.1~

------------------------------------------------------------------------

### creating data frame tracking if schools are above or below threshold

``` r
measles_df = measles |>
  filter(mmr > 0) |>
  transmute(state,
            mmr_threshold = case_when(mmr > 95 ~ "above",
                                      T ~ "below")) |>
  mutate_if(is.character, factor)

sample_n(measles_df, 10)
```

    ## # A tibble: 10 x 2
    ##    state         mmr_threshold
    ##    <fct>         <fct>        
    ##  1 California    above        
    ##  2 California    below        
    ##  3 Illinois      above        
    ##  4 Massachusetts above        
    ##  5 Arizona       above        
    ##  6 Connecticut   above        
    ##  7 Ohio          below        
    ##  8 Minnesota     above        
    ##  9 Ohio          above        
    ## 10 California    above

------------------------------------------------------------------------

### average rate of vaccination by state

``` r
measles_df |>
  group_by(state) |>
  summarise(mmr = round(mean(mmr_threshold == "above"), 3)) |>
  arrange(desc(mmr)) |>
  ggplot(aes(reorder(state, mmr), mmr)) +
  geom_col(fill = "#A390AD", position = "dodge") +
  geom_text(aes(label = mmr), hjust = -0.25, size = 3) +
  coord_flip(ylim = c(0, 0.95)) +
  labs(x = "state", y = "above threshold rate",
       title = "average vaccination rate by state")
```

![](vax_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

------------------------------------------------------------------------

### above/below threshold counts by state

``` r
measles_df |>
  count(state, mmr_threshold) |>
  ggplot(aes(reorder(state, n), n)) +
  geom_col(aes(fill = mmr_threshold), position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = c("#A5BDA3", "#D89E9E")) +
  labs(x = "state", y = "count", fill = "above/below threshold",
       title = "counts of schools above/below threshold by state") +
  theme(legend.position = "right")
```

![](vax_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

------------------------------------------------------------------------

### continue here

``` r
glimpse(measles_df)
```

    ## Rows: 44,157
    ## Columns: 2
    ## $ state         <fct> Arizona, Arizona, Arizona, Arizona, Arizona, Arizona, Ar~
    ## $ mmr_threshold <fct> above, above, above, above, above, above, above, above, ~

------------------------------------------------------------------------

### script runtime

``` r
tictoc::toc()
```

    ## 4.89 sec elapsed
