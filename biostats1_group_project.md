P8130: Biostats I - Final Project
================
Daniel Ojeranti, David Nemirovsky, Ford Holland, Jared Klug, Justin
Vargas
12/18/2020

Read and tidy data:

``` r
hate_crimes_df = 
  read_csv("./HateCrimes.csv") %>% 
  mutate(
    unemployment = as.factor(unemployment), 
    urbanization = as.factor(urbanization),
    hate_crimes_per_100k_splc = as.numeric(hate_crimes_per_100k_splc)
  ) %>% 
  drop_na()
```

EDA:

``` r
hate_crimes_df %>% 
  ggplot(aes(x = hate_crimes_per_100k_splc)) + 
  geom_histogram()
```

<img src="biostats1_group_project_files/figure-gfm/distribution of hate crimes-1.png" width="95%" />
