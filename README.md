Improving the accuracy of a regression model
================
Chad Allison
2022-11-23

``` r
# importing the data
df = read_csv("https://raw.githubusercontent.com/sacharya225/data-expts/master/Health%20Insurance%20Cost%20Prediction/insurance.csv",
         col_types = cols())
```

``` r
# finding the dimensions of the data
paste0(nrow(df), " rows; ", ncol(df), " columns")
```

    ## [1] "1338 rows; 7 columns"

``` r
# checking if/where we have NA values
colSums(is.na(df))
```

    ##      age      sex      bmi children   smoker   region  charges 
    ##        4        0        2        0        0        0        0

``` r
# using the median to impute NA values
df = df |>
  mutate(age = ifelse(is.na(age), median(df$age, na.rm = T), age),
         bmi = ifelse(is.na(bmi), median(df$bmi, na.rm = T), bmi))

# confirming we imputed where the NA values were
colSums(is.na(df))
```

    ##      age      sex      bmi children   smoker   region  charges 
    ##        0        0        0        0        0        0        0

``` r
# pairwise plots for numeric variables
df |>
  select(where(is.numeric)) |>
  pairs()
```

![](healthcare_accuracy_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
sex_counts_plot = df |>
  count(sex) |>
  ggplot(aes(sex, n)) +
  geom_col(aes(fill = sex)) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#FAD7FF", "#CEE4FC")) +
  theme_classic() +
  labs(x = NULL, y = "count",
       title = "gender counts") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2.5))
  

smoker_counts_plot = df |>
  count(smoker) |>
  ggplot(aes(smoker, n)) +
  geom_col(aes(fill = smoker)) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#FFC2B9", "#BEDFC1")) +
  theme_classic() +
  labs(x = NULL, y = "count",
       title = "smoker counts") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2.5))

children_counts_plot = df |>
  count(children) |>
  mutate(children = factor(children)) |>
  ggplot(aes(children, n)) +
  geom_col(aes(fill = children), alpha = 0.5) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  # scale_fill_manual(values = c("#FFC2B9", "#BEDFC1")) +
  theme_classic() +
  labs(x = NULL, y = "count",
       title = "children counts") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2.5))

region_counts_plot = df |>
  count(region) |>
  ggplot(aes(region, n)) +
  geom_col(aes(fill = region), alpha = 0.5) +
  geom_text(aes(label = n), vjust = -0.5, size = 3) +
  # scale_fill_manual(values = c("#FFC2B9", "#BEDFC1")) +
  theme_classic() +
  labs(x = NULL, y = "count",
       title = "region counts") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 2.5))
```

``` r
sex_counts_plot + smoker_counts_plot + children_counts_plot + region_counts_plot
```

![](healthcare_accuracy_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
