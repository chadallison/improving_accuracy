Improving the accuracy of a regression model
================

[article](https://towardsdatascience.com/how-to-improve-the-accuracy-of-a-regression-model-3517accf8604)

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

<img src="healthcare_accuracy_files/figure-gfm/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />

``` r
# creating plots for categorical variables
sex_counts_plot = df |>
  count(sex) |>
  ggplot(aes(sex, n)) +
  geom_col(aes(fill = sex)) +
  scale_fill_manual(values = c("#FAD7FF", "#CEE4FC")) +
  theme_classic() +
  labs(x = NULL, y = NULL,
       title = "gender counts") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 2.5))
  

smoker_counts_plot = df |>
  count(smoker) |>
  ggplot(aes(smoker, n)) +
  geom_col(aes(fill = smoker)) +
  scale_fill_manual(values = c("#FFC2B9", "#BEDFC1")) +
  theme_classic() +
  labs(x = NULL, y = NULL,
       title = "smoker counts") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 2.5))

children_counts_plot = df |>
  count(children) |>
  mutate(children = factor(children)) |>
  ggplot(aes(children, n)) +
  geom_col(aes(fill = children), alpha = 0.5) +
  theme_classic() +
  labs(x = NULL, y = NULL,
       title = "children counts") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 2.5))

region_counts_plot = df |>
  count(region) |>
  ggplot(aes(region, n)) +
  geom_col(aes(fill = region), alpha = 0.5) +
  theme_classic() +
  labs(x = NULL, y = NULL,
       title = "region counts") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, vjust = 2.5))
```

``` r
# combining plots into one with patchwork package
sex_counts_plot + smoker_counts_plot + children_counts_plot + region_counts_plot
```

<img src="healthcare_accuracy_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
# visualising distribution of charges among categorical variable values
charges_sex_plot = df |>
  ggplot(aes(sex, charges)) +
  geom_boxplot(aes(fill = sex), outlier.alpha = 0.25, outlier.shape = 4) +
  scale_fill_manual(values = c("#FAD7FF", "#CEE4FC")) +
  labs(x = NULL, y = NULL,
       title = "charges v. sex") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

charges_smoker_plot = df |>
  ggplot(aes(smoker, charges)) +
  geom_boxplot(aes(fill = smoker), outlier.alpha = 0.25, outlier.shape = 4) +
  scale_fill_manual(values = c("#FFC2B9", "#BEDFC1")) +
  labs(x = NULL, y = NULL,
       title = "charges v. smoker") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

charges_children_plot = df |>
  mutate(children = factor(children)) |>
  ggplot(aes(children, charges)) +
  geom_boxplot(aes(fill = children), outlier.alpha = 0.25, outlier.shape = 4, alpha = 0.5) +
  labs(x = NULL, y = NULL,
       title = "charges v. children") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))

charges_region_plot = df |>
  ggplot(aes(region, charges)) +
  geom_boxplot(aes(fill = region), outlier.alpha = 0.25, outlier.shape = 4, alpha = 0.5) +
  labs(x = NULL, y = NULL,
       title = "charges v. region") +
  theme_classic() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5))
```

``` r
# combining previous plots with patchwork
charges_sex_plot + charges_smoker_plot + charges_children_plot + charges_region_plot
```

<img src="healthcare_accuracy_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

``` r
# making more plots showing distribution of charges for smoker and children
charges_smoker_density = df |>
  ggplot(aes(charges)) +
  geom_density(aes(fill = smoker), col = "transparent", alpha = 0.8) +
  scale_fill_manual(values = c("#BEDFC1", "#FFC2B9")) +
  theme_classic() +
  labs(title = "density curves of charges v. smoker") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank())

charges_children_density = df |>
  ggplot(aes(charges)) +
  geom_density(aes(fill = factor(children)), col = "transparent", alpha = 0.25) +
  theme_classic() +
  labs(title = "density curves of charges v. children",
       fill = "children") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank())
```

``` r
# putting density curve plots together with patchwork
charges_smoker_density / charges_children_density
```

<img src="healthcare_accuracy_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r
# additional visual of charges among values of children
df |>
  mutate(children = factor(children)) |>
  group_by(children) |>
  summarise(charges = sum(charges),
            n = n()) |>
  mutate(prop = round(n / sum(n), 3),
         charges_prop = round(charges / sum(charges), 3)) |>
  select(children, prop, charges_prop) |>
  pivot_longer(!children, names_to = "prop_type", values_to = "value") |>
  ggplot(aes(children, value)) +
  geom_col(aes(fill = prop_type), position = "dodge") +
  scale_fill_manual(values = c("#BEDFC1", "#D2BEDF")) +
  labs(fill = "proportion type",
       title = "charges are higher than expected for observations with 2, 3, or 4 children") +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="healthcare_accuracy_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />
