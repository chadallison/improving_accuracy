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
# pairwise plots for numeric variables using GGally
df |>
  dplyr::select(where(is.numeric)) |>
  ggpairs()
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
charges_prop_plot = df |>
  mutate(children = factor(children)) |>
  group_by(children) |>
  summarise(charges = sum(charges),
            n = n()) |>
  mutate(prop = round(n / sum(n), 3),
         charges_prop = round(charges / sum(charges), 3)) |>
  select(children, prop, charges_prop) |>
  pivot_longer(!children, names_to = "prop_type", values_to = "value") |>
  mutate(prop_type = ifelse(prop_type == "prop", "observations", "charges")) |>
  ggplot(aes(children, value)) +
  geom_col(aes(fill = prop_type), position = "dodge") +
  geom_text(aes(label = value, group = prop_type), position = position_dodge(width = 1), vjust = -0.5, size = 3) +
  scale_fill_manual(values = c("#BEDFC1", "#D2BEDF")) +
  labs(fill = "proportion type",
       title = "charges are higher for observations with 2, 3, or 4 children") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank())

charges_prop_plot
```

<img src="healthcare_accuracy_files/figure-gfm/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />

``` r
# building a correlation heatmap with ggcorrplot package
df |>
  select(where(is.numeric)) |>
  cor() |>
  ggcorrplot(type = "lower",
             outline.color = "black",
             legend.title = "correlation",
             title = "correlation plot of numeric variables",
             colors = c("indianred3", "white", "springgreen4"),
             lab = T,
             hc.order = T) +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="healthcare_accuracy_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

``` r
# visualising distribution of sex among all smokers
df |>
  filter(smoker == "yes") |>
  count(sex) |>
  ggplot(aes(sex, n)) +
  geom_col(aes(fill = sex)) +
  scale_fill_manual(values = c("#FFD2FE", "#C7DBFF")) +
  geom_text(aes(label = n), vjust = -0.5, size = 3.5) +
  theme_classic() +
  labs(x = NULL, y = "count",
       title = "among smokers, there are more males than females") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank())
```

<img src="healthcare_accuracy_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

a few observations:

- males & females are roughly equal when it comes to median charges, but
  the range of values is larger for males
- charges appear to be significantly higher for smokers
- charges appear to be highest for people with 2 or 3 children
- customers are roughly equally distribution among the four regions and
  charges appear to be roughly the same among the four
- among smokers in the data, we have 159 males compared to 115 females

------------------------------------------------------------------------

### Preparation for modeling

``` r
# converting variables to factors
df = df |>
  mutate(sex = factor(sex),
         children = factor(children),
         smoker = factor(smoker),
         region = factor(region))
```

Note that despite taking numeric values, `children` has been converted
to a factor because it is not truly a continuous variable.

``` r
model_formula = charges ~ .
lmMod = lm(model_formula, data = df)

relImportance = relaimpo::calc.relimp(lmMod, type = "lmg",  rela = F)

# relative importances
sort(round(relImportance$lmg, 3), decreasing = T)
```

    ##   smoker      age      bmi children   region      sex 
    ##    0.619    0.089    0.032    0.007    0.003    0.002

I will now perform a series of ANOVA tests to see what the best
predictor variables will be for our model.

``` r
# experimenting with different models based on feature importance
mod1 = lm(charges ~ smoker, data = df)
mod2 = lm(charges ~ smoker + age, data = df)

anova(mod1, mod2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: charges ~ smoker
    ## Model 2: charges ~ smoker + age
    ##   Res.Df         RSS Df   Sum of Sq      F                Pr(>F)    
    ## 1   1336 74554317947                                                
    ## 2   1335 54623951146  1 19930366800 487.09 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

This tells us the model with both `smoker` and `age` as predictors is
better than the model with only `smoker` as a predictor.

``` r
mod1 = lm(charges ~ smoker + age, data = df)
mod2 = lm(charges ~ smoker + age + bmi, data = df)

anova(mod1, mod2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: charges ~ smoker + age
    ## Model 2: charges ~ smoker + age + bmi
    ##   Res.Df         RSS Df  Sum of Sq      F                Pr(>F)    
    ## 1   1335 54623951146                                               
    ## 2   1334 49511575723  1 5112375423 137.74 < 0.00000000000000022 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

This tells us our model is also improved when we add `bmi` as a
predictor.

``` r
mod1 = lm(charges ~ smoker + age + bmi, data = df)
mod2 = lm(charges ~ smoker + age + bmi + children, data = df)

anova(mod1, mod2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: charges ~ smoker + age + bmi
    ## Model 2: charges ~ smoker + age + bmi + children
    ##   Res.Df         RSS Df Sum of Sq      F   Pr(>F)   
    ## 1   1334 49511575723                                
    ## 2   1329 48868594875  5 642980848 3.4972 0.003814 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

This tells us our model is better when we include `children` as a
predictor.

``` r
mod1 = lm(charges ~ smoker + age + bmi + children, data = df)
mod2 = lm(charges ~ smoker + age + bmi + children + region, data = df)

anova(mod1, mod2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: charges ~ smoker + age + bmi + children
    ## Model 2: charges ~ smoker + age + bmi + children + region
    ##   Res.Df         RSS Df Sum of Sq      F Pr(>F)
    ## 1   1329 48868594875                           
    ## 2   1326 48642527382  3 226067493 2.0542 0.1045

This tells us that adding `region` as a predictor does not improve our
model.

``` r
mod1 = lm(charges ~ smoker + age + bmi + children, data = df)
mod2 = lm(charges ~ smoker + age + bmi + children + sex, data = df)

anova(mod1, mod2)
```

    ## Analysis of Variance Table
    ## 
    ## Model 1: charges ~ smoker + age + bmi + children
    ## Model 2: charges ~ smoker + age + bmi + children + sex
    ##   Res.Df         RSS Df Sum of Sq      F Pr(>F)
    ## 1   1329 48868594875                           
    ## 2   1328 48863492609  1   5102266 0.1387 0.7097

Similarly, this tells us that including `sex` as a predictor does not
improve our model either. Based on our ANOVA tests, our best model has
the formula of `charges ~ smoker + age + bmi + children`. However, to
ensure our model is performing well, I will test that model’s
performance against one with all features included based on metrics such
as mean absolute error (MAE) and root mean squared error (RMSE), which
are commonly used to evaluate regression models.

``` r
# here is where i will do ^^^
mod1 = lm(charges ~ smoker + age + bmi + children, data = df)
mod2 = lm(charges ~ smoker + age + bmi + children + region, data = df)
mod3 = lm(charges ~ smoker + age + bmi + children + sex, data = df)
mod4 = lm(charges ~ ., data = df)

model_summaries = data.frame(model = c("anova", "region", "sex", "full"),
           mae = c(MAE(mod1$fitted.values, df$charges),
                   MAE(mod2$fitted.values, df$charges),
                   MAE(mod3$fitted.values, df$charges),
                   MAE(mod4$fitted.values, df$charges)),
           rmse = c(RMSE(mod1$fitted.values, df$charges),
                    RMSE(mod2$fitted.values, df$charges),
                    RMSE(mod3$fitted.values, df$charges),
                    RMSE(mod4$fitted.values, df$charges)),
           rsq = c(summary(mod1)[[8]], summary(mod2)[[8]], summary(mod3)[[8]], summary(mod4)[[8]]),
           adj_rsq = c(summary(mod1)[[9]], summary(mod2)[[9]], summary(mod3)[[9]], summary(mod4)[[9]]))

model_summaries |>
  mutate(mae = round(mae, 4),
         rmse = round(rmse, 4),
         rsq = round(rsq, 4),
         adj_rsq = round(adj_rsq, 4))
```

    ##    model      mae     rmse    rsq adj_rsq
    ## 1  anova 4182.195 6043.477 0.7508  0.7493
    ## 2 region 4174.371 6029.482 0.7519  0.7499
    ## 3    sex 4182.679 6043.161 0.7508  0.7491
    ## 4   full 4173.988 6029.149 0.7519  0.7497

The rankings (from best to worst) for each of the metrics among the
different models are as follows:

`MAE`

1.  Full model
2.  Region model
3.  ANOVA model
4.  Sex model

`RMSE`

1.  Full model
2.  Region model
3.  Sex model
4.  ANOVA model

`RSQ`

1.  ANOVA model
2.  Sex model
3.  Region model
4.  Full model

`ADJ RSQ`

1.  Sex model
2.  ANOVA model
3.  Full model
4.  Region Model
