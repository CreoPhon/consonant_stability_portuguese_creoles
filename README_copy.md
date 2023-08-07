Supplementary materials for: Consonant stability in Portuguese-based
creoles
================
Steven Moran and Carlos Silva and Nicholas A. Lester
(07 agosto, 2023)

- [Overview](#overview)
- [Creole stability](#creole-stability)
- [Duration](#duration)
  - [Duration efects on the segment
    level](#duration-efects-on-the-segment-level)
- [Segment stability](#segment-stability)
- [Word position](#word-position)
- [Conditions of contact](#conditions-of-contact)
- [References](#references)

# Overview

Supplementary materials for [Consonant Stability in Portuguese-based
creoles](https://www.overleaf.com/project/60cdac0dd5871295e0f608fc).
Silva, Carlos and Steven Moran. Work in progress.

In this report, we use R (RStudio Team 2020) and the following packages
(Wickham et al. 2019; Xie 2021).

``` r
library(tidyverse)
library(knitr)
```

Load the dataset.

``` r
# Because the private token keeps resetting...
# database <- read_csv('https://raw.githubusercontent.com/CreoPhon/CreoPhonPt/main/Creoles.csv?token=AAIGDLS6YB3YSYW7QJWERXDBVYPOE')
# write_csv(database, 'database.csv')
database <- read_csv('database.csv')
```

The data look like this.

``` r
database %>% head() %>% kable()
```

| Language    | Macroarea | Area           | Lexifier   | FirstMajorSettlement | EndOfInfluence | ContactConditions | LanguageContact | Class | Position     | LexifierPhoneme | CreolePhoneme | PlaceStability | MannerStability | Word        | Gloss      | Source            |
|:------------|:----------|:---------------|:-----------|---------------------:|---------------:|:------------------|:----------------|:------|:-------------|:----------------|:--------------|---------------:|----------------:|:------------|:-----------|:------------------|
| Principense | Africa    | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-initial | p               | p             |              1 |               1 | \[ˈpɛnɛ\]   | feather    | Maurer2009\[232\] |
| Principense | Africa    | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-medial  | p               | p             |              1 |               1 | \[t̠ʃipa\]   | guts       | Maurer2009\[238\] |
| Principense | Africa    | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-initial | b               | b             |              1 |               1 | \[bwɛga\]   | belly      | Maurer2009\[216\] |
| Principense | Africa    | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-medial  | b               | b             |              1 |               1 | \[kaˈbɛlu\] | hair       | Maurer2009\[221\] |
| Principense | Africa    | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-initial | t               | t             |              1 |               1 | \[ˈtudu\]   | everything | Maurer2009\[237\] |
| Principense | Africa    | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-medial  | t               | t             |              1 |               1 | \[mata\]    | to kill    | Maurer2009\[227\] |

Let’s extend the database with some variables. Duration of contact.

``` r
database$duration <- database$`EndOfInfluence` - database$`FirstMajorSettlement`
```

Next, a variable of global stability.

``` r
database <- mutate(database, GlobalStability = (PlaceStability + MannerStability)/2)
# table(database$Language, database$GlobalStability)
```

Categorical variable for duration.

``` r
database <- database %>% mutate(duration_group = ifelse(duration <= 200, 'short', 'long'))
```

Categorical variable for changes in manner and/or place. Stability in
the database is ‘1’ (no change) and ‘0’ change.

``` r
database <- database %>% mutate(categorical_stability = ifelse(PlaceStability == 1 & MannerStability == 1, 'no manner/no place', NA))

database <- database %>% mutate(categorical_stability = ifelse(PlaceStability == 1 & MannerStability == 0, 'manner/no place', categorical_stability))

database <- database %>% mutate(categorical_stability = ifelse(PlaceStability == 0 & MannerStability == 1, 'no manner/place', categorical_stability))

database <- database %>% mutate(categorical_stability = ifelse(PlaceStability == 0 & MannerStability == 0, 'manner/place', categorical_stability))

table(database$categorical_stability)
```

    ## 
    ##    manner/no place       manner/place no manner/no place    no manner/place 
    ##                 49                 58                553                 25

# Creole stability

Which creoles in the sample are more or less stable overall?

``` r
creole_stability <- database %>% group_by(Language, Area, duration, duration_group, ContactConditions) %>% summarize(MeanStability = mean(GlobalStability, na.rm = TRUE))
```

Plot it by area.

``` r
ggplot(creole_stability) + 
  geom_bar(aes(x = MeanStability, y = reorder(Language, MeanStability), fill = Area), 
           stat = "identity", show.legend = TRUE) +
  theme(axis.title.y = element_blank()) +
  labs(x = "Stability score")
```

![](README_copy_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
table(creole_stability$Area)
```

    ## 
    ## Gulf of Guinea Northern India Southeast Asia Southern India   Upper Guinea 
    ##              4              3              3              2              7

Plot it by conditions of contact.

``` r
ggplot(creole_stability) + 
  geom_bar(aes(x = MeanStability, y = reorder(Language, MeanStability), fill = ContactConditions), 
           stat = "identity", show.legend = TRUE) +
  theme(axis.title.y = element_blank()) +
  labs(x = "Stability score")
```

![](README_copy_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

# Duration

We have the overall stability values. What are these in relation to the
duration of contact?

There does not seem to be a relationship between overall duration and
overall stability.

``` r
ggplot(creole_stability, aes(x=duration, y=MeanStability)) +
  geom_point()
```

![](README_copy_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
ggplot(creole_stability, aes(x=duration, y=MeanStability)) +
  geom_point() +
  geom_text(label=creole_stability$Language)
```

![](README_copy_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

Results from the simple regression.

``` r
msd <- lm(MeanStability ~ duration, data=creole_stability)
summary(msd)
```

    ## 
    ## Call:
    ## lm(formula = MeanStability ~ duration, data = creole_stability)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21701 -0.01214  0.03417  0.04673  0.07143 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 8.336e-01  4.471e-02  18.644  9.4e-13 ***
    ## duration    8.525e-05  1.231e-04   0.692    0.498    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08344 on 17 degrees of freedom
    ## Multiple R-squared:  0.02742,    Adjusted R-squared:  -0.02979 
    ## F-statistic: 0.4793 on 1 and 17 DF,  p-value: 0.4981

However, there does seem to be two groups of languages – ones that
belong to “long duration” (\>= 400 years) and those that below to “short
duration” (\<= 200 years).

We can try to split the data and rerun the models, but we note that
there are very few data points.

``` r
tmp_short <- creole_stability %>% filter(duration <= 200)
tmp_long <- creole_stability %>% filter(duration > 200)
```

``` r
ggplot(tmp_short, aes(x=duration, y=MeanStability)) +
  geom_point()
```

![](README_copy_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
ggplot(tmp_short, aes(x=duration, y=MeanStability)) +
  geom_point() +
  geom_text(label=tmp_short$Language)
```

![](README_copy_files/figure-gfm/unnamed-chunk-15-2.png)<!-- -->

``` r
ggplot(tmp_long, aes(x=duration, y=MeanStability)) +
  geom_point()
```

![](README_copy_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggplot(tmp_long, aes(x=duration, y=MeanStability)) +
  geom_point() +
  geom_text(label=tmp_long$Language)
```

![](README_copy_files/figure-gfm/unnamed-chunk-16-2.png)<!-- -->

Or perhaps a single model with an interaction term MeanSim ~ duration,
group \* duration.

``` r
msd <- lm(MeanStability ~ duration + duration_group * duration, data=creole_stability)
summary(msd)
```

    ## 
    ## Call:
    ## lm(formula = MeanStability ~ duration + duration_group * duration, 
    ##     data = creole_stability)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.21107 -0.02781  0.02104  0.04323  0.07211 
    ## 
    ## Coefficients:
    ##                                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                   8.229e-01  9.637e-02   8.539 3.82e-07 ***
    ## duration                      9.639e-05  2.249e-04   0.429   0.6743    
    ## duration_groupshort          -1.869e-01  1.324e-01  -1.412   0.1784    
    ## duration:duration_groupshort  1.580e-03  6.840e-04   2.310   0.0355 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07447 on 15 degrees of freedom
    ## Multiple R-squared:  0.3164, Adjusted R-squared:  0.1796 
    ## F-statistic: 2.314 on 3 and 15 DF,  p-value: 0.1174

``` r
ggplot(creole_stability, aes(x = duration, y = MeanStability, color = duration_group)) +
  geom_smooth(method = "lm") +
  geom_point()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_copy_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

The variability in the two groups is very different. The direction of
the effect is interesting: shorter durations yield more stability more
consistently. Over time, the variability in mean stability increases.
Time is “destabillizing the pattern of stability”.

But it looks like you might have something tastier on your hands. The
creoles appear to be bouncing back toward the lexifier over time (based
on the duration findings; but perhaps I misunderstand).

And we can also increase the number of observations by running the
analysis at the segment level, rather than on mean stability.

Exploratory stuff with GAMs.

``` r
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-42. For overview type 'help("mgcv-package")'.

``` r
# Factorize duration_group
creole_stability$duration_group = as.factor(creole_stability$duration_group)

# Model with an interaction between duration_group and duration
# (with maximum of cubic-spline fit)
msd.gam <- gam(MeanStability ~ duration_group + s(duration, k=3) + s(duration, by=duration_group, k=3), data=creole_stability)

summary(msd.gam)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## MeanStability ~ duration_group + s(duration, k = 3) + s(duration, 
    ##     by = duration_group, k = 3)
    ## 
    ## Parametric coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.85451    0.02877  29.700 1.28e-14 ***
    ## duration_groupshort  0.06307    0.59085   0.107    0.916    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                    edf Ref.df     F p-value
    ## s(duration)                     0.6667 0.6667 0.036   0.879
    ## s(duration):duration_grouplong  0.6667 0.6667 0.005   0.957
    ## s(duration):duration_groupshort 0.8454 0.9921 0.003   0.955
    ## 
    ## Rank: 7/8
    ## R-sq.(adj) =  0.191   Deviance explained = 33.4%
    ## GCV = 0.0070083  Scale est. = 0.0054669  n = 19

``` r
plot(msd.gam, all.terms=T, shade=T, pages=1)
```

![](README_copy_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
qqnorm(resid(msd.gam)); qqline(resid(msd.gam))
```

![](README_copy_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
msd.gam.trimmed <- gam(MeanStability ~ duration_group + s(duration, k=3) + s(duration, by=duration_group, k=3), data=creole_stability %>% filter(MeanStability>0.7))

summary(msd.gam.trimmed)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## MeanStability ~ duration_group + s(duration, k = 3) + s(duration, 
    ##     by = duration_group, k = 3)
    ## 
    ## Parametric coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          0.87215    0.01743   50.03 2.97e-16 ***
    ## duration_groupshort  0.05635    0.18193    0.31    0.762    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                                    edf Ref.df     F p-value
    ## s(duration)                     0.6667 0.6667 0.128   0.775
    ## s(duration):duration_grouplong  0.6667 0.6667 0.003   0.963
    ## s(duration):duration_groupshort 0.6667 0.6667 0.022   0.905
    ## 
    ## Rank: 7/8
    ## R-sq.(adj) =  -0.141   Deviance explained = 7.28%
    ## GCV = 0.0028498  Scale est. = 0.0021793  n = 17

``` r
plot(msd.gam.trimmed, sel=1, shade=T, ylab="Effect on mean stability", xlab="Duration of influence", residuals=T, main="Main effect of duration", cex=5, pch=".", col='dodgerblue'); abline(h=0, lty=2, col="red")
```

![](README_copy_files/figure-gfm/unnamed-chunk-18-3.png)<!-- -->

``` r
plot(msd.gam.trimmed, sel=2, shade=T, ylab="Effect on mean stability", xlab="Duration of influence", main="Long-term influence", col='dodgerblue'); abline(h=0, lty=2, col="red")
```

![](README_copy_files/figure-gfm/unnamed-chunk-18-4.png)<!-- -->

``` r
plot(msd.gam.trimmed, sel=3, shade=T, ylab="Effect on mean stability", xlab="Duration of influence", main="Short-term influence", col='dodgerblue'); abline(h=0, lty=2, col="red")
```

![](README_copy_files/figure-gfm/unnamed-chunk-18-5.png)<!-- -->

``` r
# (dotted lines indicate error)
plot(msd.gam.trimmed, all.terms=T, sel=4, ylab="Effect on mean stability", xlab="Duration group", main="Main effect of duration group")
```

![](README_copy_files/figure-gfm/unnamed-chunk-18-6.png)<!-- -->

``` r
# checking out the model performance
qqnorm(resid(msd.gam.trimmed)); qqline(resid(msd.gam.trimmed)) # meh
```

![](README_copy_files/figure-gfm/unnamed-chunk-18-7.png)<!-- -->

Removing the two creoles with the lowest scores produces significant
effects. Doesn’t seem very reliable, especially given the small sample
size. Also, the pattern is strange; a negative trend of duration for
long-term influence and a positive one for short-term influence? Also
note that the model detected a mean difference between duration groups,
with the short group having (slightly) lower mean stability. This
appears to be the case but – again – so few observations.

## Duration efects on the segment level

Does duration affect the stability values of specific segments or
segment classes?

``` r
ggplot(database, aes(duration, MannerStability, colour=duration_group)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~Class)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 18 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 18 rows containing missing values (`geom_point()`).

![](README_copy_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
ggplot(database, aes(duration, GlobalStability, colour=duration_group)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~Class)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 18 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 18 rows containing missing values (`geom_point()`).

![](README_copy_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
ggplot(database, aes(duration, PlaceStability, colour=duration_group)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  facet_wrap(~LexifierPhoneme)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 18 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 18 rows containing missing values (`geom_point()`).

![](README_copy_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
ggplot(database, aes(duration, MannerStability, colour=duration_group)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~LexifierPhoneme)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 18 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 18 rows containing missing values (`geom_point()`).

![](README_copy_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
ggplot(database, aes(duration, GlobalStability, colour=duration_group)) + 
  geom_point() + 
  geom_smooth() +
  facet_wrap(~LexifierPhoneme)
```

    ## `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

    ## Warning: Removed 18 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 18 rows containing missing values (`geom_point()`).

![](README_copy_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

# Segment stability

Which segments are the most stable across creoles in the language
sample?

We calculate stability of place and manner for each phoneme.

``` r
place_results <- database %>% group_by(LexifierPhoneme) %>% summarize(mplace = mean(PlaceStability, na.rm = TRUE))
manner_results <- database %>% group_by(LexifierPhoneme) %>% summarize(mmanner = mean(MannerStability, na.rm = TRUE))

consonant_stability <- left_join(place_results, manner_results, by = "LexifierPhoneme")

class <- c("stop", "stop", "fricative","stop", "stop", "lateral","nasal", "nasal", "stop", "rhotic","fricative", "stop","affricate", "fricative", "fricative","nasal", "rhotic", "lateral", "fricative")

consonant_stability_class <- cbind(consonant_stability, class)
```

Next, we plot the results.

``` r
# TODO: fix the class "dots"
ggplot(consonant_stability, aes(y = mmanner, x = mplace, label = LexifierPhoneme, color=class)) +
  geom_point(position= "dodge") + 
  geom_text(aes(label=LexifierPhoneme), hjust=3, vjust=0) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
```

    ## Warning: Width not defined
    ## ℹ Set with `position_dodge(width = ...)`

![](README_copy_files/figure-gfm/unnamed-chunk-25-1.png)<!-- --> A
linear model to assess the relationship between manner and place
stability

``` r
lm_manner_place <- lm(mplace~mmanner, data=consonant_stability)

summary(lm_manner_place)
```

    ## 
    ## Call:
    ## lm(formula = mplace ~ mmanner, data = consonant_stability)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.206903 -0.000738  0.013372  0.037250  0.126426 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  0.28293    0.06764   4.183 0.000624 ***
    ## mmanner      0.70370    0.07873   8.938 7.81e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07783 on 17 degrees of freedom
    ## Multiple R-squared:  0.8246, Adjusted R-squared:  0.8142 
    ## F-statistic:  79.9 on 1 and 17 DF,  p-value: 7.811e-08

``` r
manner_place_lmplot <- ggplot(consonant_stability, aes(y = mmanner, x = mplace, label = LexifierPhoneme)) +
  geom_point(position= "dodge") + 
  geom_smooth(method = lm)+
  geom_text(aes(label=LexifierPhoneme), hjust=3, vjust=0)

print(manner_place_lmplot + labs(y = "Manner Stability", x = "Place Stability"))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

    ## Warning: Width not defined
    ## ℹ Set with `position_dodge(width = ...)`

![](README_copy_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

Here is an alternative view for the global results.

``` r
consonant_global_stability <- mutate(consonant_stability_class, mglobal = (mmanner + mplace)/2)

ggplot(consonant_global_stability) + 
  geom_bar(aes(x = mglobal, 
               y = reorder(LexifierPhoneme, mglobal),
               fill = class), stat = "identity", show.legend = TRUE) +
  labs(x = "Stability score", y = "Phoneme", fill = "Manner")
```

![](README_copy_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

Check for class effects on the global stability of consonants

``` r
ggplot(consonant_global_stability, aes(x = class, y = mglobal, fill = class)) +
  geom_smooth(method = "lm") +
  geom_violin() +
   geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Groups with fewer than two data points have been dropped.

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](README_copy_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

Now, just plotting the relation manner to manner

``` r
ggplot(consonant_global_stability, aes(x = class, y = mmanner, fill = class)) +
  geom_smooth(method = "lm") +
  geom_violin() +
   geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  theme(legend.position="none") +
 stat_summary(fun.y=mean, geom="point", size=2, shape=16)
```

    ## Warning: The `fun.y` argument of `stat_summary()` is deprecated as of ggplot2 3.3.0.
    ## ℹ Please use the `fun` argument instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Groups with fewer than two data points have been dropped.

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](README_copy_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

Check place effects on the global stability of the consonants

``` r
place <- c("labial", "alveolar", "labiodental", "velar", "velar", "alveolar", "labial", "alveolar", "labial", "alveolar", "alveolar", "alveolar", "palatal", "labiodental", "alveolar", "palatal", "alveolar", "palatal", "palatal")

consonant_stability_place <- cbind(consonant_global_stability, place)

ggplot(consonant_stability_place, aes(x = place, y = mglobal, fill = place)) +
  geom_smooth(method = "lm") +
  geom_violin()  +
   geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) 
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning in regularize.values(x, y, ties, missing(ties), na.rm = na.rm):
    ## collapsing to unique 'x' values

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](README_copy_files/figure-gfm/unnamed-chunk-30-1.png)<!-- --> Now,
just with the mean for Place stability

``` r
ggplot(consonant_stability_place, aes(x = place, y = mplace, fill = place)) +
  geom_smooth(method = "lm") +
  geom_violin()  +
   geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5)  +
  theme(legend.position="none")# +
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](README_copy_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

``` r
# stat_summary(fun.y=mean, geom="point", size=2, shape=16)
```

Calculate the stability of the segments.

``` r
# qplot(x = duration, y = MeanStability, data = consonant_global_stability, color = duration_group) +
#  geom_smooth(method = "lm") 
```

And we can also increase the number of observations in duration
regression by running the analysis at the segment level, rather than on
mean stability.

``` r
library(lmerTest)
```

    ## Loading required package: lme4

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

    ## 
    ## Attaching package: 'lme4'

    ## The following object is masked from 'package:nlme':
    ## 
    ##     lmList

    ## 
    ## Attaching package: 'lmerTest'

    ## The following object is masked from 'package:lme4':
    ## 
    ##     lmer

    ## The following object is masked from 'package:stats':
    ## 
    ##     step

``` r
# Factorizing
mod.db = database %>%
              as.data.frame() %>%
              mutate(categorical_stability = as.factor(categorical_stability),
              Lexifier = as.factor(Lexifier),
              CreolePhoneme = as.factor(CreolePhoneme),
              Language = as.factor(Language))

# Remove singletons/doubletons
#goodies = names(table(mod.db$CreolePhoneme)>2)

#mod.db = mod.db %>%
#         filter(CreolePhoneme %in% goodies)
```

``` r
plot(mod.db$categorical_stability, mod.db$duration, notch=T)
```

    ## Warning in (function (z, notch = FALSE, width = NULL, varwidth = FALSE, : some
    ## notches went outside hinges ('box'): maybe set notch=FALSE

![](README_copy_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

Hugely skewed in favor of no manner/place (10X as frequent as the next
most frequent level; this could cause problems for the models).

``` r
table(mod.db$categorical_stability)
```

    ## 
    ##    manner/no place       manner/place no manner/no place    no manner/place 
    ##                 49                 58                553                 25

``` r
# Place stability
cat.mod.place = glmer(PlaceStability ~ log(duration) + (1|CreolePhoneme), data=mod.db, family="binomial")

summary(cat.mod.place)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: PlaceStability ~ log(duration) + (1 | CreolePhoneme)
    ##    Data: mod.db
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    305.4    319.0   -149.7    299.4      682 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.3810  0.0327  0.0341  0.1593  2.3669 
    ## 
    ## Random effects:
    ##  Groups        Name        Variance Std.Dev.
    ##  CreolePhoneme (Intercept) 34.8     5.899   
    ## Number of obs: 685, groups:  CreolePhoneme, 34
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)  
    ## (Intercept)     5.5541     3.0094   1.846   0.0649 .
    ## log(duration)  -0.0260     0.2695  -0.096   0.9231  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## log(duratn) -0.560

``` r
# Manner stability
cat.mod.manner = glmer(MannerStability ~ log(duration) + (1|CreolePhoneme), data=mod.db, family="binomial")

summary(cat.mod.manner)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace
    ##   Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: MannerStability ~ log(duration) + (1 | CreolePhoneme)
    ##    Data: mod.db
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    258.1    271.7   -126.0    252.1      682 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -6.8056  0.0038  0.0044  0.1764  1.0106 
    ## 
    ## Random effects:
    ##  Groups        Name        Variance Std.Dev.
    ##  CreolePhoneme (Intercept) 406.8    20.17   
    ## Number of obs: 685, groups:  CreolePhoneme, 34
    ## 
    ## Fixed effects:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)     8.6821     2.5407   3.417 0.000633 ***
    ## log(duration)   0.3547     0.2668   1.329 0.183708    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr)
    ## log(duratn) -0.576

``` r
# Duration group
cat.mod.group = glmer(as.factor(duration_group) ~ PlaceStability + MannerStability + (1|CreolePhoneme), data=mod.db, family="binomial", nAGQ=0)
```

    ## boundary (singular) fit: see help('isSingular')

``` r
summary(cat.mod.group)
```

    ## Generalized linear mixed model fit by maximum likelihood (Adaptive
    ##   Gauss-Hermite Quadrature, nAGQ = 0) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: as.factor(duration_group) ~ PlaceStability + MannerStability +  
    ##     (1 | CreolePhoneme)
    ##    Data: mod.db
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##    858.0    876.1   -425.0    850.0      681 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -0.7519 -0.6699 -0.6699  1.4927  1.6530 
    ## 
    ## Random effects:
    ##  Groups        Name        Variance Std.Dev.
    ##  CreolePhoneme (Intercept) 0        0       
    ## Number of obs: 685, groups:  CreolePhoneme, 34
    ## 
    ## Fixed effects:
    ##                 Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)      -0.7743     0.2512  -3.083  0.00205 **
    ## PlaceStability    0.2040     0.3064   0.666  0.50549   
    ## MannerStability  -0.2310     0.2688  -0.859  0.39017   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) PlcStb
    ## PlaceStblty -0.579       
    ## MannrStblty -0.301 -0.554
    ## optimizer (bobyqa) convergence code: 0 (OK)
    ## boundary (singular) fit: see help('isSingular')

Some indication that place stability is more often associated with
shorter periods of influence.

Numerically, the manner/place category has 50% of its observations in
the longest durations from the sample. At the same time, no manner/no
place is associated with the shortest durations.

# Word position

Next we ask, does word position influence stability?

First, data preparation.

``` r
data_by_position <- database %>% select(Position, LexifierPhoneme, PlaceStability, MannerStability) %>% mutate(Position = tolower(Position))

data_by_position$PlaceStability <- as.numeric(data_by_position$PlaceStability)

data_by_position$MannerStability <- as.numeric(data_by_position$MannerStability)
```

Next, calculate stability for each segment according to its word
position.

``` r
position_stability <- mutate(data_by_position, GlobalStability = (PlaceStability + MannerStability)/2)

position_results <- position_stability %>% group_by(LexifierPhoneme, Position) %>% summarize(m = mean(GlobalStability, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'LexifierPhoneme'. You can override using
    ## the `.groups` argument.

And plot the results for all segments.

``` r
position_results$Position <- factor(position_results$Position, levels = c('word-initial', 'word-medial', 'word-final'))

ggplot(position_results, aes(x = LexifierPhoneme, y = m, fill = Position)) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
```

![](README_copy_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

Flip horizontally.

``` r
ggplot(position_results) + 
  geom_bar(aes(x = m,
               y = reorder(LexifierPhoneme, m),
               fill = Position), 
           stat = "identity", 
           show.legend = TRUE,
           position = "dodge2") +
  labs(x = "Stability score", y = "Phoneme", fill = "Position")
```

![](README_copy_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

Plot the results for segments that show differences.

``` r
# TODO: update what we show, flip horizontally, change colors
position_results1 <- position_results %>% pivot_wider(names_from = Position, values_from = m)

different_position <- subset(position_results1, position_results1$`word-initial` != position_results1$`word-medial` | position_results1$`word-final` != position_results1$`word-medial`)

different_position_results <- different_position %>% pivot_longer(c(`word-initial`, `word-medial`, `word-final`), names_to = "Position", values_to = "m")

different_position_results$Position <- factor(different_position_results$Position, levels = c('word-initial', 'word-medial', 'word-final'))

ggplot(different_position_results, 
       aes(x = LexifierPhoneme, y = m, fill = Position)) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))
```

    ## Warning: Removed 8 rows containing missing values (`geom_col()`).

![](README_copy_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

Flip horizontally.

``` r
# TODO: reorder doesn't work here
ggplot(different_position_results) + 
  geom_bar(aes(x = m,
               y = reorder(LexifierPhoneme, m),
               fill = Position), 
           stat = "identity", 
           show.legend = TRUE,
           position = "dodge2") +
  labs(x = "Stability score", y = "Phoneme", fill = "Position")
```

    ## Warning: Removed 8 rows containing missing values (`geom_bar()`).

![](README_copy_files/figure-gfm/unnamed-chunk-41-1.png)<!-- -->

# Conditions of contact

The finding that “slavery has a negative impact on stability” was mainly
observational and also literature-based (e.g. Faraclas et al. 2007;
Carvalho & Lucchesi 2016; Upper Guinea light creoles = slavery but with
lighter contact conditions vs Gulf of Guinea hard creole = slavery and
harder contact conditions).

Test whether there’s a relation between type of contact situation and
overall mean stability.

Linear model

``` r
m <- lm(MeanStability ~ ContactConditions, data=creole_stability)
summary(m)
```

    ## 
    ## Call:
    ## lm(formula = MeanStability ~ ContactConditions, data = creole_stability)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.19165 -0.01508  0.01495  0.05113  0.10001 
    ## 
    ## Coefficients:
    ##                          Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               0.88505    0.02896  30.562 2.68e-16 ***
    ## ContactConditionsSlavery -0.04062    0.03806  -1.067    0.301    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08191 on 17 degrees of freedom
    ## Multiple R-squared:  0.06279,    Adjusted R-squared:  0.007662 
    ## F-statistic: 1.139 on 1 and 17 DF,  p-value: 0.3008

``` r
ggplot(creole_stability, aes(x = ContactConditions, y = MeanStability, fill = ContactConditions)) +
  geom_smooth(method = "lm") +
  geom_violin()  +  
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5)  +
  theme(legend.position="none")
```

    ## `geom_smooth()` using formula = 'y ~ x'
    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](README_copy_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

``` r
m <- lm(MeanStability ~ duration + ContactConditions * duration, data=creole_stability)
summary(m)
```

    ## 
    ## Call:
    ## lm(formula = MeanStability ~ duration + ContactConditions * duration, 
    ##     data = creole_stability)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.20118 -0.02286  0.01146  0.05202  0.11796 
    ## 
    ## Coefficients:
    ##                                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                        8.686e-01  6.762e-02  12.845  1.7e-09 ***
    ## duration                           6.236e-05  2.314e-04   0.269    0.791    
    ## ContactConditionsSlavery          -9.622e-02  9.367e-02  -1.027    0.321    
    ## duration:ContactConditionsSlavery  1.295e-04  2.809e-04   0.461    0.651    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08308 on 15 degrees of freedom
    ## Multiple R-squared:  0.1492, Adjusted R-squared:  -0.02095 
    ## F-statistic: 0.8769 on 3 and 15 DF,  p-value: 0.4751

``` r
ggplot(creole_stability, aes(x = duration, y = MeanStability, color = ContactConditions)) +
  geom_smooth(method = "lm") +
  geom_point()
```

    ## `geom_smooth()` using formula = 'y ~ x'

![](README_copy_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-R" class="csl-entry">

RStudio Team. 2020. *RStudio: Integrated Development Environment for r*.
Boston, MA: RStudio, PBC. <http://www.rstudio.com/>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-knitr" class="csl-entry">

Xie, Yihui. 2021. *Knitr: A General-Purpose Package for Dynamic Report
Generation in r*. <https://yihui.org/knitr/>.

</div>

</div>
