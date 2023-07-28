Inventory size, similarity and dissimilarity
================
Carlos Silva
(28 julho, 2023)

- [Introduction](#introduction)
- [Inventory size](#inventory-size)
- [Consonant frequency in all languages
  involved](#consonant-frequency-in-all-languages-involved)
  - [Is there a relationship between this frequency and the stability
    values?](#is-there-a-relationship-between-this-frequency-and-the-stability-values)
  - [Is there a relationship between consonant stability and their
    presence in the inventories of the substrate
    languages?](#is-there-a-relationship-between-consonant-stability-and-their-presence-in-the-inventories-of-the-substrate-languages)
- [Inventory similarity and overall creole
  stability](#inventory-similarity-and-overall-creole-stability)
  - [Does substrate similarity influence creole
    stability?](#does-substrate-similarity-influence-creole-stability)
- [Jaccard distance](#jaccard-distance)

# Introduction

Load packages

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggplot2)
```

Get data

``` r
inv <- read.csv("Inventories.csv")
head(inv)
```

    ##   ID   Language Category Phoneme Notes              Source PhoibleID
    ## 1 19 Portuguese Lexifier       p       Castro2013[242-248]        NA
    ## 2 19 Portuguese Lexifier       b       Castro2013[242-248]        NA
    ## 3 19 Portuguese Lexifier       t       Castro2013[242-248]        NA
    ## 4 19 Portuguese Lexifier       d       Castro2013[242-248]        NA
    ## 5 19 Portuguese Lexifier       k       Castro2013[242-248]        NA
    ## 6 19 Portuguese Lexifier       g       Castro2013[242-248]        NA

Prepare data

``` r
df_inv_long <- inv %>% select(Language, Phoneme) %>%  mutate(newcol = 1)

df_inv <- df_inv_long %>% pivot_wider(names_from = Language, values_from = newcol, values_fill = 0)

df_total_inv <- df_inv %>% mutate_at(c(2:35), as.numeric)

head(df_total_inv)
```

    ## # A tibble: 6 × 35
    ##   Phoneme Portuguese `Timor Pidgin` `Papia Kristang` `Patua Macau` Tetum
    ##   <chr>        <dbl>          <dbl>            <dbl>         <dbl> <dbl>
    ## 1 p                1              1                1             1     0
    ## 2 b                1              1                1             1     1
    ## 3 t                1              1                0             1     1
    ## 4 d                1              1                1             1     1
    ## 5 k                1              1                1             1     1
    ## 6 g                1              1                1             1     0
    ## # ℹ 29 more variables: `Larantuka Malay` <dbl>, `Standard Malay` <dbl>,
    ## #   `Hokkien Chinese` <dbl>, Cantonese <dbl>, Malayalam <dbl>,
    ## #   `Sri Lanka Portuguese` <dbl>, Diu <dbl>, Daman <dbl>, Korlai <dbl>,
    ## #   Kannur <dbl>, Sinhala <dbl>, Tamil <dbl>, Gujarati <dbl>, Marathi <dbl>,
    ## #   Santome <dbl>, Principense <dbl>, Angolar <dbl>, `Fa d'Ambo` <dbl>,
    ## #   `Cape Verdean Brava` <dbl>, `Cape Verdean Sao Vicente` <dbl>,
    ## #   `Cape Verdean Santo Antao` <dbl>, `Cape Verdean Fogo` <dbl>, …

# Inventory size

Get the consonant inventory size for all languages

``` r
cons_count <- df_total_inv %>% select(c(2:35)) %>% mutate_at(c(1:34), as.numeric)
  
count <- colSums(cons_count [,c(1:34)]) %>% unname(colSums(count))

cons_lg <- select(df_inv_long, 'Language') 

Language <- unique(cons_lg$Language)

category <- inv %>% select(Language, Category)

count_lg <- data.frame(cbind(Language, count)) 
  
count_lg_1 <- inner_join(count_lg, category, by = "Language") %>% distinct()

inv_size <- transform(count_lg_1, count = as.numeric(count))

head(inv_size)
```

    ##          Language count  Category
    ## 1      Portuguese    19  Lexifier
    ## 2    Timor Pidgin    22    creole
    ## 3  Papia Kristang    20    creole
    ## 4     Patua Macau    22    creole
    ## 5           Tetum    13 substrate
    ## 6 Larantuka Malay    19 substrate

Which languages have bigger inventories?

``` r
ggplot(inv_size) + 
  geom_bar(aes(x = Language,
               y = count,
              fill = Category), 
           stat = "identity", 
           show.legend = TRUE,
           position = "dodge2") + coord_flip()
```

![](inventories_correlations_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Violin plot: the majority of creoles have larger consonant inventories
than Portuguese.

``` r
ggplot(inv_size, aes(x = Category, y = count, fill = Category)) +
  geom_violin(alpha = 0.5) +
  geom_dotplot(binaxis = "y",
               stackdir = "center",
               dotsize = 0.5) +
  theme(legend.position = "none")
```

    ## Warning: Groups with fewer than two data points have been dropped.

    ## Bin width defaults to 1/30 of the range of the data. Pick better value with
    ## `binwidth`.

![](inventories_correlations_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Is there a relationship between the inventory size of creoles and the
inventory size of substrates?

NO ANSWER YET

# Consonant frequency in all languages involved

Count frequent consonants

``` r
total <- rowSums(cons_count)

cons_freq <- data.frame(cbind(df_total_inv$Phoneme, total))

cons_freq <- transform(cons_freq, total = as.numeric(total))
```

Which consonants are more frequent across all language involved?

Maybe, it makes sense to subset the frequency of consonants in
substrates only and to consonants that exist in the lexifier.

``` r
cons_freq_1 <- subset(cons_freq, total > 5) #trim low values

ggplot(cons_freq_1) + 
  geom_bar(aes(x = V1,
               y = total,
               ), 
           stat = "identity", 
           show.legend = TRUE,
           position = "dodge2") + coord_flip()
```

![](inventories_correlations_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

## Is there a relationship between this frequency and the stability values?

Subset: Portuguese consonants only

``` r
cons_freq_pt <- cons_freq %>% subset(V1 %in% c('b','d','f','g','k','l','ʎ','m','n','ɲ','p','t̠ʃ','ɾ','s',
                                       't','v','z','ʒ', 'r'))
```

First dataset: relative frequency values

``` r
cons_freq_rel <- cons_freq_pt %>% mutate(frequency = total/34)
```

Second dataset:stability values

``` r
stability <- c(1.0000000    ,
1.0000000   ,
1.0000000   ,
1.0000000   ,
1.0000000   ,
0.9861111   ,
0.9722222   ,
0.9722222   ,
0.9722222   ,
0.9565217   ,
0.9444444   ,
0.9433962   ,
0.7916667   ,
0.7714286   ,
0.7361111   ,
0.6944444   ,
0.5151515   ,
0.4722222   ,
0.3888889   
)
V1 <- c("t",
               "p",
               "n",
               "m",
               "f",
               "b",
               "k",
               "g",
               "d",
               "z",
               "s",
               "l",
               "r",
               "t̠ʃ",
               "ɾ",
               "ɲ",
               "ʒ",
               "ʎ",
               "v")
stability_consonants <- data.frame(V1, stability)
```

Merge datasets

``` r
cor_freq_sta <- left_join(stability_consonants, cons_freq_rel, by='V1')
```

Results of a simple regression

``` r
fs <- lm(frequency ~ stability, data=cor_freq_sta)
summary(fs)
```

    ## 
    ## Call:
    ## lm(formula = frequency ~ stability, data = cor_freq_sta)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.290793 -0.067446  0.006424  0.115944  0.232360 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.07132    0.16404  -0.435 0.669184    
    ## stability    0.94725    0.18849   5.025 0.000104 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1598 on 17 degrees of freedom
    ## Multiple R-squared:  0.5977, Adjusted R-squared:  0.574 
    ## F-statistic: 25.25 on 1 and 17 DF,  p-value: 0.0001039

Plot the results

``` r
ggplot(cor_freq_sta, aes(x = frequency, y = stability, label = V1)) +
  geom_smooth(method = "lm") +
  geom_point() +
  geom_text(aes(label=V1), hjust=3, vjust=0)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

![](inventories_correlations_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

There relationship between stability and frequency across all languages
involved. But does it make sense? We are measuring the consonants in all
categories and the present of the lexifier and the creoles may influence
the results. Perhaps, we should try to subset and measure the frequency
across substrates only, but I think that this procedure would just
increase the lack of correlation.

## Is there a relationship between consonant stability and their presence in the inventories of the substrate languages?

Data preparation (substrates only)

``` r
inv_subs <- inv %>% subset(Category == 'substrate') %>%  select(Language, Phoneme)  %>% mutate(newcol = 1)

inv_subs_long <- inv_subs %>% pivot_wider(names_from = Language, values_from = newcol, values_fill = 0)

inv_subs_long <- inv_subs_long %>% mutate_at(c(2:17), as.numeric)

head(inv_subs_long)
```

    ## # A tibble: 6 × 17
    ##   Phoneme Tetum `Larantuka Malay` `Standard Malay` `Hokkien Chinese` Cantonese
    ##   <chr>   <dbl>             <dbl>            <dbl>             <dbl>     <dbl>
    ## 1 m           1                 1                1                 1         1
    ## 2 k           1                 1                1                 1         1
    ## 3 w           1                 1                1                 1         1
    ## 4 n           1                 1                1                 1         1
    ## 5 t           1                 1                1                 1         1
    ## 6 l           1                 1                1                 1         1
    ## # ℹ 11 more variables: Malayalam <dbl>, Sinhala <dbl>, Tamil <dbl>,
    ## #   Gujarati <dbl>, Marathi <dbl>, Edo <dbl>, Kikongo <dbl>, Kimbundu <dbl>,
    ## #   Wolof <dbl>, Temne <dbl>, Mandinka <dbl>

Sum row values and subset to consonants wich have correspondents in
Portuguese

``` r
subs_count <- inv_subs_long %>% select(c(2:17))

total_subs <- rowSums(subs_count)

subs_freq <- data.frame(cbind(inv_subs_long$Phoneme, total_subs))

subs_freq <- transform(subs_freq, total_subs = as.numeric(total_subs))

subs_freq <- subs_freq %>% subset(V1 %in% c('b','d','f','g','k','l','ʎ','m','n','ɲ','p','t̠ʃ','ɾ','s', 't','v','z','ʒ', 'r'))
```

Get relative values

``` r
subs_freq_rel <- subs_freq %>% mutate(frequency = total_subs/16)
```

Merge datasets

``` r
subs_sta <- left_join(stability_consonants, subs_freq_rel, by='V1')
```

Results of a simple regression

``` r
subs_sta_lm <- lm(frequency ~ stability, data=subs_sta)
summary(subs_sta_lm)
```

    ## 
    ## Call:
    ## lm(formula = frequency ~ stability, data = subs_sta)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.67791 -0.12402  0.02877  0.18209  0.29284 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  -0.3810     0.3207  -1.188  0.25217   
    ## stability     1.1534     0.3615   3.191  0.00569 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2728 on 16 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.3889, Adjusted R-squared:  0.3507 
    ## F-statistic: 10.18 on 1 and 16 DF,  p-value: 0.005685

Plot the results

``` r
ggplot(subs_sta, aes(x = frequency, y = stability, label = V1)) +
  geom_smooth(method = "lm") +
  geom_point() +
  geom_text(aes(label=V1), hjust=3, vjust=0)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).

    ## Warning: The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).

    ## Warning: Removed 1 rows containing missing values (`geom_text()`).

![](inventories_correlations_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

When we consider the correlation between the consonant stability in
creoles and the frequency of these consonants in the substrates only, we
find that there is a weaker correlation (if we compare with the results
above). However, this correlation is statistically significant (p-value:
0.005685). Nothing the outliers which are normally voiced consonants.

# Inventory similarity and overall creole stability

Getting just the consonants which have correspondents in Portuguese

``` r
inv_overlap <- df_total_inv %>% subset(Phoneme %in% c('b','d','f','g','k','l','ʎ','m','n','ɲ','p','ʀ','ɾ','s','ʃ',
                                       't','v','z','ʒ'))
```

Which languages have more have more consonants that overlap with
Portuguese consonants

``` r
cons_over <- inv_overlap %>% select(c(2:35)) %>% mutate_at(c(1:34), as.numeric)
  
absolute_overlap <- colSums(cons_over [,c(1:34)]) %>% unname(colSums(count))

cons_overlap <- select(df_inv_long, 'Language') 

Language <- unique(cons_lg$Language)

category <- inv %>% select(Language, Category)

overlap <- data.frame(cbind(Language, absolute_overlap)) 
  
inv_overlap <- inner_join(overlap, category, by = "Language") %>% distinct()

head(inv_overlap)
```

    ##          Language absolute_overlap  Category
    ## 1      Portuguese               19  Lexifier
    ## 2    Timor Pidgin               16    creole
    ## 3  Papia Kristang               13    creole
    ## 4     Patua Macau               16    creole
    ## 5           Tetum                9 substrate
    ## 6 Larantuka Malay               11 substrate

Adding a new variable: inventory dissimilarity

``` r
colnames(inv_overlap)[2]  <- "similar_consonants"
colnames(inv_size)[2] <- "total_consonants"


df_st <- merge(inv_overlap, inv_size, by = "Language")

df_st$total_consonants = as.numeric(as.character(df_st$total_consonants))
df_st$similar_consonants = as.numeric(as.character(df_st$similar_consonants))


df_st$different_consonants <- df_st$`total_consonants` - df_st$`similar_consonants`
```

Transforming values from absolute to relative

``` r
tsd_rel <- df_st %>%  mutate(total_rel = total_consonants/19, similar_rel = similar_consonants/19, diff_rel= different_consonants/19)
```

### Does substrate similarity influence creole stability?

If yes, we expect to find a positive correlation overall creole
stability ~ substrate inventory similarity, and a negative correlation
overall creole stability ~ substrate inventory divergence.

Let’s just subset the data to substrates only.

``` r
subs_tsd_rel <- tsd_rel %>% subset(Category.x=='substrate')
```

Prepare data to get the correlation between creole stability and
substrate similarity

``` r
Language <- c('Angolar',
'Cape Verdean Brava',
'Cape Verdean Fogo',
'Cape Verdean Santiago',
'Cape Verdean Santo Antão',
'Cape Verdean São Vicente',
'Daman',
'Diu',
'Fa dAmbô',
'Guinea-Bissau Kriyol',
'Kannur',
'Korlai',
'Papiá Kristang',
'Patua Macau',
'Principense',
'Santome',
'Sri Lanka',
'Timor creole')

value <- c(0.6571429    ,
0.9285714   ,
0.8857143   ,
0.9189189   ,
0.9444444   ,
0.9142857   ,
0.9000000   ,
0.8857143   ,
0.6428571   ,
0.9054054   ,
0.9242424   ,
0.8529412   ,
0.8750000   ,
0.9054054   ,
0.8243243   ,
0.7702703   ,
0.8970588   ,
0.9000000   
)

creole_stability_simple <- data.frame(Language, value)
```

``` r
substrate_similar_simple <- subs_tsd_rel %>%  select(Language, similar_rel)

colnames(substrate_similar_simple)[2]  <- "value"
```

Let’s import a new dataframe to get language pairs

``` r
correspondence <- read.csv("substrate_creole_correspondence.csv")
```

Join stability and similarity values to the new data frame

``` r
colnames(creole_stability_simple)[1]  <- "creole"
colnames(creole_stability_simple)[2]  <- "stability"

cor1 <- left_join(correspondence, creole_stability_simple, by= 'creole')

colnames(substrate_similar_simple)[1]  <- "substrate"
colnames(substrate_similar_simple)[2]  <- "similarity"

cor2 <- left_join(cor1, substrate_similar_simple, by= 'substrate')
```

Results of a simple regression

``` r
cor2_lm <- lm(similarity ~ stability, data=cor2)
summary(cor2_lm)
```

    ## 
    ## Call:
    ## lm(formula = similarity ~ stability, data = cor2)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.185015 -0.049486  0.008145  0.054483  0.168110 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.0620     0.1133   9.371 3.42e-11 ***
    ## stability    -0.5345     0.1326  -4.032 0.000275 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07956 on 36 degrees of freedom
    ## Multiple R-squared:  0.3111, Adjusted R-squared:  0.292 
    ## F-statistic: 16.26 on 1 and 36 DF,  p-value: 0.0002746

Plot the results

``` r
ggplot(cor2, aes(x = similarity, y = stability, label = pair)) +
  geom_smooth(method = "glm") +
  geom_point() +
  geom_text(aes(label=pair), hjust=1, vjust=1) +
  facet_wrap(~ area)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?
    ## The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?
    ## The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?
    ## The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?
    ## The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

![](inventories_correlations_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

It seems that stability increases as substrate similarity increases in
the Indian creoles, but not in the other creoles, unless I am not
getting this right. It would be nice to have the pairs displayed as
labels, but I think that I would need to turn this graph into five
different graphs.

# Jaccard distance

Jaccard between two vectors manually selected

``` r
jaccard <- function(a, b) {
    intersection = length(intersect(a, b))
    union = length(a) + length(b) - intersection
    return (intersection/union)
}

#data <- df_total_inv

jaccard(df_total_inv$`Portuguese`, df_total_inv$`Cantonese`)
```

    ## [1] 0.00862069

VEGAN PACKAGE – Jaccard distance between all vectors at the same time

``` r
#install.packages("vegan")
library(vegan)
```

    ## Warning: package 'vegan' was built under R version 4.3.1

    ## Loading required package: permute

    ## Warning: package 'permute' was built under R version 4.3.1

    ## Loading required package: lattice

    ## This is vegan 2.6-4

``` r
df_jaccard <- select(df_total_inv, c(2:35))
results_jaccard <- vegdist(df_jaccard, method = "jaccard")
# I may have worked, but we still need a visualization.
```

ADE4 PACKAGE – Same as above, but with hierarchical clustering– still no
visualization. Also note that the results of this vector are different
from the results generated using the Vegan package

``` r
#install.packages("ade4")
library(ade4)
```

    ## Warning: package 'ade4' was built under R version 4.3.1

``` r
mm1 <- as.matrix(df_jaccard)
mm2 <- matrix(mm1, ncol = ncol(df_jaccard), dimnames = NULL)
ID <- colnames(df_jaccard)
d <- dist.binary(mm2, method = 1, diag = FALSE, upper = FALSE) #method 1 is Jaccard index (1901) S3 coefficient of Gower & Legendre
hc <- hclust(d)               # apply hierarchical clustering 
```

Trying again, but just with the Jaccard function applied to languages in
different cycles

``` r
jaccard <- function(a, b) {
    intersection = length(intersect(a, b))
    union = length(a) + length(b) - intersection
    return (intersection/union)
}


vec_port <- df_total_inv[2]
vec_jac <- c() 
vec_crio_name <- c() 


#for (i in 2:35)
#{jaq_val <- jaccard(vec_port,df_total_inv[i])
#   vec_jac <- c(vec_jac, jaq_val)
#   vec_crio_name <- c(vec_crio_name, colnames(df_total_inv)[i])}



#jac_data <- data.frame(vec_jac, vec_crio_name)
```
