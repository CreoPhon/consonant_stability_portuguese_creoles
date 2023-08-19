Inventory size, similarity and dissimilarity
================
Carlos Silva and Steven Moran
(17 agosto, 2023)

- [Introduction](#introduction)
- [Inventory size](#inventory-size)
- [Consonant frequency in all languages
  involved](#consonant-frequency-in-all-languages-involved)
  - [Is there a relationship between this frequency and the stability
    values?](#is-there-a-relationship-between-this-frequency-and-the-stability-values)
  - [Is there a relationship between consonant stability and their
    presence in the inventories of the substrate
    languages?](#is-there-a-relationship-between-consonant-stability-and-their-presence-in-the-inventories-of-the-substrate-languages)
- [Typological frequency vs. Substrate
  frequency](#typological-frequency-vs-substrate-frequency)
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
library(ggrepel)
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

df_total_inv <- df_inv %>% mutate_at(c(2:38), as.numeric)

head(df_total_inv)
```

    ## # A tibble: 6 × 38
    ##   Phoneme Portuguese `Timor Pidgin` `Papia Kristang` `Patua Macau` Tetum
    ##   <chr>        <dbl>          <dbl>            <dbl>         <dbl> <dbl>
    ## 1 p                1              1                1             1     0
    ## 2 b                1              1                1             1     1
    ## 3 t                1              1                0             1     1
    ## 4 d                1              1                1             1     1
    ## 5 k                1              1                1             1     1
    ## 6 g                1              1                1             1     0
    ## # ℹ 32 more variables: `Larantuka Malay` <dbl>, `Standard Malay` <dbl>,
    ## #   `Hokkien Chinese` <dbl>, Cantonese <dbl>, Malayalam <dbl>,
    ## #   `Sri Lanka Portuguese` <dbl>, Diu <dbl>, Daman <dbl>, Korlai <dbl>,
    ## #   Kannur <dbl>, Sinhala <dbl>, Tamil <dbl>, Gujarati <dbl>, Marathi <dbl>,
    ## #   Santome <dbl>, Principense <dbl>, Angolar <dbl>, `Fa d'Ambo` <dbl>,
    ## #   `Cape Verdean Brava` <dbl>, `Cape Verdean Sao Vicente` <dbl>,
    ## #   `Cape Verdean Santo Antao` <dbl>, `Cape Verdean Fogo` <dbl>, …

# Inventory size

Get the consonant inventory size for all languages

``` r
cons_count <- df_total_inv %>% select(c(2:38)) %>% mutate_at(c(1:37), as.numeric)
  
count <- colSums(cons_count [,c(1:37)]) %>% unname(colSums(count))

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

colnames(cons_freq)[1] <- "LexifierPhoneme"
```

## Is there a relationship between this frequency and the stability values?

Subset: Portuguese consonants only

``` r
cons_freq_pt <- cons_freq %>% subset(LexifierPhoneme %in% c('b','d','f','g','k','l','ʎ','m','n','ɲ','p','t̠ʃ','ɾ','s',
                                       't','v','z','ʒ', 'r'))
```

First dataset: relative frequency values

``` r
cons_freq_rel <- cons_freq_pt %>% mutate(frequency = total/37)
```

Second dataset:stability values

``` r
consonant_global_stability <- read.csv("consonant_global_stability.csv")
```

Merge datasets

``` r
cor_freq_sta <- left_join(consonant_global_stability, cons_freq_rel, by='LexifierPhoneme')
```

Results of a simple regression

``` r
fs <- lm(frequency ~ mglobal, data=cor_freq_sta)
summary(fs)
```

    ## 
    ## Call:
    ## lm(formula = frequency ~ mglobal, data = cor_freq_sta)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.29863 -0.07151  0.03980  0.11383  0.22911 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.1060     0.1642  -0.645    0.527    
    ## mglobal       0.9862     0.1887   5.226 6.85e-05 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1618 on 17 degrees of freedom
    ## Multiple R-squared:  0.6163, Adjusted R-squared:  0.5938 
    ## F-statistic: 27.31 on 1 and 17 DF,  p-value: 6.851e-05

Plot the results

``` r
fs_plot <- ggplot(cor_freq_sta, aes(x = frequency, y = mglobal, label = LexifierPhoneme)) +
  geom_smooth(method = "lm") +
  geom_point() # +
  #geom_text(aes(label=V1), hjust=3, vjust=0)

fs_plot + geom_text_repel(aes(label=LexifierPhoneme))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

![](inventories_correlations_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

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

inv_subs_long <- inv_subs_long %>% mutate_at(c(2:18), as.numeric)

head(inv_subs_long)
```

    ## # A tibble: 6 × 18
    ##   Phoneme Tetum `Larantuka Malay` `Standard Malay` `Hokkien Chinese` Cantonese
    ##   <chr>   <dbl>             <dbl>            <dbl>             <dbl>     <dbl>
    ## 1 m           1                 1                1                 1         1
    ## 2 k           1                 1                1                 1         1
    ## 3 w           1                 1                1                 1         1
    ## 4 n           1                 1                1                 1         1
    ## 5 t           1                 1                1                 1         1
    ## 6 l           1                 1                1                 1         1
    ## # ℹ 12 more variables: Malayalam <dbl>, Sinhala <dbl>, Tamil <dbl>,
    ## #   Gujarati <dbl>, Marathi <dbl>, Edo <dbl>, Kikongo <dbl>, Kimbundu <dbl>,
    ## #   Wolof <dbl>, Temne <dbl>, Mandinka <dbl>, Nyun <dbl>

Sum row values and subset to consonants which have correspondents in
Portuguese

``` r
subs_count <- inv_subs_long %>% select(c(2:18))

total_subs <- rowSums(subs_count)

subs_freq <- data.frame(cbind(inv_subs_long$Phoneme, total_subs))

subs_freq <- transform(subs_freq, total_subs = as.numeric(total_subs))

colnames(subs_freq)[1] <- "LexifierPhoneme"

subs_freq <- subs_freq %>% subset(LexifierPhoneme %in% c('b','d','f','g','k','l','ʎ','m','n','ɲ','p','t̠ʃ','ɾ','s', 't','v','z','ʒ', 'r'))
```

Get relative values

``` r
subs_freq_rel <- subs_freq %>% mutate(frequency = total_subs/17)
```

Merge datasets

``` r
subs_sta <- left_join(consonant_global_stability, subs_freq_rel, by='LexifierPhoneme')
```

Results of a simple regression

``` r
subs_sta_lm <- lm(frequency ~ mglobal, data=subs_sta)
summary(subs_sta_lm)
```

    ## 
    ## Call:
    ## lm(formula = frequency ~ mglobal, data = subs_sta)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.63261 -0.13468  0.05119  0.18579  0.28145 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  -0.3825     0.3091  -1.238  0.23374   
    ## mglobal       1.1634     0.3486   3.338  0.00418 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.2672 on 16 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.4105, Adjusted R-squared:  0.3736 
    ## F-statistic: 11.14 on 1 and 16 DF,  p-value: 0.004175

Plot the results

``` r
subsfreq_sta_cor <- ggplot(subs_sta, aes(x = frequency, y = mglobal, label = LexifierPhoneme)) +
  geom_smooth(method = "lm") +
  geom_point() +
  xlab("Frequency across substrates") + ylab("Stability across creoles") # +
 # geom_text(aes(label=V1), hjust=3, vjust=0)


subsfreq_sta_cor + geom_text_repel(aes(label=LexifierPhoneme))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).

    ## Warning: The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).

    ## Warning: Removed 1 rows containing missing values (`geom_text_repel()`).

![](inventories_correlations_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

When we consider the correlation between the consonant stability in
creoles and the frequency of these consonants in the substrates only, we
find that there is a weaker correlation (if we compare with the results
above). However, this correlation is statistically significant (p-value:
0.004). Nothing the outliers which are normally voiced consonants.

# Typological frequency vs. Substrate frequency

Typological frequency data

``` r
typ_freq <- read.csv("typ_freq.csv")
```

Merge datasets

``` r
#consonant_global_stability <- read.csv("consonant_global_stability.csv")

typ_sta <- left_join(typ_freq, consonant_global_stability, by='LexifierPhoneme')
```

Results of a simple regression

``` r
typ_sta_lm <- lm(TypologicalFreq ~ mglobal, data=typ_sta)
summary(typ_sta_lm)
```

    ## 
    ## Call:
    ## lm(formula = TypologicalFreq ~ mglobal, data = typ_sta)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.32614 -0.09953 -0.02402  0.09888  0.29370 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  -0.2975     0.1761  -1.689 0.110531    
    ## mglobal       0.9638     0.2035   4.737 0.000223 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1732 on 16 degrees of freedom
    ##   (1 observation deleted due to missingness)
    ## Multiple R-squared:  0.5838, Adjusted R-squared:  0.5577 
    ## F-statistic: 22.44 on 1 and 16 DF,  p-value: 0.0002233

Plot the results

``` r
typ_sta_cor <- ggplot(typ_sta, aes(x = TypologicalFreq, y = mglobal, label = LexifierPhoneme)) +
  geom_smooth(method = "lm") +
  geom_point() +
  xlab("Frequency across world's languages") + ylab("Stability across creoles") # +
 # geom_text(aes(label=V1), hjust=3, vjust=0)


typ_sta_cor + geom_text_repel(aes(label=LexifierPhoneme))
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 1 rows containing non-finite values (`stat_smooth()`).

    ## Warning: The following aesthetics were dropped during statistical transformation: label
    ## ℹ This can happen when ggplot fails to infer the correct grouping structure in
    ##   the data.
    ## ℹ Did you forget to specify a `group` aesthetic or to convert a numerical
    ##   variable into a factor?

    ## Warning: Removed 1 rows containing missing values (`geom_point()`).

    ## Warning: Removed 1 rows containing missing values (`geom_text_repel()`).

![](inventories_correlations_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

# Inventory similarity and overall creole stability

Getting just the consonants which have correspondents in Portuguese

``` r
inv_overlap <- df_total_inv %>% subset(Phoneme %in% c('b','d','f','g','k','l','ʎ','m','n','ɲ','p','ʀ','ɾ','s','ʃ',
                                       't','v','z','ʒ'))
```

Which languages have more have more consonants that overlap with
Portuguese consonants

``` r
cons_over <- inv_overlap %>% select(c(2:38)) %>% mutate_at(c(1:37), as.numeric)
  
absolute_overlap <- colSums(cons_over [,c(1:37)]) %>% unname(colSums(count))

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
'Timor creole',
'Casamancese')

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
0.9000000   ,
0.8750000)

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
    ## -0.190873 -0.033628  0.008124  0.043246  0.160412 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   1.1227     0.1605   6.996 5.07e-07 ***
    ## stability    -0.5988     0.1887  -3.173  0.00441 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08047 on 22 degrees of freedom
    ##   (15 observations deleted due to missingness)
    ## Multiple R-squared:  0.3139, Adjusted R-squared:  0.2827 
    ## F-statistic: 10.07 on 1 and 22 DF,  p-value: 0.004406

Plot the results

``` r
ggplot(cor2, aes(x = similarity, y = stability, label = pair)) +
  geom_smooth(method = "glm") +
  geom_point() +
  geom_text(aes(label=pair), hjust=1, vjust=1) +
  facet_wrap(~ area)
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 15 rows containing non-finite values (`stat_smooth()`).

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

    ## Warning: Removed 15 rows containing missing values (`geom_point()`).

    ## Warning: Removed 15 rows containing missing values (`geom_text()`).

![](inventories_correlations_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

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

\##Steven’s code

``` r
library(tidyverse)

# Create Jaccard table based on full set of phonemes across all languages 
# in the sample; other ways to do this -- to discuss
df <- read_csv("Inventories.csv")
```

    ## Rows: 815 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Language, Category, Phoneme, Notes, Source
    ## dbl (2): ID, PhoibleID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
df <- df %>% select(Language, Phoneme)
df$presence <- 1
df_wide <- df %>% spread(Phoneme, presence)
df_wide <- df_wide %>% replace(is.na(.), 0)
head(df_wide)
```

    ## # A tibble: 6 × 116
    ##   Language         b   `b̤`    bː    bʰ  `b̤ʱ`     c     ç    cç   cçʰ    cʰ     d
    ##   <chr>        <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Angolar          1     0     0     0     0     0     0     0     0     0     1
    ## 2 Cantonese        0     0     0     0     0     0     0     0     0     0     0
    ## 3 Cape Verdea…     1     0     0     0     0     0     0     0     0     0     1
    ## 4 Cape Verdea…     1     0     0     0     0     0     0     0     0     0     1
    ## 5 Cape Verdea…     1     0     0     0     0     0     0     0     0     0     1
    ## 6 Cape Verdea…     1     0     0     0     0     0     0     0     0     0     1
    ## # ℹ 104 more variables: `d̪` <dbl>, ð <dbl>, `d̪̤` <dbl>, dʰ <dbl>, `d̪̤ʱ` <dbl>,
    ## #   dz <dbl>, `d̤z` <dbl>, `d̠ʒ` <dbl>, `d̤ʒ̤ʱ` <dbl>, ɖ <dbl>, `ɖ̤` <dbl>,
    ## #   ɖʰ <dbl>, `ɖ̤ʱ` <dbl>, f <dbl>, g <dbl>, ɣ <dbl>, ɡ <dbl>, ɡb <dbl>,
    ## #   gʰ <dbl>, ɡʰ <dbl>, ɡʱ <dbl>, `ɡ̤ʱ` <dbl>, h <dbl>, ɦ <dbl>, j <dbl>,
    ## #   ɟ <dbl>, ɟː <dbl>, ɟʰ <dbl>, ɟʝ <dbl>, `ɟ̤ʝ` <dbl>, k <dbl>, kʰ <dbl>,
    ## #   kp <dbl>, kʷ <dbl>, kʷʰ <dbl>, l <dbl>, `l̤` <dbl>, ɭ <dbl>, lː <dbl>,
    ## #   m <dbl>, `m̤` <dbl>, mː <dbl>, mb <dbl>, ɱf <dbl>, mpʰ <dbl>, ɱv <dbl>, …

``` r
# Corresponding languages for which we want Jaccard measurements
correspondences <- read_csv("substrate_creole_correspondence.csv")
```

    ## Rows: 39 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): substrate, creole, pair, area
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
tmp <- correspondences %>% slice(1:2)

# DONE: fix the language names in the correspondences table so they can be used across tables
all(correspondences$substrate %in% df_wide$Language) # OK
```

    ## [1] TRUE

``` r
all(correspondences$creole %in% df_wide$Language) # OK
```

    ## [1] TRUE

``` r
correspondences$substrate %in% df_wide$Language # OK
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
correspondences$creole %in% df_wide$Language # OK
```

    ##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
# Get method for Jaccard
#library(qvalue) #Error in library(qvalue) : there is no package called ‘qvalue’
#library(jaccard) #Error: package ‘jaccard’ was built under R version 4.3.1Error: package or namespace load failed for ‘jaccard’ 

# Test 
 x <- as.numeric(df_wide[1,2:116])
 y <- as.numeric(df_wide[1,2:116])
 jaccard(x,y)
```

    ## [1] 0.00877193

``` r
 x==y # All TRUE -- passed!
```

    ##   [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [31] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [46] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [61] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [76] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ##  [91] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
    ## [106] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE

``` r
# Run Jaccard on test data -- some bug here in the code because new_col is the same
# tmp %>% mutate(new_col = jaccard(as.numeric(as.factor(df_wide %>% filter(Language == tmp$substrate) %>% select(2:116))), as.numeric(as.factor(df_wide %>% filter(Language == tmp$creole) %>% select(2:116)))))

# When all language names match in both tables this *should* run fine
# correspondences %>% mutate(new_col = jaccard(as.numeric(as.factor(df_wide %>% filter(Language == correspondences$substrate) %>% select(2:116))), as.numeric(as.factor(df_wide %>% filter(Language == correspondences$creole) %>% select(2:116)))))
```
