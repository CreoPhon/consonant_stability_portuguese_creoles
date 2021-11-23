Supplementary materials for: Consonant stability in Portuguese-based
creoles
================
Carlos Silva and Steven Moran
(23 November, 2021)

-   [Overview](#overview)
-   [Creole stability](#creole-stability)
-   [Segment stability](#segment-stability)
-   [Word position](#word-position)
-   [References](#references)

# Overview

Supplementary materials for [Consonant Stability in Portuguese-based
creoles](https://www.overleaf.com/project/60cdac0dd5871295e0f608fc).
Silva, Carlos and Steven Moran. Work in progress.

In this report, we use R (R Core Team 2021) and the following packages
(Wickham et al. 2019; Xie 2021).

``` r
library(tidyverse)
library(knitr)
```

First load the dataset.

``` r
database <- read_csv('database.csv')
```

The data look like this.

``` r
database %>% head() %>% kable()
```

| language    | class | position     | lexifier\_phoneme | creole\_phoneme | place\_stability | manner\_stability | example     | reference        | gloss      |
|:------------|:------|:-------------|:------------------|:----------------|-----------------:|------------------:|:------------|:-----------------|:-----------|
| Principense | Stops | Word-initial | p                 | p               |                1 |                 1 | \[ˈpɛnɛ\]   | Maurer 2009: 232 | feather    |
| Principense | Stops | word-medial  | p                 | p               |                1 |                 1 | \[t͡ʃipa\]   | Maurer 2009: 238 | guts       |
| Principense | Stops | Word-initial | b                 | b               |                1 |                 1 | \[bwɛga\]   | Maurer 2009: 216 | belly      |
| Principense | Stops | word-medial  | b                 | b               |                1 |                 1 | \[kaˈbɛlu\] | Maurer 2009: 221 | hair       |
| Principense | Stops | Word-initial | t                 | t               |                1 |                 1 | \[ˈtudu\]   | Maurer 2009: 237 | everything |
| Principense | Stops | word-medial  | t                 | t               |                1 |                 1 | \[mata\]    | Maurer 2009: 227 | to kill    |

# Creole stability

Which creoles in the sample are more or less stable overall?

First, we prepare the data for analysis.

``` r
creole_stability <- database %>% select(language, place_stability, manner_stability)
creole_stability$place_stability = as.numeric(creole_stability$place_stability)
creole_stability$manner_stability = as.numeric(creole_stability$manner_stability)
```

Next, we calculate the stability measure for each creole.

``` r
global_creole_stability <- mutate(creole_stability, global_stability = (place_stability + manner_stability)/2)

final_results <- global_creole_stability %>% group_by(language) %>% summarize(m = mean(global_stability, na.rm = TRUE))
```

Lastly, we plot the results.

``` r
region <- c("GG", "UG", "UG", "UG", "UG", "UG", "NI", "NI", "GG", "UG", "SI", "NI", "SA", "GG", "GG", "SI")

final_results_region <- cbind(final_results, region)

ggplot(final_results_region) + 
  geom_bar(aes(x = m, y = reorder(language, m), fill = region), 
           stat = "identity", show.legend = FALSE) +
  theme(axis.title.y = element_blank()) +
  labs(x = "Stability score")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

We have the overall stability values. What are these in relation to the
duration of contact?

We can get the length of contact from CreoPhonPT.

``` r
cp <- read_csv('https://raw.githubusercontent.com/silva-carlos/CreoPhonPt/main/stability_creoles_dataset.csv?token=AAIGDLXLDIORFDAC64GJ4V3BTTHNW')
```

    ## Warning: Missing column names filled in: 'X17' [17]

``` r
tmp <- cp %>% select(Language, `End of influence`, `First major settlement`) %>% distinct()
tmp$duration <- tmp$`End of influence` - tmp$`First major settlement`

tmp <- left_join(tmp, final_results_region, by=c("Language"="language"))
```

Duration in years.

``` r
tmp %>% select(Language, duration, `First major settlement`, `End of influence`) %>% arrange(desc(duration)) %>% kable()
```

| Language                 | duration | First major settlement | End of influence |
|:-------------------------|---------:|-----------------------:|-----------------:|
| Cape Verdean Santiago    |      515 |                   1460 |             1975 |
| Cape Verdean Fogo        |      515 |                   1460 |             1975 |
| Cape Verdean Santo Antão |      513 |                   1462 |             1975 |
| Santome                  |      482 |                   1493 |             1975 |
| Principense              |      476 |                   1499 |             1975 |
| Patua Macau              |      429 |                   1570 |             1999 |
| Fa d’Ambô                |      425 |                   1543 |             1968 |
| Cape Verdean Brava       |      402 |                   1573 |             1975 |
| Papiamentu               |      360 |                   1650 |             2010 |
| Cavite Chabacano         |      239 |                   1659 |             1898 |
| Ternate Chabacano        |      227 |                   1671 |             1898 |
| Daman                    |      200 |                   1540 |             1740 |
| Diu                      |      200 |                   1540 |             1740 |
| Zamboanga Chabacano      |      179 |                   1719 |             1898 |
| Kannur                   |      158 |                   1505 |             1663 |
| Sri Lanka                |      153 |                   1505 |             1658 |
| Cape Verdean São Vicente |      137 |                   1838 |             1975 |
| Papiá Kristang           |      130 |                   1511 |             1641 |
| Korlai                   |      120 |                   1520 |             1640 |
| Media Lengua             |       96 |                   1925 |             2021 |
| Palenquero               |       50 |                   1650 |             1700 |
| Angolar                  |       37 |                   1493 |             1530 |
| Guinea-Bissau Kriyol     |       37 |                   1493 |             1530 |
| Timor creole             |       NA |                     NA |               NA |

There does not seem to be a relationship between overall duration and
overall stability.

``` r
ggplot(tmp, aes(x=duration, y=m)) +
  geom_point()
```

    ## Warning: Removed 8 rows containing missing values (geom_point).

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(tmp, aes(x=duration, y=m)) +
  geom_point() +
  geom_text(label=tmp$Language)
```

    ## Warning: Removed 8 rows containing missing values (geom_point).

    ## Warning: Removed 8 rows containing missing values (geom_text).

![](README_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

# Segment stability

Which segments are the most stable across creoles in the language
sample?

First we prepare the data.

``` r
data_by_phoneme <- database %>% select(lexifier_phoneme, place_stability, manner_stability)
data_by_phoneme$place_stability = as.numeric(data_by_phoneme$place_stability)
data_by_phoneme$manner_stability = as.numeric(data_by_phoneme$manner_stability)
```

Then we calculate stability of place and manner for each phoneme.

``` r
place_results <- data_by_phoneme %>% group_by(lexifier_phoneme) %>% summarize(mplace = mean(place_stability, na.rm = TRUE))

manner_results <- data_by_phoneme %>% group_by(lexifier_phoneme) %>% summarize(mmanner = mean(manner_stability, na.rm = TRUE))

consonant_stability <- left_join(place_results, manner_results, by = "lexifier_phoneme")

class <- c("nasal", "rhotic", "lateral", "fricative", "stop", "stop", "fricative", "stop", "stop", "lateral", "nasal", "nasal", "stop", "rhotic", "fricative", "stop", "affricate", "fricative", "fricative")

consonant_stability_class <- cbind(consonant_stability, class)
```

Next, we plot the results.

``` r
# TODO: fix the class "dots"
ggplot(consonant_stability, aes(y = mmanner, x = mplace, label = lexifier_phoneme, color=class)) +
  geom_point(position= "dodge") + 
  geom_text(aes(label=lexifier_phoneme), hjust=3, vjust=0) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
```

    ## Warning: Width not defined. Set with `position_dodge(width = ?)`

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Here is an alternative view for the global results.

``` r
consonant_global_stability <- mutate(consonant_stability_class, mglobal = (mmanner + mplace)/2)

ggplot(consonant_global_stability) + 
  geom_bar(aes(x = mglobal, 
               y = reorder(lexifier_phoneme, mglobal),
               fill = class), stat = "identity", show.legend = TRUE) +
  labs(x = "Stability score", y = "Phoneme", fill = "Manner")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# Word position

Next we ask, does word position influence stability?

First, data prepartion.

``` r
data_by_position <- database %>% select(position, lexifier_phoneme, place_stability, manner_stability) %>% mutate(position = tolower(position))

data_by_position$place_stability <- as.numeric(data_by_position$place_stability)

data_by_position$manner_stability <- as.numeric(data_by_position$manner_stability)
```

Next, calculate stability for each segment according to its word
position.

``` r
position_stability <- mutate(data_by_position, global_stability = (place_stability + manner_stability)/2)

position_results <- position_stability %>% group_by(position, lexifier_phoneme) %>% summarize(m = mean(global_stability, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'position'. You can override using the `.groups` argument.

And plot the results for all segments.

``` r
position_results$position <- factor(position_results$position, levels = c('word-initial', 'word-medial', 'word-final'))

ggplot(position_results, aes(x = lexifier_phoneme, y = m, fill = position)) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

Flip horizontally.

``` r
ggplot(position_results) + 
  geom_bar(aes(x = m,
               y = reorder(lexifier_phoneme, m),
               fill = position), 
           stat = "identity", 
           show.legend = TRUE,
           position = "dodge2") +
  labs(x = "Stability score", y = "Phoneme", fill = "Position")
```

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

Plot the results for segments that show differences.

``` r
# TODO: update what we show, flip horizontally, change colors
position_results1 <- position_results %>% pivot_wider(names_from = position, values_from = m)

different_position <- subset(position_results1, position_results1$`word-initial` != position_results1$`word-medial` | position_results1$`word-final` != position_results1$`word-medial`)

different_position_results <- different_position %>% pivot_longer(c(`word-initial`, `word-medial`, `word-final`), names_to = "position", values_to = "m")

different_position_results$position <- factor(different_position_results$position, levels = c('word-initial', 'word-medial', 'word-final'))

ggplot(different_position_results, 
       aes(x = lexifier_phoneme, y = m, fill = position)) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single"))
```

    ## Warning: Removed 8 rows containing missing values (geom_col).

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- --> Flip
horizontally.

``` r
# TODO: reorder doesn't work here
ggplot(different_position_results) + 
  geom_bar(aes(x = m,
               y = reorder(lexifier_phoneme, m),
               fill = position), 
           stat = "identity", 
           show.legend = TRUE,
           position = "dodge2") +
  labs(x = "Stability score", y = "Phoneme", fill = "Position")
```

    ## Warning: Removed 8 rows containing missing values (geom_bar).

![](README_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-R" class="csl-entry">

R Core Team. 2021. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

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
