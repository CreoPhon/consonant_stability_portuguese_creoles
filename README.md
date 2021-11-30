Supplementary materials for: Consonant stability in Portuguese-based
creoles
================
Carlos Silva and Steven Moran
(30 novembro, 2021)

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
database <- read_csv('https://raw.githubusercontent.com/silva-carlos/CreoPhonPt/main/Creoles.csv?token=ATUPEZPSZ6IPZGTIGKPB5ATBUZCHG')
```

The data look like this.

``` r
database %>% head() %>% kable()
```

| Language    | Area           | Lexifier   | FirstMajorSettlement | EndOfInfluence | ContactConditions | LanguageContact | Class | Position     | LexifierPhoneme | CreolePhoneme | PlaceStability | MannerStability | Word                                 | Gloss      | Source          |
|:------------|:---------------|:-----------|---------------------:|---------------:|:------------------|:----------------|:------|:-------------|:----------------|:--------------|---------------:|----------------:|:-------------------------------------|:-----------|:----------------|
| Principense | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-initial | p               | p             |              1 |               1 | \[’p&lt;U+025B&gt;n&lt;U+025B&gt;\]  | feather    | Maurer09\[232\] |
| Principense | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-medial  | p               | p             |              1 |               1 | \[t&lt;U+0320&gt;&lt;U+0283&gt;ipa\] | guts       | Maurer09\[238\] |
| Principense | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-initial | b               | b             |              1 |               1 | \[bw&lt;U+025B&gt;ga\]               | belly      | Maurer09\[216\] |
| Principense | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-medial  | b               | b             |              1 |               1 | \[ka’b&lt;U+025B&gt;lu\]             | hair       | Maurer09\[221\] |
| Principense | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-initial | t               | t             |              1 |               1 | \[’tudu\]                            | everything | Maurer09\[237\] |
| Principense | Gulf of Guinea | Portuguese |                 1499 |           1975 | Slavery           | Edo             | Stops | word-medial  | t               | t             |              1 |               1 | \[mata\]                             | to kill    | Maurer09\[227\] |

# Creole stability

Which creoles in the sample are more or less stable overall?

First, we prepare the data for analysis.

``` r
CreoleStability <- database %>% select(Language, Area, PlaceStability, MannerStability)
CreoleStability$PlaceStability = as.numeric(CreoleStability$PlaceStability)
CreoleStability$MannerStability = as.numeric(CreoleStability$MannerStability)
```

Next, we calculate the stability measure for each creole.

``` r
GlobalCreoleStability <- mutate(CreoleStability, GlobalStability = (PlaceStability + MannerStability)/2)

final_results <- GlobalCreoleStability %>% group_by(Language, Area) %>% summarize(m = mean(GlobalStability, na.rm = TRUE))
```

    ## `summarise()` has grouped output by 'Language'. You can override using the `.groups` argument.

Lastly, we plot the results.

``` r
ggplot(final_results) + 
  geom_bar(aes(x = m, y = reorder(Language, m), fill = Area), 
           stat = "identity", show.legend = FALSE) +
  theme(axis.title.y = element_blank()) +
  labs(x = "Stability score")
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

We have the overall stability values. What are these in relation to the
duration of contact?

We can get the length of contact from CreoPhonPT.

``` r
cp <- database

tmp <- cp %>% select(Language, `EndOfInfluence`, `FirstMajorSettlement`) %>% distinct()
tmp$duration <- tmp$`EndOfInfluence` - tmp$`FirstMajorSettlement`

tmp <- left_join(tmp, final_results, by=c("Language"))
```

Duration in years.

``` r
tmp <- tmp %>% rename(MeanStability = m)
tmp %>% select(Language, duration, `FirstMajorSettlement`, `EndOfInfluence`, MeanStability) %>% arrange(desc(duration)) %>% kable()
```

| Language                 | duration | FirstMajorSettlement | EndOfInfluence | MeanStability |
|:-------------------------|---------:|---------------------:|---------------:|--------------:|
| Cape Verdean Santiago    |      515 |                 1460 |           1975 |     0.9189189 |
| Cape Verdean Fogo        |      515 |                 1460 |           1975 |     0.8857143 |
| Cape Verdean Santo Antão |      513 |                 1462 |           1975 |     0.9444444 |
| Santome                  |      482 |                 1493 |           1975 |     0.7702703 |
| Principense              |      476 |                 1499 |           1975 |     0.8243243 |
| Patua Macau              |      429 |                 1570 |           1999 |     0.9054054 |
| Fa d’Ambô                |      425 |                 1543 |           1968 |     0.6428571 |
| Cape Verdean Brava       |      402 |                 1573 |           1975 |     0.9285714 |
| Daman                    |      200 |                 1540 |           1740 |     0.9000000 |
| Diu                      |      200 |                 1540 |           1740 |     0.8857143 |
| Timor creole             |      191 |                 1769 |           1960 |     0.9000000 |
| Kannur                   |      158 |                 1505 |           1663 |     0.9242424 |
| Sri Lanka                |      153 |                 1505 |           1658 |     0.8970588 |
| Cape Verdean São Vicente |      137 |                 1838 |           1975 |     0.9142857 |
| Papiá Kristang           |      130 |                 1511 |           1641 |     0.8750000 |
| Korlai                   |      120 |                 1520 |           1640 |     0.8529412 |
| Angolar                  |       37 |                 1493 |           1530 |     0.6571429 |
| Guinea-Bissau Kriyol     |       37 |                 1493 |           1530 |     0.9054054 |

There does not seem to be a relationship between overall duration and
overall stability.

``` r
ggplot(tmp, aes(x=duration, y=MeanStability)) +
  geom_point()
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
ggplot(tmp, aes(x=duration, y=MeanStability)) +
  geom_point() +
  geom_text(label=tmp$Language)
```

![](README_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

# Segment stability

Which segments are the most stable across creoles in the language
sample?

First we prepare the data.

``` r
data_by_phoneme <- database %>% select(LexifierPhoneme, PlaceStability, MannerStability)
data_by_phoneme$PlaceStability = as.numeric(data_by_phoneme$PlaceStability)
data_by_phoneme$MannerStability = as.numeric(data_by_phoneme$MannerStability)
```

Then we calculate stability of place and manner for each phoneme.

``` r
place_results <- data_by_phoneme %>% group_by(LexifierPhoneme) %>% summarize(mplace = mean(PlaceStability, na.rm = TRUE))

manner_results <- data_by_phoneme %>% group_by(LexifierPhoneme) %>% summarize(mmanner = mean(MannerStability, na.rm = TRUE))

consonant_stability <- left_join(place_results, manner_results, by = "LexifierPhoneme")

class <- c("nasal", "rhotic", "lateral", "fricative", "stop", "stop", "fricative", "stop", "stop", "lateral", "nasal", "nasal", "stop", "rhotic", "fricative", "stop", "affricate", "fricative", "fricative")

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

    ## Warning: Width not defined. Set with `position_dodge(width = ?)`

![](README_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Here is an alternative view for the global results.

``` r
consonant_global_stability <- mutate(consonant_stability_class, mglobal = (mmanner + mplace)/2)

ggplot(consonant_global_stability) + 
  geom_bar(aes(x = mglobal, 
               y = reorder(LexifierPhoneme, mglobal),
               fill = class), stat = "identity", show.legend = TRUE) +
  labs(x = "Stability score", y = "Phoneme", fill = "Manner")
```

![](README_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

# Word position

Next we ask, does word position influence stability?

First, data prepartion.

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

    ## `summarise()` has grouped output by 'LexifierPhoneme'. You can override using the `.groups` argument.

And plot the results for all segments.

``` r
position_results$Position <- factor(position_results$Position, levels = c('word-initial', 'word-medial', 'word-final'))

ggplot(position_results, aes(x = LexifierPhoneme, y = m, fill = Position)) + 
  geom_col(position = position_dodge2(width = 0.9, preserve = "single")) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
```

![](README_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

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

    ## Warning: Removed 8 rows containing missing values (geom_col).

![](README_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

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
