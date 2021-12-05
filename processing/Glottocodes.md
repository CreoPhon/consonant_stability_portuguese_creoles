Glottocodes
================
Carlos Silva
04/10/2021

Load libraries

    library(tidyverse)
    library(lingtypology)

Load data

    df = read.table("./consonant_stability_ptspcreoles_database - All.tsv", sep="\t", header=T, comment.char="", quote="", encoding="UTF-8")

Extract unique language names

    language <- levels(as.factor(df$Language))

Find glottocodes

    glottocodes <- gltc.lang(language)

Build a table

    glottocodes_for_creoles <- tibble(language, glottocodes)

Results: There was a mismatch between the language names in the df
(taken from APiCs) and the ones from Glottolog (which are the basis of
the Moroz’s package). Moreover, the code assigned to Diu creole turn out
to be incorrect. Then, I added the missing data manually.
