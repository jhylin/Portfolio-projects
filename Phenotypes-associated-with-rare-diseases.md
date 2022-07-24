Phenotypes associated with rare diseases
================
Jennifer HY Lin
2022-07-16

``` r
library(tidyverse)
library(lubridate)
library(ggplot2)
library(knitr)
```

### Initial data wrangling

This data set was also derived and downloaded from Orphanet, but because
it contained 37 columns with 112,243 rows originally, it took quite a
long time to load on RStudio (or could be due to my laptop capacity…).
It loaded relatively faster on Jupyter notebook from Anaconda, so I
decided to clean it up first using Python there. Some columns were
removed which reduced the number of columns from 37 to 13, while not
changing any of the rows. The columns were also renamed to make it
easier to read as shown below.

``` r
df <- read_csv("rare_disease_phenotypes.csv")
```

    ## Rows: 112243 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): Disorder group, Disorder type, Diagnostic criteria, HPO frequency, HPO ID, Prefer...
    ## dbl   (2): HPO disorder & clinical entity association count, Disorder Orphacode
    ## dttm  (1): Validation date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The abbreviation of HPO means human phenotype ontology.

``` r
spec(df)
```

    ## cols(
    ##   `Disorder group` = col_character(),
    ##   `Disorder type` = col_character(),
    ##   `HPO disorder & clinical entity association count` = col_double(),
    ##   `Diagnostic criteria` = col_character(),
    ##   `HPO frequency` = col_character(),
    ##   `HPO ID` = col_character(),
    ##   `Preferred HPO term` = col_character(),
    ##   `Disorder name` = col_character(),
    ##   `Disorder Orphacode` = col_double(),
    ##   Online = col_character(),
    ##   Source = col_character(),
    ##   `Validation date` = col_datetime(format = ""),
    ##   `Validation status` = col_character()
    ## )

``` r
df %>% View()
```

#### Exploratory data analysis

Since I’m not intending for this project to grow into a TL;DR version
(as most people would most likely lose interests and will to read by
then), I’m going to ask a question about the data set: What are the most
common rare disorders and the associated phenotype features with them?

So to answer it, let’s observe the spread of the disorder groups and
types by formulating a contingency table first.

``` r
df_type <- df %>% 
  group_by(`Disorder type`) %>% 
  summarize(Number = n())
df_type
```

    ## # A tibble: 11 × 2
    ##    `Disorder type`                                        Number
    ##    <chr>                                                   <int>
    ##  1 Biological anomaly                                         41
    ##  2 Category                                                  479
    ##  3 Clinical group                                            952
    ##  4 Clinical subtype                                         7394
    ##  5 Clinical syndrome                                         661
    ##  6 Disease                                                 57920
    ##  7 Etiological subtype                                      4060
    ##  8 Histopathological subtype                                  40
    ##  9 Malformation syndrome                                   37634
    ## 10 Morphological anomaly                                    2644
    ## 11 Particular clinical situation in a disease or syndrome    418

Then to actually visualise this in a graphic way, a lollypop chart was
built in a horizontal fashion, with different rare disorder types on the
y-axis and number of each type on the x-axis.

Two disorder types stood out the most, with Disease type appeared 57,920
times and Malformation syndrome at 37,634 times. To fully understand
what each of these two disorder types are, a direct reference (see note
below) was used and according to the source of the dataset:

1.  Definition of “Disease” in the rare disease context is “a disorder
    with homogeneous therapeutic possibilities and an identified
    physiopathological mechanism…”, one thing also worth noting was that
    this type did not include any developmental anomalies.
2.  For “Malformation syndrome”, this was defined as, “A disorder
    resulting from a developmental anomaly involving more than one
    morphogenetic field. Malformative sequences and associations are
    included.”

Reference: “Orphadata: Free access products description” - April 2020
<http://www.orphadata.org/cgi-bin/img/PDF/OrphadataFreeAccessProductsDescription.pdf>
Version 2

``` r
ggplot(data = df_type, aes(x = `Disorder type`, y = `Number`)) +
  geom_segment(aes(x = `Disorder type`, xend = `Disorder type`, y = 0, yend = `Number`), colour = "dark blue") +
  geom_point(colour = "dark green", size = 2, alpha = 0.6) +
  theme_light() +
  coord_flip() 
```

![](Phenotypes-associated-with-rare-diseases_files/figure-gfm/lollypop%20chart-1.png)<!-- -->

``` r
df1 <- df %>% 
  group_by(`Disorder type`) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n/sum(n))
df1
```

    ## # A tibble: 11 × 3
    ##    `Disorder type`                                            n     prop
    ##    <chr>                                                  <int>    <dbl>
    ##  1 Biological anomaly                                        41 0.000365
    ##  2 Category                                                 479 0.00427 
    ##  3 Clinical group                                           952 0.00848 
    ##  4 Clinical subtype                                        7394 0.0659  
    ##  5 Clinical syndrome                                        661 0.00589 
    ##  6 Disease                                                57920 0.516   
    ##  7 Etiological subtype                                     4060 0.0362  
    ##  8 Histopathological subtype                                 40 0.000356
    ##  9 Malformation syndrome                                  37634 0.335   
    ## 10 Morphological anomaly                                   2644 0.0236  
    ## 11 Particular clinical situation in a disease or syndrome   418 0.00372

Rearrange the table with proportions in descending order (from highest
to lowest). It also showed the top two were “Disease” (51.6%) and
“Malformation syndrome” (33.5%).

``` r
df1 %>% arrange(desc(prop))
```

    ## # A tibble: 11 × 3
    ##    `Disorder type`                                            n     prop
    ##    <chr>                                                  <int>    <dbl>
    ##  1 Disease                                                57920 0.516   
    ##  2 Malformation syndrome                                  37634 0.335   
    ##  3 Clinical subtype                                        7394 0.0659  
    ##  4 Etiological subtype                                     4060 0.0362  
    ##  5 Morphological anomaly                                   2644 0.0236  
    ##  6 Clinical group                                           952 0.00848 
    ##  7 Clinical syndrome                                        661 0.00589 
    ##  8 Category                                                 479 0.00427 
    ##  9 Particular clinical situation in a disease or syndrome   418 0.00372 
    ## 10 Biological anomaly                                        41 0.000365
    ## 11 Histopathological subtype                                 40 0.000356

Check out the distributions of HPO frequency to see which category has
the most and least number of counts.

``` r
df_freq <- df %>% 
  count(`HPO frequency`) %>% 
  arrange(desc(n))
df_freq
```

    ## # A tibble: 7 × 2
    ##   `HPO frequency`            n
    ##   <chr>                  <int>
    ## 1 Occasional (29-5%)     41140
    ## 2 Frequent (79-30%)      37480
    ## 3 Very frequent (99-80%) 25892
    ## 4 Very rare (<4-1%)       6414
    ## 5 Excluded (0%)            705
    ## 6 Obligate (100%)          610
    ## 7 <NA>                       2

Filter results for rare disorders with obligate or 100% frequency in
patient’s populations, showing disorder type, HPO frequency and disorder
name. Specifically, I wanted to find out the disorder names associated
with the “Disease” disorder type with HPO frequency of “Obligate
(100%)”.

``` r
df_freq_ob <- df %>% 
  filter(`Disorder type` == "Disease", `HPO frequency` == "Obligate (100%)") %>% 
  select(`Disorder type`, `HPO frequency`, `Disorder name`)
df_freq_ob
```

    ## # A tibble: 404 × 3
    ##    `Disorder type` `HPO frequency` `Disorder name`                                     
    ##    <chr>           <chr>           <chr>                                               
    ##  1 Disease         Obligate (100%) Retinoblastoma                                      
    ##  2 Disease         Obligate (100%) Parathyroid carcinoma                               
    ##  3 Disease         Obligate (100%) Pituitary carcinoma                                 
    ##  4 Disease         Obligate (100%) Familial hypocalciuric hypercalcemia                
    ##  5 Disease         Obligate (100%) Familial hypocalciuric hypercalcemia                
    ##  6 Disease         Obligate (100%) Ravine syndrome                                     
    ##  7 Disease         Obligate (100%) Ravine syndrome                                     
    ##  8 Disease         Obligate (100%) Interstitial granulomatous dermatitis with arthritis
    ##  9 Disease         Obligate (100%) Interstitial granulomatous dermatitis with arthritis
    ## 10 Disease         Obligate (100%) PLIN1-related familial partial lipodystrophy        
    ## # … with 394 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

I’d then like to look into associated counts of appearance of each
disorder name. When I cross-checked with the original dataset table
view, I’ve noted that the number of appearance of each disorder name is
linked to the number of preferred HPO phenotype terms of each of these
disorder types.

``` r
df2 <- df_freq_ob %>% count(`Disorder name`)
df2 %>% arrange(desc(n))
```

    ## # A tibble: 239 × 2
    ##    `Disorder name`                                                                       n
    ##    <chr>                                                                             <int>
    ##  1 Autosomal recessive complex spastic paraplegia due to Kennedy pathway dysfunction    10
    ##  2 STT3A-CDG                                                                             9
    ##  3 STT3B-CDG                                                                             9
    ##  4 Spastic paraplegia-Paget disease of bone syndrome                                     8
    ##  5 Oculocutaneous albinism type 5                                                        7
    ##  6 PLIN1-related familial partial lipodystrophy                                          7
    ##  7 Plummer-Vinson syndrome                                                               5
    ##  8 SSR4-CDG                                                                              5
    ##  9 Cholesterol-ester transfer protein deficiency                                         4
    ## 10 Isolated follicle stimulating hormone deficiency                                      4
    ## # … with 229 more rows
    ## # ℹ Use `print(n = ...)` to see more rows

To show this, let’s link preferred HPO terms to a disorder name such as,
“Autosomal recessive complex spastic paraplegia due to Kennedy pathway
dysfunction”, which had the “Disease” disorder type with obligate or
100% HPO frequency.

``` r
df %>% 
  filter(`Disorder type` == "Disease", `HPO frequency` == "Obligate (100%)", `Disorder name` == "Autosomal recessive complex spastic paraplegia due to Kennedy pathway dysfunction") %>% 
  select(`Disorder type`, `HPO frequency`, `Disorder name`, `Preferred HPO term`)
```

    ## # A tibble: 10 × 4
    ##    `Disorder type` `HPO frequency` `Disorder name`                                       Prefe…¹
    ##    <chr>           <chr>           <chr>                                                 <chr>  
    ##  1 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Progre…
    ##  2 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Microc…
    ##  3 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Modera…
    ##  4 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Nasal,…
    ##  5 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Delaye…
    ##  6 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Progre…
    ##  7 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Lower …
    ##  8 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Ankle …
    ##  9 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Retina…
    ## 10 Disease         Obligate (100%) Autosomal recessive complex spastic paraplegia due t… Progre…
    ## # … with abbreviated variable name ¹​`Preferred HPO term`

As shown in the dataframe above, there are a total of ten different HPO
phenotype terms associated with this particular rare disease with 100%
HPO frequency within the patient population for this specific type of
spastic paraplegia.

By using similar filtering method, we can quickly narrow down any
particular rare disease of interest to find out specific phenotypes or
clinical features, along with associated HPO phenotype frequency, for
further investigations.

For “Malformation syndrome” - let’s look into associated phenotypes for
it… *to be continued*
