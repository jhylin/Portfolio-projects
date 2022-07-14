Rare disease drugs from US FDA
================
Jennifer HY Lin
5-7-2022

### Why rare disease drugs?

This work was inspired by Data Is Plural website
[link](https://www.data-is-plural.com), which has provided a diverse and
dynamic range of data available to the public. I happened to come across
it randomly without any intentions and rare disease drugs jumped out
from all of the data available at the time.

Below is an initial dive-in for this particular data set for rare
disease drugs from 1983 till present - downloadable from:
<https://www.accessdata.fda.gov/scripts/opdlisting/oopd/index.cfm> (last
accessed: 1/7/2022). Future work may evolve from this initial part and
perhaps grow further as I’m slowly working on other data available from
Orphanet, a website dedicated to data relevant to rare diseases.

Read .csv file firstly to import the data set.

``` r
library(tidyverse)
```

``` r
df <- read_csv("Rare_diseases_drugs_FDA.csv")
```

    ## Rows: 1058 Columns: 18
    ## ── Column specification ───────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (16): Generic Name, Trade Name, Date Designated, Orphan Designation, Or...
    ## dbl  (1): CF Grid Key
    ## lgl  (1): FDA Orphan Approval Status
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Inspect all column names in the data set and also open full data set in
a separate table view to observe contents.

``` r
spec(df)
```

    ## cols(
    ##   `Generic Name` = col_character(),
    ##   `Trade Name` = col_character(),
    ##   `Date Designated` = col_character(),
    ##   `Orphan Designation` = col_character(),
    ##   `Orphan Designation Status` = col_character(),
    ##   `FDA Orphan Approval Status` = col_logical(),
    ##   `Approved Labeled Indication` = col_character(),
    ##   `Marketing Approval Date` = col_character(),
    ##   `Exclusivity End Date` = col_character(),
    ##   `Exclusivity Protected Indication * (Shown for approvals from Jan. 1, 2013, to the present)` = col_character(),
    ##   `Sponsor Company` = col_character(),
    ##   `Sponsor Address 1` = col_character(),
    ##   `Sponsor Address 2` = col_character(),
    ##   `Sponsor City` = col_character(),
    ##   `Sponsor State` = col_character(),
    ##   `Sponsor Zip` = col_character(),
    ##   `Sponsor Country` = col_character(),
    ##   `CF Grid Key` = col_double()
    ## )

``` r
df %>% View()
```

### Data analysis

After perusing the data set, I’ve decided to focus on selected columns
only so that I can concentrate on these data without seeing other minor
details at this stage.

``` r
select(df, `Generic Name`, `Trade Name`, `Date Designated`, `Orphan Designation`, `Approved Labeled Indication`, `Marketing Approval Date`, `Exclusivity Protected Indication * (Shown for approvals from Jan. 1, 2013, to the present)`, `Sponsor Country`)
```

    ## # A tibble: 1,058 × 8
    ##    `Generic Name`                `Trade Name` `Date Designat…` `Orphan Design…`
    ##    <chr>                         <chr>        <chr>            <chr>           
    ##  1 bosentan                      Tracleer     10/06/00         Treatment of pu…
    ##  2 bosentan                      Tracleer     10/06/00         Treatment of pu…
    ##  3 (tisagenlecleucel) Autologou… Kymriah (ti… 01/31/2014       For the treatme…
    ##  4 5-aminolevulinic acid         Gleolan      01/15/2013       Visualization o…
    ##  5 abatacept                     Orencia      12/26/2017       Prevention of g…
    ##  6 acalabrutinib                 CALQUENCE    09/21/2015       Treatment of ma…
    ##  7 acalabrutinib                 <NA>         05/13/2015       Treatment of ch…
    ##  8 acetylcysteine                Acetadote    10/19/2001       For the intrave…
    ##  9 acetylcysteine effervescent … Cetylev      02/24/2015       Preventing hepa…
    ## 10 acyclovir                     Avaclyr      12/13/2010       Treatment of ac…
    ## # … with 1,048 more rows, and 4 more variables:
    ## #   `Approved Labeled Indication` <chr>, `Marketing Approval Date` <chr>,
    ## #   `Exclusivity Protected Indication * (Shown for approvals from Jan. 1, 2013, to the present)` <chr>,
    ## #   `Sponsor Country` <chr>

I’m interested in finding out what countries were involved in developing
rare disease drugs and also how frequently each of these drugs appeared
in the data set from 1983 till present. So I’ve done a simple count() on
sponsor country column to see the distributions.

``` r
df %>% count(`Sponsor Country`)
```

    ## # A tibble: 22 × 2
    ##    `Sponsor Country`     n
    ##    <chr>             <int>
    ##  1 Australia             1
    ##  2 Austria               1
    ##  3 Barbados              1
    ##  4 Belgium               2
    ##  5 Canada                8
    ##  6 Denmark               1
    ##  7 Finland               1
    ##  8 France               10
    ##  9 Germany               4
    ## 10 Ireland              16
    ## # … with 12 more rows

I’ve noticed the last row was actually not a data entry, but rather a
caption about exclusivity indication and period so this was removed from
the newly created dataframe df1.

``` r
df1 = df %>% slice(-c(1058))
```

The count was then repeated, which had the last row removed as shown in
the table.

``` r
df1 %>% count(`Sponsor Country`)
```

    ## # A tibble: 21 × 2
    ##    `Sponsor Country`     n
    ##    <chr>             <int>
    ##  1 Australia             1
    ##  2 Austria               1
    ##  3 Barbados              1
    ##  4 Belgium               2
    ##  5 Canada                8
    ##  6 Denmark               1
    ##  7 Finland               1
    ##  8 France               10
    ##  9 Germany               4
    ## 10 Ireland              16
    ## # … with 11 more rows

A horizontal bar graph was plotted to show countries involved in rare
disease drug developments (by counts, which might include duplicates
such as different formulations under the same trade names and so on,
I’ve decided to include them all for now). It showed United States had
the highest counts of 966, which were followed by Ireland (16), then
United Kingdom (13) and also other countries.

``` r
ggplot(data = df1) +
  geom_bar(mapping = aes(x = `Sponsor Country`, fill = `Sponsor Country`)) +
  labs(title = "Countries involved in developing rare disease drugs") +
  coord_flip()
```

![](Rare_diseases_drugs_RStudio_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

#### Rare disease drugs from the UK

I’ve then decided to look into rare disease drugs with exclusivity end
date after the year of 2022 and also limited the sponsor country to UK
only. This was done using filter(). To show this result visually, a
timeline was plotted using vistime package (thanks to vistime package
creator, Sandro Raabe). The format of the dates in the data set was also
changed first, so that the timeline could be plotted.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library(dplyr)
```

``` r
df_uk <- df %>% 
  mutate(`Exclusivity End Date` = mdy(`Exclusivity End Date`), `Date Designated` = mdy(`Date Designated`), `Marketing Approval Date` = mdy(`Marketing Approval Date`)) %>%
  filter(year(`Exclusivity End Date`) > 2022 & `Sponsor Country` == "United Kingdom")
df_uk
```

    ## # A tibble: 8 × 18
    ##   `Generic Name`               `Trade Name` `Date Designated` `Orphan Designa…`
    ##   <chr>                        <chr>        <date>            <chr>            
    ## 1 belantamab mafodotin-blmf    BLENREP      2017-06-22        Treatment of mul…
    ## 2 cannabidiol                  Epidiolex    2014-02-27        Treatment of Len…
    ## 3 cannabidiol                  Epidiolex    2013-11-14        Treatment of Dra…
    ## 4 cannabidiol                  Epidiolex    2016-04-19        Treatment of tub…
    ## 5 cannabidiol                  EPIDIOLEX    2014-02-27        Treatment of Len…
    ## 6 cannabidiol                  EPIDIOLEX    2013-11-14        Treatment of Dra…
    ## 7 Coagulation factor X (human) COAGADEX     2007-11-08        Treatment of her…
    ## 8 tafenoquine                  Krintafel    2013-01-15        Treatment of mal…
    ## # … with 14 more variables: `Orphan Designation Status` <chr>,
    ## #   `FDA Orphan Approval Status` <lgl>, `Approved Labeled Indication` <chr>,
    ## #   `Marketing Approval Date` <date>, `Exclusivity End Date` <date>,
    ## #   `Exclusivity Protected Indication * (Shown for approvals from Jan. 1, 2013, to the present)` <chr>,
    ## #   `Sponsor Company` <chr>, `Sponsor Address 1` <chr>,
    ## #   `Sponsor Address 2` <chr>, `Sponsor City` <chr>, `Sponsor State` <chr>,
    ## #   `Sponsor Zip` <chr>, `Sponsor Country` <chr>, `CF Grid Key` <dbl>

``` r
library(vistime)
```

The timeline shown below starts from the date of designation to
marketing approval for all of the rare disease drugs under this
particular filter.

-   years on x-axis
-   generic names on y-axis
-   trade names on top of timeline bars

``` r
gg_vistime(df_uk, col.event = "Trade Name", col.group = "Generic Name", col.start = "Date Designated", col.end = "Marketing Approval Date", title = "Timeline from designation to marketing approval")
```

![](Rare_diseases_drugs_RStudio_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

#### Rare disease drugs from Ireland

For rare diseases drugs from Ireland, I’ve also changed the date formats
and then selected the ones with exclusivity end date after 2022.

``` r
df_ir <- df %>%
  mutate(`Exclusivity End Date` = mdy(`Exclusivity End Date`), `Date Designated` = mdy(`Date Designated`), `Marketing Approval Date` = mdy(`Marketing Approval Date`)) %>%
  filter(year(`Exclusivity End Date`) > 2022, `Sponsor Country` == "Ireland")
df_ir
```

    ## # A tibble: 6 × 18
    ##   `Generic Name`                 `Trade Name` `Date Designat…` `Orphan Design…`
    ##   <chr>                          <chr>        <date>           <chr>           
    ## 1 calcium, magnesium, potassium… Xywav        2019-07-31       Treatment of id…
    ## 2 calcium, magnesium, potassium… Xywav        1994-11-07       Treatment of na…
    ## 3 inebilizumab                   Uplizna      2016-02-10       Treatment of ne…
    ## 4 lurbinectedin                  Zepzelca     2018-08-01       Treatment of sm…
    ## 5 sodium oxybate                 Xyrem        1994-11-07       Treatment of na…
    ## 6 teprotumumab                   <NA>         2013-05-06       Treatment of ac…
    ## # … with 14 more variables: `Orphan Designation Status` <chr>,
    ## #   `FDA Orphan Approval Status` <lgl>, `Approved Labeled Indication` <chr>,
    ## #   `Marketing Approval Date` <date>, `Exclusivity End Date` <date>,
    ## #   `Exclusivity Protected Indication * (Shown for approvals from Jan. 1, 2013, to the present)` <chr>,
    ## #   `Sponsor Company` <chr>, `Sponsor Address 1` <chr>,
    ## #   `Sponsor Address 2` <chr>, `Sponsor City` <chr>, `Sponsor State` <chr>,
    ## #   `Sponsor Zip` <chr>, `Sponsor Country` <chr>, `CF Grid Key` <dbl>

The timeline is shown below starting from the date of designation to
marketing approval.

-   years on x-axis
-   generic names on y-axis
-   trade names on top of timeline bars (note: teprotumumab had no trade
    name recorded in the data set so it was not shown in timeline)

``` r
gg_vistime(df_ir, col.event = "Trade Name", col.group = "Generic Name", col.start = "Date Designated", col.end = "Marketing Approval Date", title = "Timeline from designation to marketing approval")
```

![](Rare_diseases_drugs_RStudio_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

#### Rare disease drugs from the US

Since there are a larger number of rare disease drugs from the US, I’ve
applied a filter to limit to the year of 2022 only and between the
months of January to June - in order to observe the ones that still have
exclusivity during this period. All date formats were also changed
before the timeline was plotted.

``` r
df_us <- df %>%
  mutate(`Exclusivity End Date` = mdy(`Exclusivity End Date`), `Date Designated` = mdy(`Date Designated`), `Marketing Approval Date` = mdy(`Marketing Approval Date`)) %>%
  filter(year(`Exclusivity End Date`) == 2022, month(`Exclusivity End Date`) >= 1, month(`Exclusivity End Date`) < 7, `Sponsor Country` == "United States")
df_us
```

    ## # A tibble: 15 × 18
    ##    `Generic Name`                `Trade Name` `Date Designat…` `Orphan Design…`
    ##    <chr>                         <chr>        <date>           <chr>           
    ##  1 Antivenin crotaline (pit-vip… Anavip       2004-01-29       Treatment of en…
    ##  2 cholic acid                   Cholbam      2003-07-18       Treatment of in…
    ##  3 dinutuximab                   Unituxin     2010-12-20       Treatment of ne…
    ##  4 filgrastim                    Neupogen     2013-11-20       Treatment of su…
    ##  5 ibrutinib                     Imbruvica    2013-10-15       Treatment of Wa…
    ##  6 isavuconazonium sulfate       Cresemba     2013-05-06       Treatment of in…
    ##  7 isavuconazonium sulfate       Cresemba     2013-10-25       Treatment of zy…
    ##  8 ivacaftor                     Kalydeco     2006-12-20       Treatment of pa…
    ##  9 lenalidomide                  Revlimid     2001-09-20       Treatment of mu…
    ## 10 lenvatinib                    Lenvima      2012-12-27       Treatment of fo…
    ## 11 levodopa and carbidopa        Duopa        2000-01-18       Treatment of la…
    ## 12 panobinostat                  Farydak      2012-08-20       Treatment of mu…
    ## 13 parathyroid hormone           Natpara      2007-08-31       Treatment of hy…
    ## 14 phoxillum                     <NA>         2014-02-14       For use as a re…
    ## 15 sirolimus                     Rapamune     2012-10-31       Treatment of ly…
    ## # … with 14 more variables: `Orphan Designation Status` <chr>,
    ## #   `FDA Orphan Approval Status` <lgl>, `Approved Labeled Indication` <chr>,
    ## #   `Marketing Approval Date` <date>, `Exclusivity End Date` <date>,
    ## #   `Exclusivity Protected Indication * (Shown for approvals from Jan. 1, 2013, to the present)` <chr>,
    ## #   `Sponsor Company` <chr>, `Sponsor Address 1` <chr>,
    ## #   `Sponsor Address 2` <chr>, `Sponsor City` <chr>, `Sponsor State` <chr>,
    ## #   `Sponsor Zip` <chr>, `Sponsor Country` <chr>, `CF Grid Key` <dbl>

The timeline for this particular set of rare disease drugs from the US
is shown below.

-   years on x-axis
-   generic names on y-axis
-   trade names on top of timeline bars (note: phoxillum had no trade
    name recorded in the data set so it was not shown in the timeline)

``` r
gg_vistime(df_us, col.event = "Trade Name", col.group = "Generic Name", col.start = "Date Designated", col.end = "Marketing Approval Date", title = "Timeline from designation to marketing approval")
```

![](Rare_diseases_drugs_RStudio_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

### Summary

This short dive-in session on rare disease drugs has shown that:

-   US is the country that has the most involvement in rare disease drug
    developments, which is followed by Ireland and the UK, and also a
    number of other countries
-   More work could possibly go into looking at the duplicates of brand
    names of the same generic drug e.g. cannabidiol with trade name as
    Epidiolex that had 5 repeated timelines shown above, which after
    further check into these entries, there appeared to be different
    clinical indications for each of these entries
-   The timelines have also implied that drug discovery and development
    is a very timely process, which could span many years, such as 10 -
    20 years or more, before a drug actually reaches the market for
    public use

``` r
sessionInfo()
```

    ## R version 4.2.0 (2022-04-22)
    ## Platform: x86_64-apple-darwin17.0 (64-bit)
    ## Running under: macOS Catalina 10.15.7
    ## 
    ## Matrix products: default
    ## BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_NZ.UTF-8/en_NZ.UTF-8/en_NZ.UTF-8/C/en_NZ.UTF-8/en_NZ.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] vistime_1.2.1   lubridate_1.8.0 forcats_0.5.1   stringr_1.4.0  
    ##  [5] dplyr_1.0.9     purrr_0.3.4     readr_2.1.2     tidyr_1.2.0    
    ##  [9] tibble_3.1.7    ggplot2_3.3.6   tidyverse_1.3.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.8.3               ggrepel_0.9.1             
    ##  [3] assertive.properties_0.0-5 assertive.types_0.0-3     
    ##  [5] assertthat_0.2.1           digest_0.6.29             
    ##  [7] utf8_1.2.2                 R6_2.5.1                  
    ##  [9] cellranger_1.1.0           backports_1.4.1           
    ## [11] reprex_2.0.1               evaluate_0.15             
    ## [13] httr_1.4.3                 highr_0.9                 
    ## [15] pillar_1.7.0               rlang_1.0.2               
    ## [17] lazyeval_0.2.2             readxl_1.4.0              
    ## [19] data.table_1.14.2          rstudioapi_0.13           
    ## [21] rmarkdown_2.14             labeling_0.4.2            
    ## [23] htmlwidgets_1.5.4          bit_4.0.4                 
    ## [25] munsell_0.5.0              broom_0.8.0               
    ## [27] compiler_4.2.0             modelr_0.1.8              
    ## [29] xfun_0.31                  pkgconfig_2.0.3           
    ## [31] htmltools_0.5.2            tidyselect_1.1.2          
    ## [33] codetools_0.2-18           viridisLite_0.4.0         
    ## [35] fansi_1.0.3                crayon_1.5.1              
    ## [37] tzdb_0.3.0                 dbplyr_2.2.0              
    ## [39] withr_2.5.0                grid_4.2.0                
    ## [41] assertive.base_0.0-9       jsonlite_1.8.0            
    ## [43] gtable_0.3.0               lifecycle_1.0.1           
    ## [45] DBI_1.1.2                  magrittr_2.0.3            
    ## [47] scales_1.2.0               cli_3.3.0                 
    ## [49] stringi_1.7.6              vroom_1.5.7               
    ## [51] farver_2.1.0               fs_1.5.2                  
    ## [53] xml2_1.3.3                 ellipsis_0.3.2            
    ## [55] generics_0.1.2             vctrs_0.4.1               
    ## [57] RColorBrewer_1.1-3         tools_4.2.0               
    ## [59] bit64_4.0.5                glue_1.6.2                
    ## [61] hms_1.1.1                  parallel_4.2.0            
    ## [63] fastmap_1.1.0              yaml_2.3.5                
    ## [65] colorspace_2.0-3           rvest_1.0.2               
    ## [67] plotly_4.10.0              knitr_1.39                
    ## [69] haven_2.5.0
