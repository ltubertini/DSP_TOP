V1 DSP vs TOP Analysis
================
Lorenzo Tubertini
17 April 2018

-   [R code to prepare the V1 DSP vs TOP Analysis](#r-code-to-prepare-the-v1-dsp-vs-top-analysis)
    -   [Libraries load, MongoDB data import and consolidation](#libraries-load-mongodb-data-import-and-consolidation)
    -   [Ad type bucketing, subsetting](#ad-type-bucketing-subsetting)
    -   [Dealers' ads distribution Chi-square test](#dealers-ads-distribution-chi-square-test)
    -   [KPI calculation](#kpi-calculation)
    -   [Outliers detection, exclusion](#outliers-detection-exclusion)
    -   [F-test in preparation for possible Student t-test](#f-test-in-preparation-for-possible-student-t-test)
    -   [KPI average comparison](#kpi-average-comparison)
-   [Plots:](#plots)
    -   [adVIP ~ totVIP (chart 1)](#advip-totvip-chart-1)
    -   [Days displayed ~ totVIP (chart 2)](#days-displayed-totvip-chart-2)
    -   [Days displayed ~ totVIP - DSP (chart 3)](#days-displayed-totvip---dsp-chart-3)
    -   [Days displayed ~ totVIP - TOP (chart 4)](#days-displayed-totvip---top-chart-4)
    -   [Lightbox views ~ totVIP (chart 5)](#lightbox-views-totvip-chart-5)
    -   [Lightbox views ~ totVIP by ad days (chart 6)](#lightbox-views-totvip-by-ad-days-chart-6)
    -   [adVIP ~ totVIP by ad days (chart 7)](#advip-totvip-by-ad-days-chart-7)

R code to prepare the V1 DSP vs TOP Analysis
--------------------------------------------

### Libraries load, MongoDB data import and consolidation

CSV files from the original case can be found [here](https://github.com/ltubertini/DSP_TOP/tree/master/case)

``` r
library(dplyr)
library(ggplot2)
library(reshape2)
library(vcd)

setwd("C:/Users/ltubertini/Downloads/case")
a <- (read.csv2("case.csv", sep = ";", na.strings = "-", dec = ",", skip = 1, 
    colClasses = c("character", "character", "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
        "numeric", "numeric")))
b <- (read.csv2("case2.csv", sep = ";", na.strings = "-", dec = ",", skip = 1, 
    colClasses = c("character", "character", "numeric")))

input <- merge(a, b, by = c("ad_id", "dealer_id"), all = T)
input <- input %>% group_by(ad_id, dealer_id) %>% summarise_all(sum)
```

### Ad type bucketing, subsetting

Ads are bucketed as "DSP", "TOP"" or both ("multi") based on the available flags for readibility. Ads which were not served via either are removed as well as those living longer than 6 days to reduce noise from external traffic.

``` r
input$ad_type <- ifelse(input$TOP.feature.ads == 1 & input$DSS.feature.flag == 
    1, "multi", ifelse(input$TOP.feature.ads == 1 & input$DSS.feature.flag == 
    0, "TOP", ifelse(input$TOP.feature.ads == 0 & input$DSS.feature.flag == 
    1, "DSP", "none")))
input_no_none <- subset(input, ad_type != "none")
input <- subset(input_no_none, ad_days < 7)
```

### Dealers' ads distribution Chi-square test

The representation of dealers between DSP and TOP ads is checked: the 2 distributions correlate with strengh approx 20%

``` r
dealers <- input[, c("dealer_id", "ad_type")]
dealers <- melt(dealers)
dealers_cast <- dcast(dealers, dealer_id ~ ad_type)
dealers_cast <- dealers_cast[, -1]
# Chi-square test
chisq.test(dealers_cast)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dealers_cast
    ## X-squared = 1337.8, df = 1068, p-value = 2.955e-08

``` r
# Cramér's V
assocstats(table(dealers_cast$DSP, dealers_cast$TOP))
```

    ##                     X^2  df   P(> X^2)
    ## Likelihood Ratio 349.54 238 3.2554e-06
    ## Pearson          519.78 238 0.0000e+00
    ## 
    ## Phi-Coefficient   : NA 
    ## Contingency Coeff.: 0.702 
    ## Cramer's V        : 0.263

### KPI calculation

TOP adVIPs are adjusted to 30% of their actual value to compensate for the extra traffic such placement received

``` r
input$TOPadVIPadj <- input$vipViews_TopFeature * 0.3
input$adVIP <- (input$TOPadVIPadj + input$vipViews_DSPtestFeature)
input$ad_days_DSP_TOP <- (input$TopFeature_days + input$DSPtestFeature_days)
input$adVIP_day <- input$adVIP/input$ad_days_DSP_TOP
input$totVIP_day <- input$vipViews_siteGermany/input$ad_days
input$adVIP_ad <- input$adVIP/(input$TOP.feature.ads + input$DSS.feature.flag)
input$totVIP_ad <- input$vipViews_siteGermany/(input$TOP.feature.ads + input$DSS.feature.flag)
```

### Outliers detection, exclusion

Outliers are calculated as data point exceeding the 999th permille on at least one for the main KPIs and excluded

``` r
outlier_threshold = 0.999
outlier_adVIP_day <- subset(input, adVIP_day > quantile(input$adVIP_day, outlier_threshold, 
    na.rm = TRUE))
outlier_totVIP_day <- subset(input, totVIP_day > quantile(input$totVIP_day, 
    outlier_threshold, na.rm = TRUE))
outlier_adVIP_ad <- subset(input, adVIP_ad > quantile(input$adVIP_ad, outlier_threshold, 
    na.rm = TRUE))
outlier_totVIP_ad <- subset(input, totVIP_ad > quantile(input$totVIP_ad, outlier_threshold, 
    na.rm = TRUE))
outlier_adVIP <- subset(input, adVIP > quantile(input$adVIP_day, outlier_threshold, 
    na.rm = TRUE))
outlier_totVIP <- subset(input, TOPadVIPadj > quantile(input$totVIP_day, outlier_threshold, 
    na.rm = TRUE))
outliers <- rbind(outlier_adVIP_day, outlier_totVIP_day, outlier_adVIP_ad, outlier_totVIP_ad, 
    outlier_adVIP, outlier_totVIP)
input_clean <- anti_join(input, outliers, by = c("ad_id", "dealer_id"))
```

### F-test in preparation for possible Student t-test

F-test is run to test for variance equality, a condition of the Student t-test, which would help comparing KPI distributions. As the null hypothesis is rejected, variances aren't equal and the t-test cannot be performed

``` r
input_clean_TOP <- subset(input_clean, ad_type == "TOP")
input_clean_TOP <- input_clean_TOP[, c("vipViews_siteGermany", "adVIP", "adVIP_day", 
    "totVIP_day", "adVIP_ad", "totVIP_ad")]
input_clean_DSP <- subset(input_clean, ad_type == "DSP")
input_clean_DSP <- input_clean_DSP[, c("vipViews_siteGermany", "adVIP", "adVIP_day", 
    "totVIP_day", "adVIP_ad", "totVIP_ad")]
# F-test 1
var.test(input_clean_TOP$vipViews_siteGermany, input_clean_DSP$vipViews_siteGermany)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  input_clean_TOP$vipViews_siteGermany and input_clean_DSP$vipViews_siteGermany
    ## F = 0.068447, num df = 619, denom df = 960, p-value < 2.2e-16
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  0.05939334 0.07905685
    ## sample estimates:
    ## ratio of variances 
    ##         0.06844674

``` r
# F-test 2
var.test(input_clean_TOP$adVIP, input_clean_DSP$adVIP)
```

    ## 
    ##  F test to compare two variances
    ## 
    ## data:  input_clean_TOP$adVIP and input_clean_DSP$adVIP
    ## F = 2.8992, num df = 619, denom df = 960, p-value < 2.2e-16
    ## alternative hypothesis: true ratio of variances is not equal to 1
    ## 95 percent confidence interval:
    ##  2.515740 3.348632
    ## sample estimates:
    ## ratio of variances 
    ##           2.899217

### KPI average comparison

These values are to be taken as preliminary and only operational

``` r
input_compare <- input_clean[, c("ad_type", "adVIP_day", "totVIP_day", "adVIP_ad", 
    "totVIP_ad")] %>% group_by(ad_type) %>% summarise_all(mean)
```

| ad\_type |  adVIP\_day|  totVIP\_day|  adVIP\_ad|  totVIP\_ad|
|:---------|-----------:|------------:|----------:|-----------:|
| DSP      |    2.541346|     29.57882|   3.704474|   107.53798|
| multi    |    3.803116|     27.32826|   7.584783|    34.91304|
| TOP      |    1.840766|     11.08473|   5.273710|    34.93065|

Plots:
------

### adVIP ~ totVIP (chart 1)

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%201-1.jpeg)

### Days displayed ~ totVIP (chart 2)

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%202-1.jpeg)

### Days displayed ~ totVIP - DSP (chart 3)

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%203-1.jpeg)

### Days displayed ~ totVIP - TOP (chart 4)

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%204-1.jpeg)

### Lightbox views ~ totVIP (chart 5)

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%205-1.jpeg)

### Lightbox views ~ totVIP by ad days (chart 6)

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%206-1.jpeg)

### adVIP ~ totVIP by ad days (chart 7)

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%207-1.jpeg)
