V1 DSP vs TOP Analysis
================
Lorenzo Tubertini
17 April 2018

R Code to prepare the V1 DSP vs TOP Analysis
--------------------------------------------

``` r
library(dplyr)
library(ggplot2)
library(reshape2)
library(vcd)
# data import
setwd("C:/Users/ltubertini/Downloads/case")
a <- (read.csv2("case.csv", sep = ";", na.strings = "-", dec = ",", skip = 1, 
    colClasses = c("character", "character", "numeric", "numeric", "numeric", 
        "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
        "numeric", "numeric")))
b <- (read.csv2("case2.csv", sep = ";", na.strings = "-", dec = ",", skip = 1, 
    colClasses = c("character", "character", "numeric")))

# data consolidation
input <- merge(a, b, by = c("ad_id", "dealer_id"), all = T)
input <- input %>% group_by(ad_id, dealer_id) %>% summarise_all(sum)
```

``` r
# ad type bucketing, subsetting
input$ad_type <- ifelse(input$TOP.feature.ads == 1 & input$DSS.feature.flag == 
    1, "multi", ifelse(input$TOP.feature.ads == 1 & input$DSS.feature.flag == 
    0, "TOP", ifelse(input$TOP.feature.ads == 0 & input$DSS.feature.flag == 
    1, "DSP", "none")))
input_no_none <- subset(input, ad_type != "none")
input <- subset(input_no_none, ad_days < 7)
```

``` r
# dealer distribution chi-test
dealers <- input[, c("dealer_id", "ad_type")]
dealers <- melt(dealers)
dealers_cast <- dcast(dealers, dealer_id ~ ad_type)
dealers_cast <- dealers_cast[, -1]
chisq.test(dealers_cast)
```

    ## 
    ##  Pearson's Chi-squared test
    ## 
    ## data:  dealers_cast
    ## X-squared = 1337.8, df = 1068, p-value = 2.955e-08

``` r
assocstats(table(dealers_cast$DSP, dealers_cast$TOP))
```

    ##                     X^2  df   P(> X^2)
    ## Likelihood Ratio 349.54 238 3.2554e-06
    ## Pearson          519.78 238 0.0000e+00
    ## 
    ## Phi-Coefficient   : NA 
    ## Contingency Coeff.: 0.702 
    ## Cramer's V        : 0.263

``` r
# KPI calculation
input$TOPadVIPadj <- input$vipViews_TopFeature * 0.3
input$adVIP <- (input$TOPadVIPadj + input$vipViews_DSPtestFeature)
input$ad_days_DSP_TOP <- (input$TopFeature_days + input$DSPtestFeature_days)

input$adVIP_day <- input$adVIP/input$ad_days_DSP_TOP
input$totVIP_day <- input$vipViews_siteGermany/input$ad_days

input$adVIP_ad <- input$adVIP/(input$TOP.feature.ads + input$DSS.feature.flag)
input$totVIP_ad <- input$vipViews_siteGermany/(input$TOP.feature.ads + input$DSS.feature.flag)
```

``` r
# outliers detection, exclusion
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

``` r
# outliers detection, exclusion
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

``` r
# KPI avg comparison
input_compare <- input_clean[, c("ad_type", "adVIP_day", "totVIP_day", "adVIP_ad", 
    "totVIP_ad")] %>% group_by(ad_type) %>% summarise_all(mean)
print(input_compare)
```

    ## # A tibble: 3 x 5
    ##   ad_type adVIP_day totVIP_day adVIP_ad totVIP_ad
    ##     <chr>     <dbl>      <dbl>    <dbl>     <dbl>
    ## 1     DSP  2.541346   29.57882 3.704475 107.53798
    ## 2   multi  3.803116   27.32826 7.584783  34.91304
    ## 3     TOP  1.840766   11.08473 5.273710  34.93065

``` r
# KPI Student t-test prep: F-test
input_clean_TOP <- subset(input_clean, ad_type == "TOP")
input_clean_TOP <- input_clean_TOP[, c("vipViews_siteGermany", "adVIP", "adVIP_day", 
    "totVIP_day", "adVIP_ad", "totVIP_ad")]
input_clean_DSP <- subset(input_clean, ad_type == "DSP")
input_clean_DSP <- input_clean_DSP[, c("vipViews_siteGermany", "adVIP", "adVIP_day", 
    "totVIP_day", "adVIP_ad", "totVIP_ad")]
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

``` r
# variances are NOT homogeneous
```

Plots:
------

### adVIP ~ totVIP

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%201-1.jpeg)

### Days displayed ~ totVIP

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%202-1.jpeg)

### Days displayed ~ totVIP - DSP

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%203-1.jpeg)

### Days displayed ~ totVIP - TOP

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%204-1.jpeg)

### Lightbox views ~ totVIP

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%205-1.jpeg)

### Lightbox views ~ totVIP by ad days

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%206-1.jpeg)

### adVIP ~ totVIP by ad days

![](DSP_TOP_files/figure-markdown_github-ascii_identifiers/chart%207-1.jpeg)
