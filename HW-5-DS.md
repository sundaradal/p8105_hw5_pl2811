---
title: "HW5"
author: "Pei Hsin Lin"
date: "11/17/2021"
output: 
  html_document:
    keep_md: true
---
 
#problem 1 


```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
```

```
## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
## ✓ tibble  3.1.4     ✓ dplyr   1.0.7
## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
## ✓ readr   2.0.2     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library (readr)
library(reshape2)
```

```
## 
## Attaching package: 'reshape2'
```

```
## The following object is masked from 'package:tidyr':
## 
##     smiths
```

```r
library(ggplot2)
library(mizer)
library(ggplot2)
library(plotly)
```

```
## 
## Attaching package: 'plotly'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     last_plot
```

```
## The following object is masked from 'package:stats':
## 
##     filter
```

```
## The following object is masked from 'package:graphics':
## 
##     layout
```


```r
urlfile="https://raw.githubusercontent.com/washingtonpost/data-homicides/master/homicide-data.csv"

#change unknown into NA, variable become numeric


homicide_df<-read_csv(url(urlfile), na=c("","Unknown")) %>%
  mutate(
    city_state=str_c(city, state),
  resolution=case_when(
    disposition=="Closed without arrest" ~ "unsolved",
    disposition=="Open/No arrest" ~ "unsolved",
    disposition=="Closed by arrest" ~ "solved"
    ))%>%
  relocate(city_state)%>%
filter(city_state != "TulsaAL")
```

```
## Rows: 52179 Columns: 12
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (8): uid, victim_last, victim_first, victim_race, victim_sex, city, stat...
## dbl (4): reported_date, victim_age, lat, lon
```

```
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

Let's focus on Baltimore, MD.

```r
baltimore_df=
  homicide_df %>%
  filter(city_state=="BaltimoreMD")

baltimore_summary=
  baltimore_df %>%
  summarize( 
    unsolved=sum(resolution=="unsolved"),
    n=n()
    
  )



baltimore_test=
  prop.test(x=baltimore_summary %>% pull (unsolved),
          n=baltimore_summary %>% pull (n))

baltimore_test %>%
  broom::tidy()
```

```
## # A tibble: 1 × 8
##   estimate statistic  p.value parameter conf.low conf.high method    alternative
##      <dbl>     <dbl>    <dbl>     <int>    <dbl>     <dbl> <chr>     <chr>      
## 1    0.646      239. 6.46e-54         1    0.628     0.663 1-sample… two.sided
```

#Let's try to iterate across cities!

First off, write a function and test it on a few sample cities


```r
prop_test_function= function(city_df){
  
  city_summary=
   city_df %>%
  summarize( 
    unsolved=sum(resolution=="unsolved"),
    n=n()
    
  )

city_test=
  prop.test(x=city_summary %>% pull (unsolved),
          n=city_summary %>% pull (n))

return(city_test)

}

prop_test_function(baltimore_df)
```

```
## 
## 	1-sample proportions test with continuity correction
## 
## data:  city_summary %>% pull(unsolved) out of city_summary %>% pull(n), null probability 0.5
## X-squared = 239.01, df = 1, p-value < 2.2e-16
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
##  0.6275625 0.6631599
## sample estimates:
##         p 
## 0.6455607
```

```r
  homicide_df %>%
  filter(city_state=="AlbuquerqueNM") %>%
  prop_test_function()
```

```
## 
## 	1-sample proportions test with continuity correction
## 
## data:  city_summary %>% pull(unsolved) out of city_summary %>% pull(n), null probability 0.5
## X-squared = 19.114, df = 1, p-value = 1.232e-05
## alternative hypothesis: true p is not equal to 0.5
## 95 percent confidence interval:
##  0.3372604 0.4375766
## sample estimates:
##         p 
## 0.3862434
```

Now, let's iterate acroos all cities

```r
  results_df=
  homicide_df %>%
  nest(data=uid:resolution) %>%
mutate(
  test_results=map(data, prop_test_function),
  tidy_results=map(test_results, broom::tidy)
  
)%>%
  select(city_state, tidy_results)%>%
unnest(tidy_results)%>%
  select(city_state, estimate, starts_with("conf"))
```


```r
results_df %>%
  mutate(city_state=fct_reorder(city_state, estimate))%>%
  ggplot(aes(x=city_state, y=estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
    theme(axis.text.x = element_text(angle=90, vjust=0.5, hjust=1))
```

![](HW-5-DS_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
homicide_df %>%
  group_by(city_state)%>%
  summarise(unsolved=sum(resolution=="unsolved"),
            n=n()
          )%>%
  mutate(
test_results=map2(unsolved, n, prop.test),
tidy_results=map(test_results,broom::tidy)
  )%>%
  unnest(tidy_results)%>%
  select(city_state, estimate, starts_with("conf"))
```

```
## # A tibble: 50 × 4
##    city_state    estimate conf.low conf.high
##    <chr>            <dbl>    <dbl>     <dbl>
##  1 AlbuquerqueNM    0.386    0.337     0.438
##  2 AtlantaGA        0.383    0.353     0.415
##  3 BaltimoreMD      0.646    0.628     0.663
##  4 Baton RougeLA    0.462    0.414     0.511
##  5 BirminghamAL     0.434    0.399     0.469
##  6 BostonMA         0.505    0.465     0.545
##  7 BuffaloNY        0.612    0.569     0.654
##  8 CharlotteNC      0.300    0.266     0.336
##  9 ChicagoIL        0.736    0.724     0.747
## 10 CincinnatiOH     0.445    0.408     0.483
## # … with 40 more rows
```

##problem 2


```r
file_con<- tibble(file=list.files(path ="/Users/lin/Desktop/data/", pattern = "*con*"))

file_exp<- tibble(file=list.files(path ="/Users/lin/Desktop/data/", pattern = "*exp*"))  


datacon <-file_con %>%
  map(~ read_csv(file.path("/Users/lin/Desktop/data/", .))) %>% 
  reduce(rbind) %>% 
  mutate(type="con")%>%
   dplyr::mutate(subject_id = dplyr::row_number()) %>%
   relocate(subject_id, type)%>% 
 arrange(subject_id)
```

```
## Rows: 10 Columns: 8
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8
```

```
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
dataexp <-file_exp %>%
  map(~ read_csv(file.path("/Users/lin/Desktop/data/", .))) %>% 
  reduce(rbind )%>% 
  mutate(type="exp") %>%
   dplyr::mutate(subject_id = dplyr::row_number()) %>%
   relocate(subject_id, type)%>% 
 arrange(subject_id)
```

```
## Rows: 10 Columns: 8
```

```
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## dbl (8): week_1, week_2, week_3, week_4, week_5, week_6, week_7, week_8
```

```
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
arm<-union(dataexp, datacon) %>% 
  arrange(subject_id)
arm
```

```
## # A tibble: 20 × 10
##    subject_id type  week_1 week_2 week_3 week_4 week_5 week_6 week_7 week_8
##         <int> <chr>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
##  1          1 exp     3.05   3.67   4.84   5.8    6.33   5.46   6.38   5.91
##  2          1 con     0.2   -1.31   0.66   1.96   0.23   1.09   0.05   1.94
##  3          2 exp    -0.84   2.63   1.64   2.58   1.24   2.32   3.11   3.78
##  4          2 con     1.13  -0.88   1.07   0.17  -0.83  -0.31   1.58   0.44
##  5          3 exp     2.15   2.08   1.82   2.84   3.36   3.61   3.37   3.74
##  6          3 con     1.77   3.11   2.22   3.26   3.31   0.89   1.88   1.01
##  7          4 exp    -0.62   2.54   3.78   2.73   4.49   5.82   6      6.49
##  8          4 con     1.04   3.66   1.22   2.33   1.47   2.7    1.87   1.66
##  9          5 exp     0.7    3.33   5.34   5.57   6.9    6.66   6.24   6.95
## 10          5 con     0.47  -0.58  -0.09  -1.37  -0.32  -2.17   0.45   0.48
## 11          6 exp     3.73   4.08   5.4    6.41   4.87   6.09   7.66   5.83
## 12          6 con     2.37   2.5    1.59  -0.16   2.08   3.07   0.78   2.35
## 13          7 exp     1.18   2.35   1.23   1.17   2.02   1.61   3.13   4.88
## 14          7 con     0.03   1.21   1.13   0.64   0.49  -0.12  -0.07   0.46
## 15          8 exp     1.37   1.43   1.84   3.6    3.8    4.72   4.68   5.7 
## 16          8 con    -0.08   1.42   0.09   0.36   1.18  -1.16   0.33  -0.44
## 17          9 exp    -0.4    1.08   2.66   2.7    2.8    2.64   3.51   3.27
## 18          9 con     0.08   1.24   1.44   0.41   0.95   2.75   0.3    0.03
## 19         10 exp     1.09   2.8    2.8    4.3    2.25   6.57   6.09   4.64
## 20         10 con     2.14   1.15   2.52   3.44   4.26   0.97   2.73  -0.53
```

```r
df2 <- melt(arm, id.var = c("type", "subject_id")) 
df2[,'subject_id'] <- as.factor(as.character(df2[,'subject_id']))

# plot

fig<-plot_ly(df2) %>% 
    add_lines(x = ~variable, y = ~value, 
              color = ~subject_id, linetype = ~type)
fig<-fig %>%
  layout(title = 'Arm experiment', xaxis = list(title = 'Week'), 
         yaxis = list(title = 'arm data'),legend = list(title=list(text='control and experimental <br> of observations')))
fig
```

```
## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors

## Warning in RColorBrewer::brewer.pal(N, "Set2"): n too large, allowed maximum for palette Set2 is 8
## Returning the palette you asked for with that many colors
```

```{=html}
<div id="htmlwidget-fddad44669f6bd738f0c" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-fddad44669f6bd738f0c">{"x":{"visdat":{"53512e9f7bd6":["function () ","plotlyVisDat"]},"cur_data":"53512e9f7bd6","attrs":{"53512e9f7bd6":{"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"x":{},"y":{},"type":"scatter","mode":"lines","color":{},"linetype":{},"inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Arm experiment","xaxis":{"domain":[0,1],"automargin":true,"title":"Week","type":"category","categoryorder":"array","categoryarray":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"]},"yaxis":{"domain":[0,1],"automargin":true,"title":"arm data"},"legend":{"title":{"text":"control and experimental <br> of observations"}},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[0.2,-1.31,0.66,1.96,0.23,1.09,0.05,1.94],"type":"scatter","mode":"lines","name":"1<br />con","line":{"color":"rgba(102,194,165,1)","dash":"solid"},"marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[3.05,3.67,4.84,5.8,6.33,5.46,6.38,5.91],"type":"scatter","mode":"lines","name":"1<br />exp","line":{"color":"rgba(102,194,165,1)","dash":"dash"},"marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[2.14,1.15,2.52,3.44,4.26,0.97,2.73,-0.53],"type":"scatter","mode":"lines","name":"10<br />con","line":{"color":"rgba(228,156,113,1)","dash":"solid"},"marker":{"color":"rgba(228,156,113,1)","line":{"color":"rgba(228,156,113,1)"}},"textfont":{"color":"rgba(228,156,113,1)"},"error_y":{"color":"rgba(228,156,113,1)"},"error_x":{"color":"rgba(228,156,113,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[1.09,2.8,2.8,4.3,2.25,6.57,6.09,4.64],"type":"scatter","mode":"lines","name":"10<br />exp","line":{"color":"rgba(228,156,113,1)","dash":"dash"},"marker":{"color":"rgba(228,156,113,1)","line":{"color":"rgba(228,156,113,1)"}},"textfont":{"color":"rgba(228,156,113,1)"},"error_y":{"color":"rgba(228,156,113,1)"},"error_x":{"color":"rgba(228,156,113,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[1.13,-0.88,1.07,0.17,-0.83,-0.31,1.58,0.44],"type":"scatter","mode":"lines","name":"2<br />con","line":{"color":"rgba(201,152,157,1)","dash":"solid"},"marker":{"color":"rgba(201,152,157,1)","line":{"color":"rgba(201,152,157,1)"}},"textfont":{"color":"rgba(201,152,157,1)"},"error_y":{"color":"rgba(201,152,157,1)"},"error_x":{"color":"rgba(201,152,157,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[-0.84,2.63,1.64,2.58,1.24,2.32,3.11,3.78],"type":"scatter","mode":"lines","name":"2<br />exp","line":{"color":"rgba(201,152,157,1)","dash":"dash"},"marker":{"color":"rgba(201,152,157,1)","line":{"color":"rgba(201,152,157,1)"}},"textfont":{"color":"rgba(201,152,157,1)"},"error_y":{"color":"rgba(201,152,157,1)"},"error_x":{"color":"rgba(201,152,157,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[1.77,3.11,2.22,3.26,3.31,0.89,1.88,1.01],"type":"scatter","mode":"lines","name":"3<br />con","line":{"color":"rgba(175,154,200,1)","dash":"solid"},"marker":{"color":"rgba(175,154,200,1)","line":{"color":"rgba(175,154,200,1)"}},"textfont":{"color":"rgba(175,154,200,1)"},"error_y":{"color":"rgba(175,154,200,1)"},"error_x":{"color":"rgba(175,154,200,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[2.15,2.08,1.82,2.84,3.36,3.61,3.37,3.74],"type":"scatter","mode":"lines","name":"3<br />exp","line":{"color":"rgba(175,154,200,1)","dash":"dash"},"marker":{"color":"rgba(175,154,200,1)","line":{"color":"rgba(175,154,200,1)"}},"textfont":{"color":"rgba(175,154,200,1)"},"error_y":{"color":"rgba(175,154,200,1)"},"error_x":{"color":"rgba(175,154,200,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[1.04,3.66,1.22,2.33,1.47,2.7,1.87,1.66],"type":"scatter","mode":"lines","name":"4<br />con","line":{"color":"rgba(226,148,184,1)","dash":"solid"},"marker":{"color":"rgba(226,148,184,1)","line":{"color":"rgba(226,148,184,1)"}},"textfont":{"color":"rgba(226,148,184,1)"},"error_y":{"color":"rgba(226,148,184,1)"},"error_x":{"color":"rgba(226,148,184,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[-0.62,2.54,3.78,2.73,4.49,5.82,6,6.49],"type":"scatter","mode":"lines","name":"4<br />exp","line":{"color":"rgba(226,148,184,1)","dash":"dash"},"marker":{"color":"rgba(226,148,184,1)","line":{"color":"rgba(226,148,184,1)"}},"textfont":{"color":"rgba(226,148,184,1)"},"error_y":{"color":"rgba(226,148,184,1)"},"error_x":{"color":"rgba(226,148,184,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[0.47,-0.58,-0.09,-1.37,-0.32,-2.17,0.45,0.48],"type":"scatter","mode":"lines","name":"5<br />con","line":{"color":"rgba(176,208,99,1)","dash":"solid"},"marker":{"color":"rgba(176,208,99,1)","line":{"color":"rgba(176,208,99,1)"}},"textfont":{"color":"rgba(176,208,99,1)"},"error_y":{"color":"rgba(176,208,99,1)"},"error_x":{"color":"rgba(176,208,99,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[0.7,3.33,5.34,5.57,6.9,6.66,6.24,6.95],"type":"scatter","mode":"lines","name":"5<br />exp","line":{"color":"rgba(176,208,99,1)","dash":"dash"},"marker":{"color":"rgba(176,208,99,1)","line":{"color":"rgba(176,208,99,1)"}},"textfont":{"color":"rgba(176,208,99,1)"},"error_y":{"color":"rgba(176,208,99,1)"},"error_x":{"color":"rgba(176,208,99,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[2.37,2.5,1.59,-0.16,2.08,3.07,0.78,2.35],"type":"scatter","mode":"lines","name":"6<br />con","line":{"color":"rgba(227,217,62,1)","dash":"solid"},"marker":{"color":"rgba(227,217,62,1)","line":{"color":"rgba(227,217,62,1)"}},"textfont":{"color":"rgba(227,217,62,1)"},"error_y":{"color":"rgba(227,217,62,1)"},"error_x":{"color":"rgba(227,217,62,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[3.73,4.08,5.4,6.41,4.87,6.09,7.66,5.83],"type":"scatter","mode":"lines","name":"6<br />exp","line":{"color":"rgba(227,217,62,1)","dash":"dash"},"marker":{"color":"rgba(227,217,62,1)","line":{"color":"rgba(227,217,62,1)"}},"textfont":{"color":"rgba(227,217,62,1)"},"error_y":{"color":"rgba(227,217,62,1)"},"error_x":{"color":"rgba(227,217,62,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[0.03,1.21,1.13,0.64,0.49,-0.12,-0.07,0.46],"type":"scatter","mode":"lines","name":"7<br />con","line":{"color":"rgba(245,207,100,1)","dash":"solid"},"marker":{"color":"rgba(245,207,100,1)","line":{"color":"rgba(245,207,100,1)"}},"textfont":{"color":"rgba(245,207,100,1)"},"error_y":{"color":"rgba(245,207,100,1)"},"error_x":{"color":"rgba(245,207,100,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[1.18,2.35,1.23,1.17,2.02,1.61,3.13,4.88],"type":"scatter","mode":"lines","name":"7<br />exp","line":{"color":"rgba(245,207,100,1)","dash":"dash"},"marker":{"color":"rgba(245,207,100,1)","line":{"color":"rgba(245,207,100,1)"}},"textfont":{"color":"rgba(245,207,100,1)"},"error_y":{"color":"rgba(245,207,100,1)"},"error_x":{"color":"rgba(245,207,100,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[-0.08,1.42,0.09,0.36,1.18,-1.16,0.33,-0.44],"type":"scatter","mode":"lines","name":"8<br />con","line":{"color":"rgba(219,192,155,1)","dash":"solid"},"marker":{"color":"rgba(219,192,155,1)","line":{"color":"rgba(219,192,155,1)"}},"textfont":{"color":"rgba(219,192,155,1)"},"error_y":{"color":"rgba(219,192,155,1)"},"error_x":{"color":"rgba(219,192,155,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[1.37,1.43,1.84,3.6,3.8,4.72,4.68,5.7],"type":"scatter","mode":"lines","name":"8<br />exp","line":{"color":"rgba(219,192,155,1)","dash":"dash"},"marker":{"color":"rgba(219,192,155,1)","line":{"color":"rgba(219,192,155,1)"}},"textfont":{"color":"rgba(219,192,155,1)"},"error_y":{"color":"rgba(219,192,155,1)"},"error_x":{"color":"rgba(219,192,155,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[0.08,1.24,1.44,0.41,0.95,2.75,0.3,0.03],"type":"scatter","mode":"lines","name":"9<br />con","line":{"color":"rgba(179,179,179,1)","dash":"solid"},"marker":{"color":"rgba(179,179,179,1)","line":{"color":"rgba(179,179,179,1)"}},"textfont":{"color":"rgba(179,179,179,1)"},"error_y":{"color":"rgba(179,179,179,1)"},"error_x":{"color":"rgba(179,179,179,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["week_1","week_2","week_3","week_4","week_5","week_6","week_7","week_8"],"y":[-0.4,1.08,2.66,2.7,2.8,2.64,3.51,3.27],"type":"scatter","mode":"lines","name":"9<br />exp","line":{"color":"rgba(179,179,179,1)","dash":"dash"},"marker":{"color":"rgba(179,179,179,1)","line":{"color":"rgba(179,179,179,1)"}},"textfont":{"color":"rgba(179,179,179,1)"},"error_y":{"color":"rgba(179,179,179,1)"},"error_x":{"color":"rgba(179,179,179,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

##problem 3

```r
library(tidyverse)

set.seed(10)
  iris_with_missing = iris %>% 
  map_df(~replace(.x, sample(1:150, 20), NA)) %>%
  mutate(Species = as.character(Species))
```

write a function


```r
fill_in_missing=function(vector){
  if(is.numeric(vector)){
    fill_value=round(mean(!is.na(vector)),1)
    vector[which(is.na(vector))]=fill_value
  }
   if(is.character(vector)){
     vector[which(is.na(vector))]="virginica"
     
   }
return(vector)
}

 iris_with_missing=
  iris_with_missing %>%
  mutate(
    Sepal.Length=map(Sepal.Length, fill_in_missing),
    Sepal.Width=map(Sepal.Width,fill_in_missing),
    Petal.Length=map(Petal.Length, fill_in_missing),
    Petal.Width=map(Petal.Width, fill_in_missing),
    Species=map(Species, fill_in_missing)
    ) %>% unnest(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width, Species)
 
  iris_with_missing
```

```
## # A tibble: 150 × 5
##    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
##           <dbl>       <dbl>        <dbl>       <dbl> <chr>  
##  1          5.1         3.5          1.4         0.2 setosa 
##  2          4.9         3            1.4         0.2 setosa 
##  3          4.7         3.2          1.3         0.2 setosa 
##  4          4.6         3.1          1.5         0   setosa 
##  5          5           3.6          1.4         0.2 setosa 
##  6          5.4         3.9          1.7         0.4 setosa 
##  7          0           3.4          1.4         0.3 setosa 
##  8          5           3.4          1.5         0.2 setosa 
##  9          4.4         2.9          1.4         0.2 setosa 
## 10          4.9         3.1          0           0.1 setosa 
## # … with 140 more rows
```
