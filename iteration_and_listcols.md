iteration\_and\_listcols
================
Xin He
10/24/2019

## get started

``` r
l = list(vec_numeric = 5:8,
         mat         = matrix(1:8, 2, 4),
         vec_logical = c(TRUE, FALSE),
         summary     = summary(rnorm(1000)))
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l$summary
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

``` r
l[[2]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

``` r
mean(l$vec_numeric)
```

    ## [1] 6.5

``` r
df = tibble(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

is.list(df)
```

    ## [1] TRUE

``` r
df = list(
  a = rnorm(20, 3, 1),
  b = rnorm(20, 0, 5),
  c = rnorm(20, 10, .2),
  d = rnorm(20, -3, 1)
)

is.list(df)
```

    ## [1] TRUE

``` r
df$a
```

    ##  [1] 3.7720863 2.8591606 3.3930939 3.2242186 3.0235420 2.3770373 4.2620094
    ##  [8] 2.5942260 3.6667638 3.1646392 4.7815245 3.7112140 2.6623088 2.9908510
    ## [15] 2.8746908 0.9091539 4.6973939 4.0638812 2.2333834 3.3820076

``` r
df[[2]]
```

    ##  [1]  1.2094795 -5.6637971  7.4495371 -1.2412355  0.9179185  2.0243550
    ##  [7] -4.9706223 -5.4271466 -0.2427128  2.8804280  0.3691527  3.5297279
    ## [13]  1.6749005  2.7269390 -7.0145295  3.3852695 -3.9490022 -2.3286444
    ## [19] -0.5242603 -8.2392554

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}


mean_and_sd(df[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.23 0.897

``` r
mean_and_sd(df[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.672  4.11

``` r
mean_and_sd(df[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.93 0.205

``` r
mean_and_sd(df[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.21 0.832

first for loop\!

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(df[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.23 0.897
    ## 
    ## [[2]]
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.672  4.11
    ## 
    ## [[3]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.93 0.205
    ## 
    ## [[4]]
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.21 0.832

``` r
output = map(df, mean_and_sd)
output_median = map(df, median)
output_summary = map(df, summary)

output_median = map_dbl(df, median)
output = map_dfr(df, mean_and_sd)

output = map(df, ~mean_and_sd(.x))
```

## Napo

``` r
read_page_reviews = function(url) {
  
  h = read_html(url)
  
  title = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  text = h %>%
    html_nodes(".review-data:nth-child(5)") %>%
    html_text()
  
  data_frame(title, stars, text)
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_urls = str_c(url_base, 1:5)

vec_urls
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
    ## [2] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"
    ## [3] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=3"
    ## [4] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=4"
    ## [5] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5"

``` r
read_page_reviews(vec_urls[[1]])
```

    ## Warning: `data_frame()` is deprecated, use `tibble()`.
    ## This warning is displayed once per session.

    ## # A tibble: 10 x 3
    ##    title                     stars text                                    
    ##    <chr>                     <dbl> <chr>                                   
    ##  1 "Gotta watch it!\n      …     5 "Super fun cult film. A must-see! Funni…
    ##  2 "Great movie\n          …     5 "Love this movie.\n            "        
    ##  3 "Duh\n            "           5 "Best movie ever\n            "         
    ##  4 "Great video\n          …     5 "Product as described.  Great transacti…
    ##  5 "Give me some of your to…     5 "This movie will always be my favorite …
    ##  6 "Nostalgic\n            "     5 "One of the best nostalgic movies of my…
    ##  7 "Make you giggle type mo…     5 "I love, love, love this movie.  It mak…
    ##  8 "This movie is so stupid…     5 "No, really.  It's so stupid.  Your IQ …
    ##  9 "Hilarious\n            "     5 "Hilarious\n            "               
    ## 10 "Waste of money\n       …     1 "Terrible movie! Please don’t waste you…

``` r
read_page_reviews(vec_urls[[2]])
```

    ## # A tibble: 10 x 3
    ##    title                         stars text                                
    ##    <chr>                         <dbl> <chr>                               
    ##  1 "Good movie\n            "        5 "Funny\n            "               
    ##  2 "A classic\n            "         5 "I like your sleeves. They're real …
    ##  3 "FRIKKEN SWEET MOVIE, GAWSH.…     5 "It’s Napolean Dynamite. It’s charm…
    ##  4 "You gonna eat the rest of y…     5 "One of my favorite movies ever.  Y…
    ##  5 "Tina you fat lard come get …     5 "It's a great movie\n            "  
    ##  6 "Great family movie\n       …     5 "My kids as well as the adults love…
    ##  7 "Teens love it\n            "     5 "Original and funny\n            "  
    ##  8 "Great\n            "             5 "Funny\n            "               
    ##  9 "Great Movie, Bad Packaging\…     4 "First off, the stick-on label on t…
    ## 10 "jeez napoleon\n            "     5 "gosh\n            "

``` r
read_page_reviews(vec_urls[[3]])
```

    ## # A tibble: 10 x 3
    ##    title                       stars text                                  
    ##    <chr>                       <dbl> <chr>                                 
    ##  1 "👍\n            "              5 "👍\n            "                    
    ##  2 "A classic!\n            "      5 "A classic movie.  Hilarious!\n      …
    ##  3 "A must own\n            "      5 "Great movie\n            "           
    ##  4 "If you like 80s ...you mu…     5 "My all time favorite movie. I have w…
    ##  5 "🤘\n            "              5 "🤘\n            "                    
    ##  6 "Super Slow Mooovie...\n  …     1 "Too slow and too damn quiet... My gi…
    ##  7 "Awesome!\n            "        5 "Love this movie !\n            "     
    ##  8 "Very funny\n            "      4 "Very funny\n            "            
    ##  9 "Eat your food tina\n     …     5 "Cant go wrong\n            "         
    ## 10 "Dumb funny\n            "      5 "Dumb funny\n            "

``` r
read_page_reviews(vec_urls[[4]])
```

    ## # A tibble: 10 x 3
    ##    title                           stars text                              
    ##    <chr>                           <dbl> <chr>                             
    ##  1 "Annoying! Not in a good way.\…     1 "I know that I am one of the very…
    ##  2 "Fun\n            "                 5 "Fun\n            "               
    ##  3 "such a great movie\n         …     5 "a true comedy classic\n         …
    ##  4 "Napoleon Dud\n            "        3 "Not impressed w/movie.\n        …
    ##  5 "Five stars\n            "          5 "Such a weird, awesome movie\n   …
    ##  6 "Fun!\n            "                5 "Great movie\n            "       
    ##  7 "Funny movie- bravo for Amazon…     5 "My son loves this movie, so I wa…
    ##  8 "Movie\n            "               5 "Movie\n            "             
    ##  9 "Funny movie, quotable lines\n…     5 "My kids quote this movie all the…
    ## 10 "Great for teenagers!\n       …     5 "My students loved this movie.\n …

``` r
read_page_reviews(vec_urls[[5]])
```

    ## # A tibble: 10 x 3
    ##    title                        stars text                                 
    ##    <chr>                        <dbl> <chr>                                
    ##  1 "can't believe we fell for …     1 "a pretty lame movie--can't believe …
    ##  2 "shut up tina you fat lard.…     5 "i LOVE napoleon.\n            "     
    ##  3 "Laughter is the Best Medic…     5 "FAST SHIPPING! Love this Movie! Lau…
    ##  4 "New condition\n           …     5 "Classic for the kids to watch.\n   …
    ##  5 "Napoleon, give me some of …     5 "Cul\n            "                  
    ##  6 "Yes rent\n            "         5 "Always an amazing movie, classic!\n…
    ##  7 "Cult classic.\n           …     5 "I should’ve bought this movie a lon…
    ##  8 "DIDN'T WORK\n            "      1 "I paid for the rental, but it's not…
    ##  9 "I\n            "                5 "I love this movie! My kids love thi…
    ## 10 "Laugh out loud\n          …     5 "Introduced my grandsons to this mov…

``` r
output = vector("list", length = 5)

for (i in 1:5) {
  output[[i]] = read_page_reviews(vec_urls[[i]])
}

output = map(vec_urls, read_page_reviews)

dynamite_reviews = bind_rows(output)

dynamite_reviews = map_df(vec_urls, read_page_reviews)
```

## list colums

``` r
weather = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2016-01-01",
    date_max = "2016-12-31") %>%
  mutate(
    name = recode(id, USW00094728 = "CentralPark_NY", 
                      USC00519397 = "Waikiki_HA",
                      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'crul':
    ##   method                 from
    ##   as.character.form_file httr

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## file path:          /Users/hexin/Library/Caches/rnoaa/ghcnd/USW00094728.dly

    ## file last updated:  2019-09-26 10:26:24

    ## file min/max dates: 1869-01-01 / 2019-09-30

    ## file path:          /Users/hexin/Library/Caches/rnoaa/ghcnd/USC00519397.dly

    ## file last updated:  2019-09-26 10:26:33

    ## file min/max dates: 1965-01-01 / 2019-09-30

    ## file path:          /Users/hexin/Library/Caches/rnoaa/ghcnd/USS0023B17S.dly

    ## file last updated:  2019-09-26 10:26:36

    ## file min/max dates: 1999-09-01 / 2019-09-30

nest within station

``` r
weather_nest = 
  nest(weather, data = date:tmin)

weather_nest
```

    ## # A tibble: 3 x 3
    ##   name           id                    data
    ##   <chr>          <chr>       <list<df[,4]>>
    ## 1 CentralPark_NY USW00094728      [366 × 4]
    ## 2 Waikiki_HA     USC00519397      [366 × 4]
    ## 3 Waterhole_WA   USS0023B17S      [366 × 4]

is the list colum really a list?

``` r
weather_nest %>% pull(name)
```

    ## [1] "CentralPark_NY" "Waikiki_HA"     "Waterhole_WA"

``` r
weather_nest %>% pull(data)
```

    ## <list_of<
    ##   tbl_df<
    ##     date: date
    ##     prcp: double
    ##     tmax: double
    ##     tmin: double
    ##   >
    ## >[3]>
    ## [[1]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   5.6   1.1
    ##  2 2016-01-02     0   4.4   0  
    ##  3 2016-01-03     0   7.2   1.7
    ##  4 2016-01-04     0   2.2  -9.9
    ##  5 2016-01-05     0  -1.6 -11.6
    ##  6 2016-01-06     0   5    -3.8
    ##  7 2016-01-07     0   7.8  -0.5
    ##  8 2016-01-08     0   7.8  -0.5
    ##  9 2016-01-09     0   8.3   4.4
    ## 10 2016-01-10   457  15     4.4
    ## # … with 356 more rows
    ## 
    ## [[2]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0  29.4  16.7
    ##  2 2016-01-02     0  28.3  16.7
    ##  3 2016-01-03     0  28.3  16.7
    ##  4 2016-01-04     0  28.3  16.1
    ##  5 2016-01-05     0  27.2  16.7
    ##  6 2016-01-06     0  27.2  20  
    ##  7 2016-01-07    46  27.8  18.3
    ##  8 2016-01-08     3  28.3  17.8
    ##  9 2016-01-09     8  27.8  19.4
    ## 10 2016-01-10     3  28.3  18.3
    ## # … with 356 more rows
    ## 
    ## [[3]]
    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   1.7  -5.9
    ##  2 2016-01-02    25  -0.1  -6  
    ##  3 2016-01-03     0  -5   -10  
    ##  4 2016-01-04    25   0.3  -9.8
    ##  5 2016-01-05    25   1.9  -1.8
    ##  6 2016-01-06    25   1.4  -2.6
    ##  7 2016-01-07     0   1.4  -3.9
    ##  8 2016-01-08     0   1.1  -4  
    ##  9 2016-01-09     0   1.4  -4.5
    ## 10 2016-01-10     0   2.3  -3.8
    ## # … with 356 more rows

``` r
weather_nest$data[[1]]
```

    ## # A tibble: 366 x 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2016-01-01     0   5.6   1.1
    ##  2 2016-01-02     0   4.4   0  
    ##  3 2016-01-03     0   7.2   1.7
    ##  4 2016-01-04     0   2.2  -9.9
    ##  5 2016-01-05     0  -1.6 -11.6
    ##  6 2016-01-06     0   5    -3.8
    ##  7 2016-01-07     0   7.8  -0.5
    ##  8 2016-01-08     0   7.8  -0.5
    ##  9 2016-01-09     0   8.3   4.4
    ## 10 2016-01-10   457  15     4.4
    ## # … with 356 more rows
