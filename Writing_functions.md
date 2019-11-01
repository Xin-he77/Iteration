Writing\_functions
================
Xin He
10/24/2019

## get started

``` r
x = rnorm(n =  30, mean = 4, sd = 2.3)

x_again = rnorm(n =  30, mean = 6, sd = .3)
(x - mean(x) / sd(x))
```

    ##  [1]  0.5879978  2.4512212  0.1068957  5.6979874  2.7867094  0.1417642
    ##  [7]  3.1499283  3.7269883  3.3531386  1.3264482  5.5059382  2.9254810
    ## [13]  0.5999882 -3.0649682  4.6161826  1.9254942  1.9916039  4.1996648
    ## [19]  3.9176503  3.3948146  4.1424895  3.8277550  2.2003410 -2.5466674
    ## [25]  3.4544407  1.8997454  1.6705119 -1.3538890  0.9290964  2.9901071

``` r
(x_again - mean(x_again) / sd(x_again))
```

    ##  [1] -18.90640 -19.34484 -19.19770 -19.33015 -19.72712 -19.43850 -19.43229
    ##  [8] -19.33180 -18.98400 -19.08505 -19.36336 -19.39001 -19.10492 -19.14701
    ## [15] -19.52063 -19.52625 -19.20463 -19.08345 -19.34771 -19.04967 -19.19457
    ## [22] -19.49761 -19.21167 -19.65281 -18.88410 -18.71989 -19.42417 -19.62725
    ## [29] -19.14309 -19.35452

``` r
z_score = function(x) {
  
  if(!is.numeric(x)) {
    stop ("Argument x should be numeric")
  } else if(length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  (x - mean(x) / sd(x))
  
}
```

Try out

``` r
mean_and_sd = function(mean_x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) < 3) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

test

``` r
mean_and_sd (mean_x = y)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.19  2.13

## Multiple inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 1, sd = 1),
  y = 2 + 3 * x + rnorm(30, 0, 1)
)

ls_fit = lm(y ~ x, data = sim_data)
  
beta0_hat = coef(ls_fit)[1]
beta1_hat = coef(ls_fit)[2]
```

``` r
sim_regression = function(n, beta0 = 2, beta1 = 3) {
  
  sim_data = tibble(
    x = rnorm(n, mean = 1, sd = 1),
    y = beta0 + beta1 * x + rnorm(n, 0, 1)
  )
  
  ls_fit = lm(y ~ x, data = sim_data)
  
  tibble(
    beta0_hat = coef(ls_fit)[1],
    beta1_hat = coef(ls_fit)[2]
  )
}

sim_regression(n = 3000, beta0 = 1, beta1 = 4)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1     0.977      4.01

``` r
sim_regression(3000, 2, 3)
```

    ## # A tibble: 1 x 2
    ##   beta0_hat beta1_hat
    ##       <dbl>     <dbl>
    ## 1      2.02      2.98

## scrape lots of napoleon

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-title") %>%
  html_text()

review_stars = dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = dynamite_html %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

now as a function

``` r
read_page_reviews <- function(url) {
  
  h = read_html(url)
  
  review_titles = h %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  review_stars = h %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  review_text = h %>%
    html_nodes(".review-data:nth-child(4)") %>%
    html_text()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

## if want to remove x

``` r
rm(x)
```
