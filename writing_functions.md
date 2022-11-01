Writing Functions
================

## Z-scores!!

Let’s compute the z-score version of a list of numbers.

``` r
x_vec = rnorm(25, mean = 7, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -2.45811255  0.52060575  0.40239516  1.19871296  0.86392332 -0.22071163
    ##  [7] -0.22910826  1.44032635  1.51292703 -0.24112954  0.58082936 -0.19679756
    ## [13] -0.06551075  0.78644286  0.14549738 -1.05825197 -1.33739153  0.90446375
    ## [19]  1.02437006 -0.12354229  0.65450386 -0.57672797 -1.13972370 -0.97467881
    ## [25] -1.41331127

Suppose you want to do this often.

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Z scores only work for numbers")
  }
  
  if (length(x) < 3) {
    stop("Z scores really only work if you have three or more numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
  
}
```

``` r
z_scores(x = x_vec)

z_scores(x = 1:10)
z_scores(x = rbinom(1000, 1, .6))

z_scores(x = 3)
z_scores(x = "my name is jeff")
```

## Let’s have multiple outputs

Let’s just get the mean and sd from the vector input.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Z scores only work for numbers")
  }
  
  if (length(x) < 3) {
    stop("Z scores really only work if you have three or more numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}

mean_and_sd(x = x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.99  3.84

``` r
mean_and_sd(x = 1:10)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1   5.5  3.03

``` r
mean_and_sd(x = rbinom(1000, 1, .5))
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.501 0.500

## Let’s start with simulations …

``` r
x_vec = rnorm(n = 25000, mean = 17, sd = 4)

tibble(
  mean = mean(x_vec),
  sd = sd(x_vec)
)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  17.0  3.99

Can I do this using a function … YUP

``` r
sim_mean_sd = function(n_obs, true_mean = 7, true_sd = 4) {
  
  x = rnorm(n = n_obs, mean = true_mean, sd = true_sd)

  tibble(
    mean = mean(x),
    sd = sd(x)
  )
  
}
```

does it work?

``` r
sim_mean_sd(true_mean = 2500, n_obs = 10, 7)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 2501.  6.67

## Fixing bad stuff

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Let’s write a function to get reviews.

``` r
read_page_reviews = function(url) {
  
  dynamite_html = read_html(url)

  review_titles = 
    dynamite_html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    dynamite_html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    dynamite_html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)
  
  reviews = 
    tibble(
      title = review_titles,
      stars = review_stars,
      text = review_text
    )
  
  reviews
  
}
```

Let’s try with a URL

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=5"

read_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                                                             stars text 
    ##    <chr>                                                             <dbl> <chr>
    ##  1 On a scale of 1 to 10 this rates a minus                              1 "Thi…
    ##  2 I always wondered...                                                  5 "wha…
    ##  3 Audio/video not synced                                                1 "I t…
    ##  4 Kind of feels like only a bully would actually laugh at this...       1 "...…
    ##  5 movie                                                                 5 "goo…
    ##  6 An Overdose of Comical Cringe                                         5 "Exc…
    ##  7 Glad I never wasted money on this                                     2 "I r…
    ##  8 A little disappointed                                                 3 "The…
    ##  9 An (almost) gem. Brought me back to the sweet awkwardness of hig…     5 "To …
    ## 10 How Could You Not Love Napoleon??                                     5 "I r…

What good does this do?

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_url = str_c(base_url, c(1, 2, 3, 4, 5))

dynamite_reviews = 
  bind_rows(
    read_page_reviews(vec_url[1]),
    read_page_reviews(vec_url[2]),
    read_page_reviews(vec_url[3]),
    read_page_reviews(vec_url[4]),
    read_page_reviews(vec_url[5])
  )
```
