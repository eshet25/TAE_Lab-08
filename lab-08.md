Lab 08 - University of Edinburgh Art Collection
================
Tsion

## Load Packages and Data

First, let’s load the necessary packages:

``` r
library(tidyverse) 
library(skimr)
library(robotstxt)

paths_allowed("https://collections.ed.ac.uk/art")
```

    ## [1] TRUE

Now, load the dataset. If your data isn’t ready yet, you can leave
`eval = FALSE` for now and update it when needed.

``` r
# Remove eval = FALSE or set it to TRUE once data is ready to be loaded
uoe_art <- read_csv("data/uoe-art.csv")
```

``` r
glimpse(uoe_art)
```

    ## Rows: 220
    ## Columns: 3
    ## $ title  <chr> "Frieze from the Temple of Athena Nike (left slab) (1827)", "Un…
    ## $ artist <chr> NA, "Gillian Curry", "Claudia Petretti", "Michael T R B Turnbul…
    ## $ link   <chr> "https://collections.ed.ac.uk/art/record/20625?highlight=*:*", …

## Exercise 10

Let’s start working with the **title** column by separating the title
and the date:

``` r
uoe_art <- uoe_art %>%
  separate(title, into = c("title", "date"), sep = "\\(") %>%
  mutate(year = str_remove(date, "\\)") %>% as.numeric()) %>%
  select(title, artist, year, link)  
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 7 rows [1, 7, 64, 85, 106,
    ## 126, 154].

    ## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 26 rows [23, 28, 35, 41,
    ## 44, 45, 46, 70, 72, 78, 80, 82, 84, 115, 123, 133, 151, 157, 160, 162, ...].

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `year = str_remove(date, "\\)") %>% as.numeric()`.
    ## Caused by warning in `str_remove(date, "\\)") %>% as.numeric()`:
    ## ! NAs introduced by coercion

When I ran this chunk, two warnings appeared. The first warning says
that 26 rows were missing the second piece after separating the title at
the opening parenthesis. This means those titles did not have a date in
parentheses, so R filled the `date` column with `NA`.

The second warning says that some values became `NA` when converting the
date column to numeric. This happened because not every value in the
date column was a clean year. Some titles may have extra text, unclear
dates, or missing dates, so R could not turn them into numbers.

In my opinion, these warnings make sense because this is scraped data
from a real website, and the titles are not all formatted the same way.
The code still works, but it shows that the dataset needs some cleaning
before analyzing the years?

## Exercise 11

Now I will use `skim()` to summarize the cleaned dataset.

``` r
skim(uoe_art)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | uoe_art |
| Number of rows                                   | 220     |
| Number of columns                                | 4       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 3       |
| numeric                                          | 1       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| title         |         0 |          1.00 |   3 |  68 |     0 |      142 |          0 |
| artist        |        16 |          0.93 |   2 |  37 |     0 |      155 |          0 |
| link          |         0 |          1.00 |  59 |  60 |     0 |      220 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |    sd |   p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|--------:|------:|-----:|-----:|-----:|-----:|-----:|:------|
| year          |        99 |          0.55 | 1967.33 | 16.34 | 1932 | 1958 | 1963 | 1968 | 2020 | ▁▇▂▁▁ |

16 pieces have title mising. And 99 have year missing. This is a bit too
high of a number I think for years to be missing? hmm cause the data has
220 obs?

``` r
uoe_art <- read_csv("data/uoe-art.csv", show_col_types = FALSE)

uoe_art %>%
  select(title) %>%
  print(n = 20)
```

    ## # A tibble: 220 × 1
    ##    title                                                   
    ##    <chr>                                                   
    ##  1 Frieze from the Temple of Athena Nike (left slab) (1827)
    ##  2 Untitled (2002)                                         
    ##  3 Man in Black (possibly)                                 
    ##  4 Untitled - Kings and Queens (1969)                      
    ##  5 untitled (1984)                                         
    ##  6 Composition with Sinking Ship and Lightbulb (1986)      
    ##  7 2. (a) Baskerville Serif (1972)                         
    ##  8 Untitled - Portrait of a Nude Female (1972)             
    ##  9 Circus (1950)                                           
    ## 10 Untitled - Abstract Bridge and Ballons (1968)           
    ## 11 Untitled (Unknown)                                      
    ## 12 Seated Female Nude (1939)                               
    ## 13 Seated Nude (1957)                                      
    ## 14 Stretching Male Nude (Circa 1953)                       
    ## 15 Untitled (1969)                                         
    ## 16 Untitled (1955)                                         
    ## 17 Reclining Nude (1968)                                   
    ## 18 Portrait of a Woman (1959)                              
    ## 19 Untitled (Unknown)                                      
    ## 20 South Frieze of the Parthenon Frieze (1836-1837)        
    ## # ℹ 200 more rows

### Let me try rerunning Exercise 10 first.

``` r
#Trying to reload the original scraped data so the title still has the year in parentheses
uoe_art <- read_csv("data/uoe-art.csv", show_col_types = FALSE)

# Extract the first 4-digit year from the title
# Then remove the final parentheses from the title
uoe_art <- uoe_art %>%
  mutate(
    year = str_extract(title, "\\d{4}"),
    year = as.numeric(year),
    title = str_remove(title, "\\s*\\([^\\)]*\\)$")
  ) %>%
  select(title, artist, year, link)

uoe_art
```

    ## # A tibble: 220 × 4
    ##    title                                             artist           year link 
    ##    <chr>                                             <chr>           <dbl> <chr>
    ##  1 Frieze from the Temple of Athena Nike (left slab) <NA>             1827 http…
    ##  2 Untitled                                          Gillian Curry    2002 http…
    ##  3 Man in Black                                      Claudia Petret…    NA http…
    ##  4 Untitled - Kings and Queens                       Michael T R B …  1969 http…
    ##  5 untitled                                          David Hosie      1984 http…
    ##  6 Composition with Sinking Ship and Lightbulb       David Hosie      1986 http…
    ##  7 2. (a) Baskerville Serif                          Anthony Benjam…  1972 http…
    ##  8 Untitled - Portrait of a Nude Female              Lindsay R S Go…  1972 http…
    ##  9 Circus                                            Cecile E M Joh…  1950 http…
    ## 10 Untitled - Abstract Bridge and Ballons            Alastair Burns   1968 http…
    ## # ℹ 210 more rows

### Rerun Exercise 11

``` r
skim(uoe_art)
```

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | uoe_art |
| Number of rows                                   | 220     |
| Number of columns                                | 4       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 3       |
| numeric                                          | 1       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

Data summary

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| title         |         0 |          1.00 |   3 |  67 |     0 |      141 |          0 |
| artist        |        16 |          0.93 |   2 |  37 |     0 |      155 |          0 |
| link          |         0 |          1.00 |  59 |  60 |     0 |      220 |          0 |

**Variable type: numeric**

| skim_variable | n_missing | complete_rate |    mean |    sd |   p0 |  p25 |  p50 |  p75 | p100 | hist  |
|:--------------|----------:|--------------:|--------:|------:|-----:|-----:|-----:|-----:|-----:|:------|
| year          |        41 |          0.81 | 1954.37 | 38.31 | 1827 | 1954 | 1962 | 1967 | 2020 | ▁▁▁▇▂ |

Ok, now it has 16 pieces where title is missing and 41 where the year is
missing. (Much better than before?).

## Exercise 12

``` r
ggplot(uoe_art, aes(x = year)) +
  geom_histogram(binwidth = 10, color = "white") +
  labs(
    title = "Distribution of Years in the University of Edinburgh Art Collection",
    x = "Year",
    y = "Number of art pieces"
  ) +
  theme_minimal()
```

    ## Warning: Removed 41 rows containing non-finite outside the scale range
    ## (`stat_bin()`).

![](lab-08_files/figure-gfm/making%20histogram-1.png)<!-- -->

I do see something a little out of the ordinary. Most of the artwork
years are clustered around the mid-1900s, especially around the 1950s
and 1960s. That part makes sense for an art collection connected to
Edinburgh College of Art. I think?

But there are a few pieces that appear much earlier, around the
1820s–1840s, almost close to year 0? I think it might be worth checking
these because it might be data that were extracted from titles in a
slightly messy way. Although there is the possibility that they were
real historical pieces, but jusssst in case!

The warning also says that 41 rows were removed because they had
non-finite values, which means those pieces had missing years. That is
not too surprising because some titles had “Unknown” or did not include
a clear year.

### Exercise 13

``` r
uoe_art %>%
  filter(year < 1900) %>%
  arrange(year)
```

    ## # A tibble: 14 × 4
    ##    title                                             artist   year link         
    ##    <chr>                                             <chr>   <dbl> <chr>        
    ##  1 Frieze from the Temple of Athena Nike (left slab) <NA>     1827 https://coll…
    ##  2 Frieze from the Temple of Athena Nike (2nd right) <NA>     1827 https://coll…
    ##  3 Frieze from the Temple of Athena Nike (far right) <NA>     1827 https://coll…
    ##  4 South Frieze of the Parthenon Frieze              <NA>     1836 https://coll…
    ##  5 Riders from the West Frieze of the Parthenon      <NA>     1836 https://coll…
    ##  6 Riders from the North Frieze of the Parthenon     <NA>     1836 https://coll…
    ##  7 Riders from South Frieze of the Parthenon         <NA>     1836 https://coll…
    ##  8 Riders from the West Frieze of the Parthenon      <NA>     1836 https://coll…
    ##  9 Riders from West Frieze of the Parthenon          <NA>     1836 https://coll…
    ## 10 Chariot from North Frieze of the Parthenon        <NA>     1836 https://coll…
    ## 11 Riders from South Frieze of the Parthenon         <NA>     1836 https://coll…
    ## 12 Riders from the West Frieze of the Parthenon      <NA>     1836 https://coll…
    ## 13 Marshal from the East Frieze of the Parthenon     <NA>     1836 https://coll…
    ## 14 Mourner No. 54 from the Tomb of John the fearless Unknown  1895 https://coll…

In my opinion, these early values do not look like simple coding
mistakes. They seem to be real dates connected to a specific group of
artworks or reproductions. However, many of these rows have missing
artist information, so they still show that the scraped data is somewhat
incomplete.

I think I did not get an obviously incorrect year because I used
`str_extract(title, "\\d{4}")` when I rerun Exercise 10, which extracts
the first four-digit year from the title. This also may have avoided the
error that can happen when the code tries to separate the title only at
the first parenthesis.

So, since the early years appear to match the artwork titles and do not
look like mistakes, I did not change them. If one of the years had been
incorrect, I would use `mutate()` with `if_else()` to replace the wrong
year with the corrected year.

### Exercise 14

``` r
uoe_art %>%
  count(artist, sort = TRUE)
```

    ## # A tibble: 156 × 2
    ##    artist                 n
    ##    <chr>              <int>
    ##  1 <NA>                  16
    ##  2 Unknown               12
    ##  3 Ann F Ward             4
    ##  4 Boris Bućan            4
    ##  5 Kirkland Main          4
    ##  6 Emma Gillies           3
    ##  7 John Bellany           3
    ##  8 John P Busby           3
    ##  9 Margaret Proudfoot     3
    ## 10 Ruth Illingworth       3
    ## # ℹ 146 more rows

``` r
uoe_art <- uoe_art %>%
 mutate(
    artist = enc2utf8(artist),
    artist = str_replace_all(artist, "Bu.*an", "Boris Bucan")
  )
```

``` r
uoe_art %>%
  count(artist, sort = TRUE)
```

    ## # A tibble: 156 × 2
    ##    artist                 n
    ##    <chr>              <int>
    ##  1 <NA>                  16
    ##  2 Unknown               12
    ##  3 Ann F Ward             4
    ##  4 Boris Boris Bucan      4
    ##  5 Kirkland Main          4
    ##  6 Emma Gillies           3
    ##  7 John Bellany           3
    ##  8 John P Busby           3
    ##  9 Margaret Proudfoot     3
    ## 10 Ruth Illingworth       3
    ## # ℹ 146 more rows

Unfrotunately, the most common artist shows as ‘Unknown.’ I guess we’ll
never get to know. THe second most common artist shows uo asAnn F Ward.
Not familiar with the artist, and I looked them up. Their art seems
pretty good but I don’t know if there is any particular reason for the
university to have so many pieces from them.

### Exercise 15

``` r
uoe_art %>%
  filter(str_detect(title, regex("child", ignore_case = TRUE)))
```

    ## # A tibble: 0 × 4
    ## # ℹ 4 variables: title <chr>, artist <chr>, year <dbl>, link <chr>

It shows 0 rows.
