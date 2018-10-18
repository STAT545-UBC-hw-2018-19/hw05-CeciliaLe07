Homework 04: Tidy data and joins
================
Cecilia Leon

-   [Homework 05: Factor and figure management](#homework-05-factor-and-figure-management)
    -   [Overview](#overview)
    -   [The Assignment](#the-assignment)
        -   [Part 1: Factor management](#part-1-factor-management)
        -   [Part 2: File I/O](#part-2-file-io)
        -   [Part 3: Visualization design](#part-3-visualization-design)
        -   [Part 4: Writing figures to file](#part-4-writing-figures-to-file)
        -   [But I want to do more!](#but-i-want-to-do-more)
        -   [Finishing up, and Reflection](#finishing-up-and-reflection)

Homework 05: Factor and figure management
=========================================

Overview
--------

Goals:

-   Reorder a factor in a principled way based on the data and demonstrate the effect in arranged data and in figures.
-   Write some data to file and load it back into R.
-   Improve a figure (or make one from scratch), using new knowledge, e.g., control the color scheme, use factor levels, smoother mechanics.
-   Make a plotly visual.
-   Implement visualization design principles.

Remember the [sampler concept](http://en.wikipedia.org/wiki/Sampler_(needlework)). Your homework should serve as your own personal cheatsheet in the future for canonical tasks. Make things nice -- your future self will thank you!

The Assignment
--------------

### Part 1: Factor management

With the data set of your choice, after ensuring the variable(s) you're exploring are indeed factors, you are expected to:

1.  Drop factor / levels;
2.  Reorder levels based on knowledge from data.

We've elaborated on these steps for the gapminder and singer data sets below.

Be sure to also characterize the (derived) data before and after your factor re-leveling:

-   Explore the effects of `arrange()`. Does merely arranging the data have any effect on, say, a figure?
-   Explore the effects of reordering a factor and factor reordering coupled with `arrange()`. Especially, what effect does this have on a figure?

These explorations should involve the data, the factor levels, and some figures.

#### Elaboration for the gapminder data set

All dependencies used for this asssigment

``` r
suppressPackageStartupMessages(library(tidyverse))
library(ggplot2)
library(gapminder)
library(knitr)
library(kableExtra)
library(forcats)
knitr::opts_chunk$set(echo = TRUE)
```

**i) Drop Oceania.** Filter the Gapminder data to remove observations associated with the `continent` of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.

First of all we are going to ensure that `gapmider`dataframe cotains factor data:

``` r
str(gapminder)
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

As we can see by using command `str()` this dataframe has two variables of type **factor**, which are:

| Variable  | Levels |
|-----------|--------|
| country   | 142    |
| continent | 5      |

Then, if we filter by continents that are not *Oceania* we have:

``` r
gapminder %>% 
  filter(continent != "Oceania") %>% 
  str() 
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

When we filter by condition `continent != "Oceania"` we can note that both *continent* and *country* keep the same number of levels without filtering, this happens because we have empty levels on the filtered dataframe. In order to drop this kind of levels we can use `droplevels()`:

``` r
gapminder %>% 
  filter(continent != "Oceania") %>% 
  droplevels() %>% 
  str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1680 obs. of  6 variables:
    ##  $ country  : Factor w/ 140 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 4 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...

This time, the number of levels for continent changed from **5** to **4** as we dropped one level (*Oceania*). Furthermore, the number of levels in country changed from **142** to **140**, which means there were two countries associated to the continent Oceania. We can confirm that statement by:

``` r
gapminder %>% 
   filter(continent == "Oceania") %>% 
   droplevels() %>% 
   str()
```

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    24 obs. of  6 variables:
    ##  $ country  : Factor w/ 2 levels "Australia","New Zealand": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 1 level "Oceania": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  69.1 70.3 70.9 71.1 71.9 ...
    ##  $ pop      : int  8691212 9712569 10794968 11872264 13177000 14074100 15184200 16257249 17481977 18565243 ...
    ##  $ gdpPercap: num  10040 10950 12217 14526 16789 ...

Finally, we can known the number of countries per continent on this data frame by:

``` r
gapminder %>% 
 filter(continent != "Oceania") %>% 
 droplevels() %>% 
 select(country, continent) %>% 
 group_by(continent) %>% 
 summarize(total = length(unique(country))) %>% 
 kable(col.names=c("Continent","Number of levels (countries)"))
```

<table>
<thead>
<tr>
<th style="text-align:left;">
Continent
</th>
<th style="text-align:right;">
Number of levels (countries)
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Africa
</td>
<td style="text-align:right;">
52
</td>
</tr>
<tr>
<td style="text-align:left;">
Americas
</td>
<td style="text-align:right;">
25
</td>
</tr>
<tr>
<td style="text-align:left;">
Asia
</td>
<td style="text-align:right;">
33
</td>
</tr>
<tr>
<td style="text-align:left;">
Europe
</td>
<td style="text-align:right;">
30
</td>
</tr>
</tbody>
</table>
Thus, we can confirm the other 140 levels correspond to the rest of continents.

**ii) Reorder the levels of `country` or `continent`.** Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median.

If we dont't apply any kind of `arrange`to the dataframe, **R** consider the levels of a factor un alphabetic order. We can illustrate this by the following graph of number of observations by continent:

``` r
ggplot(gapminder,aes(continent, fill = continent)) +
  geom_bar() +
  ggtitle("Observations on gapminder dataframe by continent") +
  xlab("Continent\n(In alphabetic order)") +
  ylab("Number of observations")
```

![](hw05-CeciliaLe07_files/figure-markdown_github/witouht_order_criteria-1.png)

But we can provide a different cirteria for the order of factors, for example, if we want to show the number of observations in the dataframe by continent, considering the order given by the `spreadness`of \*\*life expectancy\*:

``` r
gapminder %>%
  mutate(continent = fct_reorder(continent, lifeExp, .fun = sd)) %>% 
  ggplot(aes(continent, fill = continent)) +
  geom_bar() +
  ggtitle("Observations on gapminder dataframe by continent") +
  xlab("Continent\n(From the highest spreadness of life expectancy to the lowest)") +
  ylab("Number of observations")
```

![](hw05-CeciliaLe07_files/figure-markdown_github/unnamed-chunk-1-1.png)

It allows to appreciate how *Oceania*, which was the continent with the fewest number of observations has the greatest variability on the records of life expectancy, while Asia was the continent that presented the fewest variable life expectancy even when it has not the biggest number of observations in the dataframe.

In the following example we are going to show the gdp per capita of the countries of America during 2007, ordering this countries by life expectancy and gdp Per capita from lowest to highest:

``` r
gapminder %>% 
  filter(continent == "Americas" , year == 2007) %>% 
  mutate(country = fct_reorder2(country, lifeExp, gdpPercap)) %>% 
  ggplot(aes(gdpPercap,country)) +
  geom_point(aes(gdpPercap, colour=lifeExp)) +
  ggtitle("Countries of Americas' GDP per capita and Life Expectancy") +
  xlab("GDP per capita") +
  ylab("Countries\n(Ordered by GDP per capita and life expectancy)")
```

![](hw05-CeciliaLe07_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Part 2: File I/O

Experiment with one or more of `write_csv()/read_csv()` (and/or TSV friends), `saveRDS()/readRDS()`, `dput()/dget()`. Create something new, probably by filtering or grouped-summarization of Singer or Gapminder. I highly recommend you fiddle with the factor levels, i.e. make them non-alphabetical (see previous section). Explore whether this survives the round trip of writing to file then reading back in.

We are going to create a new factor variable called **category** and the export the new dataframe (without chaging the levels of this factor) to a file with format `csv`.

``` r
gap_to_export <- gapminder %>% 
                    mutate(category = factor(ifelse(lifeExp>mean(lifeExp),
                                             "Over world's mean",
                                             "Under world's mean")))

write_csv(gap_to_export,"new_gapminder.csv")
```

Then, when we read it again:

``` r
read_csv("new_gapminder.csv") %>%  str()
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double(),
    ##   category = col_character()
    ## )

    ## Classes 'tbl_df', 'tbl' and 'data.frame':    1704 obs. of  7 variables:
    ##  $ country  : chr  "Afghanistan" "Afghanistan" "Afghanistan" "Afghanistan" ...
    ##  $ continent: chr  "Asia" "Asia" "Asia" "Asia" ...
    ##  $ year     : int  1952 1957 1962 1967 1972 1977 1982 1987 1992 1997 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : int  8425333 9240934 10267083 11537966 13079460 14880372 12881816 13867957 16317921 22227415 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...
    ##  $ category : chr  "Under world's mean" "Under world's mean" "Under world's mean" "Under world's mean" ...
    ##  - attr(*, "spec")=List of 2
    ##   ..$ cols   :List of 7
    ##   .. ..$ country  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ continent: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   .. ..$ year     : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ lifeExp  : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ pop      : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_integer" "collector"
    ##   .. ..$ gdpPercap: list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_double" "collector"
    ##   .. ..$ category : list()
    ##   .. .. ..- attr(*, "class")= chr  "collector_character" "collector"
    ##   ..$ default: list()
    ##   .. ..- attr(*, "class")= chr  "collector_guess" "collector"
    ##   ..- attr(*, "class")= chr "col_spec"

We can realize that the format for variables of type factors changed to character. On one to handle this problem is using command `read.csv` instead of `read_csv`in order to use the parameters `colClasses`:

``` r
data <- read.csv('new_gapminder.csv', colClasses = c(rep('factor',2),rep('numeric',4),'factor')) %>% str()
```

    ## 'data.frame':    1704 obs. of  7 variables:
    ##  $ country  : Factor w/ 142 levels "Afghanistan",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ continent: Factor w/ 5 levels "Africa","Americas",..: 3 3 3 3 3 3 3 3 3 3 ...
    ##  $ year     : num  1952 1957 1962 1967 1972 ...
    ##  $ lifeExp  : num  28.8 30.3 32 34 36.1 ...
    ##  $ pop      : num  8425333 9240934 10267083 11537966 13079460 ...
    ##  $ gdpPercap: num  779 821 853 836 740 ...
    ##  $ category : Factor w/ 2 levels "Over world's mean",..: 2 2 2 2 2 2 2 2 2 2 ...

### Part 3: Visualization design

Remake at least one figure or create a new one, in light of something you learned in the recent class meetings about visualization design and color. Maybe juxtapose your first attempt and what you obtained after some time spent working on it. Reflect on the differences. If using Gapminder, you can use the country or continent color scheme that ships with Gapminder. Consult the dimensions listed in [All the Graph Things](http://stat545.com/graph00_index.html).

Then, make a new graph by converting this visual (or another, if you'd like) to a `plotly` graph. What are some things that `plotly` makes possible, that are not possible with a regular `ggplot2` graph?

### Part 4: Writing figures to file

Use `ggsave()` to explicitly save a plot to file. Then use `![Alt text](/path/to/img.png)` to load and embed it in your report. You can play around with various options, such as:

-   Arguments of `ggsave()`, such as width, height, resolution or text scaling.
-   Various graphics devices, e.g. a vector vs. raster format.
-   Explicit provision of the plot object `p` via `ggsave(..., plot = p)`. Show a situation in which this actually matters.

### But I want to do more!

If you're particularly keen on levelling up the challenge of this assignment, try these things (this is all optional):

Make a deeper exploration of the forcats packages, i.e. try more of the factor level reordering functions.

Revalue a factor, e.g.:

-   **Gapminder version**: Pick a handful of countries, each of which you can associate with a stereotypical food (or any other non-controversial thing ... sport? hobby? type of music, art or dance? animal? landscape feature?). Create an excerpt of the Gapminder data, filtered to just these countries. Create a new factor -- you pick the name! -- by mapping the existing country factor levels to the new levels.
    -   Examples: Italy --&gt; wine, Germany --&gt; beer, Japan --&gt; sake. (Austria, Germany) --&gt; German, (Mexico, Spain) --&gt; Spanish, (Portugal, Brazil) --&gt; Portuguese. Let your creativity flourish.
-   **Singer version**: Pick a handful of locations (they are named `city`, try using `distinct()`) that you can pinpoint to a geographical place (city, region, country, continent,...). Create an excerpt of the Singer data, filtered to just those rows. Create a (couple of) new factor(s) -- you pick the name(s)! -- by mapping the existing `city` factor levels to the new (city, region, country...) levels.
    -   Examples: "London, England" --&gt; "London", "England", "UK", "Europe";
    -   "Los Angeles, CA" --&gt; "Los Angeles", "California", "USA", "Americas";
    -   ...
    -   "310, Louisiana" --&gt; "New Orleans", "Louisiana", "USA", "Americas".

You could even try to make this process of geolocalization more streamlined: you may want to try and use the `separate` function from tidyr.

### Finishing up, and Reflection

Once you're done the above, go back to [UBC canvas](https://canvas.ubc.ca/), and find the "Homework 05" page.

You're encouraged to reflect on what was hard/easy, problems you solved, helpful tutorials you read, etc. As usual, put this reflection on your canvas submission. No need to write lots here.

Please add a link to your homework respository to help out our wonderful TA's.
