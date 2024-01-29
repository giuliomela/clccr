
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clccr

<!-- badges: start -->
<!-- badges: end -->

The clccr package provides a set of functions to compute the commodity
life cycle costing (or CLCC) indicator, a measure of resource use by a
product/process during its life cycle. The indicator can be thought as a
life cycle costing (LCC) indicator developed like it were and life cycle
assessment (LCA) indicator: costs are treated like characterization
factors, while the classification ad characterization phases are carried
out like a conventional Life Cycle Impact Assessment (LCIA). Unlike
conventional LCC - which takes into account all costs related to the
life cycle of a product - the proposed indicator only considers costs
related to natural resource use.

## Installation

You can install the development version of clccr like so:

``` r
devtools::install_github("giuliomela/clccr")
```

## Examples

These are basic examples which show you how to use the `clccr` package
to compute the CLCC indicator. The baseline indicator is calculated with
the function `clcc`. The function returns a list of two elements. The
first, `table`, is a tibble with the calculation result, while the
second, `plot`, is a `ggplot` plot object representing the results. The
`table` element contains both the baseline and the critical CLCC
indicators.

``` r
library(clccr)

# computing the CLCC indicator for three example inventories (cars)

res <- clcc(path = path_to_folder)

res[["table"]]
#> NULL
```

The default plot looks like:

``` r

res[["plot"]]
#> NULL
```

Even though data on reference prices cannot be updated programmatically
since not all source have an API, it is possible to extract a tibble
with reference prices and minimum and maximum values (used in Monte
Carlo simulations) of the last 10-year period. The user can get
information on price levels and data sources in the
`clccr::clcc_prices_ref` tibble with data sources.

``` r

# A tibble with information on prices used and data sources
prices <- clcc_prices_ref

head(prices, 5)
#> # A tibble: 5 × 11
#>   comm      no_comm um    source   code   mean   min   max n_obs ref_yr critical
#>   <chr>       <dbl> <chr> <chr>    <chr> <dbl> <dbl> <dbl> <int>  <dbl> <chr>   
#> 1 Acids           1 kg    none     <NA>     0    0      0     NA   2022 no      
#> 2 Actinium        2 kg    comtrade 2844…  597.  36.8 1107.    10   2022 no      
#> 3 Additives       3 kg    none     <NA>     0    0      0     NA   2022 no      
#> 4 Air             4 kg    none     <NA>     0    0      0     NA   2022 no      
#> 5 Alloys          5 kg    none     <NA>     0    0      0     NA   2022 no
```

The `clcc` function returns the total CLCC indicator (computed taking
into account all material and energy flows) but the user might be
interested in having information on the relative importance of each flow
on total baseline or critical CLCC. The `clcc_detail` function fills
this gap. It returns a list containing a tibble with the relative share
of each material or energy flow on the CLCC indicator (`table`) and a
`ggplot` object (`plot`). Such shares can be computed for either the
baseline and critical CLCC indicators (setting the `detail` parameter to
either `TRUE` or `FALSE`) as well as for the entire life-cycle or just
one of the life-cycle phases.

``` r

detail_info <- clcc_detail(path = path_to_folder,
                           critical = FALSE,
                           phase_of_int = "total",
                           collapse_share = 0.9)
#> Joining with `by = join_by(object, phase, clcc_type)`

head(detail_info[["table"]], 10)
#> NULL
```

Also in this case the function returns a default plot of the results.

``` r

detail_info[["plot"]]
#> NULL
```

The `clccr` package also allows to run a Monte Carlo simulation, given
the high volatility that characterizes market prices. Minimum and
maximum prices are used, for each commodity, to generate random values
using triangular distributions. Function `clcc_mc` runs the simulations
and allows to pick the desired number of simulations with the `rep`
parameter (default is `rep = 10000`) and the life cycle phase to
consider (default is `"total"`). The function returns a tibble nested
tibble containing project and object names, baseline clcc, all
simulations, and the probability that the indicator is lower than the
baseline (probability computed using the empirical distribution
function). Similarly to the `clcc()`, also `clcc_mc()` returns a list: a
tibble with the results and a plot displaying the simulation results.
Please refer to the `clcc_mc()` documentation to see which are the
plotting alternatives available.

``` r

# Monte Carlo simulation

rep <- 1000 # number of simulations (default is 10,000)
phase <- "total" # the life cycle phase for which running the simulation

mc_res <- clcc_mc(path = path_to_folder, rep = rep)
#> Joining with `by = join_by(comm, um, no_comm)`
#> Joining with `by = join_by(object, phase)`

mc_res[["table"]]
#> NULL
```

Setting the argument `prob_inf_alt` to `TRUE` (default is `FALSE`),
`clcc_mc` returns a tibble with the probability that each object’s CLCC
is lower than that of every other object (also across projects, even if
it is not always appropriate). Such probabilities are computed, for each
object pair, using the empirical distribution function computed on the
differences between all the simulated values for each object.

``` r

# Monte Carlo simulation returning the probability that an object's CLCC is lower/higher than that of the
# other alternatives

mc_res2 <- clcc_mc(path = path_to_folder, rep = rep, prob_inf_alt = TRUE)
#> Joining with `by = join_by(comm, um, no_comm)`
#> Joining with `by = join_by(object, phase)`

mc_res2[["table"]]
#> NULL
```

Finally, the package allows the user to export reference prices for both
the baseline and critical indicators to a .csv file compatible with
SimaPro, one of the most popular LCA software. To accomplish this task
the user can call the `simapro_export()` function. Such function has no
arguments and, once called, automatically opens a pop-up windows that
asks the user where to save the file.
