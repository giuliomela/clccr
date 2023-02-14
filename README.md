
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
the function `clcc`.

``` r
library(clccr)

# computing the CLCC indicator for three example inventories (cars)

clcc(path = path_to_folder)
#> # A tibble: 33 × 3
#>    object     phase                   clcc
#>    <chr>      <chr>                  <dbl>
#>  1 2019       1 kwh bioenergy 2020 0.0394 
#>  2 2019       1 kwh pv 2020        0.0147 
#>  3 2019       total                0.0409 
#>  4 car_diesel batteria             0      
#>  5 car_diesel manto stradale       0      
#>  6 car_diesel manutenzione         0.00195
#>  7 car_diesel produzione           0.00726
#>  8 car_diesel total                0.0421 
#>  9 car_diesel uso                  0      
#> 10 car_diesel usura freni          0      
#> # … with 23 more rows
```

Setting the `critical` argument to `TRUE`, function `clcc` returns the
CLCC indicator computed considering critical materials only. The output
tibble provides also the relative share of critical materials on the
baseline CLCC.

``` r

# computing the critical CLCC indicator

clcc(path = path_to_folder, critical = TRUE)
#> # A tibble: 33 × 5
#>    object     phase                   clcc clcc_critical share_critical
#>    <chr>      <chr>                  <dbl>         <dbl>          <dbl>
#>  1 2019       1 kwh bioenergy 2020 0.0394      0.000141           0.358
#>  2 2019       1 kwh pv 2020        0.0147      0.000326           2.21 
#>  3 2019       total                0.0409      0.000174           0.425
#>  4 car_diesel batteria             0           0                  0    
#>  5 car_diesel manto stradale       0           0                  0    
#>  6 car_diesel manutenzione         0.00195     0.0000102          0.523
#>  7 car_diesel produzione           0.00726     0.0000986          1.36 
#>  8 car_diesel total                0.0421      0.000166           0.394
#>  9 car_diesel uso                  0           0                  0    
#> 10 car_diesel usura freni          0           0                  0    
#> # … with 23 more rows
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
#>   comm      no_comm um    source   critical code   mean   min   max n_obs ref_yr
#>   <chr>       <dbl> <chr> <chr>    <chr>    <chr> <dbl> <dbl> <dbl> <int>  <dbl>
#> 1 Acids           1 kg    none     no       <NA>     0     0     0     NA   2021
#> 2 Actinium        2 kg    comtrade no       2844…  605.  208. 1083.    10   2021
#> 3 Additives       3 kg    none     no       <NA>     0     0     0     NA   2021
#> 4 Air             4 kg    none     no       <NA>     0     0     0     NA   2021
#> 5 Alloys          5 kg    none     no       <NA>     0     0     0     NA   2021
```

The `clcc` function returns the total CLCC indicator (computed taking
into account all material and energy flows) but the user might be
interested in having information on the relative importance of each flow
on total baseline or critical CLCC. The `clcc_detail` function fills
this gap. It returns a tibble with the relative share of each material
or energy flow on the CLCC indicator. Such shares can be computed for
either the baseline and critical CLCC indicators as well as for the
entire life-cycle or just one of the life-cycle phases.

``` r

detail_info <- clcc_detail(path = path_to_folder,
                           critical = FALSE,
                           phase_of_int = "total",
                           collapse_share = 0.9)
#> Joining, by = c("object", "phase", "clcc_type")

head(detail_info, 10)
#> # A tibble: 10 × 7
#>    object     comm            phase clcc_type     clcc clcc_tot  share
#>    <chr>      <chr>           <chr> <chr>        <dbl>    <dbl>  <dbl>
#>  1 2019       Gas, natural/m3 total baseline  0.0245     0.0409 0.600 
#>  2 2019       Other           total baseline  0.00552   25.2    0.135 
#>  3 2019       Oil, crude      total baseline  0.00546    0.0409 0.133 
#>  4 2019       Coal, hard      total baseline  0.00366    0.0409 0.0894
#>  5 2019       Shale           total baseline  0.00172    0.0409 0.0421
#>  6 car_diesel Oil, crude      total baseline  0.0329     0.0421 0.783 
#>  7 car_diesel Other           total baseline  0.00423   15.6    0.101 
#>  8 car_diesel Gas, natural/m3 total baseline  0.00204    0.0421 0.0485
#>  9 car_diesel Coal, hard      total baseline  0.00111    0.0421 0.0264
#> 10 car_diesel Oxygen          total baseline  0.000987   0.0421 0.0235
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
function).

``` r

# Monte Carlo simulation

rep <- 1000 # number of simulations (default is 10,000)
phase <- "total" # the life cycle phase for which running the simulation

clcc_mc(path = path_to_folder, rep = rep)
#> Joining, by = c("comm", "um", "no_comm")
#> Joining, by = c("object", "phase")
#> # A tibble: 4 × 6
#>   object       phase clcc_sim      ecdf_fn   clcc prob_inf_base
#>   <chr>        <chr> <list>        <list>   <dbl>         <dbl>
#> 1 2019         total <dbl [1,000]> <ecdf>  0.0409         0.432
#> 2 car_diesel   total <dbl [1,000]> <ecdf>  0.0421         0.401
#> 3 car_elet_nmc total <dbl [1,000]> <ecdf>  0.0212         0.22 
#> 4 car_petrol   total <dbl [1,000]> <ecdf>  0.0491         0.406
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

clcc_mc(path = path_to_folder, rep = rep, prob_inf_alt = TRUE)
#> Joining, by = c("comm", "um", "no_comm")
#> Joining, by = c("object", "phase")
#> # A tibble: 16 × 3
#>    obj1         obj2           prob
#>    <chr>        <chr>         <dbl>
#>  1 2019         2019         NA    
#>  2 2019         car_diesel    0.567
#>  3 2019         car_elet_nmc  0    
#>  4 2019         car_petrol    0.819
#>  5 car_diesel   2019          0.433
#>  6 car_diesel   car_diesel   NA    
#>  7 car_diesel   car_elet_nmc  0    
#>  8 car_diesel   car_petrol    1    
#>  9 car_elet_nmc 2019          1    
#> 10 car_elet_nmc car_diesel    1    
#> 11 car_elet_nmc car_elet_nmc NA    
#> 12 car_elet_nmc car_petrol    1    
#> 13 car_petrol   2019          0.181
#> 14 car_petrol   car_diesel    0    
#> 15 car_petrol   car_elet_nmc  0    
#> 16 car_petrol   car_petrol   NA
```
