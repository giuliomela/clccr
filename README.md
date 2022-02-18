
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
#> # A tibble: 30 x 4
#>    project  object     phase                 clcc
#>    <chr>    <chr>      <chr>                <dbl>
#>  1 mobility car_diesel batteria           0      
#>  2 mobility car_diesel manto stradale     0      
#>  3 mobility car_diesel manutenzione       0.00164
#>  4 mobility car_diesel produzione         0.00640
#>  5 mobility car_diesel total              0.0330 
#>  6 mobility car_diesel uso                0      
#>  7 mobility car_diesel usura freni        0      
#>  8 mobility car_diesel usura pneumatici   0      
#>  9 mobility car_diesel usura strada       0      
#> 10 mobility car_diesel vettore energetico 0.0250 
#> # ... with 20 more rows
```

Setting the `critical` argument to `TRUE`, function `clcc` returns the
CLCC indicator computed considering critical materials only. The output
tibble provides also the relative share of critical materials on the
baseline CLCC.

``` r
# computing the critical CLCC indicator

clcc(path = path_to_folder, critical = TRUE)
#> # A tibble: 30 x 6
#>    project  object     phase                 clcc clcc_critical share_critical
#>    <chr>    <chr>      <chr>                <dbl>         <dbl>          <dbl>
#>  1 mobility car_diesel batteria           0          0                   0    
#>  2 mobility car_diesel manto stradale     0          0                   0    
#>  3 mobility car_diesel manutenzione       0.00164    0.00000988          0.603
#>  4 mobility car_diesel produzione         0.00640    0.0000873           1.36 
#>  5 mobility car_diesel total              0.0330     0.000147            0.446
#>  6 mobility car_diesel uso                0          0                   0    
#>  7 mobility car_diesel usura freni        0          0                   0    
#>  8 mobility car_diesel usura pneumatici   0          0                   0    
#>  9 mobility car_diesel usura strada       0          0                   0    
#> 10 mobility car_diesel vettore energetico 0.0250     0.0000499           0.200
#> # ... with 20 more rows
```

Even though data on reference prices cannot be updated programmatically
since not all source have an API, it is possible to extract a tibble
with reference prices and minimum and maximum values (used in Monte
Carlo simulations) of the last 10-year period. The function
`ref_prices()` returns a tibble with data sources and reference prices
for each commodity considered.

``` r
# Function with no arguments
ref_prices()
#> # A tibble: 154 x 9
#>    source   code     comm         no_comm um    critical    mean     min     max
#>    <chr>    <chr>    <chr>          <dbl> <chr> <chr>      <dbl>   <dbl>   <dbl>
#>  1 comext   28045090 Tellurium        592 kg    no       5.27e+1 1.07e+1 2.13e+2
#>  2 comext   28053020 Praseodymium     940 kg    yes      2.21e+1 1.50e+1 3.97e+1
#>  3 comext   28053030 Europium         964 kg    yes      6.93e+1 5.51e+1 1.11e+2
#>  4 comext   81129281 Indium           228 kg    yes      2.81e+2 8.26e+1 4.93e+2
#>  5 comext   81129289 Gallium          935 kg    yes      2.74e+2 1.33e+2 6.22e+2
#>  6 comtrade 2501     Sodium chlo~      20 kg    no       4.38e-2 3.68e-2 5.15e-2
#>  7 comtrade 2503     Sulfur            31 kg    no       1.02e-1 5.20e-2 1.39e-1
#>  8 comtrade 2504     Metamorphou~     118 kg    no       9.41e-1 7.06e-1 1.30e+0
#>  9 comtrade 2505     Sand             133 kg    no       1.82e-2 5.48e-3 3.39e-2
#> 10 comtrade 2507     Kaolinite        136 kg    no       1.29e-1 1.17e-1 1.46e-1
#> # ... with 144 more rows
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
#> Joining, by = c("comm", "no_comm")
#> Joining, by = c("project", "object", "phase")
#> # A tibble: 3 x 7
#>   project  object       phase clcc_sim      ecdf_fn   clcc prob_inf_base
#>   <chr>    <chr>        <chr> <list>        <list>   <dbl>         <dbl>
#> 1 mobility car_diesel   total <dbl [1,000]> <ecdf>  0.0330         0.445
#> 2 mobility car_elet_nmc total <dbl [1,000]> <ecdf>  0.0216         0.273
#> 3 mobility car_petrol   total <dbl [1,000]> <ecdf>  0.0375         0.447
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
#> Joining, by = c("comm", "no_comm")
#> Joining, by = c("project", "object", "phase")
#> # A tibble: 9 x 4
#>   project  obj1         obj2           prob
#>   <chr>    <chr>        <chr>         <dbl>
#> 1 mobility car_diesel   car_diesel   NA    
#> 2 mobility car_diesel   car_elet_nmc  0.057
#> 3 mobility car_diesel   car_petrol    1    
#> 4 mobility car_elet_nmc car_diesel    0.943
#> 5 mobility car_elet_nmc car_elet_nmc NA    
#> 6 mobility car_elet_nmc car_petrol    0.977
#> 7 mobility car_petrol   car_diesel    0    
#> 8 mobility car_petrol   car_elet_nmc  0.023
#> 9 mobility car_petrol   car_petrol   NA
```
