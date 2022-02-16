#' Run Monte Carlo analysis for the CLCC indicator
#'
#' This function loads price and inventory data (using other clccr functions)
#' and runs a Monte Carlo simulation of the CLCC indicator. A high number
#' (decided by the user) of random prices are generated using tringular distributions
#' (parameters of such distributions are mean, min and max prices of the last decade),
#' which are then used to compute as many values of the CLCC indicator.
#' The function provides also the probability (computed using the empirical distribution
#' function) that the CLCC of a given object is lower than the baseline or lower/higher
#' than that of any other object belonging to the same project.
#'
#' @param path A character vector. Path to the folder in which raw xlsx files are stored.
#' File names must be of the following format: project_name-object_name.xlsx
#' @param rep Number of Monte Carlo iterations. Default is 10,000
#' @param phase LCA phase for which the simulation is run. Default is "total"
#' @param prob_inf_alt If TRUE the function computes the probability that an
#' object's CLCC is lower than that of the other objects belonging to the same project.
#' Default is set to FALSE.
#' @return If prob_inf_alt is set to FALSE: a nested tibble containing project
#' and object names, baseline clcc, all simulations, and the probability that
#' the indicator is lower than the baseline.
#' If prob_inf_alt is set to TRUE: a tibble with the probability that each object's
#' CLCC is lower than that of every other object (also across project, even if it
#' is not always appropiate.
#' @export
clcc_mc <- function(path, rep = 10000, phase = "total", prob_inf_alt = FALSE){

  inventories <- inventory_load_fn(data_path = path) # loads the inventories

  prices <- ref_prices()

  phase_to_cons <- phase

  baseline <- clcc(path = path) %>%
    dplyr::filter(phase == phase_to_cons)

  # generating random prices (nested tibble)

  rnd_prices <- prices %>%
    dplyr::rowwise() %>%
    dplyr::mutate(rnd_price = list(triangle::rtriangle(n = rep,
                                                        a = min,
                                                        b = max,
                                                        c = mean))) %>%
    dplyr::select(!mean:max)

  # joining invetory and random price tibbles

  inv_prices <- inventories %>%
    dplyr::left_join(rnd_prices)

  # running the simulation

  sim <- inv_prices %>%
    dplyr::filter(phase == phase_to_cons) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(p_q = list(quantity * rnd_price)) %>%
    dplyr::group_by(project, object, phase) %>%
    dplyr::summarise(clcc_sim = list(Reduce("+", p_q))) %>%
    dplyr::ungroup()

  # calculating the empirical cumulative distribution function and the probability that the clcc indicator is lower than the baseline

  sim <- sim %>%
    dplyr::rowwise() %>%
    dplyr::mutate(ecdf_fn = list(stats::ecdf(clcc_sim))) %>%
    dplyr::ungroup()

  # joining baseline values and computing the probability that the value is lowr th

  sim <- sim %>%
    dplyr::left_join(baseline) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(prob_inf_base = ecdf_fn(clcc)) %>%
    dplyr::ungroup()

  if(prob_inf_alt == FALSE){

    sim

  }else if(prob_inf_alt == TRUE){

    # Probabilities that the clcc calculated for a given object is lower than that calculated for the others

    # creating nested tibble with all objects combinations (per project/analysis)

    comb_all <- sim %>%
      dplyr::select(project, object) %>%
      dplyr::mutate(object1 = object) %>%
      dplyr::group_by(project) %>%
      tidyr::nest() %>%
      dplyr::rowwise() %>%
      dplyr::mutate(obj_combinations = list(tidyr::expand_grid(obj1 = data$object, obj2= data$object1)),
             data = NULL) %>%
      dplyr::ungroup()

    # creating a tibble with all clcc simulations
    sim_only <- sim %>%
      dplyr::select(project, object, clcc_sim)

    # creating a tibble with the differential clcc (respecting grouping by project and analysis)
    # the clcc diff keeps all permutations, including identical pairs (useful later for tiles plots)

    sim_diff <- comb_all %>%
      tidyr::unnest(obj_combinations) %>%
      dplyr::left_join(sim_only, by = c("project", "obj1" = "object")) %>%
      dplyr::left_join(sim_only, by = c("project", "obj2" = "object")) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(clcc_diff = list(clcc_sim.x - clcc_sim.y),
             clcc_sim.x = NULL,
             clcc_sim.y = NULL) %>%
      dplyr::ungroup()

    # Calculating the ecdf for the clcc differences and probabilities
    # Column prob expresses the probability that clcc of obj1 < clcc of obj2

    prob_inf_alt <- sim_diff %>%
      dplyr::rowwise() %>%
      dplyr::mutate(ecdf_diff = list(stats::ecdf(clcc_diff)),
             prob = dplyr::if_else(obj1 == obj2, NA_real_, ecdf_diff(0)),
             clcc_diff = NULL, ecdf_diff = NULL) %>%
      dplyr::ungroup()

    prob_inf_alt

  }

}
