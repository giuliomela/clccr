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
#' @param rep Number of Monte Carlo iterations. Default is 10,000
#' @param phase LCA phase for which the simulation is run. Default is "total"
#' @param prob_inf_alt If TRUE the function computes the probability that an
#' object's CLCC is lower than that of the other objects belonging to the same project.
#' Default is set to FALSE.
#' @param func_unit A string, the functional unit of the LCA analysis the inventories refer to. This parameter
#'     is needed to plot the data it is not used to compute the indicator.
#' @return If prob_inf_alt is set to FALSE: a nested tibble containing
#' object names, baseline clcc, all simulations, and the probability that
#' the indicator is lower than the baseline.
#' If prob_inf_alt is set to TRUE: a tibble with the probability that each object's
#' CLCC is lower than that of every other object loaded.
#' @export
#'
#' @examples
#' \dontrun{
#'
#' data_path <- path_to_inventory_folder
#'
#' clcc_mc(path = data_path)
#'
#' clcc_mc(path = data_path, rep = 5000, prob_inf_alt = TRUE)
#'
#' }
clcc_mc <- function(path, rep = 10000, phase = "total",
                    func_unit = "km", prob_inf_alt = FALSE){

  inventories <- inventory_load_fn(data_path = path) # loads the inventories

  prices <- clccr::clcc_prices_ref

  phase_to_cons <- phase

  phase <- min <- max <- mean <- quantity <- rnd_price <- object <- p_q <-
    clcc_sim <- ecdf_diff <- clcc_diff <- obj1 <- obj2 <- clcc_sim.y <- clcc_sim.x <-
    obj_combinations <- nested_data <- ecdf_fn <- clcc <- NULL # removes notes when running R RMD check

  baseline <- clcc(path = path)[["table"]]

  baseline <- baseline[baseline$phase == phase_to_cons, ]

  # generating random prices (nested tibble)

  rnd_prices <- prices |>
    dplyr::rowwise() |>
    dplyr::mutate(rnd_price = list(triangle::rtriangle(n = rep,
                                                        a = min,
                                                        b = max,
                                                        c = mean))) |>
    dplyr::select(!mean:max)

  # joining invetory and random price tibbles

  inv_prices <- inventories |>
    dplyr::left_join(rnd_prices)

  # running the simulation

  sim <- inv_prices |>
    dplyr::filter(phase == phase_to_cons) |>
    dplyr::rowwise() |>
    dplyr::mutate(p_q = list(quantity * rnd_price)) |>
    dplyr::group_by(object, phase) |>
    dplyr::summarise(clcc_sim = list(Reduce("+", p_q))) |>
    dplyr::ungroup()

    # calculating the empirical cumulative distribution function and the probability that the clcc indicator is lower than the baseline

  sim <- sim |>
    dplyr::rowwise() |>
    dplyr::mutate(ecdf_fn = list(stats::ecdf(clcc_sim))) |>
    dplyr::ungroup()

  # joining baseline values and computing the probability that the value is lowr th

  sim <- sim |>
    dplyr::left_join(baseline) |>
    dplyr::rowwise() |>
    dplyr::mutate(prob_inf_base = ecdf_fn(clcc)) |>
    dplyr::ungroup()

  if(prob_inf_alt == FALSE){

    # creating a plot of the simulations

    data_plot <- sim |>
      dplyr::mutate(object = forcats::fct_reorder(.data[["object"]], .data[["clcc"]]))

    plot <- data_plot |>
      tidyr::unnest(tidyr::all_of("clcc_sim")) |>
      ggplot2::ggplot(ggplot2::aes(x = .data[["clcc_sim"]], y = .data[["object"]],
                 fill = .data[["object"]], color = .data[["object"]])) +
      ggridges::geom_density_ridges(alpha = 0.4) +
      ggplot2::theme(legend.position = "none") +
      viridis::scale_fill_viridis(discrete = T) +
      viridis::scale_color_viridis(discrete = T) +
      ggplot2::labs(x = paste0("euro/", func_unit), y = "") +
      ggplot2::geom_point(data = data_plot, ggplot2::aes(x = .data[["clcc"]],
                                                         y = .data[["object"]]),
                 shape = 8, size = 5)

    list(table = sim, plot = plot)

  }else if(prob_inf_alt == TRUE){

    # Probabilities that the clcc calculated for a given object is lower than that calculated for the others

    # creating nested tibble with all objects combinations

    comb_all <- tidyr::expand_grid(
      obj1 = sim$object,
      obj2 = sim$object)

    # creating a tibble with all clcc simulations
    sim_only <- sim |>
      dplyr::select(object, clcc_sim)

    # creating a tibble with the differential clcc (respecting grouping by project and analysis)
    # the clcc diff keeps all permutations, including identical pairs (useful later for tiles plots)

    sim_diff <- comb_all |>
      dplyr::left_join(sim_only, by = c("obj1" = "object")) |>
      dplyr::left_join(sim_only, by = c("obj2" = "object")) |>
      dplyr::rowwise() |>
      dplyr::mutate(clcc_diff = list(clcc_sim.x - clcc_sim.y)) |>
      dplyr::ungroup()

    sim_diff <- within(sim_diff, {
      clcc_sim.x  <- NULL
      clcc_sim.y <- NULL
    })

    # Calculating the ecdf for the clcc differences and probabilities
    # Column prob expresses the probability that clcc of obj1 < clcc of obj2

    prob_inf_alt <- sim_diff |>
      dplyr::rowwise() |>
      dplyr::mutate(ecdf_diff = list(stats::ecdf(clcc_diff)),
             prob = dplyr::if_else(obj1 == obj2, NA_real_, ecdf_diff(0))) |>
      dplyr::ungroup()

    prob_inf_alt <- within(prob_inf_alt, {
      clcc_diff <- NULL
      ecdf_diff <- NULL
    })

    # Plot

    levels_for_plot <- forcats::fct_reorder(
      baseline$object,
      baseline$clcc
    ) |> levels()

    plot <- prob_inf_alt |>
      tidyr::drop_na() |>
      dplyr::mutate(dplyr::across(dplyr::contains("obj"), \(x) factor(x, levels = levels_for_plot))) |>
      ggplot2::ggplot(ggplot2::aes(x = .data[["obj1"]], y = .data[["obj2"]],
                                   label = paste0(round(.data[["prob"]] * 100, 1), "%"),
                                   fill = .data[["prob"]])) +
      ggplot2::geom_tile() +
      ggplot2::theme(legend.position = "none") +
      viridis::scale_fill_viridis(discrete = F, option = "rocket", direction = -1) +
      ggfittext::geom_fit_text(contrast = T, family = "Verdana") +
      ggplot2::labs(x = "", y = "") +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))

    list(table = prob_inf_alt, plot = plot)

  }

}
