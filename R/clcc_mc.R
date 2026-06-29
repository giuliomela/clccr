#' Run Monte Carlo analysis for the CLCC indicator
#'
#' This function loads price and inventory data (using other clccr functions)
#' and runs a Monte Carlo simulation of the CLCC indicator. A high number
#' (decided by the user) of random prices are generated using triangular distributions
#' (parameters of such distributions are mean, min and max prices of the last decade),
#' which are then used to compute as many values of the CLCC indicator.
#' The function provides also the probability (computed using the empirical distribution
#' function) that the CLCC of a given object is lower than the baseline or lower/higher
#' than that of any other object belonging to the same project.
#'
#' @importFrom stats formula ecdf rtriangle
#' @param data_path A character vector. Path to the folder in which raw xlsx files are stored.
#' @param use_weights A logical value. If set to `TRUE`, the function uses the critical weights
#' @param weights_path A character vector. Path to the file containing the critical weights for each commodity and phase.
#' @param rep Number of Monte Carlo iterations. Default is 10,000
#' @param phase LCA phase for which the simulation is run. Default is "total"
#' @param critical A logical value. If set to `TRUE`, shares referred to the critical-clcc indicator
#'      are returned. Default is set to `FALSE`.
#' @param critical_type A string. If set to `EU`, the critical CLCC indicator is based on the list of critical
#'      materials of the European Union. If it is set to `IEA` the International Energy Agency list is used instead. Default is set to `EU`
#' @param prob_inf_alt If TRUE the function computes the probability that an
#' object's CLCC is lower than that of the other objects belonging to the same project.
#' Default is set to FALSE.
#' @param func_unit A string, the functional unit of the LCA analysis the inventories refer to. This parameter
#'      is needed to plot the data it is not used to compute the indicator.
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
#' clcc_mc(data_path = data_path)
#'
#' clcc_mc(data_path = data_path, rep = 5000, prob_inf_alt = TRUE)
#'
#' }
clcc_mc <- function(data_path,
                    use_weights = FALSE,
                    weights_path = NULL,
                    rep = 10000, phase = "total",
                    critical = FALSE,
                    critical_type = "EU",
                    func_unit = "km", prob_inf_alt = FALSE){

  inventories <- inventory_load_fn(
    data_path = data_path,
    use_weights = use_weights,
    weights_path = weights_path
  ) # loads the inventories

  # 🟢 FIX ALLINEAMENTO MINUSCOLE PRIMA DEL JOIN
  inventories$comm <- tolower(inventories$comm)

  prices <- clccr::clcc_prices_ref
  prices$comm <- tolower(prices$comm)

  phase_to_cons <- phase

  phase <- min <- max <- mean <- quantity <- rnd_price <- object <- p_q <-
    clcc_sim <- ecdf_diff <- clcc_diff <- obj1 <- obj2 <- clcc_sim.y <- clcc_sim.x <-
    obj_combinations <- nested_data <- ecdf_fn <- clcc <- NULL

  baseline <- clcc(
    data_path = data_path,
    use_weights = use_weights,
    weights_path = weights_path,
    price_source = "latest"
  )[["table"]]

  baseline <- baseline[baseline$phase == phase_to_cons, ]

  # 🟢 OTTIMIZZAZIONE MATRIX-LIKE: Generazione controllata e pulita delle distribuzioni triangolari
  rnd_prices <- prices |>
    dplyr::rowwise() |>
    dplyr::mutate(rnd_price = list(triangle::rtriangle(n = rep, a = min, b = max, c = mean))) |>
    dplyr::select(comm, source, rnd_price) |>
    dplyr::ungroup()

  # 🟢 FIX CRITICO DEL JOIN: Specifichiamo la chiave univoca per impedire prodotti cartesiani
  inv_prices <- inventories |>
    dplyr::left_join(rnd_prices, by = "comm") |>
    tidyr::as_tibble()

  if (isTRUE(critical)){
    if (critical_type == "EU") {
      inv_prices <- inv_prices |>
        dplyr::filter(.data[["critical_eu"]] == "yes") |>
        dplyr::mutate(clcc_type = "critical-clcc_eu")
    } else if (critical_type == "IEA") {
      inv_prices <- inv_prices |>
        dplyr::filter(.data[["critical_iea"]] == "yes") |>
        dplyr::mutate(clcc_type = "critical-clcc_IEA")
    }
  }

  # 🟢 VELOCIZZAZIONE STRATEGICA: Calcolo vettoriale nativo senza pmap/Reduce per risparmiare RAM
  sim <- inv_prices |>
    dplyr::filter(phase == phase_to_cons) |>
    dplyr::rowwise() |>
    dplyr::mutate(p_q = list(.data[["quantity"]] * .data[["rnd_price"]] * .data[["weight"]])) |>
    dplyr::ungroup() |>
    dplyr::group_by(object, phase) |>
    dplyr::summarise(clcc_sim = list(colSums(do.call(rbind, p_q))), .groups = "drop")

  # Calculating the empirical cumulative distribution function
  sim <- sim |>
    dplyr::rowwise() |>
    dplyr::mutate(ecdf_fn = list(stats::ecdf(clcc_sim))) |>
    dplyr::ungroup()

  baseline_to_compare <- dplyr::case_when(
    critical == TRUE & critical_type == "EU" ~ "clcc_critical_eu",
    critical == TRUE & critical_type == "IEA" ~ "clcc_critical_iea",
    TRUE ~ "clcc"
  )

  sim <- sim |>
    dplyr::left_join(baseline, by = c("object", "phase")) |>
    dplyr::rowwise() |>
    dplyr::mutate(prob_inf_base = ecdf_fn(.data[[baseline_to_compare]])) |>
    dplyr::ungroup()

  if(prob_inf_alt == FALSE){
    data_plot <- sim |>
      dplyr::mutate(object = forcats::fct_reorder(.data[["object"]], .data[["clcc"]]))

    plot <- data_plot |>
      tidyr::unnest(tidyr::all_of("clcc_sim")) |>
      ggplot2::ggplot(ggplot2::aes(x = .data[["clcc_sim"]], y = .data[["object"]],
                                   fill = .data[["object"]], color = .data[["object"]])) +
      ggridges::geom_density_ridges(alpha = 0.35) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      viridis::scale_fill_viridis(discrete = TRUE) +
      viridis::scale_color_viridis(discrete = TRUE) +
      ggplot2::labs(x = paste0("euro/", func_unit), y = "") +
      ggplot2::geom_point(data = data_plot, ggplot2::aes(x = .data[[baseline_to_compare]], y = .data[["object"]]),
                          shape = 8, size = 5) +
      ggplot2::geom_vline(xintercept = 0) +
      ggplot2::geom_hline(yintercept = 0)

    return(list(table = sim, plot = plot))

  } else if(prob_inf_alt == TRUE){
    comb_all <- tidyr::expand_grid(obj1 = sim$object, obj2 = sim$object)
    sim_only <- sim |> dplyr::select(object, clcc_sim)

    sim_diff <- comb_all |>
      dplyr::left_join(sim_only, by = c("obj1" = "object")) |>
      dplyr::left_join(sim_only, by = c("obj2" = "object")) |>
      dplyr::rowwise() |>
      dplyr::mutate(clcc_diff = list(clcc_sim.x - clcc_sim.y)) |>
      dplyr::ungroup()

    sim_diff$clcc_sim.x <- NULL
    sim_diff$clcc_sim.y <- NULL

    prob_inf_alt_df <- sim_diff |>
      dplyr::rowwise() |>
      dplyr::mutate(ecdf_diff = list(stats::ecdf(clcc_diff)),
                    prob = dplyr::if_else(obj1 == obj2, NA_real_, ecdf_diff(0))) |>
      dplyr::ungroup()

    prob_inf_alt_df$clcc_diff <- NULL
    prob_inf_alt_df$ecdf_diff <- NULL

    levels_for_plot <- forcats::fct_reorder(baseline$object, baseline[[baseline_to_compare]]) |> levels()

    plot <- prob_inf_alt_df |>
      tidyr::drop_na(prob) |>
      dplyr::mutate(dplyr::across(dplyr::contains("obj"), \(x) factor(x, levels = levels_for_plot))) |>
      ggplot2::ggplot(ggplot2::aes(x = .data[["obj1"]], y = .data[["obj2"]],
                                   label = paste0(round(.data[["prob"]] * 100, 1), "%"),
                                   fill = .data[["prob"]])) +
      ggplot2::geom_tile() +
      ggplot2::theme(legend.position = "none") +
      viridis::scale_fill_viridis(discrete = FALSE, option = "rocket", direction = -1) +
      ggplot2::theme_minimal() +
      ggfittext::geom_fit_text(contrast = TRUE, place = "centre") +
      ggplot2::labs(x = "", y = "") +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10))

    return(list(table = prob_inf_alt_df, plot = plot))
  }
}
