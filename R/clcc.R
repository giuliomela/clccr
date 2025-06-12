#' Calculate the CLCC indicator
#'
#' This function calculates the CLCC indicator, a measure of resource scarcity,
#' for a product/process during its life cycle. The function automatically loads
#' the latest prices available and SimaPro inventories using other functions of
#' the clccr package.
#' The function can also return the CLCC indicator calculated taking into account
#' critical materials only.
#'
#' @param path A character vector. Path to the folder in which raw xlsx files are stored.
#' @param path_weights A character vector. Path to the file containing the critical weights for each commodity and phase.
#' @param func_unit A string, the functional unit of the LCA analysis the inventories refer to. This parameter
#'     is needed to plot the data it is not used to compute the indicator.
#' @param label_digits An integer: the number of digits to display in the plots.
#' @param plot_phases A logical value. if set to `TRUE` the function returns a plot object (`ggplot2`) displaying
#'     the relative importance of each life cycle phase on the CLCC indicator.
#' @param plot_phases_critical A logical value. if set to `TRUE` and `plot_phases` is also set to `TRUE`, the
#'     plot returned refers to the Critical CLCC indicator.
#' @param plot_phases_critical_version A string. If set to `EU`, the critical CLCC indicator is based on the list of critical
#'     materials of the European Union. If it is set to `IEA` the International Energy Agency list is used instead. Default is set to `EU`
#' @param price_source A string. If set to `2023` price sources used in the 2023 RDS report are used. If set to `2024`,
#'     the default, the 2024 updated price sources are used.
#' @return A list of 2 elements: a tibble containing the CLCC indicator calculated for each object and phase
#'     and a `ggplot` object containing the plot of the results.
#' @export
#'
#' @examples
#' \dontrun{
#' data_path <- path_to_inventory_folder
#'
#' clcc(path = data_path)
#'
#' }
clcc <- function(path,
                 path_weights,
                 func_unit = "km", label_digits = 3,
                 plot_phases = FALSE, plot_phases_critical = FALSE,
                 plot_phases_critical_version = "EU",
                 price_source = "2024"){

  if (isFALSE(plot_phases) & isTRUE(plot_phases_critical))
    stop("The 'plot_phases_critical' argument can be set to TRUE only if 'plot_phases' = TRUE as well")

  if(isFALSE(is.character(func_unit)))
    stop("The func_unit parameter must be a string")

  if(isFALSE(is.numeric(label_digits)))
    stop("The paramter 'label_digits' must be an integer")

  if(!is.element(price_source, c("2023", "2024")))
    stop("Please use a valid price source version: either '2023' or '2024'")

  clcc_critical <- object <- phase <- clcc_rel <-  NULL

  inventories <- inventory_load_fn(data_path = path) # loads the inventories

  critical_weights <-
    critical_weights_load_fn(path_weights = path_weights) # loads the critical weights

  inventories$phase <- tolower(inventories$phase)

  if (length(intersect(unique(critical_weights$object), unique(inventories$object))) == 0)
    stop("The objects in the critical weights file are not present in the inventories. Please check the files.")

  if (length(intersect(unique(critical_weights$phase), unique(inventories$phase))) == 0)
    stop("The phases in the critical weights file are not present in the inventories. Please check the files.")

  inventories <-
    inventories |>
    dplyr::left_join(critical_weights)

  inventories$weight <- ifelse(is.na(inventories$weight), 1, inventories$weight) # if weight is NA, set it to 1

  if (price_source == "2024"){

    prices <- clccr::clcc_prices_ref

  } else if (price_source == "2023"){

    prices <- clccr::clcc_prices_ref |>
      dplyr::left_join(clccr::prices_23) |>
      dplyr::mutate(mean = NULL) |>
      dplyr::rename(mean = .data[["price23"]])

  }

  test_commodity <- unique(inventories$comm) %in% prices$comm

  if(isTRUE(any(test_commodity == F))) stop("At least one commodity in the inventory is not present in the master file") # if true, at least one commodity is not in the master file

  inv_prices <- merge(inventories, prices, all.x = TRUE)

  inv_prices <- inv_prices[, -which(names(inv_prices) %in% c("um", "source", "code"))]


  # Computing the CLCC indicator

  clcc_res <-
    inv_prices |>
    dplyr::group_by(.data[["object"]], .data[["phase"]]) |>
    dplyr::summarise(
      clcc = sum(.data[["mean"]] * .data[["quantity"]], na.rm = TRUE),
      clcc_critical_eu = sum(ifelse(.data[["critical_eu"]] == "yes", .data[["mean"]] * .data[["quantity"]] * .data[["weight"]], 0), na.rm = TRUE),
      clcc_critical_iea = sum(ifelse(.data[["critical_iea"]] == "yes", .data[["mean"]] * .data[["quantity"]] * .data[["weight"]], 0), na.rm = TRUE)
    ) |>
    dplyr::ungroup()


  clcc_res <- within(clcc_res, {
    share_critical_eu <- ifelse(clcc != 0,
                                clcc_critical_eu / clcc * 100,
                                0)

    share_critical_iea <- ifelse(clcc != 0,
                                 clcc_critical_iea / clcc * 100,
                                 0)
  })

  # }

  output <- tidyr::as_tibble(clcc_res) |>
    dplyr::arrange(object, phase)


  if(isFALSE(plot_phases)) {

    res <- output |>
      dplyr::filter(.data[["phase"]] == "total") |>
      dplyr::mutate(object = forcats::fct_reorder(.data[["object"]], .data[["clcc"]])) |>
      tidyr::pivot_longer(tidyr::all_of(c("clcc", "clcc_critical_eu", "share_critical_eu",
                                          "clcc_critical_iea", "share_critical_iea")),
                          names_to = "indicator",
                          values_to = "value")

    clcc_labs <- c(paste0("CLCC (euro/", func_unit, ")"),
                   paste0("EU Critical CLCC (euro/", func_unit, ")"),
                   "% Critical (EU)",
                   paste0("IEA Critical CLCC (euro/", func_unit, ")"),
                   "% Critical (IEA)")

    names(clcc_labs) <- c("clcc", "clcc_critical_eu", "share_critical_eu",
                          "clcc_critical_iea", "share_critical_iea")

    plot <- res |>
      ggplot2::ggplot(ggplot2::aes(x = .data[["value"]], y = .data[["object"]], fill = .data[["indicator"]],
                                   label = dplyr::case_when(
                                     .data[["indicator"]] == "clcc" ~ round(.data[["value"]], label_digits),
                                     .data[["indicator"]] == "clcc_critical_eu" ~ round(.data[["value"]], label_digits + 1),
                                     .data[["indicator"]] == "clcc_critical_iea" ~ round(.data[["value"]], label_digits + 1),
                                     TRUE ~ round(value, 1)
                                   )
      )) +
      ggplot2::geom_bar(stat = "identity", width = 0.6) +
      ggplot2::facet_wrap(~ .data[["indicator"]], labeller = ggplot2::labeller(indicator = clcc_labs),
                          scales = "free_x") +
      viridis::scale_fill_viridis(discrete = T) +
      ggfittext::geom_bar_text(outside = T, min.size = 10) +
      ggplot2::labs(x = "", y = "") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8),
                     legend.position = "none") +
      ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 20))

  } else if (isTRUE(plot_phases)){

    indicator <-
      dplyr::case_when(
        isTRUE(plot_phases_critical) & plot_phases_critical_version == "EU" ~ "clcc_critical_eu",
        isTRUE(plot_phases_critical) & plot_phases_critical_version == "IEA" ~ "clcc_critical_iea",
        isFALSE(plot_phases_critical) ~ "clcc"
      )

    # indicator <- ifelse(
    #   isTRUE(plot_phases_critical),
    #   "clcc_critical",
    #   "clcc"
    # )

    res <- output |>
      dplyr::filter(.data$phase != "total", !!rlang::sym(indicator) != 0) |>
      dplyr::group_by(.data$object) |>
      dplyr::mutate(clcc_rel = !!rlang::sym(indicator) / sum(!!rlang::sym(indicator)) * 100) |>
      dplyr::ungroup()

    plot <- ggplot2::ggplot(res,
                            ggplot2::aes(
                              area = !!rlang::sym(indicator), fill = phase,
                              label = paste0(round(clcc_rel, 0), "%")
                            )) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white", place = "middle", reflow = T) +
      ggplot2::facet_wrap(~ object) +
      viridis::scale_fill_viridis(discrete = T, option = "cividis")

  }

  output <- list(output, plot)

  names(output) <- c("table", "plot")

  return(output)


}
