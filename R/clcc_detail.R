#' Compute the relative share of individual material/energy flows on total CLCC
#'
#' This function computes the relative shares of each commodity considered for the
#' computation of the CLCC indicator on the total value of the indicator for both the
#' `baseline` and `critical` versions of the indicator itself.
#'
#' @param data_path A character vector. Path to the folder in which raw xlsx files are stored.
#' @param use_weights A logical value. If set to `TRUE`, the function uses the critical weights
#' @param weights_path A character vector. Path to the file containing the critical weights for each commodity and phase.
#' @param critical A logical value. If set to `TRUE`, shares referred to the critical-clcc indicator
#'     are returned. Default is set to `FALSE`.
#' @param critical_type A string. If set to `EU`, the critical CLCC indicator is based on the list of critical
#'     materials of the European Union. If it is set to `IEA` the International Energy Agency list is used instead. Default is set to `EU`
#' @param phase_of_int A character string. The life-cycle phase for which the relative shares
#'     must be computed. Default is `total`. The user can however select any phase included in the
#'     inventory file.
#' @param collapse_share A numeric value. A parameter that the user can use to limit the number of
#'     commodities considered. For example, if `collapse_share = 0.9` (the default) only commodities which
#'     shares sum up to 90% of the total are returned as output. All the others are summed up in the
#'     `others` category. The parameter must be between `0` and `1`.
#'@param price_source A string. If set to `2023` price sources used in the 2023 RDS report are used. If set to `2024`,
#'     the default, the 2024 updated price sources are used.
#' @param viridis_palette A string indicating the palette (from the Viridis package to be applied to the plots)
#' @param palette_direction A number (either `1` or `-1`) to provide the palette direction
#' @return A list containing a table with the results and a `ggplot` object.
#' @importFrom rlang .data
#' @export
#' @examples
#' \dontrun{
#' data_path <- path_to_inventory_folder
#'
#' clcc_detail(data_path = data_path, weights_path = critical_path)
#'
#' clcc_detail(data_path = data_path,
#' weights_path = critical_path,
#' critical = TRUE, collapse_share = 0.5)
#'
#' }
clcc_detail <- function (data_path,
                         use_weights = FALSE,
                         weights_path = NULL,
                         critical = FALSE,
                         critical_type = "EU",
                         phase_of_int = "total",
                         collapse_share = 0.9,
                         price_source = "2024",
                         viridis_palette = "viridis",
                         palette_direction = 1
) {

  if(collapse_share > 1 | collapse_share < 0)
    stop("Please provide a valid value for 'collapse_share'; between 0 and 1.")

  if(!is.element(price_source, c("2023", "2024")))
    stop("Please use a valid price source version: either '2023' or '2024'")

  if (!is.element(critical_type, c("EU", "IEA")))
    stop("Parameter 'critical type' can only assume EU or IEA values")

  valid_palettes <- c("viridis", "plasma", "magma", "inferno", "cividis", "turbo", "mako", "rocket")
  if (!viridis_palette %in% valid_palettes) {
    stop("The Viridis palette chosen does not exists. Please choose one from: ",
         paste(valid_palettes, collapse = ", "))
  }

  quantity <- phase <- comm <- object <- clcc_type <- desc <- share <- cum_share <- macro_cat <- critical_eu <- critical_iea <- weight <- NULL

  inventories <- inventory_load_fn(
    data_path = data_path,
    use_weights = use_weights,
    weights_path = weights_path
  ) # loads the inventories


  if (price_source == "2024"){

    prices <- clccr::clcc_prices_ref

  } else if (price_source == "2023"){

    prices <- clccr::clcc_prices_ref |>
      dplyr::left_join(clccr::prices_23) |>
      dplyr::mutate(mean = NULL) |>
      dplyr::rename(mean = .data$price23)

  }

  prices$comm <- tolower(prices$comm)

  test_commodity <- unique(inventories$comm) %in% prices$comm

  if(isTRUE(any(test_commodity == F))) stop("At least one commodity in the inventory is not present in the master file") # if true, at least one commodity is not in the master file

  inv_prices <-
    inventories |>
    dplyr::left_join(prices)

  if (isTRUE(critical)) {

    if (critical_type == "EU") {

      inv_prices <- inv_prices |>
        dplyr::filter(critical_eu == "yes") |>
        dplyr::mutate(clcc_type = "critical-clcc_eu")

    } else if (critical_type == "IEA") {

      inv_prices <- inv_prices |>
        dplyr::filter(critical_iea == "yes") |>
        dplyr::mutate(clcc_type = "critical-clcc_IEA")

    }


  } else {

    inv_prices$clcc_type <- "baseline-clcc"

  }


  #inv_prices <- inv_prices[, -which(names(inv_prices) %in% c("um", "source", "code"))]

  # Computing the CLCC indicator

  clcc_raw <- inv_prices |>
    dplyr::mutate(clcc = .data[["mean"]] * .data[["quantity"]] * .data[["weight"]]) |>
    dplyr::filter(phase == phase_of_int) |>
    dplyr::group_by(object, phase, clcc_type, macro_cat) |>
    dplyr::summarise(clcc = sum(clcc)) |>
    dplyr::ungroup() |>
    #dplyr::select(comm, object, phase, clcc_type, clcc) |>
    tibble::as_tibble()

  clcc_tot <- clcc_raw |>
    dplyr::group_by(object, phase, clcc_type) |>
    dplyr::summarise(clcc_tot = sum(clcc)) |>
    dplyr::ungroup()

  clcc_detail_cat <- clcc_raw |>
    dplyr::left_join(clcc_tot) |>
    dplyr::mutate(share = clcc / clcc_tot,
                  clcc_tot = NULL) |>
    dplyr::group_by(object) |>
    dplyr::arrange(desc(share)) |>
    dplyr::mutate(cum_share = cumsum(share),
                  macro_cat = ifelse(
                    cum_share <= collapse_share,
                    macro_cat,
                    "Other"
                  )) |>
    dplyr::group_by(object, macro_cat, phase, clcc_type) |>
    dplyr::summarise(dplyr::across(c(clcc, share), sum)) |>
    dplyr::ungroup() |>
    dplyr::arrange(object, desc(share))

  # Generating palette

  # 1. Recover categories
  cats <- unique(clcc_detail_cat$macro_cat)

  # 2. Removing the "Other" category
  cats_viridis <- cats[cats != "Other"]
  n_viridis <- length(cats_viridis)

  # 3. Generating palette
  viridis_colors <- viridis::viridis_pal(
    option = viridis_palette,
    direction = palette_direction # This improves readability
  )(n_viridis)

  # 4. Assigning names to colors
  names(viridis_colors) <- cats_viridis

  # 5. Combining the viridis palette with the Other category (which must be gray)
  custom_colors <- c(viridis_colors, "Other" = "grey60")

  # Defining tile text colors (white or black according to the background)

  # Removing the alpha part from color codes
  custom_colors_no_alpha <- substr(custom_colors, 1, 7)

  # default text colors
  text_colors <- rep("white", length(custom_colors_no_alpha))

  # 2. converting colors
  rgb_colors <- t(grDevices::col2rgb(custom_colors_no_alpha)) / 255

  # 3. Creating sRGB object
  srgb_colors <- colorspace::sRGB(rgb_colors)

  # 4. Converting from sRGB to LUV to get luminosity
  luv_colors <- methods::as(srgb_colors, "LUV")

  # Getting color lumunosity
  color_luminosity <- luv_colors@coords[, "L"]

  # Defining text color according to luminosity
  text_colors[color_luminosity > 50] <- "black"
  names(text_colors) <- names(custom_colors)


  plot <- clcc_detail_cat |>
    ggplot2::ggplot(
      ggplot2::aes(area = share, fill = macro_cat, label = paste0(stringr::str_trunc(macro_cat, 20), " ",
                                                                  round(share * 100),
                                                                  "%"))) +
    treemapify::geom_treemap() +
    ggplot2::facet_wrap(~ object) +
    ggplot2::scale_fill_manual(values = custom_colors) +
    treemapify::geom_treemap_text(
      ggplot2::aes(colour = .data[["macro_cat"]]),
      reflow = TRUE,
      place = "centre", # Centra il testo per una migliore leggibilità
      padding.x = ggplot2::unit(3, "mm"),
      padding.y = ggplot2::unit(3, "mm")
    ) +
    ggplot2::scale_colour_manual(values = text_colors, guide = "none") + # applying text colour
    ggplot2::theme(legend.position = "none")

  output <- list(table = clcc_detail_cat, plot = plot)

  return(output)


}
