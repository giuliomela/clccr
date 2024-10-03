#' Generate a SimaPro-compatible csv file of characterization factors
#'
#' This function generates a csv file with commodity prices (characterization factors) compatible with
#' SimaPro. Such file can be loaded into SimaPro and used by LCA practitioners to compute the CLCC indicator.
#' The function automatically asks the user where to save the csv file. This function has no arguments.
#'
#' @param groups A logical value (either TRUE or FALSE). If set to TRUE, the commodities are grouped
#'     in macro-categories to make interpretation easier.
#' @param crit A logical value that can be selected only when `groups = TRUE`. If it is set to `TRUE` a .csv
#'     file containing critical materials only is generated to be exported to Simapro.
#' @return A SimaPro-compatible csv file
#' @export
#' @examples
#' \dontrun{
#' simapro_export()
#'
#' simapro_export(critical_only = FALSE)
#' }
simapro_export <- function(groups = FALSE, crit = FALSE){

  comp <- var1 <- comm <- um <- code1 <- code2 <- critical <- energy <- formula <- price <- NULL

  if (isFALSE(groups) & isTRUE(crit))
    stop("The critical parameter can be selected only when
                                               'groups' == TRUE")


  if (isTRUE(groups)) {


    if(isTRUE(crit)){

      prices_simple <- clccr::clcc_prices_ref |>
        dplyr::filter(
            .data[["critical"]] == "yes"
        ) |>
        dplyr::select(dplyr::all_of(c("comm", "macro_cat", "mean", "um")))

      top <- simapro_template$gruppi_critical_top

      bottom <- simapro_template$gruppi_critical_bottom

      comm_to_retain <- clccr::clcc_prices_ref[clccr::clcc_prices_ref$source != "none" &
                                                 clccr::clcc_prices_ref$critical == "yes", ]$comm

    } else if (isFALSE(crit)) {

      prices_simple <- clccr::clcc_prices_ref |>
        dplyr::select(dplyr::all_of(c("comm", "macro_cat", "mean", "um")))

    top <- simapro_template$gruppi_top

    bottom <- simapro_template$gruppi_bottom

    comm_to_retain <- clccr::clcc_prices_ref[clccr::clcc_prices_ref$source != "none", ]$comm

    }


    macro_cat_names <- unique(prices_simple$macro_cat)

    output_data <- simapro_codes |>
      dplyr::left_join(prices_simple) |>
      dplyr::select(.data[["comp"]],
                    .data[["var1"]],
                    .data[["comm"]],
                    .data[["code1"]],
                    .data[["mean"]],
                    .data[["macro_cat"]],
                    .data[["um"]]) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) ifelse(is.na(x), "", x))) |>
      dplyr::filter(comm %in% comm_to_retain)


    output_data_l <- split(output_data[-6],
                           output_data$macro_cat)

    #creating a list of matrices

    output_list <- purrr::map(
      1:length(output_data_l),
      \(x) {

        cat <- matrix(
          c("Impact category", names(output_data_l)[x], "", "", "EUR", rep("", 13)),
          nrow = 3
        )

        subs <- as.matrix(
          output_data_l[[x]]
        )

        colnames(subs) <- NULL

        out <- do.call(
          rbind,
          list(
            cat,
            matrix(c("Substances", rep("", 5)), nrow = 1),
            subs,
            matrix(rep("", 6), nrow = 1)
          )
        )

        out

      }
    )

    output_df <- do.call(
      rbind,
      output_list
    )

    top <- top |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), \(x) ifelse(is.na(x), "", x))
      )

    top[, 3:6] <- ""

    colnames(top) <- NULL

    top <- as.matrix(top)

    bottom <- bottom |>
      dplyr::mutate(
        dplyr::across(dplyr::everything(), \(x) ifelse(is.na(x), "", x))
      )

    bottom[, 3:6] <- ""

    colnames(bottom) <- NULL

    bottom <- as.matrix(bottom)


    to_append <- list(top, matrix(rep("", 6), nrow = 1), output_df, bottom)

  }

  else if (isFALSE(groups)) {

  #selecting flows of interest

  data_raw <- simapro_codes |>
    dplyr::left_join(clccr::clcc_prices_ref) |>
    dplyr::filter(source != "none") |>
    dplyr::select(comp, var1, comm, price = mean, um, code1, code2, critical)


  data_ready <- lapply(list(c("yes", "no"), "yes"),
                       function(x) data_raw |>
                         dplyr::mutate(price = ifelse(
                           critical %in% x,
                           price,
                           0
                         )) |>
                         dplyr::select(comp, var1, comm, code1, price, um, code2)
  )

  names(data_ready) <- c("CLCC", "critical-CLCC")

  # actual data to export

  # commodity metadata to add to the csv file

  data_meta <- data_ready[[1]] |>
    dplyr::bind_rows() |>
    dplyr::left_join(simapro_codes) |>
    dplyr::arrange(comm) |>
    dplyr::select(comm, um, code1, formula, code2) |>
    as.data.frame() |>
    unique()

  # Removing variable names

  names(data_meta) <- NULL

  data_clcc <- as.data.frame(data_ready[["CLCC"]]) |>
    unique()

  data_critical <- as.data.frame(data_ready[["critical-CLCC"]]) |>
    unique()

  names(data_clcc) <- NULL

  names(data_critical) <- NULL

  #generating the csv file

  to_append <- list(simapro_template$top, data_clcc, simapro_template$mid1, data_critical,
                    simapro_template$mid2, data_meta, simapro_template$bottom)

  }

  file_user <- file.choose() #asking the user where to save the file

  for(i in seq_along(to_append))
  {
    utils::write.table(
      to_append[[i]],
      file_user,
      append    = i > 1,
      sep       = ";",
      row.names = FALSE,
      col.names = FALSE,
      na = ""
    )
  }



}
