#' Generate a SimaPro-compatible csv file of characterization factors
#'
#' This function generates a csv file with commodity prices (characterization factors) compatible with
#' SimaPro. Such file can be loaded into SimaPro and used by LCA practitioners to compute the CLCC indicator.
#' The function automatically asks the user where to save the csv file. This function has no arguments.
#'
#' @importFrom utils choose.files
#' @importFrom rlang .data
#' @param groups A logical value (either TRUE or FALSE). If set to TRUE, the commodities are grouped
#'     in macro-categories to make interpretation easier.
#' @param critical A logical value that can be selected only when `groups = TRUE`. If it is set to `TRUE` a .csv
#'     file containing critical materials only is generated to be exported to Simapro.
#' @param critical_type A string. If set to `EU`, the critical CLCC indicator is based on the list of critical
#'     materials of the European Union. If it is set to `IEA` the International Energy Agency list is used instead. Default is set to `EU`
#' @return A SimaPro-compatible csv file
#' @export
#' @examples
#' \dontrun{
#' simapro_export()
#'
#' simapro_export(critical_only = FALSE)
#' }
simapro_export <- function(groups = F, critical = F, critical_type = "EU"){

  if (isFALSE(groups) & isTRUE(critical))
    stop("The critical parameter can be selected only when
                                               'groups' == TRUE")

  if (!is.element(critical_type, c("EU", "IEA")))
    stop("Parameter 'critical type' can only assume EU or IEA values")


  if (isTRUE(groups)) {

    prices_simple <- clccr::clcc_prices_ref |>
      dplyr::select(dplyr::all_of(c("comm", "critical_eu", "critical_iea", "macro_cat", "mean", "um")))

    comm_to_retain <- clccr::clcc_prices_ref[clccr::clcc_prices_ref$source != "none", ]$comm


    top <- simapro_template$gruppi_top

    bottom <- simapro_template$gruppi_bottom


    if(isTRUE(critical)){

      critical_variable <-
        ifelse(
          critical_type == "EU",
          "critical_eu",
          "critical_iea"
        )


      prices_simple <- prices_simple |>
        dplyr::filter(
          .data[[critical_variable]] == "yes"
        )

      top <- simapro_template$gruppi_critical_top

      bottom_critical <-
        paste0("gruppi_", critical_variable, "_bottom")

      bottom <- simapro_template[[bottom_critical]]

      comm_to_retain <- clccr::clcc_prices_ref[clccr::clcc_prices_ref$source != "none" &
                               clccr::clcc_prices_ref[[critical_variable]] == "yes", ]$comm


      indicator_name <- # indicator name to write on the .csv output file
        paste0(
          "RSE CLCC Critical ",
          critical_type,
          " Group"
        )

    }

    macro_cat_names <- unique(prices_simple$macro_cat)

    output_data <- clccr::simapro_codes |>
      dplyr::left_join(prices_simple) |>
      dplyr::select(.data[["comp"]],
                    .data[["var1"]],
                    .data[["comm"]],
                    .data[["code1"]],
                    .data[["mean"]],
                    .data[["macro_cat"]],
                    .data[["um"]]) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), \(x) ifelse(is.na(x), "", x))) |>
      dplyr::filter(.data[["comm"]] %in% comm_to_retain)

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

    if(isTRUE(critical)) {

      top[[24]] <- indicator_name

    }

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

    data_raw <- clccr::simapro_codes |>
      dplyr::left_join(clccr::clcc_prices_ref) |>
      dplyr::filter(source != "none") |>
      dplyr::select(.data[["comp"]], .data[["var1"]], .data[["comm"]],
                    price = .data[["mean"]], .data[["um"]], .data[["code1"]],
                    .data[["code2"]],
                    .data[["critical_eu"]], .data[["critical_iea"]])


    data_critical_eu <-
      data_raw |>
      dplyr::mutate(price = ifelse(
        .data[["critical_eu"]] == "yes",
        .data[["price"]],
        0
      ))

    data_critical_iea <-
      data_raw |>
      dplyr::mutate(price = ifelse(
        .data[["critical_iea"]] == "yes",
        .data[["price"]],
        0
      ))

    data_ready <-
      list(
        "CLCC" = data_raw,
        "critical-CLCC_EU" = data_critical_eu,
        "critical-CLCC_IEA" = data_critical_iea
      )

    data_ready <-
      purrr::map(
        data_ready,
        \(x) x[, c("comp", "var1", "comm", "code1", "price", "um", "code2")]
      )


    # actual data to export

    # commodity metadata to add to the csv file

    data_meta <- data_ready[["CLCC"]] |>
      dplyr::bind_rows() |>
      dplyr::left_join(clccr::simapro_codes) |>
      dplyr::arrange(.data[["comm"]]) |>
      dplyr::select(.data[["comm"]], .data[["um"]], .data[["code1"]], .data[["formula"]], .data[["code2"]]) |>
      as.data.frame() |>
      unique()


    # Removing variable names

    names(data_meta) <- NULL

    data_clcc <- as.data.frame(data_ready[["CLCC"]]) |>
      unique()

    data_critical_eu <- as.data.frame(data_ready[["critical-CLCC_EU"]]) |>
      unique()

    data_critical_iea <- as.data.frame(data_ready[["critical-CLCC_IEA"]]) |>
      unique()

    names(data_clcc) <- NULL

    names(data_critical_eu) <- NULL

    names(data_critical_iea) <- NULL


    #generating the csv file

    to_append <- list(simapro_template$top, data_clcc, simapro_template$mid_eu, data_critical_eu,
                      simapro_template$mid_iea, data_critical_iea,
                      simapro_template$mid2, data_meta, simapro_template$bottom)


  }


  if (isFALSE(groups)) {

    file_name <- "export_indicatori_clcc"

  } else if (isTRUE(groups) & isFALSE(critical)) {

    file_name <- "export_clcc_gruppi"

  } else if (isTRUE(groups) & isTRUE(critical) & critical_type == "EU") {

    file_name <- "export_clcc_critical_eu_gruppi"

  } else if (isTRUE(groups) & isTRUE(critical)& critical_type == "IEA") {

    file_name <- "export_clcc_critical_iea_gruppi"
  }

  #return(file_name)

  file_user <-
    choose.files(
      default = paste0(file_name, ".csv"),
      caption = "Scegli dove salvare l'export",
      filters = c("Comma Delimited Files (.csv)","*.csv")
    )

  #file_user <- file.choose() #asking the user where to save the file

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
