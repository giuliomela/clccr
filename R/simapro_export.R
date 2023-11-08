#' Generate a SimaPro-compatible csv file of characterization factors
#'
#' This function generates a csv file with commodity prices (characterization factors) compatible with
#' SimaPro. Such file can be loaded into SimaPro and used by LCA practitioners to compute the CLCC indicator.
#' The function automatically asks the user where to save the csv file. This function has no arguments.
#'
#' @return A SimaPro-compatible csv file
#' @export
#' @examples
#' \dontrun{
#' simapro_export()
#'
#' simapro_export(critical_only = FALSE)
#' }
simapro_export <- function(){

  comp <- var1 <- comm <- um <- code1 <- code2 <- critical <- energy <- formula <- price <- NULL

  #selecting flows of interest

  data_raw <- simapro_codes %>%
    dplyr::left_join(clccr::clcc_prices_ref) %>%
    dplyr::filter(source != "none") %>%
    dplyr::select(comp, var1, comm, price = mean, um, code1, code2, critical)


  data_ready <- lapply(list(c("yes", "no"), "no"),
                       function(x) data_raw %>%
                         dplyr::mutate(price = ifelse(
                           critical %in% x,
                           price,
                           0
                         )) %>%
                         dplyr::select(comp, var1, comm, code1, price, um, code2)
  )

  names(data_ready) <- c("CLCC", "critical-CLCC")

  # actual data to export

  # commodity metadata to add to the csv file

  data_meta <- data_ready[[1]] %>%
    dplyr::bind_rows() %>%
    dplyr::left_join(simapro_codes) %>%
    dplyr::arrange(comm) %>%
    dplyr::select(comm, um, code1, formula, code2) %>%
    as.data.frame() %>%
    unique()

  # Removing variable names

  names(data_meta) <- NULL

  data_clcc <- as.data.frame(data_ready[["CLCC"]]) %>%
    unique()

  data_critical <- as.data.frame(data_ready[["critical-CLCC"]]) %>%
    unique()

  names(data_clcc) <- NULL

  names(data_critical) <- NULL

  #generating the csv file

  to_append <- list(simapro_template$top, data_clcc, simapro_template$mid1, data_critical,
                    simapro_template$mid2, data_meta, simapro_template$bottom)

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
