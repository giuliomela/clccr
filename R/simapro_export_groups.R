#
#
# prices_simple <- clccr::clcc_prices_ref |>
#   dplyr::select(dplyr::all_of(c("comm", "macro_cat", "mean")))
#
#
# macro_cat_names <- unique(prices_simple$macro_cat)
#
# comm_to_retain <- clccr::clcc_prices_ref[clccr::clcc_prices_ref$source != "none", ]$comm
#
# output_data <- simapro_codes |>
#   dplyr::left_join(prices_simple) |>
#   dplyr::select(.data[["comp"]],
#                 .data[["var1"]],
#                 .data[["comm"]],
#                 .data[["code1"]],
#                 .data[["mean"]],
#                 .data[["macro_cat"]]) |>
#   dplyr::mutate(um = "kg") |>
#   dplyr::mutate(dplyr::across(dplyr::everything(), \(x) ifelse(is.na(x), "", x))) |>
#   dplyr::filter(comm %in% comm_to_retain)
#
#
# output_data_l <- split(output_data[-6],
#       output_data$macro_cat)
#
# #creating a list of matrices
#
# output_list <- purrr::map(
#   1:length(output_data_l),
#   \(x) {
#
#     cat <- matrix(
#       c("Impact category", names(output_data_l)[x], "", "EUR", rep("", 14)),
#       nrow = 3
#     )
#
#     subs <- as.matrix(
#       output_data_l[[x]]
#     )
#
#     colnames(subs) <- NULL
#
#     out <- do.call(
#       rbind,
#       list(
#         cat,
#         matrix(c("Substances", rep("", 5)), nrow = 1),
#         subs,
#         matrix(rep("", 6), nrow = 1)
#       )
#     )
#
#     out
#
#   }
# )
#
# output_df <- do.call(
#   rbind,
#   output_list
# )
#
# top <- simapro_template$gruppi_top |>
#   dplyr::mutate(
#     dplyr::across(dplyr::everything(), \(x) ifelse(is.na(x), "", x))
#   )
#
# top[, 3:6] <- ""
#
# colnames(top) <- NULL
#
# top <- as.matrix(top)
#
# bottom <- simapro_template$gruppi_bottom |>
#   dplyr::mutate(
#     dplyr::across(dplyr::everything(), \(x) ifelse(is.na(x), "", x))
#   )
#
# bottom[, 3:6] <- ""
#
# colnames(bottom) <- NULL
#
# bottom <- as.matrix(bottom)
#
#
# to_append <- list(top, matrix(rep("", 6), nrow = 1), output_df, bottom)
#
# file_user <- file.choose() #asking the user where to save the file
#
# for(i in seq_along(to_append))
# {
#   utils::write.table(
#     to_append[[i]],
#     file_user,
#     append    = i > 1,
#     sep       = ";",
#     row.names = FALSE,
#     col.names = FALSE,
#     na = ""
#   )
# }

