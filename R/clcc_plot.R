clcc_plot <- function(path,
                      recap_plot = F,
                      phases_plot = F,
                      commodity_plot = F,
                      func_unit = "km",
                      label_digits = 3){


  if (isTRUE(recap_plot)) {

    res <- clccr::clcc(
      path = {{path}},
      critical = T
    )

  res <- res %>%
    dplyr::filter(.data$phase == "total") %>%
    dplyr::mutate(object = forcats::fct_reorder(.data$object, .data$clcc)) %>%
    tidyr::pivot_longer(c(.data$clcc, .data$clcc_critical, .data$share_critical),
                        names_to = "indicator",
                        values_to = "value")

  clcc_labs <- c(paste0("CLCC (euro/", func_unit, ")"),
                 paste0("Critical CLCC (euro/", func_unit, ")"),
                 "% Critical")

  names(clcc_labs) <- c("clcc", "clcc_critical", "share_critical")

  plot <- res %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$value, y = .data$object, fill = .data$indicator,
               label = dplyr::case_when(
                 .data$indicator == "clcc" ~ round(.data$value, label_digits),
                 .data$indicator == "clcc_critical" ~ round(.data$value, label_digits + 1),
                 TRUE ~ round(value, 1)
                 )
               )) +
    ggplot2::geom_bar(stat = "identity", width = 0.6) +
    ggplot2::facet_wrap(~ .data$indicator, labeller = ggplot2::labeller(indicator = clcc_labs),
               scales = "free_x") +
    utilsgm::rse_theme(legend_pos = "none") +
    viridis::scale_fill_viridis(discrete = T) +
    ggfittext::geom_bar_text(outside = T, family = "Verdana", min.size = 10) +
    ggplot2::labs(x = "", y = "") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8)) +
    ggplot2::scale_y_discrete(labels = function(x) stringr::str_wrap(x, width = 20))


  } else if (isTRUE(phases_plot)) {

    res <- clccr::clcc(
      path = {{path}},
      critical = T
    )

    res <- res %>%
      dplyr::filter(.data$phase != "total", .data$clcc != 0) %>%
      dplyr::group_by(.data$object) %>%
      dplyr::mutate(clcc_rel = .data$clcc / sum(.data$clcc) * 100) %>%
      dplyr::ungroup()

    plot <- ggplot2::ggplot(res,
                            ggplot2::aes(
      area = clcc, fill = phase, label = paste0(round(clcc_rel, 0), "%")
    )) +
      treemapify::geom_treemap() +
      treemapify::geom_treemap_text(colour = "white", place = "middle", reflow = T) +
      ggplot2::facet_wrap(~ object) +
      utilsgm::rse_theme() +
      viridis::scale_fill_viridis(discrete = T, option = "cividis")

  } else if (isTRUE(commodity_plot)){

    res <- clccr::clcc_detail(
      path = {{path}},
      critical = F
    )

    plot <- res %>%
    ggplot2::ggplot(
      ggplot2::aes(area = share, fill = comm, label = paste0(stringr::str_trunc(comm, 20), " ",
                                                         round(share * 100),
                                                         "%"))) +
      treemapify::geom_treemap() +
      ggplot2::facet_wrap(~ object) +
      viridis::scale_fill_viridis(discrete = T, option = "turbo") +
      utilsgm::rse_theme(legend_pos = "none") +
      treemapify::geom_treemap_text(color = "white", reflow = T)

  }


    plot



}
