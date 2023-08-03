normal_correlation <- function(data, method){

  numeric_subset <- data[sapply(data, is.numeric)]

  length_numeric_colnames <- length(colnames(numeric_subset))

  qq_plots_list <- purrr::map(seq_len(length_numeric_colnames),
                  ~one_qq_plot(numeric_subset, numeric_subset_names[[.x]]))

  # In order to determine ideal qq plot layout
  cols <- round(sqrt(length(qq_plots)),0)
  rows <- ceiling(length(qq_plots)/cols)

  qq_plots_out <-
    ggpubr::ggarrange(plotlist = qq_plots_list, ncol = cols, nrow = rows)

  corr_mat <- cor(numeric_subset, method = method)

  normality_res <- lapply(numeric_subset, shapiro.test) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    tibble::rownames_to_column("Variable") %>%
    dplyr::select(-c(method, `data.name`)) %>%
    dplyr::rename("Test Statistic" = "statistic", "p-value" = "p.value")

out_list <- list("Normality Test" = normality_res,
                 "Correlation Matrix" = corr_mat,
                 "QQ Plots" = qq_plots_out)

return(out_list)

}








