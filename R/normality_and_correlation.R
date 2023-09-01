#' normality_correlation
#'
#' Allows you to check the normality of each numeric variable by conducting the
#' Shapiro-Wilk test and examining QQ plots. Additionally, it provides
#' a correlation matrix based on the specified method.
#'
#' @import ggplot2
#' @import ggpubr
#' @importFrom magrittr %>%
#' @import purrr
#' @import tidyr
#'
#' @param data Data with numeric variables.
#' @param method The method used to calculate correlation:
#' c("pearson", "kendall", "spearman")
#' @param digits_desired Optional argument for desired number of digits in
#' numeric output. Default value is 5.
#' @return List containing qq plots, correlation matrix, and Shapiro-Wilk test
#' results
#' @examples
#' normality_correlation(penguins, "pearson")
#' mutate_all(mtcars, as.factor) %>% normality_correlation(., "kendall") # Error
#' normality_correlation(mtcars, "invalid_method") # Error
#' @export
normality_correlation <- function(data, method, digits_desired = NULL){

  if(is.null(digits_desired)){digits_desired <- 5}
  else{digits_desired <- digits_desired}

  if(!(method %in% c("pearson", "kendall", "spearman"))){stop("The method specified is not one supported by the `cor()` function.")}

  numeric_subset <- data[sapply(data, is.numeric)]

  numeric_subset_names <- colnames(numeric_subset)

  if(length(numeric_subset_names) == 0){stop("The dataframe contains no numeric variables. Check variable types using `typeof()`")}

  length_numeric_colnames <- length(numeric_subset_names)

  qq_plots_list <- purrr::map(seq_len(length_numeric_colnames),
                  ~one_qq_plot(numeric_subset, numeric_subset_names[[.x]]))

  # In order to determine ideal qq plot layout
  cols <- round(sqrt(length(qq_plots_list)),0)
  rows <- ceiling(length(qq_plots_list)/cols)

  qq_plots_out <-
    ggpubr::ggarrange(plotlist = qq_plots_list, ncol = cols, nrow = rows)

  corr_mat <- cor(numeric_subset, method = method) %>%
    round(., digits = digits_desired)

  normality_res <- lapply(numeric_subset, shapiro.test) %>%
    do.call(rbind, .) %>%
    data.frame() %>%
    tibble::rownames_to_column("Variable") %>%
    dplyr::select(-c(method, `data.name`)) %>%
    dplyr::rename("Test Statistic" = "statistic", "p-value" = "p.value") %>%
    tidyr::unnest(cols = c(`Test Statistic`, `p-value`)) %>%
    dplyr::mutate_if(is.numeric, ~round(.x, digits = digits_desired))

out_list <- list("Normality Test" = normality_res,
                 "Correlation Matrix" = corr_mat,
                 "QQ Plots" = qq_plots_out)

return(out_list)

}

