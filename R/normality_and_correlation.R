
### DRAFT ###
method <- "pearson"

data <- drop_na(penguins)
numeric_subset <- data[sapply(data, is.numeric)]

corr_mat <- cor(numeric_subset, method = method) %>% round(.,3)

normality_res <- lapply(numeric_subset, shapiro.test) %>%
  do.call(rbind, .) %>%
  data.frame() %>%
  tibble::rownames_to_column("Variable") %>%
  dplyr::select(-c(method, `data.name`)) %>%
  dplyr::rename("Test Statistic" = "statistic", "p-value" = "p.value")
