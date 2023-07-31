# Variables need to be supplied in quotations

one_qq_plot <- function(data, variable) {
  ggplot2::ggplot(data, aes(sample = get(variable))) +
    ggplot2::stat_qq_line(
      linetype = "dashed",
      color = "black", linewidth = 1
    ) +
    ggplot2::stat_qq(color = "#6A6C6E") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Theoretical", y = "Sample", title = paste0("QQ Plot for ",
      sep = "",
      deparse(substitute(variable))
    ))
}

independent_qq_plot <- function(data, variable, grouping_variable) {
  split_dfs <- data %>%
    dplyr::group_split(get(grouping_variable))

  levels_vector <-
    sapply(split_dfs, function(x) (x[[grouping_variable]])[[1]]) %>%
    as.character()

  if (length(split_dfs) < 2) {
    stop(cat("There are less than two levels to the grouping variable, it is", levels_vector, "\n"))
  } else if (length(split_dfs) > 2) {
    stop(cat("There are more than two levels to the grouping variable, they are:", levels_vector, "\n"))
  }

  qq_1 <-
    ggplot2::ggplot(
      split_dfs[[1]],
      aes(sample = get(variable))
    ) +
    ggplot2::stat_qq_line(
      linetype = "dashed",
      color = "black", linewidth = 1
    ) +
    ggplot2::stat_qq(color = "#6A6C6E") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Theoretical", y = "Sample", title = paste0("QQ Plot for ",
      sep = "",
      levels_vector[[1]]
    ))

  qq_2 <-
    ggplot2::ggplot(
      split_dfs[[2]],
      aes(sample = get(variable))
    ) +
    ggplot2::stat_qq_line(
      linetype = "dashed",
      color = "black", linewidth = 1
    ) +
    ggplot2::stat_qq(color = "#6A6C6E") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Theoretical", y = "Sample", title = paste0("QQ Plot for ",
      sep = "",
      levels_vector[[2]]
    ))
  ggpubr::ggarrange(qq_1, qq_2)
}

dependent_qq_plot <- function(data, variable, grouping_variable, first_group, second_group) {
  split_dfs <- data %>%
    dplyr::group_split(get(grouping_variable))

  levels_vector <-
    sapply(split_dfs, function(x) (x[[grouping_variable]])[[1]]) %>%
    as.character()

  if (length(split_dfs) < 2) {
    stop(cat("There are less than two levels to the grouping variable, it is", levels_vector, "\n"))
  } else if (length(split_dfs) > 2) {
    stop(cat("There are more than two levels to the grouping variable, they are:", levels_vector, "\n"))
  }

  first_group_df <-
    ifelse(split_dfs[[1]][[grouping_variable]][[1]] == first_group,
      split_dfs[1], split_dfs[2]
    )

  second_group_df <-
    ifelse(split_dfs[[2]][[grouping_variable]][[1]] == second_group,
      split_dfs[2], split_dfs[1]
    )

  data.frame(diff = first_group_df[[1]][[variable]] - second_group_df[[1]][[variable]]) %>%
    ggplot2::ggplot(
      .,
      aes(sample = diff)
    ) +
    ggplot2::stat_qq_line(
      linetype = "dashed",
      color = "black", linewidth = 1
    ) +
    ggplot2::stat_qq(color = "#6A6C6E") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Theoretical", y = "Sample", title = paste0("QQ Plot for ",
      sep = "",
      first_group, "-", second_group
    ))
}

