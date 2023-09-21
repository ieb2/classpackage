dependent_qq_plot <-
  function(data, variable, grouping_variable, first_group, second_group) {

    numeric_columns <- data[sapply(data, is.numeric)]

    if (!(grouping_variable %in% colnames(numeric_columns))) {
      data <- tidyr::pivot_longer(numeric_columns, names_to = grouping_variable, cols = everything()) %>%
        dplyr::filter(.[[grouping_variable]] %in% c(first_group, second_group))

      colnames(data) <- c(grouping_variable, variable)
    } else {
      data <- data
    }

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

    diff <- first_group_df[[1]][[variable]] - second_group_df[[1]][[variable]]

    if (first_group_df[[1]][[grouping_variable]][[1]] != first_group) {
      stop(cat(first_group, "is not a valid level of the grouping variable"))
    }

    if (second_group_df[[1]][[grouping_variable]][[1]] != second_group) {
      stop(cat(second_group, "is not a valid level of the grouping variable"))
    }

    data.frame(diff) %>%
      ggplot2::ggplot(
        .,
        ggplot2::aes(sample = diff)
      ) +
      ggplot2::stat_qq_line(
        linetype = "dashed",
        color = "black", linewidth = 1
      ) +
      ggplot2::stat_qq(color = "#6A6C6E") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Theoretical", y = "Sample", title = paste0(
        "QQ Plot for ",
        variable, " in order of: ",
        first_group, "-", second_group
      ))
  }
