#' one_qq_plot
#'
#' Constructs QQ plots for single vectors.
#'
#' @import ggplot2
#' @import ggpubr
#' @import magrittr
#'
#' @param data Data in either long or wide format.
#' @param variable The variable for which the QQ plot is being constructed.
#' @return Graphic object
#' @examples
#' one_qq_plot(penguins, "bill_length_mm")
one_qq_plot <-
  function(data, variable) {

    ggplot2::ggplot(data, ggplot2::aes(sample = get(variable))) +
      ggplot2::stat_qq_line(
        linetype = "dashed",
        color = "black", linewidth = 1
      ) +
      ggplot2::stat_qq(color = "#6A6C6E") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Theoretical", y = "Sample", title = paste0("QQ Plot for ",
        sep = "",
        variable
      ))
  }


#' independent_qq_plot
#'
#' Constructs QQ plots for independet t-tests
#'
#' @import ggplot2
#' @import ggpubr
#' @import magrittr
#'
#' @param data Data in either long or wide format.
#' @param variable The variable for which the QQ plot is being constructed.
#' For wide datasets, this alters the titles of the QQ plot.
#' @param grouping_variable The variable that contains the grouping indicator for data in long format.
#' For data in wide format, this only alters the titles of the QQ plot.
#' @param ... For data in wide format, a character vector containing the column names of the measurements must be supplied.
#' @return Graphic object
#' @examples
#' independent_qq_plot(penguins, "bill_length_mm", "sex")
independent_qq_plot <-
  function(data, variable, grouping_variable, ...) {
    if (!(grouping_variable %in% colnames(data))) {
      data <- tidyr::pivot_longer(data, names_to = grouping_variable, cols = everything()) %>%
        dplyr::filter(.[[grouping_variable]] %in% ...)

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

    qq_1 <-
      ggplot2::ggplot(
        split_dfs[[1]],
        ggplot2::aes(sample = get(variable))
      ) +
      ggplot2::stat_qq_line(
        linetype = "dashed",
        color = "black", linewidth = 1
      ) +
      ggplot2::stat_qq(color = "#6A6C6E") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Theoretical", y = "Sample", title = paste0(
        "QQ Plot for ", variable, ": ",
        grouping_variable, "=", levels_vector[[1]]
      ))

    qq_2 <-
      ggplot2::ggplot(
        split_dfs[[2]],
        ggplot2::aes(sample = get(variable))
      ) +
      ggplot2::stat_qq_line(
        linetype = "dashed",
        color = "black", linewidth = 1
      ) +
      ggplot2::stat_qq(color = "#6A6C6E") +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Theoretical", y = "Sample", title = paste0(
        "QQ Plot for ", variable, ": ",
        grouping_variable, "=", levels_vector[[2]]
      ))
    ggpubr::ggarrange(qq_1, qq_2)
  }

#' dependent_qq_plot
#'
#' Constructs QQ plots for dependent t-test.
#'
#' @import ggplot2
#' @import ggpubr
#' @import magrittr
#'
#' @param data Data in either long or wide format.
#' @param variable The variable for which the QQ plot is being constructed.
#' For wide datasets, this alters the titles of the QQ plot.
#' @param grouping_variable The variable that contains the grouping indicator for data in long format.
#' For data in wide format, this only alters the titles of the QQ plot.
#' @param first_group The variable from which the other is subtracted. For data in
#' long format, this is the level of the grouping variable we would like to be first.
#' For data in wide format, this is the column's name containing the first set of measurements.
#' @param second_group Analogous to first_group.
#' @return Graphic object
#' @examples
#' dependent_qq_plot(a1c_measurements, "time_point", "first_measurement", "second_measurement")
dependent_qq_plot <-
  function(data, variable, grouping_variable, first_group, second_group) {
    if (!(grouping_variable %in% colnames(data))) {
      data <- tidyr::pivot_longer(data, names_to = grouping_variable, cols = everything()) %>%
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
