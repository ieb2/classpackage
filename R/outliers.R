#' cooks
#'
#' Returns Cook's distance plot for supplied linear models.
#' @import ggplot2
#' @import ggpubr
#' @importFrom magrittr %>%
#' @import purrr
#' @import tidyr
#' @import broom
#'
#' @param fitted.lm A linear model generated via "glm", "lm" or "aov".
#' @param label Logical. Whether or not outliers should be labeled on the plot.
#' @param show.threshold Logical. Whether or not the threshold value should be on the plot.
#' @param threshold Determines the formula used in the calculation of the threshold value,
#' used to determine whether an observation is an outlier. "baseR" (0.5 and 1), "matlab" (mean(cooksd)*3), and "convention" (4/n and 1).
#' @param scale.factor Integer value that scales the point size and linewidth to allow customized viewing. Defaults to 0.5
#' @return gg object
#' @examples
#' model <- lm(bill_length_mm ~ bill_depth_mm + year, penguins)
#' cooks(model)
#' @export
cooks <-
  function(fitted.lm, label = TRUE, show.threshold = FALSE, threshold = "convention", scale.factor = 0.5)
  {

  if ("lm" %in% class(model) == FALSE) {
    stop("Not a valid regression model.
       Make sure object is created either via `glm`, lm` or `aov`")
  }

  # obtain linear model matrix
  lm_matrix <- broom::augment(fitted.lm) %>%
    as.data.frame()
  lm_matrix[, "rowname"] <- 1:nrow(lm_matrix)

  # threshold for outlier
  cooksd = lm_matrix[, ".cooksd"]
  n = nrow(lm_matrix)

  # compute the threshold value for cook's distance plot
  if (threshold == "matlab") {
    threshold = mean(cooksd) * 3
  }
  else if (threshold == "baseR") {
    threshold = c(0.5, 1)
  }
  else if (threshold == "convention") {
    threshold = c(4/n, 1)
  }
  else {
    stop("invalid threshold specified for gg_cooksd")
  }

  # window limit
  limit = max(cooksd, na.rm = T)
  margin_factor = 5
  margin = round(limit / margin_factor)
  max_cook = limit + margin

  .cooksd <- NULL

  base_plot <- (ggplot2::ggplot(fitted.lm, aes(1:nrow(lm_matrix), .cooksd, ymin = 0, ymax = cooksd)) +
                  ggplot2::geom_point(size = scale.factor) +
                  ggplot2::geom_linerange(size = scale.factor) +
                  ggplot2::xlab("Observation Number") +
                  ggplot2::ylab("Cook's Distance") +
                  ggplot2::ylim(0, max_cook+0.05)) +
    ggplot2::theme_bw()

  # labelling of potential outliers
  if (label) {
    out_inds <- which(cooksd < min(threshold))
    lm_matrix[out_inds, "rowname"] <- rep("", length(out_inds))
    base_plot = base_plot +
      ggplot2::geom_text(label = lm_matrix[, "rowname"], nudge_y = 0.05, color = "black")
  }

  if (show.threshold) {
    base_plot <- base_plot +
      ggplot2::geom_hline(yintercept = threshold, linetype = "dashed")
  }

  return(base_plot)
  }


#' outlier_count
#'
#' Returns a tibble with count of suspected outliers in the dataset.
#' @import ggplot2
#' @import ggpubr
#' @importFrom magrittr %>%
#' @import purrr
#' @import tidyr
#' @import broom
#' @param df dataset of interest.
#' @param model A linear model generated via "glm", "lm" or "aov".
#' model <- lm(bill_length_mm ~ bill_depth_mm + year, penguins)
#' outlier_count(penguins, model)
#' @export
outlier_count <- function(df, model) {
  if ("lm" %in% class(model) == FALSE) {
    stop("Not a valid regression model.
       Make sure object is created either via `glm`, lm` or `aov`")
  }

  broom::augment(model) %>%
    dplyr::mutate(outlier = dplyr::if_else(abs(.std.resid) > 2.5, true = "Suspected", false = "Not Suspected", missing = "Missing")) %>%
    dplyr::group_by(outlier) %>%
    dplyr::summarise(count = n())
}

#' outlier_grpah
#'
#' Returns a scatterplot augmented with outlier information.
#' @import ggplot2
#' @import ggpubr
#' @importFrom magrittr %>%
#' @import purrr
#' @import tidyr
#' @import broom
#' @param df dataset of interest.
#' @param model A linear model generated via "glm", "lm" or "aov".
#' @param x_var The variable to be plotted on the x-axis.
#' @param y_var The variable to be plotted on the y-axis.
#' @param x_lab String to be used as the x-axis label. If NULL, defaults to the name of the x_var.
#' @param y_lab String to be used as the y-axis label. If NULL, defaults to the name of the y_var.
#' model <- lm(bill_length_mm ~ bill_depth_mm + year, penguins)
#' outlier_graph(penguins, model, x_var = bill_depth_mm, y_var = bill_length_mm) # Axis labels will be bill_depth_mm and bill_length_mm, respectively.
#' outlier_graph(penguins, model, x_var = bill_depth_mm, y_var = bill_length_mm, x_lab = "a", y_lab = "b") # Axis labels will be a and b, respectively.
#' @export
outlier_graph <- function(df, model, x_var, y_var, x_lab = NULL, y_lab = NULL){
  if ("lm" %in% class(model) == FALSE) {
    stop("Not a valid regression model.
       Make sure object is created either via `glm`, lm` or `aov`")
  }

  if(is.null(x_lab)){
    x_lab <- deparse(substitute(x_var))
  } else {x_lab <- x_lab}

  if(is.null(y_lab)){
    y_lab <- deparse(substitute(y_var))
  } else {y_lab <- y_lab}

  data <- broom::augment(model, newdata = df) %>%
    dplyr::mutate(.std.resid = .resid / sd(.resid, na.rm = TRUE)) %>%
    dplyr::filter(!is.na(.std.resid)) %>%
    dplyr::mutate(outlier = dplyr::if_else(abs(.std.resid) > 2.5, true = "Suspected", false = "Not Suspected", missing = "Missing"))

  n_suspected <- sum(data$outlier == "Suspected")

  plot <- data %>%
    ggplot2::ggplot(., aes(x = {{ x_var }}, y = {{ y_var }}, color = outlier)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c("#999999", "#000000")) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x_lab, y = y_lab, color = "Outlier",
                  title = paste0("There are ", n_suspected, " suspected outliers."))

  return(plot)
}

