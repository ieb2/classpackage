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
#' cooks(model) # Works
#' cooks(model, label = FALSE, show.threshold = TRUE, threshold = "matlab", scale.factor = 0.9) # Works
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

  return(base_plot)
}

#' outlier_count
#'
#' Returns a count of suspected outliers in the dataset.
#' @import ggplot2
#' @import ggpubr
#' @importFrom magrittr %>%
#' @import purrr
#' @import tidyr
#' @import broom
#' @param df dataset of interest.
#' @param model A linear model generated via "glm", "lm" or "aov".
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

outlier_graph <- function(df, model, x_var, y_var, x_lab, y_lab){
  if ("lm" %in% class(model) == FALSE) {
    stop("Not a valid regression model.
       Make sure object is created either via `glm`, lm` or `aov`")
  }

  broom::augment(model) %>%
    dplyr::mutate(outlier = dplyr::if_else(abs(.std.resid) > 2.5, true = "Suspected", false = "Not Suspected", missing = "Missing")) %>%
    dplyr::select(outlier) %>%
    merge(., df) %>%
    ggplot2::ggplot(., aes(x = {{ x_var }}, y = {{ y_var }}, color = outlier)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values = c("#999999", "#000000")) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = x_lab, y = y_lab, color = "Outlier")
}
model <- lm(bill_depth_mm ~ ., palmerpenguins::penguins)
df <- palmerpenguins::penguins

outlier_graph(df = df, model = model, x_var = bill_length_mm, y_var = bill_depth_mm,
              x_lab = "poop", y_lab = "pee")


