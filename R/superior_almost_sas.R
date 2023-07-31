# binwidth = x argument may be passed to the ellipsis
superior_almost_sas <- function(model, ...) {
  if ("lm" %in% class(model) == FALSE) {
    stop("Not a valid regression model.
       Make sure object is created either via `lm` or `aov`")
  }

  res_fitted <-
    ggplot2::ggplot(model, aes(x = .fitted, y = .resid)) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed",
      color = "black", linewidth = 1
    ) +
    ggplot2::geom_point(color = "#6A6C6E") +
    ggplot2::labs(y = "Residual", x = "Fitted", title = "Residual vs. Fitted Plot") +
    ggplot2::theme_classic()

  res_his <-
    ggplot2::ggplot(model, aes(x = .resid)) +
    ggplot2::geom_histogram(aes(y = after_stat(count / sum(count))),
      colour = "black",
      fill = "#6A6C6E", ...
    ) +
    ggplot2::stat_function(
      fun = dnorm, args = list(
        mean = mean(model$residuals),
        sd = sd(model$residuals)
      ),
      linewidth = 1, color = "black"
    ) +
    ggplot2::geom_vline(
      xintercept = 0, linetype = "dashed",
      color = "black", linewidth = 1.5
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ) +
    ggplot2::labs(x = "Residual", y = "", title = "Histogram of Residuals")

  qq_res <-
    ggplot2::ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
    ggplot2::geom_abline(color = "black", linewidth = 1, linetype = "dashed") +
    ggplot2::geom_point(color = "#6A6C6E") +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Theoretical Quantiles", y = "Standardized Residuals",
      title = "Q-Q Plot of Residuals"
    )

  out <-
    ggpubr::ggarrange(res_fitted, res_his, qq_res)

  return(out)
}
