
# binwidth = x argument may be passed to the ellipsis
superior_almost_sas <- function(model, ...) {
  require(ggplot2)
  require(ggpubr)
  if ("lm" %in% class(model) == FALSE) {
    stop("Not a valid regression model.
       Make sure object is created either via `lm` or `aov`")
  }

  res_fitted <-
    ggplot(model, aes(x = .fitted, y = .resid)) +
    geom_hline(
      yintercept = 0, linetype = "dashed",
      color = "black", linewidth = 1
    ) +
    geom_point(color = "#6A6C6E") +
    labs(y = "Residual", x = "Fitted", title = "Residual vs. Fitted Plot") +
    theme_classic()

  res_his <-
    ggplot(model, aes(x = .resid)) +
    geom_histogram(aes(y = after_stat(count/sum(count))),
                   colour = "black",
                   fill = "#6A6C6E", ...
    ) +
    stat_function(
      fun = dnorm, args = list(
        mean = mean(model$residuals),
        sd = sd(model$residuals)
      ),
      linewidth = 1, color = "black"
    ) +
    geom_vline(
      xintercept = 0, linetype = "dashed",
      color = "black", linewidth = 1.5
    ) +
    theme_classic() +
    theme(
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    ) +
    labs(x = "Residual", y = "", title = "Histogram of Residuals")

  qq_res <-
    ggplot(model, aes(qqnorm(.stdresid)[[1]], .stdresid)) +
    geom_abline(color = "black", linewidth = 1, linetype = "dashed") +
    geom_point(color = "#6A6C6E") +
    theme_classic() +
    labs(
      x = "Theoretical Quantiles", y = "Standardized Residuals",
      title = "Q-Q Plot of Residuals"
    )

  out <-
    ggarrange(res_fitted, res_his, qq_res)

  return(out)
}
