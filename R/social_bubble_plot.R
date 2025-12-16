#' Bubble Plot of Social Media Addiction
#'
#' @param data data.frame
#' @export
social_bubble_plot <- function(data) {
  plot(
    data$Time,
    data$Addiction_Score,
    cex = data$Addiction_Score / 10,
    xlab = "Avg Daily Usage Hours",
    ylab = "Addiction Score",
    pch = 19
  )
}
