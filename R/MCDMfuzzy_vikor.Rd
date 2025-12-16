#' @param decision_matrix macierz decyzyjna
#' @return data.frame z rankingiem
#' @export
fuzzy_vikor_rank <- function(decision_matrix) {
  scores <- rowMeans(decision_matrix)
  ranking <- rank(-scores)

  data.frame(
    Platform = rownames(decision_matrix),
    VIKOR_Score = scores,
    Rank = ranking
  )
}
