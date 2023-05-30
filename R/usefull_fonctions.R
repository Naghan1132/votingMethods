#' Scores_to_preferences
#' @export
#' @param scores voters scores
#' @returns ranks
scores_to_preferences <- function(scores) {
  preferences <- apply(scores, 2,rank_with_ties <- function(x) {rank(x, ties.method = "random")})
  # Inverser les rangs pour que le candidat le plus préféré soit le numéro 1
  preferences <- nrow(preferences) + 1 - preferences
  # Retourner les preferences
  return(preferences)
}

#' scores_to_borda_points
#' @export
#' @param scores voters preferences
#' @returns points
scores_to_borda_points <- function(scores) {
  # Calculer les rangs de chaque élément sur chaque colonne
  points <- apply(scores, 2,rank_with_ties <- function(x) {rank(x, ties.method = "random")})
  # Retourner les rangs
  return(points)
}

#' Rearrange borda points
#' @export
#' @param points_matrix borda_points
#' @param eliminated_candidates eliminated_candidates
#' @returns preferences_matrix
rearrange_points <- function(points_matrix,eliminated_candidates) {
  points_matrix <- points_matrix[!(rownames(points_matrix) %in% eliminated_candidates),]
  rearranged_matrix <- apply(points_matrix, 2,rank_with_ties <- function(x) {rank(x, ties.method = "random")})
  return(rearranged_matrix)
}

#' Make duel matrix
#' @export
#' @param scores voters preferences
#' @return duel_matrix
make_duel_matrix <- function(scores) {
  n_candidates <- nrow(scores)
  n_voters <- ncol(scores)
  preferences <- scores_to_preferences(scores)
  print(preferences)
  # Calcule la matrice des duels :
  duel_matrix <- matrix(0, n_candidates,n_candidates)
  candidates_names <- rownames(preferences)
  colnames(duel_matrix) <- candidates_names
  rownames(duel_matrix) <- candidates_names
  # Divise les calculs par 2 !
  for (i in 1:(n_candidates - 1)) {
    for (j in (i + 1):n_candidates) {
      win_i_j <- sum(preferences[i,] < preferences[j,])
      duel_matrix[i,j] <- win_i_j
      duel_matrix[j,i] <- n_voters - win_i_j
    }
  }
  return(duel_matrix)
}

ligne_sup <- function(row, indice_ligne,majority) {
  indices <- seq_along(row)
  indices <- indices[indices != indice_ligne]
  all(row[indices] > majority)
}

colonne_sup <- function(col, indice_colonne,majority) {
  indices <- seq_along(col)
  indices <- indices[indices != indice_colonne]
  all(col[indices] > majority)
}

ligne_min <- function(row, indice_ligne) {
  indices <- seq_along(row)
  indices <- indices[indices != indice_ligne]
  min(row[indices])
}



