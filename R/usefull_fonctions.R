#' Preferences_to_ranks
#' @export
#' @param preferences voters preferences
#' @returns ranks
preferences_to_ranks <- function(preferences) {
  # Calculer les rangs de chaque élément sur chaque colonne
  # score_to_pref()
  ranks <- apply(preferences, 2,rank_with_ties <- function(x) {rank(x, ties.method = "random")})
  # Inverser les rangs pour que le candidat le plus préféré soit le numéro 1
  ranks <- nrow(ranks) + 1 - ranks
  # Retourner les rangs
  return(ranks)
}

#' preferences_to_borda_points
#' @export
#' @param preferences voters preferences
#' @returns points
preferences_to_borda_points <- function(preferences) {
  # Calculer les rangs de chaque élément sur chaque colonne
  points <- apply(preferences, 2,rank_with_ties <- function(x) {rank(x, ties.method = "random")})
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
#' @param preference_matrix voters preferences
#' @return duel_matrix
make_duel_matrix <- function(preference_matrix) {
  n_candidates <- nrow(preference_matrix)
  n_voters <- ncol(preference_matrix)
  preference_matrix <- preferences_to_ranks(preference_matrix)
  print(preference_matrix)
  # Calcule la matrice des duels :
  duel_matrix <- matrix(0, n_candidates,n_candidates)
  candidates_names <- rownames(preference_matrix)
  colnames(duel_matrix) <- candidates_names
  rownames(duel_matrix) <- candidates_names
  # for (i in 1:n_candidates) {
  #   for (j in 1:n_candidates) {
  #     if (i != j) {
  #       count_i_j <- sum(preference_matrix[i, ] < preference_matrix[j, ])
  #       duel_matrix[i, j] <- ifelse(count_i_j > n_voters/2, 1, 0)
  #     }
  #   }
  # }
  for (i in 1:(n_candidates - 1)) {
    for (j in (i + 1):n_candidates) {
      win_i_j <- sum(preference_matrix[i,] < preference_matrix[j,])
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





