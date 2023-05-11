#' Preferences_to_ranks
#' @export
#' @param preferences voters preferences
#' @returns ranks
preferences_to_ranks <- function(preferences) {
  # Calculer les rangs de chaque élément sur chaque colonne
  ranks <- apply(preferences, 2, rank)
  # Inverser les rangs pour que le candidat le plus préféré soit le numéro 1
  ranks <- nrow(ranks) + 1 - ranks
  # Retourner les rangs
  return(ranks)
}

#' preferences_to_borda_points
#' @export
#' @param preferences voters preferences
#' @returns ranks
preferences_to_borda_points <- function(preferences) {
  # Calculer les rangs de chaque élément sur chaque colonne
  ranks <- apply(preferences, 2, rank)
  # Retourner les rangs
  return(ranks)
}

#' preferences_to_points
#' @export
#' @param preferences voters preferences
#' @returns ranks
preferences_to_points<- function(preferences){
  # on ne prends pas en compte les zéro !!
}

#' points_to_pref
#' @export
#' @param points_matrix voters preferences
#' @returns preferences_matrix
points_to_preferences <- function(points_matrix) {
  num_voters <- ncol(points_matrix)
  num_candidates <- nrow(points_matrix)
  preferences_matrix <- matrix(0, nrow = num_candidates, ncol = num_voters)

  for (i in 1:num_voters) {
    voter_points <- points_matrix[,i]
    voter_ranks <- order(voter_points, decreasing = TRUE)
    preferences_matrix[voter_ranks, i] <- 1:num_candidates
  }

  View(preferences_matrix)

  return(preferences_matrix)
}

#' reallocate_preferences
#' @export
#' @param pref_matrix voters preferences
#' @param eliminated_candidate eliminated index
#' @returns reallocated_prefs
reallocate_preferences <- function(pref_matrix, eliminated_candidates) {
  # éliminer plusieurs candidats en même temps
  num_voters <- ncol(pref_matrix)
  num_candidates <- nrow(pref_matrix)
  reallocated_prefs <- matrix(0, nrow = num_candidates, ncol = num_voters)
  candidate_indices <- setdiff(1:num_candidates, eliminated_candidates)
  for (i in 1:num_voters) {
    voter_prefs <- pref_matrix[,i]
    shifted_ranks <- voter_prefs
    for (e in eliminated_candidates) {
      shifted_ranks[voter_prefs > voter_prefs[e]] <- shifted_ranks[voter_prefs > voter_prefs[e]] - 1
    }
    shifted_ranks[eliminated_candidates] <- 0
    reallocated_prefs[candidate_indices, i] <- shifted_ranks[candidate_indices]
  }
  return(reallocated_prefs)
}

