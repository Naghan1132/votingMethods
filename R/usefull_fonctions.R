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

#' Preferences_to_points
#' @export
#' @param preferences voters preferences
#' @returns ranks
preferences_to_points <- function(preferences) {
  # Calculer les rangs de chaque élément sur chaque colonne
  ranks <- apply(preferences, 2, rank)
  # Retourner les rangs
  return(ranks)
}

#' reallocate_preferences
#' @export
#' @param pref_matrix voters preferences
#' @param eliminated_candidate eliminated index
#' @returns reallocated_prefs
reallocate_preferences <- function(pref_matrix, eliminated_candidate) {
  num_voters <- ncol(pref_matrix)
  num_candidates <- nrow(pref_matrix)
  reallocated_prefs <- matrix(0, nrow = num_candidates, ncol = num_voters)
  for (i in 1:num_voters) {
    voter_prefs <- pref_matrix[,i]
    shifted_ranks <- voter_prefs
    shifted_ranks[voter_prefs > voter_prefs[eliminated_candidate]] <- shifted_ranks[voter_prefs > voter_prefs[eliminated_candidate]] - 1
    shifted_ranks[eliminated_candidate] <- 0
    reallocated_prefs[,i] <- shifted_ranks
  }
  print(reallocated_prefs)
  return(reallocated_prefs)
}





