#' Preferences_to_ranks
#' @export
#' @param preferences voters preferences
#' @returns ranks
preferences_to_ranks <- function(preferences) {
  # Calculer les rangs de chaque élément sur chaque colonne
  ranks <- apply(preferences, 2, rank) # score_to_pref() à mettre random (rank) !!!
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
  points <- apply(preferences, 2,rank)
  # Retourner les rangs
  return(points)
}

#' preferences_to_points
#' @export
#' @param preferences voters preferences
#' @returns ranks
preferences_to_points<- function(preferences){
  # on ne prends pas en compte les zéro !!
}

#' reallocate_points
#' @export
#' @param points_matrix voters preferences
#' @param eliminated_candidates eliminated_candidates
#' @returns preferences_matrix
reallocate_points <- function(points_matrix,eliminated_candidates) {
  num_voters <- ncol(points_matrix)
  num_candidates <- nrow(points_matrix)
  row_names <- rownames(points_matrix)
  preferences_matrix <- matrix(0, nrow = num_candidates, ncol = num_voters,dimnames = list(row_names, NULL))
  candidate_indices <- setdiff(row_names, eliminated_candidates)
  for (i in 1:num_voters) {
    voter_prefs <- points_matrix[,i]
    shifted_ranks <- voter_prefs
    for (e in eliminated_candidates) {
      #on baisse les points
      shifted_ranks[voter_prefs > voter_prefs[e]] <- shifted_ranks[voter_prefs > voter_prefs[e]] - 1
    }
    shifted_ranks[eliminated_candidates] <- 0
    preferences_matrix[candidate_indices, i] <- shifted_ranks[candidate_indices]
  }
  eliminated_indices <- which(row_names %in% eliminated_candidates)
  matrice_sans_zero <- preferences_matrix[-eliminated_indices, ]
  print(matrice_sans_zero) # OK
  return(matrice_sans_zero)
}

#' reallocate_preferences
#' @export
#' @param pref_matrix voters preferences
#' @param eliminated_candidates eliminated candidates
#' @returns matrice_sans_zero
reallocate_preferences <- function(pref_matrix, eliminated_candidates) {
  num_voters <- ncol(pref_matrix)
  num_candidates <- nrow(pref_matrix)
  row_names <- rownames(pref_matrix)
  reallocated_prefs <- matrix(0, nrow = num_candidates, ncol = num_voters,dimnames = list(row_names, NULL))
  candidate_indices <- setdiff(row_names, eliminated_candidates)
  for (i in 1:num_voters) {
    voter_prefs <- pref_matrix[,i]
    shifted_ranks <- voter_prefs
    for (e in eliminated_candidates) {
      shifted_ranks[voter_prefs > voter_prefs[e]] <- shifted_ranks[voter_prefs > voter_prefs[e]] - 1
    }
    shifted_ranks[eliminated_candidates] <- 0
    reallocated_prefs[candidate_indices, i] <- shifted_ranks[candidate_indices]
  }
  eliminated_indices <- which(row_names %in% eliminated_candidates)
  matrice_sans_zero <- reallocated_prefs[-eliminated_indices, ]
  print(matrice_sans_zero)
  return(matrice_sans_zero)
}

#' draw_test
#' @export
#' @param pref_matrix voters preferences
#' @param remaining_candidates remaining candidates
#' @returns draw - false/true
draw_test <- function(pref_matrix,remaining_candidates){
  print(length(remaining_candidates))
  if(length(remaining_candidates) != 1){
    candidate_votes <- rowSums(pref_matrix)
    # Vérification si les sommes des lignes sont toutes égales
    draw <- length(unique(candidate_votes)) == 1
  }else{
    return(TRUE)
  }
  return(draw)
}

duel_matrix <- function(situation){
  n <- nrow(situation)
  matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {

      }
    }
  }
  return(NULL)
}




