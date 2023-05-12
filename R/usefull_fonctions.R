#' Rename_rows
#' @export
#' @param preferences voters preferences
#' @returns preferences
rename_rows <- function(preferences) {
  n_candidates <- nrow(preferences)
  n_voters <- ncol(preferences)
  rownames(preferences) <- paste0("Candidate ", seq_len(n_candidates))
  return(preferences)
}
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

#' points_to_preferences
#' @export
#' @param points_matrix voters preferences
#' @returns preferences_matrix
points_to_preferences <- function(points_matrix,eliminated_candidates) {
  num_voters <- ncol(points_matrix)
  num_candidates <- nrow(points_matrix)
  row_names <- rownames(points_matrix)
  preferences_matrix <- matrix(0, nrow = num_candidates, ncol = num_voters,dimnames = list(row_names, NULL))
  candidate_indices <- setdiff(row_names, eliminated_candidates)
  print("debut fonction : ")
  print(points_matrix)
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

  print("fin fonction : ")
  print(matrice_sans_zero)
  return(matrice_sans_zero)
}

#' reallocate_preferences
#' @export
#' @param pref_matrix voters preferences
#' @param eliminated_candidate eliminated index
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
  print("reallocate matrix : ")
  print(matrice_sans_zero)
  return(matrice_sans_zero)
}


