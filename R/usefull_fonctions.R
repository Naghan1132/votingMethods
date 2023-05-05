#' Preferences_to_ranks
#' @export
#' @param pref voters preferences
#' @returns ranks
preferences_to_ranks <- function(preferences) {
  print(preferences)
  # Calculer les rangs de chaque élément sur chaque colonne
  ranks <- apply(preferences, 2, rank)
  # Inverser les rangs pour que le candidat le plus préféré soit le numéro 1
  ranks <- nrow(ranks) + 1 - ranks
  # Retourner les rangs
  return(ranks)
}
