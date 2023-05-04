# library(voteSim)
# situation <- generate_beta(100,5)
# scrutin <- function(nVoter,nCandidat,situation) {
#
#     le paramètre "situation" est le résultat d'une fonction du package "voteSim"
#     pas besoin de nVoter et nCandidat, juste à récupérer le nombre de ligne/col
#     dans le tableau situation (si elle retourne bien un tableau)
#
# }


# USE :
#   library(devtools)
#   install_github("Naghan1132/voteSim")
#   library(voteSim)
#   uninominal_vote(generate_beta(10,3))




#' Uninominal vote
#' @export
#' @param situation voters preferences
#' @returns winner_idx
uninominal_vote <- function(situation) {
  #situation <- matrix(runif(30, min = 0, max = 1), nrow = 3, ncol = 10)

  # Calculer le nombre de candidats et de votants
  n_candidates <- nrow(situation)
  n_voters <- ncol(situation)

  # Initialiser le vecteur de voix pour chaque candidat
  vote_counts <- rep(0, n_candidates)

  # Pour chaque votant, trouver le candidat préféré et ajouter une voix pour ce candidat
  for (i in 1:n_voters) {
    # Trouver l'indice du candidat préféré du votant i sur la colonne i
    fav_candidate <- which.max(situation[, i])

    # Ajouter une voix pour le candidat préféré du votant i
    vote_counts[fav_candidate] <- vote_counts[fav_candidate] + 1
  }

  # Trouver le candidat avec le plus de voix
  winner_idx <- which.max(vote_counts)

  # Retourner l'indice du candidat gagnant
  return(winner_idx)
}


#' Approbal vote
#' @export
#' @param situation voters preferences
#' @returns winner
approbal_vote <- function(situation) {
  # !!! Gérer les EGALITÉS ?!
  # Calcule le nombre d'approbations pour chaque candidat
  approbations <- apply(situation, 1, function(x) sum(x > 0.5)) # TEST : à changer peut-être
  # Retourne les candidats ayant obtenu le plus grand nombre d'approbations
  winner <- which(approbations == max(approbations))
  return(winner)
}

#' Borda method
#' @param situation voters preferences
#' @returns winner
#' @export
borda_method <- function(situation) {
  n_candidats <- nrow(situation)
  n_voters <- ncol(situation)
  # Remplacer les valeurs de chaque colonne selon la méthode de Borda
  for (i in 1:n_voters) {
    vec_col <- situation[,i]
    vec_col_ordonne <- sort(vec_col, decreasing = TRUE)
    for (j in 1:n_candidats) {
      vec_col[vec_col == vec_col_ordonne[j]] <- n_candidats - j + 1
    }
    situation[,i] <- vec_col
  }
  # Calculer le total de chaque ligne
  totaux <- rowSums(situation)
  print(totaux)
  # Retourner tous les indices des lignes ayant un total maximal
  winner <- which(totaux == max(totaux))
  return(winner)
}




