# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# faire qql chose de la manière (pour le package méthode de vote :

# library(voteSim)
# situation <- generate_beta(100,5)
# scrutin <- function(nVoter,nCandidat,situation) {
#
#     le paramètre "situation" est le résultat d'une fonction du package "voteSim"
#     pas besoin de nVoter et nCandidat, juste à récupérer le nombre de ligne/col
#     dans le tableau situation (si elle retourne bien un tableau)
#
# }




#' Uninominal vote
#' @export
#' @returns winner_idx
uninominal_vote <- function() {
  pref_matrix <- matrix(runif(30, min = 0, max = 1), nrow = 3, ncol = 10)
  # Calculer le nombre de candidats et de votants
  n_candidates <- nrow(pref_matrix)
  n_voters <- ncol(pref_matrix)

  View(pref_matrix)

  # Initialiser le vecteur de voix pour chaque candidat
  vote_counts <- rep(0, n_candidates)

  # Pour chaque votant, trouver le candidat préféré et ajouter une voix pour ce candidat
  for (i in 1:n_voters) {
    # Trouver l'indice du candidat préféré du votant i sur la colonne i
    fav_candidate <- which.max(pref_matrix[, i])
    print("candidat fav :",fav_candidate)
    print(pref_matrix[, i])
    print(which.max(pref_matrix[, i]))

    # Ajouter une voix pour le candidat préféré du votant i
    vote_counts[fav_candidate] <- vote_counts[fav_candidate] + 1
  }
  View(vote_counts)

  # Trouver le candidat avec le plus de voix
  winner_idx <- which.max(vote_counts)

  # Retourner l'indice du candidat gagnant
  return(winner_idx)
}
