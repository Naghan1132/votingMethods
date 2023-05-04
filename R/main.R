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
  pref_matrix <- matrix(runif(250, min = 0, max = 1), nrow = 5, ncol = 50)
  View(pref_matrix)

  nb_candidates <- nrow(pref_matrix)
  nb_voters <- ncol(pref_matrix)

  # Calculer le nombre de voix pour chaque candidat
  votes_count <- sapply(1:nb_candidates, function(cand_idx) {
    sum(pref_matrix[cand_idx,] == max(pref_matrix[,cand_idx]))
  })

  # Trouver l'index du candidat avec le plus grand nombre de voix
  winner_idx <- which(votes_count == max(votes_count))

  # Retourner le(s) gagnant(s) sous forme de vecteur d'index
  cat("Le(s) gagnant(s) est/sont le(s) candidat(s) numéro :", winner_idx, "\n")
  return(winner_idx)
}
