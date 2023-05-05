# USE :
#   library(devtools)
#   install_github("Naghan1132/voteSim")
#   library(voteSim)
#   uninominal_vote(generate_beta(10,3))

# ==== ==== ==== ==== ==== ==== ==== ==== ==== ====

#' Uninominal vote
#' @export
#' @param situation voters preferences
#' @param n_round int
#' @returns winner_idx
uninominal_vote <- function(situation, n_round = 1) {
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

  if(n_round == 1) {
    # Vote uninominal à un tour : trouver le candidat avec le plus de voix et retourner son indice
    winner_idx <- which(vote_counts == max(vote_counts)) # pour gérer les égalités
    return(winner_idx)
  } else if(n_round == 2) {
    # Vote uninominal à deux tours
    winner_idx <- which.max(vote_counts)
    if(vote_counts[winner_idx] / n_voters < 0.5) {
      # Si aucun candidat n'a la majorité absolue,on passe au second tour
      top2_indices <- order(vote_counts, decreasing = TRUE)[1:2]
      situation2 <- situation[top2_indices,]
      winner_idx2 <- uninominal_vote(situation2, n_round = 1)
      return(winner_idx2)
    } else {
      # Sinon, le premier candidat est élu
      return(winner_idx)
    }
  } else {
    stop("Number of rounds must be between 1 or 2")
    }
}


#' Approbal vote
#' @export
#' @param situation voters preferences
#' @returns winner
approbal_vote <- function(situation) {
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
