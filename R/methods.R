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
  # Calcule le nombre d'approbations pour chaque candidat (ligne)
  approbations <- apply(situation, 1, function(x) sum(x > 0.5)) # 0.5 : à changer peut-être
  # Retourne le(s) candidat(s) ayant obtenu le plus grand nombre d'approbations
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
  situation <- preferences_to_points(situation)
  # Calculer le total de chaque ligne
  totaux <- rowSums(situation)
  print(totaux)
  # Retourner tous les indices des lignes ayant un total maximal
  winner <- which(totaux == max(totaux))
  return(winner)
}

# vote par Élimination successive
#' Succesif elimination
#' @export
#' @param situation voters preferences
#' @returns remaining
vote_elimination <- function(situation) {
n_candidates <- nrow(situation)
n_voters <- ncol(situation)
eliminated <- c() # Vecteur des candidats éliminés
remaining <- 1:n_candidates # Vecteur des candidats restants

# Boucle jusqu'à ce qu'il ne reste qu'un candidat ou une égalité
while (length(remaining) > 1) {
  # Calculer le nombre de voix pour chaque candidat restant
  vote_counts <- rep(0, n_candidates)
  for (i in remaining) {
    vote_counts[i] <- sum(situation[i, ])
  }
  print(vote_counts)

  # Trouver le candidat avec le moins de voix
  loser_idx <- which.min(vote_counts[remaining])
  print("indice perdant : ",loser_idx)

  # Ajouter le candidat éliminé à la liste
  eliminated <- c(eliminated, remaining[loser_idx])

  # Retirer le candidat éliminé de la liste des candidats restants
  remaining <- remaining[-loser_idx]
  print("restants  :",remaining)

  # Répartir les voix des partisans du candidat éliminé
  for (j in 1:n_voters) {
    # Trouver le candidat préféré du votant j parmi les candidats restants
    fav_candidate <- which.max(situation[remaining, j])

    # Ajouter une voix pour le candidat préféré du votant j
    vote_counts[fav_candidate] <- vote_counts[fav_candidate] + 1
  }

  # Vérifier s'il y a une égalité entre les deux candidats restants
  if (length(remaining) == 2 && vote_counts[remaining[1]] == vote_counts[remaining[2]]) {
    return(remaining) # Les deux candidats sont à égalité
  }
}

# Il ne reste qu'un candidat, retourner son indice
return(remaining)
}

#' Condorcet
#' @export
#' @param preference_matrix voters preferences
#' @return winner, can be NULL
condorcet <- function(preference_matrix) {
  n <- nrow(preference_matrix) # nombre de candidats
  preference_matrix <- preferences_to_ranks(preference_matrix)
  wins <- rep(0, n) # initialise le vecteur de victoires

  # Pour chaque paire de candidats, compte le nombre de victoires en tête-à-tête
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      count <- sum(preference_matrix[i,] > preference_matrix[j,])
      # test de la majorité
      if (count > n/2) {
        wins[i] <- wins[i] + 1
      } else if (count < n/2) {
        wins[j] <- wins[j] + 1
      }
    }
  }

  # Trouve le candidat avec le plus de victoires
  max_wins <- max(wins)
  if (max_wins == 0) {
    # Il n'y a pas de vainqueur de Condorcet
    return(NULL)
  } else if (sum(wins == max_wins) > 1) {
    # Il y a une égalité de vainqueurs de Condorcet
    return(NULL)
  } else {
    # winner
    return(which.max(wins))
  }
}




