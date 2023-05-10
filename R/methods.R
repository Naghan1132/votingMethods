# USE :
#   library(devtools)
#   install_github("Naghan1132/voteSim")
#   library(voteSim)
#   uninominal_vote(generate_beta(10,3))


# Uninomial OK
# Approbation OK
# Borda OK
# Elination successive KO
# Condorcet KO
# Copeland OK
# Minimax OK


# Matrice de préférences
#preferences <- matrix(c(1, 3, 1, 2, 1, 3, 2, 2, 3, 1, 3, 2, 3, 1, 2, 2, 3, 2, 1, 1), nrow = 3, ncol = 5, byrow = TRUE)
#rownames(preferences) <- c("Candidat 1", "Candidat 2", "Candidat 3")
#colnames(preferences) <- paste0("V", 1:ncol(preferences))


# ==== ==== ==== ==== ==== ==== ==== ==== ==== ====

#' Uninominal vote
#' @export
#' @param situation voters preferences
#' @param n_round int
#' @returns winner_idx
uninominal <- function(situation, n_round = 1) {
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
    stop("Number of rounds must be 1 or 2")
    }
}


#' Approbal vote
#' @export
#' @param situation voters preferences
#' @returns winner
approbal <- function(situation) {
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
borda <- function(situation) {
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
condorcet <- function(preference_matrix) {
  n <- nrow(preference_matrix) # nombre de candidats
  preference_matrix <- preferences_to_ranks(preference_matrix)
  wins <- rep(0, n) # initialise le vecteur de victoires
  View(preference_matrix)
  # Pour chaque paire de candidats, compte le nombre de victoires en tête-à-tête
  for (i in 1:(n)) {
    for (j in 1:(n)){
      if(i!=j){
      countWin <- sum(preference_matrix[i,] < preference_matrix[j,])
      # test de la majorité
      if (countWin > n/2) {
        wins[i] <- wins[i] + 1
      } else if (countWin < n/2) {
        wins[j] <- wins[j] + 1
      }
    }
   }
  }
  View(wins)
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


#' CondorcetV2
#' @export
#' @param preference_matrix voters preferences
#' @return winner, can be NULL
condorcetV2 <- function(preference_matrix) {
  n <- nrow(preference_matrix)
  preference_matrix <- preferences_to_ranks(preference_matrix)
  View(preference_matrix)
  # Calcule la matrice des duels
  duel_matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        count <- sum(preference_matrix[i,] < preference_matrix[j,])
        if (count > n/2) {
          duel_matrix[i,j] <- duel_matrix[i, j] + 1
        } else if (count < n/2) {
          duel_matrix[j, i] <- duel_matrix[j, i] + 1
        }
      }
    }
  }
  View(duel_matrix)
  # Vérifie s'il y a un vainqueur de Condorcet
  row_sums <- rowSums(duel_matrix)
  col_sums <- colSums(duel_matrix)
  condorcet_winner <- NULL
  if (any(row_sums == n-1)) {
    condorcet_winner <- which.max(row_sums)
  } else if (any(col_sums == n-1)) {
    condorcet_winner <- which.max(col_sums)
  }
  # Renvoie le vainqueur de Condorcet ou NULL s'il n'y en a pas
  return(condorcet_winner)
}


#' Copeland procedure
#' @export
#' @param preference_matrix voters preferences
#' @return winner, can be NULL
copeland <- function(preference_matrix) {
  n <- nrow(preference_matrix)
  preference_matrix <- preferences_to_ranks(preference_matrix)
  scores <- rep(0, n)
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      wins_i <- sum(preference_matrix[i,] < preference_matrix[j,])
      wins_j <- sum(preference_matrix[i,] > preference_matrix[j,])
      if (wins_i == wins_j) {
        scores[i] <- scores[i] + 0.5
        scores[j] <- scores[j] + 0.5
      } else if (wins_i > wins_j) {
        scores[i] <- scores[i] + 1
      } else {
        scores[j] <- scores[j] + 1
      }
    }
  }

  winners <- which(scores == max(scores))
  if (length(winners) == 1) {
    return(winners)
  } else {
    return(NULL)
  }
}


#' Minimax procedure
#' @export
#' @param preference_matrix voters preferences
#' @return winner, can be NULL
minimax <- function(preference_matrix) {
  n <- nrow(preference_matrix)
  preference_matrix <- preferences_to_ranks(preference_matrix)
  # Calcule les distances entre chaque paire de candidats - matrice de duels
  distances <- matrix(0, n, n)
  View(preference_matrix)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        distances[i,j] <- sum(preference_matrix[i,] < preference_matrix[j,])
      }
    }
  }
  # Calcule le risque maximum pour chaque candidat
  risks <- apply(distances, 1, max)
  View(distances)
  # Trouve le candidat avec le risque maximum le plus faible
  winner <- which.min(risks)
  # Renvoie le candidat élu
  return(winner)
}


#' Succesif elimination test
#' @export
#' @param pref_matrix voters preferences
#' @returns remaining_candidates
find_winner <- function(pref_matrix) {
  pref_matrix <- preferences_to_ranks(pref_matrix) # ok
  num_candidates <- nrow(pref_matrix) # ok
  remaining_candidates <- 1:num_candidates # ok
  View(pref_matrix)
  while (length(remaining_candidates) > 1) {
    # Calculer le total de voix pour chaque candidat restant
    candidate_votes <- rep(0, num_candidates)
    for (i in remaining_candidates) {
      candidate_votes[i] <- sum(pref_matrix[i,] == 1) # on choisit le candidat pref de chaque votant
    }
    egalite <- unique(candidate_votes[remaining_candidates])
    # Vérifier si la liste d'entiers contient une seule valeur unique
    longueurUnique <- length(egalite)
    if(longueurUnique == 1){
      #print("ÉGALITÉ")
      print(candidate_votes)
      return(remaining_candidates)
    }
    # Éliminer le candidat ayant le moins de voix
    min_votes <- min(candidate_votes[remaining_candidates]) # min(candidate_votes) ???
    eliminated_candidates <- which(candidate_votes == min_votes)
    if(length(eliminated_candidates) > 1){
      eliminated_candidates <- sample(length(eliminated_candidates),1)
    }
    remaining_candidates <- setdiff(remaining_candidates, eliminated_candidates)

    # Redistribuer les préférences des votants pour prendre en compte l'élimination du candidat
    pref_matrix <- reallocate_preferences(pref_matrix,eliminated_candidates)
  }
  return(remaining_candidates)
}



