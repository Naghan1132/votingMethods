# USE :
#   library(devtools)
#   install_github("Naghan1132/voteSim")
#   library(voteSim)
#   uninominal_vote(generate_beta(10,3))

# ==== Vote ordre de préférences ====
# Uninomial 1T OK -- Refonte OK
# Uninomial 2T OK -- Refonte OK
# Elination successive OK -- Refonte OK
# Bucklin OK -- Refonte OK
# Borda OK -- Refonte OK
# Nanson OK -- Refonte OK
# Minimax (à vérifier)
# Copeland OK -- Refonte OK
# Kemeny ?

# ==== Vote par évaluation ====
# Vote à la moyenne ?
# Jugement Majoritaire ?
# Approbation OK -- Refonte OK


# Condorcet (à revoir)


# ==== ==== ==== ==== ==== ==== ==== ==== ==== ====

#' Uninominal vote
#' @export
#' @param situation voters preferences
#' @param n_round int
#' @returns winner_idx
uninominal <- function(situation, n_round = 1) {
  situation <- rename_rows(situation)
  # Calculer le nombre de candidats et de votants
  n_candidates <- nrow(situation)
  n_voters <- ncol(situation)
  # Initialiser le vecteur de voix pour chaque candidat
  candidates_names <- rownames(situation)
  vote_counts <- rep(0, length(candidates_names))
  names(vote_counts) <- candidates_names

  # Pour chaque votant, trouver le candidat préféré et ajouter une voix pour ce candidat
  for (i in 1:n_voters) {
    # Trouver l'indice du candidat préféré du votant i sur la colonne i
    fav_candidate <- which.max(situation[, i])
    # Ajouter une voix pour le candidat préféré du votant i
    vote_counts[fav_candidate] <- vote_counts[fav_candidate] + 1
  }
  print(vote_counts)
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
      winner_idx2 <- uninominal(situation2, n_round = 1)
      return(winner_idx2)
    } else {
      # Sinon, le premier candidat est élu
      return(winner_idx)
    }
  } else {
    stop("Number of rounds must be 1 or 2")
    }
}


#' Borda method
#' @param situation voters preferences
#' @returns winner
#' @export
borda <- function(situation) {
  situation <- rename_rows(situation)
  situation <- preferences_to_borda_points(situation)
  # Calculer le total de chaque ligne
  totaux <- rowSums(situation)
  print(totaux)
  # Retourner tous les indices des lignes ayant un total maximal
  winner <- which(totaux == max(totaux))
  return(winner)
}


#' Condorcet
#' @export
#' @param preference_matrix voters preferences
condorcet <- function(preference_matrix) {
  n <- nrow(preference_matrix) # nombre de candidats
  preference_matrix <- preferences_to_ranks(preference_matrix)
  wins <- rep(0, n) # initialise le vecteur de victoires
  #(preference_matrix)
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
  ###View(wins)
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
  ###View(preference_matrix)
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
  ##View(duel_matrix)
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
  preference_matrix <- rename_rows(preference_matrix)
  candidates_names <- rownames(preference_matrix)
  scores <- rep(0, length(candidates_names))
  names(scores) <- candidates_names
  print(scores)
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
  print(scores)

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
  preference_matrix <- rename_rows(preference_matrix)
  candidates_names <- rownames(preference_matrix)
  # Calcule les distances entre chaque paire de candidats - matrice de duels
  distances <- matrix(0, n, n)
  ##View(preference_matrix)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        distances[i,j] <- sum(preference_matrix[i,] < preference_matrix[j,])
      }
    }
  }
  # Calcule le risque maximum pour chaque candidat
  risks <- apply(distances, 1, max)
  ##View(distances)
  ##View(risks)
  # Trouve le candidat avec le risque maximum le plus faible
  winner <- which.min(risks)
  # Renvoie le candidat élu
  return(candidates_names[winner])
}


#' Succesif elimination
#' @export
#' @param pref_matrix voters preferences
#' @returns remaining_candidates
successif_elimination <- function(pref_matrix) {
  pref_matrix <- preferences_to_ranks(pref_matrix)
  pref_matrix <- rename_rows(pref_matrix)
  num_candidates <- nrow(pref_matrix)
  remaining_candidates <- rownames(pref_matrix)
  print(pref_matrix)
  while (length(remaining_candidates) > 1) {
    # Calculer le total de voix pour chaque candidat restant
    candidate_votes <- rep(0, length(remaining_candidates))
    names(candidate_votes) <- remaining_candidates
    for (i in remaining_candidates) {
      candidate_votes[i] <- sum(pref_matrix[i,] == 1) # on choisit le candidat pref de chaque votant
    }
    egalite <- unique(candidate_votes[remaining_candidates])
    # Vérifier si la liste d'entiers contient une seule valeur unique
    longueurUnique <- length(egalite)
    if(longueurUnique == 1){
      # == ÉGALITÉ ==
      return(remaining_candidates)
    }
    # Éliminer le(s) candidat(s) ayant le moins de voix
    min_votes <- min(candidate_votes[remaining_candidates])
    eliminated_indices <- which(candidate_votes == min_votes)
    eliminated_candidates <- rownames(pref_matrix)[eliminated_indices]
    remaining_candidates <- setdiff(remaining_candidates, eliminated_candidates)

    # Redistribuer les préférences des votants pour prendre en compte l'élimination du candidat
    pref_matrix <- reallocate_preferences(pref_matrix,eliminated_candidates)
  }
  return(remaining_candidates)
}

#' Bulkin method
#' @export
#' @param pref_matrix voters preferences
#' @returns winner
bucklin <- function(pref_matrix) {
  pref_matrix <- preferences_to_ranks(pref_matrix)
  pref_matrix <- rename_rows(pref_matrix)
  num_voters <- ncol(pref_matrix)
  num_candidates <- nrow(pref_matrix)
  remaining_candidates <- rownames(pref_matrix)
  candidate_votes <- rep(0, length(remaining_candidates))
  names(candidate_votes) <- remaining_candidates

  winner <- FALSE
  n_round <- 1
  majority_threshold <- ceiling(num_voters / 2)
  print("Majorite : ")
  print(majority_threshold)
  while(!winner) {
    print("Round : ")
    print(n_round)
    # Compter le nombre de votes pour chaque candidat
    for (i in 1:num_candidates) {
      candidate_votes[i] <- candidate_votes[i] + sum(pref_matrix[i,] == n_round)
    }
    print("votes : ")
    print(candidate_votes)

    # Trouver les candidats ayant obtenu une majorité
    majority_candidates <- which(candidate_votes > majority_threshold)
    print("candidats majoritaires :")
    print(majority_candidates)

    if (length(majority_candidates) > 0) {
      # S'il y a un seul candidat avec une majorité, il est élu
      if (length(majority_candidates) == 1) {
        winner <- majority_candidates
        return(winner)
      } else {
        # Sinon, trouver le candidat avec la plus grande majorité
        max_vote <- max(candidate_votes[majority_candidates])
        max_candidates <- which(candidate_votes == max_vote)
        # peut y avoir égalité parfaite
        winner <- max_candidates
        return(winner)
      }
    }
    n_round <- n_round +1
  }
  return(winner)
}

#' Nanson method
#' @export
#' @param pref_matrix voters preferences
#' @returns remaining_candidates
nanson <- function(pref_matrix) {
  n_candidats <- nrow(pref_matrix)
  n_voters <- ncol(pref_matrix)
  ##View(pref_matrix)
  pref_matrix <- preferences_to_borda_points(pref_matrix)
  pref_matrix <- rename_rows(pref_matrix)
  remaining_candidates <- rownames(pref_matrix)
  draw <- FALSE

  # tester si vainqueur de Condorcet ! sinon procédure =>

  while(length(remaining_candidates) > 2 | !draw){
    candidate_votes <- rep(0, length(remaining_candidates))
    names(candidate_votes) <- remaining_candidates
    # BORDA :
    candidate_votes <- rowSums(pref_matrix)
    print("somme : ")
    print(candidate_votes)
    mean <- sum(candidate_votes/length(remaining_candidates))
    print("moyenne :")
    print(mean)
    # Éliminations :
    below_mean <- min(candidate_votes[remaining_candidates])
    eliminated_indices <- which(candidate_votes < mean)
    eliminated_candidates <- rownames(pref_matrix)[eliminated_indices]
    remaining_candidates <- setdiff(remaining_candidates, eliminated_candidates)
    pref_matrix <- reallocate_points(pref_matrix,eliminated_candidates)

    print("remainning : ")
    print(remaining_candidates)

    # test égalité :
    draw <- draw_test(pref_matrix,remaining_candidates)
  }
  # =====
  return(remaining_candidates)
}


#' kemeny method
#' @export
#' @param pref_matrix voters preferences
#' @returns remaining_candidates
kemeny <- function(pref_matrix) {
  # WARNING :
  # il y a 3 628 800 ordres
  # possibles pour 10 candidats et plus de 2 milliards de milliards pour 20
  # candidats !
  # ordres de pref inversé, calcul nombre inversions
  pref_matrix <- preferences_to_ranks(pref_matrix)
}


# ==== VOTE PAR ÉVALUATION ====



# placer les votes des évaluations avec d'autres fonction de génération ?? comme ça pas de seuil à avoir

#' Approbal vote
#' @export
#' @param situation voters preferences
#' @param mode n_approbation mode
#' @returns winner
approbal <- function(situation, mode = "seuil") {
  situation <- rename_rows(situation)
  n_candidate <- nrow(situation)
  n_voter <- ncol(situation)
  candidate_votes <- rep(0, n_candidate)
  names(candidate_votes) <- rownames(situation)
  # Calcule le nombre d'approbations pour chaque candidat (ligne)
  if(mode == "fixe"){
    if(n_candidate == 2){
      n_appro <- 1
    }else if(n_candidate < 5){
      n_appro <- round(n_candidate/2)
    }else if(n_candidate < 10){
      n_appro <- round(n_candidate/2 -1)
    }else{
      n_appro <- round(n_candidate/2 - 2)
    }
    for(i in 1:n_voter){
      indices_lignes <- tail(order(situation[,i]), n_appro)
      candidate_votes[indices_lignes] <- candidate_votes[indices_lignes] + 1
    }
    print(candidate_votes)
  }else if(mode == "poisson"){
    lambda <- n_candidate/2 # moyenne & variance de la loi
    poisson_values <- rpois(n_voter, lambda)
    bounded_poisson_values <- pmax(pmin(poisson_values, n_candidate - 1), 1) # n_appro de 1 à length(n_candidat-1)
    hist(bounded_poisson_values)
    for(i in 1:n_voter){
      indices_lignes <- tail(order(situation[,i]), bounded_poisson_values[i])
      candidate_votes[indices_lignes] <- candidate_votes[indices_lignes] + 1
    }
    print(candidate_votes)
  }else{
    # approbation pour tout ceux dont la préférences > 0.5
    candidate_votes <- apply(situation, 1, function(x) sum(x > 0.5))
    print(candidate_votes)
  }
  # Retourne le(s) candidat(s) ayant obtenu le plus grand nombre d'approbations
  winner <- which(candidate_votes == max(candidate_votes))
  return(winner)
}


