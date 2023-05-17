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
# Minimax OK -- Refonte OK
# Copeland OK -- Refonte OK
# Kemeny ? (pas utile)

# ==== Vote par évaluation ====
# Vote à la moyenne OK
# Jugement Majoritaire ?
# Approbation OK -- Refonte OK


# Condorcet OK


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
#' @return winner, can be NULL
condorcet <- function(preference_matrix) {
  n <- nrow(preference_matrix)
  preference_matrix <- preferences_to_ranks(preference_matrix)
  print(preference_matrix)
  # Calcule la matrice des duels
  duel_matrix <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        #count <- sum(preference_matrix[i,] < preference_matrix[j,])
        count_i_j <- sum(preference_matrix[i, ] < preference_matrix[j, ])
        #print(count_i_j)
        #print(" < ")
        count_j_i <- sum(preference_matrix[i, ] > preference_matrix[j, ])
        #print(count_j_i)
        if (count_i_j > count_j_i) {
          duel_matrix[i, j] <- 1
        }
        else if (count_i_j < count_j_i) {
          duel_matrix[j, i] <- 1
        }
      }
    }
  }
  print(duel_matrix)
  # Vérifie s'il y a un vainqueur de Condorcet
  row_sums <- rowSums(duel_matrix)
  condorcet_winner <- NULL
  if (any(row_sums == n-1)) {
    condorcet_winner <- which(row_sums == max(row_sums))
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
  n_voter <- ncol(preference_matrix)
  preference_matrix <- preferences_to_ranks(preference_matrix)
  preference_matrix <- rename_rows(preference_matrix)
  candidates_names <- rownames(preference_matrix)
  # Calcule les distances entre chaque paire de candidats - matrice de duels
  duel_matrix <- matrix(0, n, n)
  print(preference_matrix)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i != j) {
        win_i_j <- sum(preference_matrix[i,] < preference_matrix[j,]) # ok
        duel_matrix[i,j] <- win_i_j
        duel_matrix[j,i] <- n_voter - win_i_j
      }
    }
  }
  print(duel_matrix) # OK
  row_sums <- rowSums(duel_matrix)
  col_sums <- colSums(duel_matrix)
  # Vainqueur de Condorcet :
  rows_greater_than_half <- apply(duel_matrix, 1, function(row) all(row > n_voter/2))
  cols_less_than_half <- apply(duel_matrix, 2, function(col) all(col < n_voter/2))
  if (any(rows_greater_than_half == TRUE)) {
    #print("plus grand Condorcet")
    #print(rows_greater_than_half)
    winner <- rows_greater_than_half
  }else if(any(cols_less_than_half == TRUE)){
    #print("plus petit Condorcet")
    #print(cols_less_than_half)
    winner <- cols_less_than_half
  }else{
    # sinon le moins pire des valeurs
    row_worst_values <- rep(Inf, n)  # Initialiser avec une valeur infinie
    for (i in 1:n) {
      for (j in 1:n) {
        if (i != j) {  # Exclure la diagonale symétrique
          value <- duel_matrix[i, j]
          if (value < row_worst_values[i]) {
            row_worst_values[i] <- value
          }
        }
      }
    }
    row_with_highest_worst_value <- which.max(row_worst_values) # gérer les égalités ??
    winner <- row_with_highest_worst_value
  }

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
  condorcet <- condorcet(pref_matrix)
  if(!is.null(condorcet)){
    print("Gagnant de Condorcet !")
    return(condorcet)
  }else{
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

#' Range voting (vote à la moyenne)
#' @export
#' @param situation voters preferences
#' @returns winner
range_voting <- function(situation) {
  situation <- rename_rows(situation)
  n_candidate <- nrow(situation)
  n_voter <- ncol(situation)
  candidate_votes <- rep(0, n_candidate)
  names(candidate_votes) <- rownames(situation)
  seuil <- c(0,1,2,3,4,5,6,7,8,9)
  for (i in 1:n_candidate){
    for(j in 1:n_voter){
      note <- tail(seuil[seuil <= 10*situation[i,j]],1) # 10* car préfs entre 0 et 1
      candidate_votes[i] <- candidate_votes[i] + note
    }
  }
  print(candidate_votes)
  print(candidate_votes/n_voter)
  res <- candidate_votes/n_voter
  winner <- which(res == max(res))
  return(winner)
}

#' Majority Jugement (Balinski) medianne
#' @export
#' @param situation voters preferences
#' @returns winner
majority_jugement <- function(situation) {
  #  À rejeter, Insuffisant, Passable, Assez Bien,  Bien,   Très Bien,  Excellent
  #     < 0        < 2        < 4        < 6         < 7       < 8         9
  #   1 point   2 points    3 points   4 points    5 points  6 points   7 points

  situation <- rename_rows(situation)
  n_candidate <- nrow(situation)
  n_voter <- ncol(situation)

  medianne <- ifelse(n_voter %% 2 == 0, n_voter/2, (n_voter+1)/2)
  print(medianne)

  seuils <- c(0,2,4,6,7,8,9)
  notes <- rep(1:7,1)
  candidate_votes <- lapply(1:n_candidate, function(x) rep(0,length(notes)))

  # attribution des évaluations
  for (i in 1:n_candidate){
    for(j in 1:n_voter){
      seuil <- tail(seuils[seuils <= 10*situation[i,j]],1) # 10* car préfs entre 0 et 1
      indice_seuil <- which(seuils == seuil)
      #print(10*situation[i,j])
      #print(indice_seuil)
      candidate_votes[[i]][[indice_seuil]] <- candidate_votes[[i]][[indice_seuil]] + 1
    }
  }
  # comptage les votes
  res_votes <- rep(0,n_candidate)
  for (i in 1:n_candidate){
    print(candidate_votes[[i]])
    cpt <- 1
    vote_count <-0
    for(j in candidate_votes[[i]]){
      vote_count <- vote_count + j
      if(vote_count >= medianne){
        print(vote_count)
        print(notes[cpt])
        # stocker dans un truc
        res_votes[[i]] <- c(notes[cpt],vote_count)
        print(res_votes[[i]])
        break
      }
      cpt <- cpt + 1
    }
  }
  print(res_votes)
  # vérif
  remainning_candidates <- rep(1,n_candidate)

  #print(candidate_votes/n_voter)
  #res <- candidate_votes/n_voter
  #winner <- which(res == max(res))
  winner <- NULL
  return(winner)
}

library("stats")
library("utils")
#' Approbal vote
#' @export
#' @param situation voters preferences
#' @param mode n_approbation mode
#' @import stats
#' @import utils
#' @returns winner
approbal <- function(situation, mode = "fixe") {
  situation <- rename_rows(situation)
  n_candidate <- nrow(situation)
  n_voter <- ncol(situation)
  candidate_votes <- rep(0, n_candidate)
  names(candidate_votes) <- rownames(situation)
  # Calcule le nombre d'approbations pour chaque candidat
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
    print(bounded_poisson_values)
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



