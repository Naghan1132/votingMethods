# Condorcet winner - OK
# Condorcet looser - OK
# Uninomial 1T - OK
# Uninomial 2T - OK
# Elination successive - OK
# Bucklin - OK
# Borda - OK
# Nanson - OK
# Minimax - OK
# Copeland - OK
# Anti Plularity - OK
# Star - OK
# Inf - OK
# Vote à la moyenne / Range Voting - OK
# Jugement Majoritaire - OK
# Approbation - OK


# ==== ==== ==== ==== ==== ==== ==== ==== ==== ====



#' Uninominal vote
#' @export
#' @param scores voters scores
#' @param n_round int
#' @import nnet
#' @returns winner
uninominal <- function(scores, n_round = 1) {
  if(!(n_round == 1 | n_round == 2)){
    stop("Number of rounds must be 1 or 2")
  }
  n_voters <- ncol(scores)
  # Initialiser le vecteur de voix pour chaque candidat
  vote_counts <- table(rownames(scores)[apply(scores, 2, which.is.max)])
  winner <- names(vote_counts)[which.is.max(vote_counts)]
  if(n_round == 2) {
    # Vote uninominal à deux tours
    if((max(vote_counts) / n_voters) < 0.5) {
      # Si aucun candidat n'a la majorité absolue, on passe au second tour
      top2_indices <- names(vote_counts)[order(vote_counts, decreasing = TRUE)[1:2]]
      scores2 <- scores[top2_indices,]
      return(uninominal(scores2, n_round = 1))
    }
  }
  return(winner)
}

#' Succesif elimination
#' @export
#' @param scores voters scores
#' @param first_it first iteration
#' @returns remaining_candidates
successif_elimination <- function(scores, first_it = TRUE) {
  if(first_it){
      scores <- scores_to_preferences(scores)
  }
  table <- table(rownames(scores)[apply(scores, 2, which.min)])
  missing_rownames <- setdiff(rownames(scores),rownames(table))
  if(length(missing_rownames) > 0){
    table[missing_rownames] <- 0 # si un candidat n'est jamais le pref, alors on l'ajoute à la main dans la table
  }
  if(length(table) <= 2){
    return(names(table)[which.is.max(table)])
  }else{
    looser <- names(which.min(table))
    scores <- subset(scores, rownames(scores) != looser)
    return(successif_elimination(scores,FALSE))
  }
}


#' Borda method
#' @param scores voters scores
#' @returns winner
#' @export
borda <- function(scores) {
  points <- scores_to_borda_points(scores)
  # Calculer le total de chaque ligne
  totals <- rowSums(points)
  # Retourner tous les indices des lignes ayant un total maximal
  winner <- rownames(points)[which.is.max(totals)]
  return(winner)
}

#' Condorcet Winner
#' @export
#' @param scores scores matrix
#' @return winner
condorcet_winner <- function(scores){
  n_voters <- ncol(scores)
  duel_matrix <- make_duel_matrix(scores)
  # Appliquer la fonction personnalisée sur chaque ligne de la matrice
  resultat <- rownames(duel_matrix)[which(sapply(1:nrow(duel_matrix), function(i) ligne_sup(duel_matrix[i, ], i,n_voters/2)))]
  if(length(resultat) == 0){
    return(NULL)
  }
  return(resultat)
}

#' Condorcet Looser
#' @export
#' @param scores scores matrix
#' @return looser
condorcet_looser <- function(scores){
  n_voters <- ncol(scores)
  duel_matrix <- make_duel_matrix(scores)
  # Appliquer la fonction personnalisée sur chaque colonne de la matrice
  resultat <- rownames(duel_matrix)[which(sapply(1:ncol(duel_matrix), function(j) colonne_sup(duel_matrix[,j], j,n_voters/2)))]
  if(length(resultat) == 0){
    return(NULL)
  }
  return(resultat)
}


#' Copeland procedure
#' @export
#' @param scores voters scores
#' @return Copeland Winner & Condorcet Winner
copeland <- function(scores) {
  n_candidates <- nrow(scores)
  n_voters <- ncol(scores)
  preferences <- scores_to_preferences(scores)
  votes <- setNames(rep(0, n_candidates), rownames(preferences))
  duel_matrix <- matrix(0, n_candidates,n_candidates) # condorcet matrix
  colnames(duel_matrix) <- rownames(preferences)
  rownames(duel_matrix) <- rownames(preferences)
  majority_threshold <- ceiling(n_voters / 2) + ifelse(n_voters %% 2 == 1, 0, 1)
  for (i in 1:(n_candidates - 1)) {
    for (j in (i + 1):n_candidates) {
      wins_i <- sum(preferences[i,] < preferences[j,])
      duel_matrix[i,j] <- wins_i
      duel_matrix[j,i] <- n_voters - wins_i
      if (wins_i == n_voters/2) {
        votes[i] <- votes[i] + 0.5 # draw
        votes[j] <- votes[j] + 0.5
      } else if (wins_i >= majority_threshold) {
        votes[i] <- votes[i] + 1 # i win
      } else {
        votes[j] <- votes[j] + 1 # j win
      }
    }
  }
  condorcet <- rownames(duel_matrix)[which(sapply(1:nrow(duel_matrix), function(i) ligne_sup(duel_matrix[i, ], i,n_voters/2)))]
  if(length(condorcet) == 0){
    condorcet <- "None"
  }
  copeland <- names(votes)[which.is.max(votes)]

  return_list <- list("copeland" = copeland, "condorcet" = condorcet)
  return(return_list)
}


#' Minimax procedure
#' @export
#' @param scores voters scores
#' @return winner
minimax <- function(scores) {
  condorcet_winner <- condorcet_winner(scores)
  if(!is.null(condorcet_winner)){
    return(condorcet_winner)
  }
  else{# sinon le moins pire des valeurs (minimum de chaque ligne, hors diagonale)
    duel_matrix <- make_duel_matrix(scores)
    resultat <- sapply(1:nrow(duel_matrix), function(i) ligne_min(duel_matrix[i, ], i))
    winner <- rownames(duel_matrix)[which.is.max(resultat)]
    return(winner)
  }
}

#' Bucklin method
#' @export
#' @param scores voters scores
#' @returns winner
bucklin <- function(scores) {
  preferences <- scores_to_preferences(scores)
  candidate_votes <- setNames(rep(0, nrow(preferences)), rownames(preferences))
  winner <- NULL
  n_round <- 1
  majority_threshold <- ceiling(ncol(preferences) / 2) + ifelse(ncol(preferences) %% 2 == 1, 0, 1)
  while(is.null(winner)) {
    # Compter le nombre de votes pour chaque candidat
    candidate_votes <- sapply(1:nrow(preferences), function(i) {
      candidate_votes[i] + sum(preferences[i, ] <= n_round)
    })
    someone_has_majority <- length(which(candidate_votes >= majority_threshold) > 0)
    if (someone_has_majority) {
      winner <- names(candidate_votes)[which.is.max(candidate_votes)] # random between max vote if draw
      return(winner)
    }
    n_round <- n_round + 1
  }
}

#' Nanson method
#' @export
#' @param scores voters scores
#' @param first_it first iteration
#' @returns winner
nanson <- function(scores,first_it = TRUE) {
  if(first_it){
    scores <- scores_to_borda_points(scores)
  }
  candidate_votes <- rowSums(scores)
  mean <- sum(candidate_votes/length(candidate_votes))
  loosers <- names(candidate_votes)[candidate_votes < mean]
  if((length(candidate_votes)-length(loosers) == 1) | (length(unique(candidate_votes)) == 1)){
    winner <- names(candidate_votes)[which.is.max(candidate_votes)]
    return(winner) # win / draw (draw => random winner)
  }else{
    # Éliminations -> récursivité :
    scores <- rearrange_points(scores,loosers)
    return(nanson(scores,FALSE))
  }
}

#' anti plularity,each voter votes against a single candidate, and the candidate with the fewest votes against wins
#' @export
#' @param scores voters scores
#' @returns winner
anti_plularity <- function(scores) {
  vote_counts <- table(rownames(scores)[apply(scores, 2, which.min)])
  missing_rownames <- setdiff(rownames(scores),rownames(vote_counts))
  if(length(missing_rownames) > 0){
    vote_counts[missing_rownames] <- 0 # si un candidat n'est jamais le pire, alors on l'ajoute à la main dans la table
  }
  indices_min <- which(vote_counts == min(vote_counts))
  # test égalité :
  if(length(indices_min) > 1){
    indice_aleatoire <- sample(indices_min, 1)
    winner <- names(vote_counts)[indice_aleatoire]
  }else{
    winner <- names(vote_counts)[indices_min]
  }
  return(winner)
}


#' Star, 2 meilleures moyenne et on regarde le résultat d'un vote uninominal sur les 2
#' @export
#' @param scores voters scores
#' @returns winner
star <- function(scores) {
  mean <- apply(scores,1,mean)
  top2_indices <- names(mean)[order(mean, decreasing = TRUE)[1:2]]
  winner <- uninominal(scores[top2_indices,])
  return(winner)
}

#' Infinity
#' @export
#' @param scores voters scores
#' @returns winner
infinity <- function(scores) {
  mean_min_max <- apply(scores, 1, function(row) mean(c(min(row), max(row))))
  winner <- names(mean_min_max)[which.is.max(mean_min_max)]
  return(winner)
}

#' Range voting (vote à la moyenne)
#' @export
#' @param scores voters scores
#' @returns winner
range_voting <- function(scores) {
  mean <- apply(scores,1,mean)
  winner <- names(mean)[which.is.max(mean)]
  return(winner)
}

#' JM (median)
#' @export
#' @param scores voters scores
#' @returns winner
JM <- function(scores) {
  median <- apply(scores,1,median)
  winner <- names(median)[which.is.max(median)]
  return(winner)
}

library("stats")
library("utils")
#' Approval vote
#' @export
#' @param scores voters scores
#' @param mode n_approbation mode
#' @import stats
#' @import utils
#' @returns winner
approval <- function(scores, mode = "autre") {
  n_candidate <- nrow(scores)
  n_voter <- ncol(scores)
  candidate_votes <- setNames(rep(0, n_candidate), rownames(scores))
  # Calcule le nombre d'approbations pour chaque candidat
  if(mode == "fixe"){
    n_appro <- resultat <- (n_candidate - (n_candidate %% 2)) / 2
    for(i in 1:n_voter){
      indices_lignes <- tail(order(scores[,i]), n_appro)
      candidate_votes[indices_lignes] <- candidate_votes[indices_lignes] + 1
    }
  }else if(mode == "poisson"){
    #set.seed(2023) # à mettre pour la simu finale
    lambda <- n_candidate/2 # moyenne & variance de la loi
    poisson_values <- rpois(n_voter, lambda)
    bounded_poisson_values <- pmax(pmin(poisson_values, n_candidate - 1), 1) # n_appro de 1 à length(n_candidat-1)
    for(i in 1:n_voter){
      indices_lignes <- tail(order(scores[,i]), bounded_poisson_values[i])
      candidate_votes[indices_lignes] <- candidate_votes[indices_lignes] + 1
    }
  }else{
    for (j in 1:n_voter) {
      column <- scores[, j]
      if(length(candidate_votes[column > 0.7]) == n_candidate){
        # tous  > 0.7 =>  enlever le pire vote
        min_score_index <- which.min(column)
        candidate_votes[-min_score_index] <- candidate_votes[-min_score_index] + 1
      } else if (any(column > 0.7)) {
        candidate_votes[column > 0.7] <- candidate_votes[column > 0.7] + 1
      } else {
        # aucun candidats > 0.7 => on ajoute +1 au meilleur
        max_score_index <- which.is.max(column)
        candidate_votes[max_score_index] <- candidate_votes[max_score_index] + 1
      }
    }
  }
  # Retourne le candidat ayant obtenu le plus grand nombre d'approbations
  winner <- names(candidate_votes)[which.is.max(candidate_votes)]
  return(winner)
}
