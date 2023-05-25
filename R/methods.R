# USE :
#   library(devtools)
#   install_github("Naghan1132/voteSim")
#   library(voteSim)
#   uninominal_vote(generate_beta(10,3))

# ==== Vote ordre de préférences ====
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

# ==== Vote par évaluation ====
# Vote à la moyenne / Range Voting - OK
# Jugement Majoritaire - À VOIR PLUS TARD
# Approbation


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
  print(vote_counts)
  winner <- names(vote_counts)[which.is.max(vote_counts)]
  if(n_round == 2) {
    # Vote uninominal à deux tours
    if(max(vote_counts) / n_voters < 0.5) {
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
  print(scores)
  table <- table(rownames(scores)[apply(scores, 2, which.min)])
  print(table)
  if(length(table) <= 2){
    return(names(table)[which.is.max(table)])
  }else{
    looser <- names(table)[which.min(table)]
    scores <- scores[names(table) != looser, ]
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
  print(totals)
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
#' @return winner
copeland <- function(scores) {
  n_candidates <- nrow(scores)
  n_voters <- ncol(scores)
  preferences <- scores_to_preferences(scores)
  votes <- setNames(rep(0, n_candidates), rownames(preferences))
  for (i in 1:(n_candidates - 1)) {
    for (j in (i + 1):n_candidates) {
      wins_i <- sum(preferences[i,] < preferences[j,])
      if (wins_i == n_voters/2) {
        votes[i] <- votes[i] + 0.5 # draw
        votes[j] <- votes[j] + 0.5
      } else if (wins_i > n_voters/2) {
        votes[i] <- votes[i] + 1 # i win
      } else {
        votes[j] <- votes[j] + 1 # j win
      }
    }
  }
  print(votes)
  winner <- names(votes)[which.is.max(votes)]
  return(winner)
}


#' Minimax procedure
#' @export
#' @param scores voters scores
#' @return winner
minimax <- function(scores) {
  preferences <- scores_to_preferences(scores)
  duel_matrix <- make_duel_matrix(preferences)
  print(duel_matrix)
  condorcet_winner <- condorcet_winner(duel_matrix)
  if(!is.null(condorcet_winner)){
    return(condorcet_winner)
  }
  else{# sinon le moins pire des valeurs (minimum de chaque ligne, hors diagonale)
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
  majority_threshold <- ceiling(ncol(preferences) / 2)
  while(is.null(winner)) {
    # Compter le nombre de votes pour chaque candidat
    candidate_votes <- sapply(1:nrow(preferences), function(i) {
      candidate_votes[i] + sum(preferences[i, ] == n_round)
    })
    print(candidate_votes)
    someone_has_majority <- length(which(candidate_votes > majority_threshold) > 0)
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
#' @returns winner
nanson <- function(scores,first_it = TRUE) {
  if(first_it){
    scores <- scores_to_borda_points(scores)
  }
  candidate_votes <- rowSums(scores)
  print(candidate_votes)
  mean <- sum(candidate_votes/length(candidate_votes))
  loosers <- names(candidate_votes)[candidate_votes < mean]
  if((length(candidate_votes)-length(loosers) == 1) | (length(unique(candidate_votes)) == 1)){
    winner <- names(candidate_votes)[which.is.max(candidate_votes)]
    return(winner) # win / draw (random winner)
  }else{
    # Éliminations -> récursivité :
    scores <- rearrange_points(scores,loosers)
    return(nanson(scores,FALSE))
  }
}

# ==== VOTE PAR ÉVALUATION ====

#' Range voting (vote à la moyenne)
#' @export
#' @param scores voters scores
#' @returns winner
range_voting <- function(scores) {
  mean <- apply(scores,1,mean)
  print(mean)
  winner <- names(mean)[which.is.max(mean)]
  return(winner)
}

#' Majority Jugement (Balinski) mediane
#' @export
#' @param situation voters preferences
#' @returns winner
majority_jugement <- function(situation) {
  #set.seed(2023)
  #  À rejeter, Insuffisant, Passable, Assez Bien,  Bien,   Très Bien,  Excellent
  #     < 0        < 2        < 4        < 6         < 7       < 8         9
  #   1 point   2 points    3 points   4 points    5 points  6 points   7 points

  n_candidate <- nrow(situation)
  n_voter <- ncol(situation)

  medianne <- ifelse(n_voter %% 2 == 0, n_voter/2, (n_voter+1)/2)
  #print(medianne)

  seuils <- c(0,2,4,6,7,8,9)
  notes <- rep(1:7,1)
  candidate_votes <- lapply(1:n_candidate, function(x) rep(0,length(notes)))

  # Attribution des évaluations
  for (i in 1:n_candidate){
    for(j in 1:n_voter){
      seuil <- tail(seuils[seuils <= 10*situation[i,j]],1) # 10* car préfs entre 0 et 1
      indice_seuil <- which(seuils == seuil)
      candidate_votes[[i]][[indice_seuil]] <- candidate_votes[[i]][[indice_seuil]] + 1
    }
  }
  # Comptage des votes
  res_votes <- list()
  for (i in 1:n_candidate){
    cpt <- 1
    vote_count <-0
    for(j in candidate_votes[[i]]){
      vote_count <- vote_count + j
      if(vote_count >= medianne){
        res_votes[[i]] <- c(notes[cpt],vote_count)
        break
      }
      cpt <- cpt + 1
    }
  }
  #print(res_votes)
  max_notes <- -Inf  # Valeur minimale initiale pour les notes
  max_vote_count <- -Inf  # Valeur minimale initiale pour les votes
  indice_max <- NULL
  # Parcourir la liste des résultats
  for (i in 1:length(res_votes)) {
    # Extraire les valeurs de notes et vote_count
    current_notes <- res_votes[[i]][1]
    current_vote_count <- res_votes[[i]][2]
    # Vérifier si les notes actuelles sont supérieures à la valeur maximale
    if (current_notes > max_notes) {
      # Mettre à jour la valeur maximale des notes et vote_count
      max_notes <- current_notes
      max_vote_count <- current_vote_count
      indice_max <- i
    }else if(current_notes == max_notes){
      if(current_vote_count > max_vote_count){
        max_notes <- current_notes
        max_vote_count <- current_vote_count
        indice_max <- i
      }
    }
  }
  #print(indice_max)
  #print(max_vote_count)
  winner <- rownames(situation)[indice_max]
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
approval <- function(scores, mode = "fixe") {
  n_candidate <- nrow(scores)
  n_voter <- ncol(scores)
  candidate_votes <- setNames(rep(0, n_candidate), rownames(scores))
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
    # apply(tail(order(situation)),2,function(col){
    #   print(col)
    # })
    # apply(situation, 2, function(x) {
    #   indices_lignes <- tail(order(x), n_appro)
    #   candidate_votes[indices_lignes] <- candidate_votes[indices_lignes] + 1
    # })
    for(i in 1:n_voter){
      indices_lignes <- tail(order(scores[,i]), n_appro)
      candidate_votes[indices_lignes] <- candidate_votes[indices_lignes] + 1
    }
  }else if(mode == "poisson"){
    lambda <- n_candidate/2 # moyenne & variance de la loi
    poisson_values <- rpois(n_voter, lambda)
    bounded_poisson_values <- pmax(pmin(poisson_values, n_candidate - 1), 1) # n_appro de 1 à length(n_candidat-1)
    print(bounded_poisson_values)
    hist(bounded_poisson_values)
    for(i in 1:n_voter){
      indices_lignes <- tail(order(scores[,i]), bounded_poisson_values[i])
      candidate_votes[indices_lignes] <- candidate_votes[indices_lignes] + 1
    }
  }else{
    # approbation pour tout ceux dont la préférences > 0.5
    candidate_votes <- apply(scores, 1, function(x) sum(x > 0.5))
  }
  print(candidate_votes)
  # Retourne le candidat ayant obtenu le plus grand nombre d'approbations
  winner <- names(candidate_votes)[which.is.max(candidate_votes)]
  return(winner)
}
