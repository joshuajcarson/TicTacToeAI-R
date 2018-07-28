library(data.table)
library(caret)
library(uuid)
source('R/GameVariables.R')
game_manager <- new.env()

create_empty_game_state <- function() {
  game_manager$cur_player <- game_variables$X_SPOT
  new_game_state = game_variables$EMPTY_GAME_STATE
  new_game_state[12] = UUIDgenerate()
  game_manager$game_state <- new_game_state
}

initiate_initial_game <- function() {
  create_empty_game_state()
  cur_game_state <- game_manager$game_state
  game_manager$game_history <- data.frame(top_left = cur_game_state[1], top_middle = cur_game_state[2], top_right = cur_game_state[3],
                            middle_left = cur_game_state[4], middle_middle = cur_game_state[5], middle_right = cur_game_state[6],
                            bottom_left = cur_game_state[7], bottom_middle = cur_game_state[8], bottom_right = cur_game_state[9],
                            game_turn = cur_game_state[10], winner = cur_game_state[11], game_guid = cur_game_state[12], 
                            stringsAsFactors = FALSE)
  game_manager$game_history <- game_manager$game_history[-1,]
}

set_game_spot_to_value <- function(game_state, index, value) {
  game_state[index] = value
  game_state
}

add_to_game_history <- function(row_to_be_added) {
  game_manager$game_history[nrow(game_manager$game_history) + 1,] <- row_to_be_added
}

pick_future_game_state <- function(possible_future_game_states, X_AI = NA, O_AI = NA) {
  if(game_manager$cur_player == game_variables$X_SPOT && !is.na(X_AI)) {
    future_game_states_as_matrix <- matrix(unlist(possible_future_game_states), nrow=length(possible_future_game_states), byrow=T)
    future_game_states_as_dataframe <- data.frame(future_game_states_as_matrix, stringsAsFactors=FALSE)
    names(future_game_states_as_dataframe) <- c('top_left', 'top_middle', 'top_right', 'middle_left', 'middle_middle', 'middle_right', 'bottom_left', 'bottom_middle', 'bottom_right', 'game_turn', 'winner', 'game_guid')
    probabilities_for_future_moves <- predict(X_AI, newdata = future_game_states_as_dataframe[, 1:11], type='prob')
    if(max(probabilities_for_future_moves$X_WIN) > max(probabilities_for_future_moves$X_EVENTUAL_WIN))
    {
      top_pick_for_x_win <- which.max(probabilities_for_future_moves$X_WIN)
      possible_future_game_states[top_pick_for_x_win]
    } else {
      top_pick_for_x_win <- which.max(probabilities_for_future_moves$X_EVENTUAL_WIN)
      possible_future_game_states[top_pick_for_x_win]
    }
  } else if(game_manager$cur_player == game_variables$O_SPOT && !is.na(O_AI)) {
    future_game_states_as_matrix <- matrix(unlist(possible_future_game_states), nrow=length(possible_future_game_states), byrow=T)
    future_game_states_as_dataframe <- data.frame(future_game_states_as_matrix, stringsAsFactors=FALSE)
    names(future_game_states_as_dataframe) <- c('top_left', 'top_middle', 'top_right', 'middle_left', 'middle_middle', 'middle_right', 'bottom_left', 'bottom_middle', 'bottom_right', 'game_turn', 'winner', 'game_guid')
    probabilities_for_future_moves <- predict(O_AI, newdata = future_game_states_as_dataframe[, 1:11], type='prob')
    if(max(probabilities_for_future_moves$O_WIN) > max(probabilities_for_future_moves$O_EVENTUAL_WIN))
    {
      top_pick_for_o_win <- which.max(probabilities_for_future_moves$O_WIN)
      possible_future_game_states[top_pick_for_o_win]
    } else {
      top_pick_for_o_win <- which.max(probabilities_for_future_moves$O_EVENTUAL_WIN)
      possible_future_game_states[top_pick_for_o_win]
    }
  } else {
    index_to_pick <- sample(1:length(possible_future_game_states), 1)
    possible_future_game_states[index_to_pick]
  }
}

switch_cur_player <- function() {
  if(game_manager$cur_player == game_variables$X_SPOT) {
    game_manager$cur_player <- game_variables$O_SPOT
  } else {
    game_manager$cur_player <- game_variables$X_SPOT
  }
  game_manager$game_state[10] <- as.numeric(game_manager$game_state[10]) + 1
}

play_turn <- function(X_AI = NA, O_AI = NA) {
  cur_empty_spaces = which(game_manager$game_state == game_variables$EMPTY_SPOT)
  possible_future_game_states <- lapply(cur_empty_spaces, 
                                        function(x) set_game_spot_to_value(game_manager$game_state, 
                                                                           x, 
                                                                           game_manager$cur_player))
  game_manager$game_state <- unlist(pick_future_game_state(possible_future_game_states, X_AI, O_AI))
  add_to_game_history(game_manager$game_state)
  switch_cur_player()
}

check_all_win_conditions_for <- function(win_condition, spot_to_check) {
  all(game_manager$game_state[win_condition] == c(spot_to_check, spot_to_check, spot_to_check))
}

did_win_happened_for <- function(spot_to_check) {
  resultsFromAllWinConditions <- unlist(lapply(game_variables$WIN_CONDITIONS,
                                               function(x) check_all_win_conditions_for(x, spot_to_check)))
  any(resultsFromAllWinConditions)
}

is_there_no_spots_remaining <- function() {
  length(game_manager$game_state[game_manager$game_state == game_variables$EMPTY_SPOT]) == 0
}

determine_winner <- function() {
  if(did_win_happened_for(game_variables$X_SPOT)) {
    game_variables$X_WINNER
  } else if(did_win_happened_for(game_variables$O_SPOT)) {
    game_variables$O_WINNER
  } else if(is_there_no_spots_remaining()) {
    game_variables$TIE_WINNER
  } else {
    game_variables$UNKNOWN_WINNER
  }
}

convert_to_eventual_winner <- function(winner) {
  if(game_variables$X_WINNER == winner) {
    game_variables$X_EVENTUAL_WINNER
  } else if(game_variables$O_WINNER == winner) {
    game_variables$O_EVENTUAL_WINNER
  } else if(game_variables$TIE_WINNER == winner) {
    game_variables$TIE_EVENTUAL_WINNER
  }
}

play_multiple_rounds <- function(number_of_rounds = 100, X_AI = NA, O_AI = NA) {
  for(i in 1:number_of_rounds) {
    while(determine_winner() == game_variables$UNKNOWN_WINNER) {
      play_turn(X_AI = X_AI, O_AI = O_AI)
    }
    game_manager$game_history[nrow(game_manager$game_history),]$winner <<- determine_winner()
    game_manager$game_history[game_manager$game_history == game_variables$UNKNOWN_WINNER] <<- convert_to_eventual_winner(determine_winner())
    game_manager$game_state <<- create_empty_game_state()
  }
}

initiate_initial_game()
play_multiple_rounds(1000)

training_rows <- createDataPartition(y = game_manager$game_history$winner, p = .8, list = FALSE)
game_history_training <- game_manager$game_history[training_rows, 1:11]
game_history_testing <- game_manager$game_history[-training_rows, 1:11]

pls_fit_tic_tac_toe_ai <- train(winner ~ ., data = game_history_training, method = 'pls', preProc = c('center', 'scale'))
pls_props_tic_tac_toe_ai <- predict(pls_fit_tic_tac_toe_ai, newdata = game_history_testing, type='prob')

play_multiple_rounds(number_of_rounds = 100, X_AI = pls_fit_tic_tac_toe_ai)

table(game_manager$game_history$winner)
