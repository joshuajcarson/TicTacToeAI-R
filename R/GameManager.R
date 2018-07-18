library(data.table)
library(caret)
library(uuid)

EMPTY_SPOT = 'E'
X_SPOT = 'X'
O_SPOT = 'O'
UNKNOWN_WINNER = 'U'
X_WINNER = 'X_WIN'
O_WINNER = 'O_WIN'
TIE_WINNER = 'TIE_WIN'

TOP_ROW = c(1,2,3)
MIDDLE_ROW = c(4,5,6)
BOTTOM_ROW = c(7,8,9)
LEFT_COLUMN = c(1,4,7)
MIDDLE_COLUMN = c(2,5,8)
RIGHT_COLUMN = c(3,6,9)
BACK_SLASH = c(1,5,9)
FORWARD_SLASH = c(3,5,7)
WIN_CONDITIONS = list(TOP_ROW, MIDDLE_ROW, BOTTOM_ROW, LEFT_COLUMN, MIDDLE_COLUMN, RIGHT_COLUMN, BACK_SLASH, FORWARD_SLASH)

EMPTY_GAME_STATE = c(EMPTY_SPOT, EMPTY_SPOT, EMPTY_SPOT, 
                     EMPTY_SPOT, EMPTY_SPOT, EMPTY_SPOT, 
                     EMPTY_SPOT, EMPTY_SPOT, EMPTY_SPOT, 
                     UNKNOWN_WINNER)

create_empty_game_state <- function() {
  cur_player <<- X_SPOT
  new_game_state = EMPTY_GAME_STATE
  new_game_state[11] = UUIDgenerate()
  new_game_state
}

initiate_initial_game <- function() {
  cur_game_state <<- create_empty_game_state()
  
  game_history <<- data.frame(top_left = cur_game_state[1], top_middle = cur_game_state[2], top_right = cur_game_state[3],
                            middle_left = cur_game_state[4], middle_middle = cur_game_state[5], middle_right = cur_game_state[6],
                            bottom_left = cur_game_state[7], bottom_middle = cur_game_state[8], bottom_right = cur_game_state[9],
                            winner = cur_game_state[10], game_guid = cur_game_state[11], stringsAsFactors = FALSE)
  game_history <<- game_history[-1,]
}

set_game_spot_to_value <- function(game_state, index, value) {
  game_state[index] = value
  game_state
}

add_to_game_history <- function(row_to_be_added) {
  game_history[nrow(game_history) + 1,] <<- row_to_be_added
}

pick_future_game_state <- function(possible_future_game_states, X_AI = NA, O_AI = NA) {
  if(cur_player == X_SPOT && !is.na(X_AI)) {
    future_game_states_as_maxtrix <- matrix(unlist(possible_future_game_states), nrow=length(possible_future_game_states), byrow=T)
    future_game_states_as_dataframe <- data.frame(future_game_states_as_maxtrix, stringsAsFactors=FALSE)
    names(future_game_states_as_dataframe) <- c('top_left', 'top_middle', 'top_right', 'middle_left', 'middle_middle', 'middle_right', 'bottom_left', 'bottom_middle', 'bottom_right', 'winner', 'game_guid')
    probabilities_for_future_moves <- predict(X_AI, newdata = future_game_states_as_dataframe[, 1:10], type='prob')
    top_pick_for_x_win <- which.max(probabilities_for_future_moves$X_WIN)
    possible_future_game_states[top_pick_for_x_win]
  } else if(cur_player == O_SPOT && !is.na(O_AI)) {
    future_game_states_as_maxtrix <- matrix(unlist(possible_future_game_states), nrow=length(possible_future_game_states), byrow=T)
    future_game_states_as_dataframe <- data.frame(future_game_states_as_maxtrix, stringsAsFactors=FALSE)
    names(future_game_states_as_dataframe) <- c('top_left', 'top_middle', 'top_right', 'middle_left', 'middle_middle', 'middle_right', 'bottom_left', 'bottom_middle', 'bottom_right', 'winner', 'game_guid')
    probabilities_for_future_moves <- predict(O_AI, newdata = future_game_states_as_dataframe[, 1:10], type='prob')
    top_pick_for_o_win <- which.max(probabilities_for_future_moves$O_WIN)
    possible_future_game_states[top_pick_for_o_win]
  } else {
    index_to_pick <- sample(1:length(possible_future_game_states), 1)
    possible_future_game_states[index_to_pick]
  }
}

switch_cur_player <- function() {
  if(cur_player == X_SPOT) {
    cur_player <<- O_SPOT
  } else {
    cur_player <<- X_SPOT
  }
}

play_turn <- function(X_AI = NA, O_AI = NA) {
  cur_empty_spaces = which(cur_game_state == EMPTY_SPOT)
  possible_future_game_states <- lapply(cur_empty_spaces, function(x) set_game_spot_to_value(cur_game_state, x, cur_player))
  cur_game_state <<- unlist(pick_future_game_state(possible_future_game_states, X_AI, O_AI))
  add_to_game_history(cur_game_state)
  switch_cur_player()
}

check_all_win_conditions_for <- function(win_condition, spot_to_check) {
  all(cur_game_state[win_condition] == c(spot_to_check, spot_to_check, spot_to_check))
}

did_win_happened_for <- function(spot_to_check) {
  resultsFromAllWinConditions <- unlist(lapply(WIN_CONDITIONS, function(x) check_all_win_conditions_for(x, spot_to_check)))
  any(resultsFromAllWinConditions)
}

is_there_no_spots_remaining <- function() {
  length(cur_game_state[cur_game_state == EMPTY_SPOT]) == 0
}

determine_winner <- function() {
  if(did_win_happened_for(X_SPOT)) {
    X_WINNER
  } else if(did_win_happened_for(O_SPOT)) {
    O_WINNER
  } else if(is_there_no_spots_remaining()) {
    TIE_WINNER
  } else {
    UNKNOWN_WINNER
  }
}

initiate_initial_game()
for(i in 1:1000) {
  while(determine_winner() == UNKNOWN_WINNER) {
    play_turn()
  }
  game_history[game_history == UNKNOWN_WINNER] = determine_winner()
  cur_game_state <- create_empty_game_state()
}

training_rows <- createDataPartition(y = game_history$winner, p = .8, list = FALSE)
game_history_training <- game_history[training_rows, 1:10]
game_history_testing <- game_history[-training_rows, 1:10]

pls_fit_tic_tac_toe_ai <- train(winner ~ ., data = game_history_training, method = 'pls', preProc = c('center', 'scale'))
pls_props_tic_tac_toe_ai <- predict(pls_fit_tic_tac_toe_ai, newdata = game_history_testing, type='prob')

old_game_history <- game_history
initiate_initial_game()
for(i in 1:100) {
  while(determine_winner() == UNKNOWN_WINNER) {
    play_turn(X_AI = pls_fit_tic_tac_toe_ai)
  }
  game_history[game_history == UNKNOWN_WINNER] = determine_winner()
  cur_game_state <- create_empty_game_state()
}

table(old_game_history$winner)
table(game_history$winner)
