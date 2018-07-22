game_variables <- new.env()
game_variables$EMPTY_SPOT <- 'E'
game_variables$X_SPOT <- 'X'
game_variables$O_SPOT <- 'O'
game_variables$UNKNOWN_WINNER <- 'U'
game_variables$X_WINNER <- 'X_WIN'
game_variables$O_WINNER <- 'O_WIN'
game_variables$TIE_WINNER <- 'TIE_WIN'

game_variables$TOP_ROW <- c(1,2,3)
game_variables$MIDDLE_ROW <- c(4,5,6)
game_variables$BOTTOM_ROW <- c(7,8,9)
game_variables$LEFT_COLUMN <- c(1,4,7)
game_variables$MIDDLE_COLUMN <- c(2,5,8)
game_variables$RIGHT_COLUMN <- c(3,6,9)
game_variables$BACK_SLASH <- c(1,5,9)
game_variables$FORWARD_SLASH <- c(3,5,7)
game_variables$WIN_CONDITIONS <- list(game_variables$TOP_ROW, game_variables$MIDDLE_ROW, game_variables$BOTTOM_ROW, 
                                     game_variables$LEFT_COLUMN, game_variables$MIDDLE_COLUMN, game_variables$RIGHT_COLUMN, 
                                     game_variables$BACK_SLASH, game_variables$FORWARD_SLASH)

game_variables$EMPTY_GAME_STATE <- c(rep(game_variables$EMPTY_SPOT, 9), game_variables$UNKNOWN_WINNER)
