install.packages("purrr")
install.packages("dplyr")
install.packages("tidyverse")
library(purrr)
library(dplyr)
library(tidyverse)


knight_offsets <- matrix(c(
  1,    2,
  2,    1,
  -2,    1,
  -1,    2,
  2,   -1,
  1,   -2,
  -1,   -2,
  -2,   -1
), ncol = 2, byrow = TRUE)

move_knight <- function(this_move, moves, visited) {
  
 
  moves <- append(moves, list(this_move))
  visited[this_move[1] + (this_move[2] - 1)*8] <- TRUE
  
 
  if (all(visited)) {
    return(moves)
  }
  
 
  next_move<-cbind(knight_offsets[,1]+this_move[1],knight_offsets[,2]+this_move[2])
  
  
  on_board  <- next_move[,1] %in% 1:8 & next_move[,2] %in% 1:8
  next_move <- next_move[on_board,,drop=FALSE]
  
  
  not_yet_visited <- !visited[next_move]
  next_move <- next_move[not_yet_visited,, drop = FALSE]
  
 
  for (i in seq_len(nrow(next_move))) {
    res <- move_knight(next_move[i,, drop = FALSE], moves, visited)
    if (!is.null(res)) {
      return(res)
    }
  }
  
  NULL
}

system.time({
  moves <- move_knight(c(4, 8), moves = list(), visited = matrix(FALSE, 8, 8))
})


moves_df <- as.data.frame(do.call(rbind, moves))
moves_df <- set_names(moves_df, c('x', 'y'))
moves_df$idx <- 1:nrow(moves_df)



ggplot(moves_df, aes(x, y)) + 
  geom_tile(aes(fill=as.logical((x+y)%%2)), colour = 'black') +
  geom_path(alpha = 0.7, linetype = 1, size = 0.25) +
  geom_text(aes(label = idx)) + 
  scale_fill_manual(values = c('grey70', 'white')) + 
  theme_void() + 
  theme(legend.position = 'none') + 
  coord_equal() + 
  labs(
    title = "A knight's tour in #RStats"
  )