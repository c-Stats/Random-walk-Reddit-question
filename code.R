n_min_given_N <- function(N ,points, win_at){ceiling((win_at - N*points[2]) / (points[1] - points[2]))}

S_N <- function(N, points, p, win_at){

	#If N < min number of games required, then P(game not over) = 1
	if(N < ceiling(win_at / max(points))){

		return(1)

	#If N > max number of games required, then P(game not over) = 0
	} else if(N > ceiling(win_at / min(points))){

		return(0)

	#Else, sum over n
	} else {

		#Probability that the game is over
		F <- 0
		n_start <- n_min_given_N(N ,points, win_at)

		for(n in n_start:N){

			F <- F + choose(N, n) * p^n * (1-p)^(N - n)

		}

		#Survival probability
		return(1 - F)

	}

}

compute_E_X <- function(points, p, win_at){

	from <- ceiling(win_at / max(points))
	to <- ceiling(win_at / min(points))

	E_X <- from
	for(i in from:to){

		E_X <- E_X + S_N(i, points, p, win_at)

	}

	return(E_X)

}



p <- seq(from = 0, to = 1, by = 0.01)
E_X <- sapply(p, function(p){compute_E_X(c(5, 2), p, 40)})
plot(x = p, y = E_X, type = "l")

compute_E_X(c(5, 2), 1/2, 40)
