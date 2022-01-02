find_cat <- function(hypotenuse = 5){

  if(is.character(hypotenuse) & (grepl("[D-d]onut", hypotenuse) |
                                 grepl("[D-d]oughnut", hypotenuse))){

    return(cat("Get a grip, you\n",
    " ____                    _
 |  _ \\  ___  _ __  _   _| |_
 | | | |/ _ \\| '_ \\| | | | __|
 | |_| | (_) | | | | |_| | |_ _
 |____/ \\___/|_| |_|\\__,_|\\__(_)
                                "))

  } else if(!is.numeric(hypotenuse)){

    stop(paste0("Invalid input. '", hypotenuse, "' is not a number."))

    } else if(is.infinite(hypotenuse)){

      stop("Invalid input. The hypotenuse must be finite.")

    } else if(!abs(hypotenuse) - round(hypotenuse, 0) == 0){

    stop("Not supported numeric format. Input value must be an integer.")

  }

  options(digits = 10)

  h2 <- hypotenuse^2

  n_seq <- 1:h2
  sqrts <- sqrt(n_seq)
  n_seq_sqrt <- n_seq[abs(sqrts) - round(sqrts, 0) == 0]

  for(i in 1:length(n_seq_sqrt)){

    c1 <- n_seq_sqrt[i]

    c2 <- h2 - c1

    if((abs(sqrt(c2)) - round(sqrt(c2), 0) == 0)){

      if(c2 == 0 & sqrt(c1) == i){

        stop(paste("No matches found. It is not possible to find a triangle with",
                 "hypotenuse", hypotenuse, "and whole catheti."))

        } else {

          return(paste0("The two catheti for the hypotenuse of ", hypotenuse, " are ",
                    sqrt(c1), " and ", sqrt(c2), "."))
          break

        }
    }
  }
}

find_cat(2022)

