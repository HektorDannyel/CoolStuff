find_cat <- function(hypotenuse = 5){

  if(any(!is.numeric(hypotenuse))){

    if(length(hypotenuse) == 1){

      stop(paste0("Invalid input. '", hypotenuse, "' is not a number."))

    } else {

      stop(paste0("Invalid input. At least one hypotenuse is not a number."))

    }

    } else if(any(is.infinite(hypotenuse))){

      if(length(hypotenuse) == 1){

        stop("Invalid input. The hypotenuse must be finite.")

      } else {

        stop("Invalid input. All hypotenuses must be finite.")

      }

    } else if(any(!abs(hypotenuse) - round(hypotenuse, 0) == 0)){

    if(length(hypotenuse) == 1){

      stop("Not supported numeric format. Input value must be an integer.")

    } else {

      stop("Not supported numeric format. All hypotenuses must be integers.")

    }

    } else if(any(hypotenuse <= 0)){

      if(length(hypotenuse) == 1){

        stop("Invalid input. The hypotenuse must be > 0.")

      } else {

        stop("Invalid input. All hypotenuses must be > 0.")

      }

  }

  options(digits = 10)

  if(length(hypotenuse) == 1){

    seq_to_hyp <- seq_len(hypotenuse)

    hyp_divisors <- seq_to_hyp[hypotenuse%%seq_to_hyp == 0]

    if(hypotenuse == 1){

      grt_divisor <- 1

    } else {

      grt_divisor <- hyp_divisors[length(hyp_divisors)/2]

    }

    hyp_reduced <- hypotenuse/grt_divisor

    if(hyp_reduced < 500){

      h2 <- hyp_reduced^2

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

              return(sqrt(c(c1, c2))*grt_divisor)
              break

            }
        }
      }
    } else {

      seq_divisors <- c(0, seq_to_hyp[seq_to_hyp %% 500 == 0], ceiling(hypotenuse/500)*500)

      for(i in 1:length(seq_divisors)){

        h2 <- hyp_reduced^2

        n_seq <- ((1 + seq_divisors[i]):seq_divisors[i + 1])^2
        sqrts <- sqrt(n_seq)

        for(j in 1:length(n_seq)){

          c1 <- n_seq[j]

          c2 <- h2 - c1

          if((abs(sqrt(c2)) - round(sqrt(c2), 0) == 0)){

            if(c2 == 0 | sqrt(c1) == j){

              stop(paste("No matches found. It is not possible to find a triangle with",
                         "hypotenuse", hypotenuse, "and whole catheti."))

            } else {

              return(sqrt(c(c1, c2))*grt_divisor)
              break

            }
          }
        }
      }
    }
  }
}



# Próximos passos
#
# Comportar múltiplos inputs:
#
# Condicionar a função a trabalhar de acordo com o tamanho do argumento hypotenuse;
# Se len(hypotenuse) == 1, manter o algoritmo como tal
# Se len(hypotenuse) > 1, deixar o algoritmo dinâmico para se mover ao longo do vetor e
# cuspir uma matriz de tamanho 2:n com as combinações de catetos.

