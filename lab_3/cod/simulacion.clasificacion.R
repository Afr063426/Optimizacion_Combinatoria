
#' Esta funcion genera valores para una tabla de clasificacion binaria
#' @param n: Numero de observaciones
#' @param p:  numero de variables variables
#' @param correlaciones: se pasa una matriz de correlaciones con las correlaciones entre las variables
#' @param probabilidades: se para un vector con las probabilida de 0 o 1 de cada cluster
#' @return retonar una tabla de un cluster 

generacion.individuos <- function(n, p, correlaciones, probabilidades) {
  # Generar una matriz de correlaciones

  #Se validar que las correlaciones sean validas
#   if(length(correlaciones) != p * (p - 1) / 2) {
#     stop("El numero de relaciones debe ser igual a p*(p-1)/2")
#   }
  #Se procede a validad que las proabilidad se ubican entre 0 y 1
  if(any(probabilidades>1) | any(probabilidades<0)){
    stop("Las probabilidades deben estar entre 0 y 1")
  }

  #Se verifica que las correlaciones sean validas
  for(i in 1:p){
    for(j in 1:i){
        if(i == j){
            if(correlaciones[i,j] != 1){
            stop("La correlacion de una variable consigo misma debe ser 1")
            }
        } else {

            cota.inferior <- max(c(
                -sqrt(probabilidades[i]*probabilidades[j]/((1-probabilidades[i])*(1-probabilidades[j]))),
                -1/(sqrt(probabilidades[i]*probabilidades[j]/((1-probabilidades[i])*(1-probabilidades[j]))))
            ))
            cota.superior <- min(c(
                sqrt(probabilidades[i]*(1-probabilidades[j])/((1-probabilidades[i])*(probabilidades[j]))),

                1/sqrt(probabilidades[i]*(1-probabilidades[j])/((1-probabilidades[i])*(probabilidades[j])))
            )
            )
            if(correlaciones[i,j] < cota.inferior | correlaciones[i,j] > cota.superior) {
            stop(paste("La correlación de ", i, j, " debe estar entre ", cota.inferior, " y ", cota.superior, sep = ""))
            }
        }
    }
    #Se procede a generar una matriz con las correlaciones para una distrbucion normal multivariada
    correlaciones.matriz <- matrix(0, nrow = p, ncol = p)
    for(i in 1:p) {
        for(j in 1:i) {
            if(i == j) {
                correlaciones.matriz[i,j] <- 1

            } else {
                correlaciones.matriz[i,j] <- solucion.correlacion(probabilidades[i], probabilidades[j], correlaciones[i,j])
                correlaciones.matriz[j,i] <- correlaciones.matriz[i,j]
            }
        }
    }
  }
  #Se procede a simular la distribucion normal multivariada
  datos.mv <- mvtnorm::rmvnorm(n, sigma = correlaciones.matriz)

  #Se procede a colocar 0 o 1 dependiendo del valor presente  en cada columna 
    for(i in 1:p) {
        datos.mv[,i] <- ifelse(datos.mv[,i] <= qnorm(probabilidades[i]), 1, 0)
    }
    return(datos.mv)
  
}
  
solucion.correlacion <- function(p.i, p.j, delta.ij){
    quantile.i <- qnorm(p.i)
    quantile.j <- qnorm(p.j)

    # Se define una funcion objetivo para minimizar
    objetivo <- function(x) {
        matriz.cor <- matrix(c(1, x, x, 1), nrow = 2, byrow = TRUE)
        prob <- mvtnorm::pmvnorm(corr = matriz.cor, lower = c(-Inf, -Inf), upper = c(quantile.i, quantile.j))
        return((delta.ij*sqrt(p.i*(1-p.i)*p.j*(1-p.j))+p.i*p.j-prob)^2)
    }

    #Se usa optim para encontrar la solucion
    resultado <- optim(par = 1, fn = objetivo, method = "Brent", lower = 0, upper = 1)$par
    return(resultado)
}

#' @description Esta funcion genera una tabla de clasificacion binaria, en este caso el que pertenezca a un grupo o a otro se basa en las correlaciones entre las variables y las probabilidades de tener 0 o 1 en cada variable.
#' @param n: Numero de observaciones
#' @param p: Numero de variables
#' @param k: cantidad de clusters
#' @param seeds: semilla para la generacion de los clusters
#' @return retorna una tabla de clasificacion binaria

generacion.tabla.binaria <- function(n, p, k, seeds){
    #Se procede a generar las probabilidades de pertenecer a 0 a 1 de manera aleatoria
    probabilidades <- c()
    generacion.tabla.binaria <- list() #Se usa esta lista para guardar la informacion de cada cluster
    for(K in 1:k){
        set.seed(seeds[K])
        probabilidades <- runif(p, 0, 1)

        # Se genera la matriz de correlaciones de manera aleatoria
        correlaciones <- matrix(1, nrow = p, ncol = p)
        for(i in 1:p) {
            for(j in 1:i) {
                if(i != j) {
                    cota.inferior <- max(c(
                        -sqrt(probabilidades[i]*probabilidades[j]/((1-probabilidades[i])*(1-probabilidades[j]))),
                        -1/(sqrt(probabilidades[i]*probabilidades[j]/((1-probabilidades[i])*(1-probabilidades[j]))))
                    ))
                    cota.superior <- min(c(
                        sqrt(probabilidades[i]*(1-probabilidades[j])/((1-probabilidades[i])*(probabilidades[j]))),

                        1/sqrt(probabilidades[i]*(1-probabilidades[j])/((1-probabilidades[i])*(probabilidades[j])))
                    )
                    )
                    set.seed(seeds[K])
                    correlaciones[i,j] <- runif(1, cota.inferior, cota.superior)
                    correlaciones[j,i] <- correlaciones[i,j]
                }
            }
        }
        #Se procede a generar los individuos para cada cluster
        individuos <- generacion.individuos(n, p, correlaciones, probabilidades)

        #Se guarda la informacion de cada cluster
        generacion.tabla.binaria[[K]] <- list(
            individuos = individuos,
            probabilidades = probabilidades,
            correlaciones = correlaciones, 
            cluster = K
        )
        if(K==1){
            df.tabla.binaria <- as.data.frame(individuos) %>%
                dplyr::mutate(cluster = K)
        }else{
            df.tabla.binaria <- rbind(df.tabla.binaria, as.data.frame(individuos) %>%
                dplyr::mutate(cluster = K))

        }
    }
    #Se annade la tabla binaria a la lista de clusters
    generacion.tabla.binaria$tabla.binaria <- df.tabla.binaria
    return(generacion.tabla.binaria)
}