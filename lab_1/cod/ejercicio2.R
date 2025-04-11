# Este codigo tiene como objetivo realizar in multidmensional scaling (MDS) usando el metodo del 
# descenso de gradiente 


# Se define la funcion objetivo que se va minimizar

# Funcion objetivo

fn.objetivo <- function(x, datos, indice.1, indice.2, n) {
    
    valor.1 <- (x[indice.1] - x[indice.2])^2

    valor.2 <- (x[indice.1 + n] - x[indice.2 + n])^2
    
    suma.cuadrado <- sum((datos$V3 - sqrt(valor.1 + valor.2))^2)
    #Se compara con la matriz de distancias original

    
    #diferencia <- sum(1/nrow(distancia.original) * (as.matrix(distancias) - as.matrix(distancia.original))^2)

    return(suma.cuadrado)
}

#Se procede a obtejner lo solicitado por el ejercicio


fn.ejercicio.2 <- function(datos, multistart = 100, control = list()) {
    # Se calcula la matriz de distancias
    #distancia.original <- as.matrix(datos)
    stress <- c()
    tiempos <- c()
    iteraciones <- c()
    
    #Se procede a hacer los multistart
    for(j in 1:multistart){
        # Se inicializa la matriz de puntos aleatorios
        print(j)
        #Se toman los valores unicos de los puntos
        valores.unicos <- unique(c(datos$V1, datos$V2))

        x.inicial <- runif(length(valores.unicos) * 2)
        

        #Se procede a tomar los indices de los puntos
        union.valores <- data.frame(valores.unicos = valores.unicos, 
                                     index = 1:length(valores.unicos))
        

        #Se procede a hacer un for para poder determinar los valores que se deben tomar
        #en cada caso
        indice.1 <- c()

        indice.2 <- c()

        for(k in 1:nrow(datos)){
            indice.1 <- c(indice.1, union.valores$index[union.valores$valores.unicos == datos$V1[k]])
            indice.2 <- c(indice.2, union.valores$index[union.valores$valores.unicos == datos$V2[k]])
        }

        
        # Se optimiza la funcion objetivo
        inicio <- Sys.time()
        resultado <- optim(par = x.inicial, 
            fn = fn.objetivo, 
            method = "BFGS", 
            datos = datos, 
            indice.1 = indice.1, 
            indice.2 = indice.2, 
            n = length(valores.unicos), 
            control = control
            )
        tiempos <- c(tiempos, Sys.time() - inicio)
        stress <- c(stress, resultado$value)
        iteraciones <- c(iteraciones, resultado$counts[2])

        #Se guardan los valores obtenidos
        
        # Se guarda el resultado
        if(j == 1){
            mejor.resultado <- resultado
            i <- 1
            valores <- c(resultado$par)
            df.valores <- data.frame(Etiquetas = valores.unicos, 
                                 x = resultado$par[1:length(valores.unicos)], 
                                 y = resultado$par[(1:length(valores.unicos)) + length(valores.unicos)])
        }else{
            if(resultado$value < mejor.resultado$value){
                mejor.resultado <- resultado
                i <- 1
            ############# Se guarda el resultado
            valores <- c(resultado$par)
            }else if(resultado$value == mejor.resultado$value & all(valores == c(resultado$par))){
                i <- i + 1
            }

            df.valores <- rbind(df.valores, data.frame(Etiquetas = valores.unicos, 
                                 x = resultado$par[1:length(valores.unicos)], 
                                 y = resultado$par[(1:length(valores.unicos)) + length(valores.unicos)]))
        }
    }

    #Se estima la tasa de atraccion
    tau <- i / multistart
    
    #Se reporta el valor promedio del stress
    promedio.stress <- mean(stress)

    #Se reporta el tiempo de los multistart
    plot.time <- ggplot2::ggplot(data.frame(tiempos), aes(x = tiempos)) +
        ggplot2::geom_histogram() +
        ggplot2::labs(title = "Distribucion de los tiempos de los multistart", x = "Tiempo (s)", y = "Frecuencia") +
        ggplot2::theme_minimal()
    
    #Se reporta el promedio de iteraciones
    promedio.iteraciones <- mean(iteraciones)

    #Se procede a graficar el resultado los optimos locales
    plot.optimos <- ggplot2::ggplot(data.frame(stress), aes(x = stress)) +
        ggplot2::geom_histogram() +
        ggplot2::labs(title = "Distribucion de los optimos locales", x = "Stress", y = "Frecuencia") +
        ggplot2::theme_minimal()
    
    plot.coordendas.optimas <- ggplot2::ggplot(df.valores, aes(x = x, y = y, color = Etiquetas)) +
        ggplot2::geom_point() +
        ggplot2::labs(title = "Coordenadas optimas", x = "X", y = "Y") +
        ggplot2::theme_minimal()
    #Se reporta el resultado
    resultado <- list(
        "promedio.stress" = promedio.stress,
        "plot.time" = plot.time,
        "promedio.iteraciones" = promedio.iteraciones,
        "tau" = tau,
        "promedio.stress" = promedio.stress,
        "plot.time" = plot.time,
        "plot.optimos" = plot.optimos, 
        "plot.coordendas.optimas" = plot.coordendas.optimas,
        "stress" = stress,
        "tiempos" = tiempos,
        "iteraciones" = iteraciones, 
        "mejor.resultado" = mejor.resultado, 
        "df.valores" = df.valores
    )   
   
}
