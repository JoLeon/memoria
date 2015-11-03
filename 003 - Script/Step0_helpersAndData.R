# HELPERS, LIBRERÍAS, Y CARGA DE DATOS

  # HELPERS
  
    minmaxNormalization <- function(sample,min,max){
      if(is.na(sample)){
        return(NA)
      }
      else{
        return((sample-min)/(max-min))
      }
    }
    zscoreNormalization <- function(sample,sd,mean){
      if(is.na(sample)){
        return(NA)
      }
      else{
        return((sample-mean)/(sd))
      }
    }
    getDayName <- function(x){
      if(x==0){
        as.factor("Lunes")
      }
      else{
        if(x==1){
          as.factor("Martes")
        }
        else{
          if(x==2){
            as.factor("Miercoles")
          }
          else{
            if(x==3){
              as.factor("Jueves")
            }
            else{
              if(x==4){
                as.factor("Viernes")
              }
              else{
                if(x==5){
                  as.factor("Sabado")
                }
                else{
                  as.factor("Domingo")
                }
              }
            }
          }
        }
      }
    }
    setUserShareFrequency <- function(x,y){
      if(y==0){
        0
      }
      else{
        x/y
      }
    }
    getUserQuality <- function(x,y){
      if(x==-1){
        as.factor("No interesado/No comprendió")
      }
      else{
        if(x==0){
          as.factor("No capturado")
        }
        else{
          if(y<=1){
            if(x>=30 && x<=60){
              as.factor("Diario por un mes")
            }
            else{
              if(x>=7 && x<=29){
                as.factor("Diario por una semana")
              }
              else{
                if(x <=6){
                  as.factor("Perdido")
                }
                else{
                  as.factor("Diario constante")
                }
              }
            }
          }
          else{
            if(y<=7){
              if(x>=30 && x<=60){
                as.factor("Semenal por un mes")
              }
              else{
                if(x<=29){
                  as.factor("Perdido")
                }
                else{
                  as.factor("Semanal constante")
                }
              }
            }
            else{
              as.factor("Perdido")
            }
          }
        }
      }
    }
    getEdad <- function(fecha_nacimiento){
      if(!is.na(fecha_nacimiento)){
        nacimiento <- strptime(fecha_nacimiento, format = "%Y-%m-%d")
        hoy <- Sys.Date()
        edad <- difftime(hoy,nacimiento)
        
        as.factor(as.integer(as.numeric(edad, units="days")/365))
      }
      else{
        as.factor(NA)
      }
    }
    cleanTicketsCanjeados <- function(tickets_canjeados){
      if(is.na(tickets_canjeados)){
        0
      }
      else{
        as.numeric(tickets_canjeados)
      }
    }
    generalRanges <- function(variable, range, unit){
      if(is.na(variable) || variable == 0){
        return(NA)
      }
      else{
        rango = as.integer(variable/range)
        paste("Entre",((rango*range)+1),"y",((rango*range)+range),unit)
      }
    }
    generalRangesWithZero <- function(variable, range, unit){
      if(is.na(variable)){
        return(NA)
      }
      else{
        rango = as.integer(variable/range)
        paste("Entre",((rango*range)),"y",((rango*range)+(range-1)),unit)
      }
    }
    testingOptimalK <- function(data,interval){
      optimistic <- as.integer(sqrt(nrow(data)/2))
      kmin <- optimistic-interval
      kmax <- optimistic+interval
      if(kmin <= 0){
        kmin <- 2
      }
      result <- data.frame(list(K = 1, betweens_to_max = 2, withins_to_min = 3))
      print(paste("Iterando entre",kmin,"y",kmax,"..."))
      current_row <- 0
      for(k in kmin:kmax){
        print(paste("k:",k))
        current_row <- current_row + 1
        kbet <- c()
        kwit <- c()
        for(i in 1:100){
          kmeans_result <- kmeans(data,k,iter.max = 40)
          kbet <- append(kbet,kmeans_result$betweenss)
          kwit <- append(kwit,kmeans_result$tot.withinss)
        }
        result[current_row,] <- c(k, mean(kbet), mean(kwit))
      }
      return(result)
    }
    testingOptimalKFixed <- function(data,kmin, kmax){
      #optimistic <- as.integer(sqrt(nrow(data)/2))
      #kmin <- optimistic-interval
      #kmax <- optimistic+interval
      #if(kmin <= 0){
        #kmin <- 2
      #}
      result <- data.frame(list(K = 1, betweens_to_max = 2, withins_to_min = 3))
      print(paste("Iterando entre",kmin,"y",kmax,"..."))
      current_row <- 0
      for(k in kmin:kmax){
        print(paste("k:",k))
        current_row <- current_row + 1
        kbet <- c()
        kwit <- c()
        for(i in 1:100){
          kmeans_result <- kmeans(data,k,iter.max = 40)
          kbet <- append(kbet,kmeans_result$betweenss)
          kwit <- append(kwit,kmeans_result$tot.withinss)
        }
        result[current_row,] <- c(k, mean(kbet), mean(kwit))
      }
      return(result)
    }
    testingOptimalKVector <- function(data,interval){
      optimistic <- as.integer(sqrt(length(data)/2))
      kmin <- optimistic-interval
      kmax <- optimistic+interval
      if(kmin <= 0){
        kmin <- 2
      }
      result <- data.frame(list(K = 1, betweens_to_max = 2, withins_to_min = 3))
      print(paste("Iterando entre",kmin,"y",kmax,"..."))
      current_row <- 0
      for(k in kmin:kmax){
        print(paste("k:",k))
        current_row <- current_row + 1
        kbet <- c()
        kwit <- c()
        for(i in 1:100){
          kmeans_result <- kmeans(data,k)
          kbet <- append(kbet,kmeans_result$betweenss)
          kwit <- append(kwit,kmeans_result$tot.withinss)
        }
        result[current_row,] <- c(k, mean(kbet), mean(kwit))
      }
      return(result)
    }
    releaseDifferenceToDays <- function(x){
      if(is.na(x) || x == 0 || x == -1){ 
        return(NA)
      }
      else {
        if(x < 0){
          return(NA)
        }
        else{
          return(round((x/60/60/24),0))
        }
      }
    }
    getAvgPpv <- function(views, points){
      if(points == 0 || views == 0){ 
        return(NA)
      }
      else { 
        return(points/views) 
      }
    }
    isDepleted <- function(saldo, points_given){
      if(saldo < 100 && points_given > 100){
        return(as.factor("1"))
      }
      else{
        return(as.factor("0"))
      }
    }
    getReleaseDay <- function(date){
      return(as.factor(weekdays(as.Date(date))))
    }
    max_na <- function(vector){
      maximo <- -Inf
      for(i in 1:length(vector)){
        if(!is.na(vector[i])){
          if(vector[i] > maximo){
            maximo <- vector[i]
          }
        }
      }
      return(maximo)
    }
    min_na <- function(vector){
      maximo <- Inf
      for(i in 1:length(vector)){
        if(!is.na(vector[i])){
          if(vector[i] < maximo){
            maximo <- vector[i]
          }
        }
      }
      return(maximo)
    }
    mean_na <- function(vector){
      registros <- 0
      total <- 0
      for(i in 1:length(vector)){
        if(!is.na(vector[i])){
          registros <- registros + 1
          total <- total + vector[i]
        }
      }
      return(total/registros)
    }
    fixCategoria <- function(cat){
      print(cat)
      if(!is.na(cat) && cat == "M"){
        return(as.factor("MISC"))
      }
      else{
        return(as.factor(cat))
      }
    }
    
  # LIBRARIES
  
    library(arules)
    library(klaR)
    library(cluster)
    library(graphics)
    library(fpc)
    library(optpart)
    library(Hmisc)
    library(foreach)
  
  # DATA
  
    users <- read.csv("../002 - Data/users.csv", header = TRUE, sep=";")
    videos <- read.csv("../002 - Data/videos.csv", header = TRUE, sep=";")
