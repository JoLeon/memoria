# RELACIONES INTERESANTES

# submuestrar - sobremuestrar
# Utilizar herramientas de clasificacion
# Usar árboles y naive bayes? confusion matrix? prediciton (pasarle arbol) y performance

# OVERSAMPLING
#
# Videos:
#   Targets:
#     - Active users (+150) 
#     - Penetración (+70%)
#

#   Active users

    videos_base_active_users <- videos_processed[c("duracion", "shares_first_day", "new_users", "active_raffles", "release_difference_hours", "active_canjes", "active_users", "penetracion")]
    videos_base_active_users$success <- sapply(videos_base_active_users$active_users, function(actives){
      if(actives > 150){
        return(1)
      }
      return(0)
    })
    videos_base_active_users <- videos_base_active_users[c("duracion", "shares_first_day", "new_users", "active_raffles", "release_difference_hours", "active_canjes", "success")]
    
    active_users_success_group <- sqldf("SELECT * FROM videos_base_active_users WHERE success = 1")
    
    # Valores posibles para cada variable
    duracion <- sqldf("SELECT DISTINCT(duracion) FROM active_users_success_group")
    shares_first_day <- sqldf("SELECT DISTINCT(shares_first_day) FROM active_users_success_group")
    new_users <- sqldf("SELECT DISTINCT(new_users) FROM active_users_success_group")
    active_raffles <- sqldf("SELECT DISTINCT(active_raffles) FROM active_users_success_group")
    release_difference_hours <- sqldf("SELECT DISTINCT(release_difference_hours) FROM active_users_success_group")
    active_canjes <- sqldf("SELECT DISTINCT(active_canjes) FROM active_users_success_group")
    
    remaining_to_balance <- 609
    for (i in (1:remaining_to_balance)){
      print(paste("Iteraring ...", i,remaining_to_balance))
      random_row <- c(
        duracion$duracion[round(runif(1,1,length(duracion$duracion)))],
        shares_first_day$shares_first_day[round(runif(1,1,length(shares_first_day$shares_first_day)))],
        new_users$new_users[round(runif(1,1,length(new_users$new_users)))],
        active_raffles$active_raffles[round(runif(1,1,length(active_raffles$active_raffles)))],
        release_difference_hours$release_difference_hours[round(runif(1,1,length(release_difference_hours$release_difference_hours)))],
        active_canjes$active_canjes[round(runif(1,1,length(active_canjes$active_canjes)))],
        1
      )
      videos_base_active_users <- rbind(videos_base_active_users,random_row)
    }
    
# Penetracion
    
    videos_base_penetracion <- videos_processed[c("duracion", "shares_first_day", "new_users", "active_raffles", "release_difference_hours", "active_canjes", "active_users", "penetracion")]
    videos_base_penetracion$success <- sapply(videos_base_penetracion$penetracion, function(penetracion){
      if(penetracion > 0.7){
        return(1)
      }
      return(0)
    })
    videos_base_penetracion <- videos_base_penetracion[c("duracion", "shares_first_day", "new_users", "active_raffles", "release_difference_hours", "active_canjes", "active_users", "success")]
    
    active_users_success_group <- sqldf("SELECT * FROM videos_base_penetracion WHERE success = 1")
    
    # Valores posibles para cada variable
    duracion <- sqldf("SELECT DISTINCT(duracion) FROM active_users_success_group")
    shares_first_day <- sqldf("SELECT DISTINCT(shares_first_day) FROM active_users_success_group")
    new_users <- sqldf("SELECT DISTINCT(new_users) FROM active_users_success_group")
    active_raffles <- sqldf("SELECT DISTINCT(active_raffles) FROM active_users_success_group")
    release_difference_hours <- sqldf("SELECT DISTINCT(release_difference_hours) FROM active_users_success_group")
    active_canjes <- sqldf("SELECT DISTINCT(active_canjes) FROM active_users_success_group")
    active_users <- sqldf("SELECT DISTINCT(active_users) FROM active_users_success_group")
    
    remaining_to_balance <- 545
    for (i in (1:remaining_to_balance)){
      print(paste("Iteraring ...", i,remaining_to_balance))
      random_row <- c(
        duracion$duracion[round(runif(1,1,length(duracion$duracion)))],
        shares_first_day$shares_first_day[round(runif(1,1,length(shares_first_day$shares_first_day)))],
        new_users$new_users[round(runif(1,1,length(new_users$new_users)))],
        active_raffles$active_raffles[round(runif(1,1,length(active_raffles$active_raffles)))],
        release_difference_hours$release_difference_hours[round(runif(1,1,length(release_difference_hours$release_difference_hours)))],
        active_canjes$active_canjes[round(runif(1,1,length(active_canjes$active_canjes)))],
        active_users$active_users[round(runif(1,1,length(active_users$active_users)))],
        1
      )
      videos_base_penetracion <- rbind(videos_base_penetracion,random_row)
    }


#
# Users:
#   Targets:
#     - Quality
#     - Good/Bad quality