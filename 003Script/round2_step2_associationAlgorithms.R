# WIN

setwd("C:/Users/J/Documents/GitHub/memoria")
users_processed <- read.csv("002ProcessedData/users.csv", header = TRUE, sep=";")
videos_processed <- read.csv("002ProcessedData/videos.csv", header = TRUE, sep=";")

# UBUNTU

users_processed <- read.csv("/home/jleon/memoria/002ProcessedData/users.csv", header = TRUE, sep=";")
videos_processed <- read.csv("/home/jleon/memoria/002ProcessedData/videos.csv", header = TRUE, sep=";")

# Discretizing

users_discrete <- users_processed[c("puntos_historicos", "genero", "dia_afiliacion", "shares_totales", "recruitments", "concursos_participados", "edad", "quality", "sistema_registro", "densidad_videos_semanas_registro", "calidad_videos", "densidad_concursos")]
users_discrete$puntos_historicos <- discretize(users_discrete$puntos_historicos, method="cluster", categories=5)
users_discrete$shares_totales <- discretize(users_discrete$shares_totales, method="cluster", categories=5)
users_discrete$concursos_participados <- discretize(users_discrete$concursos_participados, method="cluster", categories=5)
users_discrete$edad <- discretize(users_discrete$edad, method="cluster", categories=5)
users_discrete$recruitments <- sapply(users_discrete$recruitments, function(rec){
  if(rec >0){
    return(as.factor(1)) 
  } 
  return(as.factor((0)))
})
users_discrete$genero <- sapply(users_discrete$genero, function(g){
  if(is.null(g) || is.na(g)){
    return(as.factor(sample(c("M","F"), size = 1, replace = TRUE, prob = c(0.5,0.5))))
  }
  return(as.factor(g))
})
# VIDEOS

disRaffles <- function(raffles){
  if(raffles >= 10){
    return(as.factor("10 o m?s"))
  }
  if(raffles >= 7){
    return(as.factor("Entre 7 y 9"))
  }
  if(raffles >= 5){
    return(as.factor("Entre 5 y 6"))
  }
  if(raffles >= 3){
    return(as.factor("Entre 3 y 4"))
  }
  return(as.factor("2 o menos"))
}
disActiveUsers <- function(users){
  if(users > 160){ return(as.factor("M?s de 160"))}
  if(users > 120){ return(as.factor("(120, 160]"))}
  if(users > 80){ return(as.factor("(80, 120]"))}
  if(users > 40){ return(as.factor("(40, 80]"))}
  return(as.factor("[0, 40]"))
}
disDuracion <- function(duracion){
  if(duracion <= 30){
    return(as.factor("30s o menos"))
  }
  if(duracion <= 60){
    return(as.factor("(30, 60]"))
  }
  if(duracion <= 120){
    return(as.factor("(60, 120]"))
  }
  if(duracion <= 180){
    return(as.factor("(120, 180]"))
  }
  if(duracion <= 240){
    return(as.factor("(180, 240]"))
  }
  if(duracion <= 300){
    return(as.factor("(240, 300]"))
  }
  if(duracion < 600){
    return(as.factor("(300, 600]"))
  }
  return(as.factor("600 o m?s"))
}
disRelease <- function(difference){
  if(difference <= 6){ return(as.factor("[0, 6]"))}
  if(difference <= 24){ return(as.factor("Entre 6 horas y 1 d?a"))}
  if(difference <= 72){ return(as.factor("Entre 1 y 3 d?as"))}
  if(difference <= 168){ return(as.factor("Entre 3 d?as y 1 semana"))}
  if(difference <= 372){ return(as.factor("Entre 1 y 2 semanas"))}
  return(as.factor("M?s de 2 semanas"))
}
disPenetracion <- function(penetracion){
  if(penetracion > 0.9){ return(as.factor("M?s del 90%"))}
  if(penetracion >= 0.8){ return(as.factor("Entre 80% y 90%"))}
  if(penetracion >= 0.7){ return(as.factor("Entre 70% y 80%"))}
  if(penetracion >= 0.6){ return(as.factor("Entre 60% y 70%"))}
  if(penetracion >= 0.5){ return(as.factor("Entre 50% y 60%"))}
  if(penetracion >= 0.4){ return(as.factor("Entre 40% y 50%"))}
  if(penetracion >= 0.3){ return(as.factor("Entre 30% y 40%"))}
  if(penetracion >= 0.2){ return(as.factor("Entre 20% y 30%"))}
  if(penetracion >= 0.1){ return(as.factor("Entre 10% y 20%"))}
  return(as.factor("Menos del 10%"))
}
videos_discrete <- videos_processed
videos_discrete$active_users                  <- sapply(videos_processed$active_users, disActiveUsers)
videos_discrete$active_raffles                <- sapply(videos_processed$active_raffles, disRaffles)
videos_discrete$duracion                      <- sapply(videos_processed$duracion, disDuracion)
videos_discrete$release_difference_hours      <- sapply(videos_processed$release_difference_hours, disRelease)
videos_discrete$penetracion                   <- sapply(videos_processed$penetracion, disPenetracion)

videos_relation_1 <- videos_discrete[c(
  "active_raffles",
  "active_users"
)]

videos_relation_2<- videos_discrete[c(
  "duracion",
  "release_difference_hours",
  "penetracion"
)]

apriori_videos_appereance_list = list(lhs = c("active_raffles=10 o m?s", "active_raffles=Entre 7 y 9", "active_raffles=Entre 5 y 6", "active_raffles=Entre 3 y 4", "active_raffles=2 o menos"),default = "rhs")
apriori_relation_test <- apriori(videos_discrete, parameter =list(support=0.1,confidence=0.4))


apriori_videos_appereance_list = list(
  lhs = c(
    "duracion=30s o menos",
    "duracion=(30, 60]",
    "duracion=(60, 120]",
    "duracion=(120, 180]",
    "duracion=(180, 240]",
    "duracion=(240, 300]",
    "duracion=(300, 600]",
    "duracion=600 o m?s",
    "release_difference_hours=[0, 6]",
    "release_difference_hours=Entre 6 horas y 1 d?a",
    "release_difference_hours=Entre 1 y 3 d?as",
    "release_difference_hours=Entre 3 d?as y 1 semana",
    "release_difference_hours=Entre 1 y 2 semanas"
  ),
  default = "rhs"
)
apriori_relation_2 <- apriori(videos_relation_2, parameter =list(support=0.005,confidence=0.4), appearance = apriori_videos_appereance_list)


#USUARIOS

users_relations <- users_processed[c("quality", "sistema_registro","densidad_videos","calidad_videos","densidad_concursos","densidad_videos_semanas_registro")]

apriori_users_appeareance_list = list(
  rhs = c(
    "quality=Not interested/Didn't get it",
    "quality=Not captured",
    "quality=Lost"
  ),
  default = "lhs"
)
apriori_relation_3 <- apriori(users_relations, parameter =list(support=0.1,confidence=0.5), appearance = apriori_users_appeareance_list)

apriori_users_appeareance_list = list(
  rhs = c(
    "quality=Daily, for a month",
    "quality=Daily, for a week",
    "quality=Daily, constant",
    "quality=Weekly, for a month",
    "quality=Weekly, constant"
    
  ),
  default = "lhs"
)
apriori_relation_4 <- apriori(users_relations, parameter =list(support=0.005,confidence=0.1), appearance = apriori_users_appeareance_list)


simplified_user_relations <- users_relations
simplified_user_relations$good_user <- sapply(users_relations$quality,function(q){
  if(q == "Not interested/Didn't get it" || q == "Not captured" || q == "Lost"){
    return(as.factor(0))
  }
  return(as.factor(1))
})
simplified_user_relations <- simplified_user_relations[c("good_user", "sistema_registro","densidad_videos","calidad_videos","densidad_concursos","densidad_videos_semanas_registro")]
apriori_users_appeareance_list = list(
  rhs = c(
    "good_user=1"
    
  ),
  default = "lhs"
)
apriori_relation_5 <- apriori(simplified_user_relations, parameter =list(support=0.01,confidence=0.3), appearance = apriori_users_appeareance_list)

apriori_users_appeareance_list = list(
  rhs = c(
    "good_user=0"
    
  ),
  default = "lhs"
)
apriori_relation_6 <- apriori(simplified_user_relations, parameter =list(support=0.2,confidence=0.3), appearance = apriori_users_appeareance_list)

# RELACIONES INTERESANTES
  # submuestrar - sobremuestrar
  # Utilizar herramientas de clasificacion
  # Usar ?rboles y naive bayes? confusion matrix? prediciton (pasarle arbol) y performance

  # Active raffles vs active users
  
    inspect(apriori_relation_1)
    # El soporte es peque?o porque la cantidad de datos "de inter?s" es peque?a
    observaciones <- sqldf("SELECT count(*) as total FROM videos");
    videos_mas_160_actives <- sqldf("SELECT count(*) as total FROM videos WHERE active_users > 160")
    
    observaciones$total
    videos_mas_160_actives$total
    
    videos_mas_160_actives$total/observaciones$total
    
    # Aproximadamente un 11% de los datos tienen m?s de 160 usuarios activos al momento del lanzamiento

  # Duraci?n y release difference vs penetraci?n
    
    inspect(apriori_relation_2)
    
    # Rangos peque?os de confianza, justamente por la baja cantidad de casos de "?xito" contra casos totales
    
  # Relaciones entre caldiades usuarias negativas
    
    # Objetivo: qu? variables influyen m?s en un usuario de calidad negativa? (perdido, no capturado, no interesado/no entendi?)
    # No interesado, no entendi?: No comparti? nada jam?s (S?lo registro)
    # No capturado: Interectu? con la plataforma un ?nico d?a
    # Perdido: Interactu? con la plataforma pormenos de una semana
    
    # Densidad concursos:
      # Low               1 - 2
      # Somewhat Low      3 - 4
      # Regular           5 - 6
      # Somewhat high     7 - 8
      # High              9 o m?s
    
    # Densidad videos:
      # Low               1 - 3
      # Somewhat Low      4 - 5
      # Regular           6 - 7
      # Somewhat high     8 - 9
      # High              10 o m?s
    
    # Calidad videos:
      # Low               p < 20%
      # Somewhat Low      20% <= p < 40%
      # Regular           40% <= p < 60%
      # Somewhat high     50% <= p < 80%
      # High              60% <= p >= 80%
    
    inspect(apriori_relation_3)
  
  # Relaciones entre calidades usuarias positivas
    
    inspect(apriori_relation_4)
    
  # Relaciones calidad positiva agrupada vs resto
    
    inspect(apriori_relation_5)
  
  # Relaciones calidad negativa agrupada vs resto 
    
    inspect(apriori_relation_6)
  
# Todo junto?
  all_rules <- apriori(users_discrete, parameter =list(support=0.2,confidence=0.7,target="rules"))
  inspect(sort(all_rules, decreasing= FALSE, by ="lift"))
  