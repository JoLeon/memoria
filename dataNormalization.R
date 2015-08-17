# Data

users <- read.csv("Data/users.csv", header = TRUE, sep=";")
videos <- read.csv("Data/videos.csv", header = TRUE, sep=";")

minmaxNormalization <- function(sample,min,max){
  return((sample-min)/(max-min))
}
zscoreNormalization <- function(sample,mean,sd){
  return((sample-mean)/(sd))
}

# SE usa z-score normalization ya que teine mejor tolerancia en torno a outliers (que es el caso)



max_puntos_historicos <- max(users$puntos_historicos)
min_puntos_historicos <- min(users$puntos_historicos)
mean_puntos_historicos <- mean(users$puntos_historicos)
sd_puntos_historicos <- sd(users$puntos_historicos)

users_mm <- data.frame(mm_puntos_historicos=mapply(minmaxNormalization, users$puntos_historicos, min_puntos_historicos, max_puntos_historicos))
users_zs <- data.frame(zs_puntos_historicos=mapply(zscoreNormalization, users$puntos_historicos, mean_puntos_historicos, sd_puntos_historicos))




max_puntos_gastados <- max(users$puntos_gastados)
min_puntos_gastados <- min(users$puntos_gastados)
mean_puntos_gastados <- mean(users$puntos_gastados)
sd_puntos_gastados <- sd(users$puntos_gastados)

users_mm$puntos_gastados <- mapply(minmaxNormalization, users$puntos_gastados, min_puntos_gastados, max_puntos_gastados)
users_zs$puntos_gastados <- mapply(zscoreNormalization, users$puntos_gastados, mean_puntos_gastados, sd_puntos_gastados)




max_shares_totales <- max(users$shares_totales)
min_shares_totales <- min(users$shares_totales)
mean_shares_totales <- mean(users$shares_totales)
sd_shares_totales <- sd(users$shares_totales)

users_mm$mm_shares_totales <- mapply(minmaxNormalization, users$shares_totales, min_shares_totales, max_shares_totales)
users_zs$zs_shares_totales <- mapply(zscoreNormalization, users$shares_totales, mean_shares_totales, sd_shares_totales)




max_concursos_participados <- max(users$concursos_participados)
min_concursos_participados <- min(users$concursos_participados)
mean_concursos_participados <- mean(users$concursos_participados)
sd_concursos_participados <- sd(users$concursos_participados)

users_mm$mm_concursos_participados <- mapply(minmaxNormalization, users$concursos_participados, min_concursos_participados, max_concursos_participados)
users_zs$zs_concursos_participados <- mapply(zscoreNormalization, users$concursos_participados, mean_concursos_participados, sd_concursos_participados)

