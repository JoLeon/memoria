# WIN

setwd("C:/Users/J/Documents/GitHub/memoria")
users_processed <- read.csv("002ProcessedData/users.csv", header = TRUE, sep=";")
videos_processed <- read.csv("002ProcessedData/videos.csv", header = TRUE, sep=";")

# Funcions

disRaffles <- function(raffles){
  if(raffles >= 9){
    return(as.factor("9 o más"))
  }
  if(raffles >= 6){
    return(as.factor("Entre 6 y 8"))
  }
  if(raffles >= 4){
    return(as.factor("Entre 4 y 5"))
  }
  return(as.factor("3 o menos"))
}
disActiveUsers <- function(users){
  if(users > 100){ return(as.factor("Más de 100"))}
  if(users > 90){ return(as.factor("(90, 100]"))}
  if(users > 80){ return(as.factor("(80, 90]"))}
  if(users > 70){ return(as.factor("(70, 80]"))}
  if(users > 60){ return(as.factor("(60, 70]"))}
  if(users > 50){ return(as.factor("(50, 60]"))}
  if(users > 40){ return(as.factor("(40, 50]"))}
  if(users > 30){ return(as.factor("(30, 40]"))}
  if(users > 20){ return(as.factor("(20, 30]"))}
  if(users > 10){ return(as.factor("(10, 20]"))}
  return(as.factor("Menos de 10"))
}
videos_discrete <- videos_processed
videos_discrete$duracion            <- discretize(videos_processed$duracion, method="cluster", categories=20)
videos_discrete$release_difference  <- discretize(videos_processed$release_difference, method="cluster", categories=10)
videos_discrete$total_views         <- discretize(v$total_views, method="cluster", categories=10)
videos_discrete$shares_first_day    <- discretize(videos_processed$shares_first_day, method="cluster", categories=10)
videos_discrete$shares_first_month  <- discretize(videos_processed$shares_first_month, method="cluster", categories=10)
videos_discrete$shares_first_week   <- discretize(videos_processed$shares_first_week, method="cluster", categories=10)
videos_discrete$total_shares        <- discretize(videos_processed$total_shares, method="cluster", categories=10)
videos_discrete$total_users         <- discretize(videos_processed$total_users, method="cluster", categories=10)
videos_discrete$active_users        <- sapply(videos_processed$active_users, disActiveUsers)
videos_discrete$new_users           <- discretize(videos_processed$new_users, method="cluster", categories=10)
videos_discrete$active_raffles      <- sapply(videos_processed$active_raffles, disRaffles)
videos_discrete$active_canjes       <- discretize(videos_processed$active_canjes, method="cluster", categories=5)
videos_discrete$penetracion         <- discretize(videos_processed$penetracion, method="cluster", categories=10)

videos_test <- videos_discrete[c(
  "active_raffles",
  "active_users"
)]

str(videos_discrete$active_raffles)

res <- apriori(videos_test)
inspect(res)

sqldf("SELECT * FROM videos_test WHERE active_raffles = '9 o más'")

sqldf("SELECT active_users FROM videos_processed WHERE active_raffles >= 9 AND active_users < 100")
