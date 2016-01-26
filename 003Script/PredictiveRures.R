library(party)


# Data

users <- read.csv("Data/users.csv", header = TRUE, sep=";")
videos <- read.csv("Data/videos.csv", header = TRUE, sep=";")


#
#   SACANDO OUTLIERS
#

users_clean <- users
str(users_clean)

# puntos_historicos
plot(users_clean$puntos_historicos)
plot(users_clean$puntos_historicos,ylim=c(0,80000))
users_clean <- subset(users_clean, puntos_historicos <= 80000)
plot(users_clean$puntos_historicos)

# puntos

plot(users_clean$puntos)
plot(users_clean$puntos,ylim=c(0,60000))
users_clean <- subset(users_clean, puntos <= 60000)
plot(users_clean$puntos)

# puntos_gastados

plot(users_clean$puntos_gastados)
plot(users_clean$puntos_gastados,ylim=c(0,60000))
users_clean <- subset(users_clean, puntos_gastados <= 60000)
plot(users_clean$puntos_gastados)

# shares_totales

plot(users_clean$shares_totales)
plot(users_clean$shares_totales,ylim=c(0,100))
users_clean <- subset(users_clean, shares_totales <= 100)
plot(users_clean$shares_totales)

# concursos_participados

plot(users_clean$concursos_participados)
plot(users_clean$concursos_participados,ylim=c(0,10))
users_clean <- subset(users_clean, concursos_participados <= 10)
plot(users_clean$concursos_participados)

# tickets_canjeados

plot(users_clean$tickets_canjeados)
plot(users_clean$tickets_canjeados,ylim=c(0,45))
users_clean$tickets_canjeados <- sapply(users_clean$tickets_canjeados, function(x){if(is.na(x)){ return(0)} else {return(x)} })
users_clean <- subset(users_clean, tickets_canjeados <= 45)
plot(users_clean$tickets_canjeados)


videos_clean <- videos
str(videos_clean)

# points_per_view

plot(videos_clean$points_per_view)
plot(videos_clean$points_per_view,ylim=c(0,110))
videos_clean <- subset(videos_clean, points_per_view <= 110)
plot(videos_clean$points_per_view)

# duracion

plot(videos_clean$duracion)
plot(videos_clean$duracion,ylim=c(0,600))
videos_clean <- subset(videos_clean, duracion <= 600)
plot(videos_clean$duracion)

# release_difference

plot(videos_clean$release_difference)
plot(videos_clean$release_difference,ylim=c(0,1000000))
videos_clean <- subset(videos_clean, release_difference <= 1000000)
videos_clean <- subset(videos_clean, release_difference >= 0)
plot(videos_clean$release_difference)

# total_views

plot(videos_clean$total_views)
plot(videos_clean$total_views,ylim=c(0,6500))
videos_clean <- subset(videos_clean, total_views <= 6500)
plot(videos_clean$total_views)

# points_given

plot(videos_clean$points_given)
plot(videos_clean$points_given,ylim=c(0,100000))
videos_clean <- subset(videos_clean, points_given <= 100000)
plot(videos_clean$points_given)

# shares_first_day

plot(videos_clean$shares_first_day)
plot(videos_clean$shares_first_day,ylim=c(0,15))
videos_clean <- subset(videos_clean, shares_first_day <= 15)
plot(videos_clean$shares_first_day)

# shares_first_week

plot(videos_clean$shares_first_week)
plot(videos_clean$shares_first_week,ylim=c(0,30))
videos_clean <- subset(videos_clean, shares_first_week <= 30)
plot(videos_clean$shares_first_week)

# total_shares

plot(videos_clean$total_shares)
plot(videos_clean$total_shares,ylim=c(0,80))
videos_clean <- subset(videos_clean, total_shares <= 80)
plot(videos_clean$total_shares)


videos_clean$avg_ppv <- mapply(function(views, points){if(points == 0 || views == 0){ return(NA) } else { return(points/views) }}, videos_clean$total_views, videos_clean$points_given)
videos$avg_ppv <- mapply(function(views, points){if(points == 0 || views == 0){ return(NA) } else { return(points/views) }}, videos$total_views, videos$points_given)

videos_clean$success <- mapply(function(views, shares){if(shares > 100 || views >= 8000){ return(1) } else { return(0) }}, videos_clean$total_views, videos_clean$total_shares)
videos$success <- mapply(function(views, shares){if(shares > 100 || views >= 8000){ return(1) } else { return(0) }}, videos$total_views, videos$total_shares)

keep <- c("category", "duracion", "release_difference", "shares_first_day", "X1_week_active_users_at_release","X1_week_active_raffles", "avg_ppv", "success")
videos_clean_kept <- videos_clean[keep]
videos_kept <- videos[keep]
videos_kept$success <- as.factor(videos_kept$success)

videos_kept <- videos_kept[videos_kept$release_difference != -1, ] #SACAR RELEASE DIFFERENCE -1 (NA)
videos_kept <- videos_kept[complete.cases(videos_kept), ] # SACAR ROWS CONMISSING VALUES

# Separando el dataframe en test y training

library(party)

# 3/4 de los datos para entrentamiento
smp_size <- floor(0.75 * nrow(videos_kept))

# Setteo de seen para que sea reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(videos_kept)), size = smp_size)

videos_train <- videos_kept[train_ind, ]
videos_test <- videos_kept[-train_ind, ]

# SUBMUESTREAR (LOS QUE SE REPITEN MÁS, EN ESTE CASO LOS "0", Y SUPERMUESTRAR LOS QUE SEAN MENOS, EN ESTE CASO "1")

videos_tree <- ctree(success ~ ., data = videos_train)
plot(videos_tree)

library(klaR)

videos_bayes <- NaiveBayes(success ~ ., data = videos_train)
videos_bayes

predict(videos_bayes)
plot(videos_bayes)
