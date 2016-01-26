# Reglas de asociación

  # APRIORI

    # USERS
      apriori_users <- apriori(users_dis, list(support=0.6))
      inspect(apriori_users)
      
      # La gran cantidad de datos en 0, dan una lista de reglas sin valor, se modifican los parámetros de la función (lógica-> esconder los rangos 0)
      apriori_users_appereance_list = list(
        none = c(
          "rango_tickets_canjeados=[  0.00,  9.05)",
          "rango_puntos_historicos=[     0,  4651)",
          "rango_puntos_gastados=[     0,  4252)",
          "rango_shares_totales=[  0.00,  9.29)",
          "rango_concursos_participados=[ 0.00, 1.37)",
          "rango_shares_frequecy=[  0.00,  3.93)",
          "rango_total_activity=[  0.0, 23.2)"   
        ),
        default = "both"
      )
      apriori_users2 <- apriori(users_dis, parameter =list(support=0.05,confidence=0.4), appearance = apriori_users_appereance_list)
      inspect(apriori_users2)
      
      keep_users_dis <- c(
        "categoria_dominante", 
        "uni", 
        "genero", 
        "hora_afiliacion", 
        "dia_afiliacion", 
        "edad", 
        "rango_shares_totales" 
      )
      users_dis_reduced <- users_dis[keep_users_dis]
      apriori_users_appereance_list = list(
        none = c(
          "rango_shares_totales=[  0.00,  9.29)"
        ),
        default = "both"
      )
      apriori_users_reduced <- apriori(users_dis_reduced, parameter=list(support=0.01), appearance = apriori_users_appereance_list)
      inspect(apriori_users_reduced)
      
      # Las reglas resultantes siguen sin entregar información de valor, se deshecha algoritmo apriori
      
    # VIDEOS
      apriori_videos <- apriori(videos_dis, parameter = list(support=0.8, confidence=0.8))
      inspect(apriori_videos)
      
      # Interesa saber si hay algún patrón que signifique que el video es compartido hasta que se depleta
      apriori_videos_appereance_list = list(
        rhs = c(
          "is_depleted=1"   
        ),
        default = "lhs"
      )
      apriori_videos_depleted <- apriori(videos_dis, parameter = list(support=0.01, confidence=0.1), appearance = apriori_videos_appereance_list)
      inspect(apriori_videos_depleted)
      # No se encontraron reglas de asociacion para este caso
      
      keep_videos_dis <- c(
        "release_day",
        "rango_duracion",
        "rango_total_views",
        "rango_active_users",
        "rango_avg_ppv",
        "rango_total_shares"
      )
      videos_dis_reduced <- videos_dis[keep_videos_dis]
      apriori_videos_reduced_appereance_list = list(
        none = c(
          "rango_total_views=[    1,  386)",
          "rango_avg_ppv=[ 27.423, 44.946)"
        ),
        default = "both"
      )
      apriori_videos_reduced <- apriori(videos_dis_reduced, parameter = list(support=0.1, confidence=0.6), appearance = apriori_videos_reduced_appereance_list)
      inspect(apriori_videos_reduced)    

  # ECLAT

    # USERS
      
      keep_users_eclat_dis <- c(
        "uni",
        "genero",
        "edad",
        "rango_shares_totales",
        "rango_total_activiy"
      )
      users_eclat_dis = users_dis[keep_users_eclat_dis]
      eclat_users <- eclat(users_eclat_dis, list(support=0.7))
      inspect(eclat_users)
      
    # VIDEOS
      keep_videos_eclat_dis <- c(
        "category",
        "release_day",
        "rango_duracion",
        "rango_total_views",
        "rango_total_shares",
        "rango_active_users",
        "rango_active_raffles",
        "rango_shares_first_week",
        "rango_points_given",
        "rango_release_difference"
      )
      videos_eclat_dis = videos_dis[keep_videos_eclat_dis]
      eclat_videos <- eclat(videos_eclat_dis, list(support=0.7))
      inspect(eclat_videos)
  
