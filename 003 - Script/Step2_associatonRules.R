# Reglas de asociación

  # APRIORI

    # USERS
      apriori_users <- apriori(users_dis, list(support=0.6))
      inspect(apriori_users)
      
      # La gran cantidad de datos en 0, dan una lista de reglas sin valor, se modifican los parámetros de la función (lógica-> esconder los rangos 0)
      apriori_users_appereance_list = list(
        none = c(
          "rango_tickets_canjeados=[  0.00,  6.43)",
          "rango_puntos_historicos=[     0,  4651)",
          "rango_puntos_gastados=[     0,  4252)",
          "rango_shares_totales=[  0.00,  8.69)",
          "rango_concursos_participados=[ 0.00, 1.21)",
          "rango_shares_frequecy=[  0.00,  3.93)",
          "rango_total_activity=[  0.0, 26.7)"   
        ),
        default = "both"
      )
      apriori_users2 <- apriori(users_dis, parameter =list(support=0.05,confidence=0.4), appearance = apriori_users_appereance_list)
      inspect(apriori_users2)
      
      # Las reglas resultantes siguen sin entregar información de valor, se deshecha algoritmo apriori
      
    # VIDEOS
      apriori_videos <- apriori(videos_dis, paramete = list(support=0.8, confidence=0.8))
      inspect(apriori_videos)
      
      # Interesa saber si hay algún patrón que signifique que el video es compartido hasta que se depleta
      apriori_videos_appereance_list = list(
        rhs = c(
          "is_depleted=1"   
        ),
        default = "lhs"
      )
      apriori_videos_depleted <- apriori(videos_dis, parameter = list(support=0.5, confidence=0.7), appearance = apriori_videos_appereance_list)
      inspect(apriori_videos_depleted)
      # No se encontraron reglas de asociacion para este caso

  # ECLAT

    # USERS
      eclat_users <- eclat(users_dis, list(support=0.7))
      inspect(eclat_users)
      
    # VIDEOS
      eclat_videos <- eclat(videos_dis, list(support=0.7))
      inspect(eclat_videos)
  
