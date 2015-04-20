#Data treatment for Kikvi Database analysis
#Dependencies
if(!require("RMySQL")){
  install.packages("RMySQL",dep=TRUE)
}
warn=1
con <- dbConnect(MySQL(), user="root", password="1234", dbname="mem_playgue", host="localhost")
# QUERY PARA VIDEOS
videos <- dbGetQuery(con, 
	"SELECT 
		videos.id, 
		url, 
		categorias.nombre AS category, 
		puja AS points_per_view, 
		videos.created_at AS release_date,
		views AS total_views,
		#
		#
		#			INFORMACION DE CANTIDAD DE SHARES
		#
		# * Primer dia
		# * Primera semana
		# * Primer mes
		# * Totales
		# * Males
		# * Females
		#
		(SELECT
			count(shares.id)
		FROM
			shares
		WHERE
			shares.video_id = videos.id
			AND shares.created_at <= DATE_ADD(videos.created_at, INTERVAL 1 DAY)
		GROUP BY
			videos.id
		) AS shares_first_day,
		(SELECT
			count(shares.id)
		FROM
			shares
		WHERE
			shares.video_id = videos.id
			AND shares.created_at <= DATE_ADD(videos.created_at, INTERVAL 1 WEEK)
		GROUP BY
			videos.id
		) AS shares_first_week,
		(SELECT
			count(shares.id)
		FROM
			shares
		WHERE
			shares.video_id = videos.id
			AND shares.created_at <= DATE_ADD(videos.created_at, INTERVAL 1 MONTH)
		GROUP BY
			videos.id
		) AS shares_first_month,
		(SELECT 
			count(1)
		FROM 
			shares 
			JOIN users ON users.id = shares.user_id 
		WHERE 
			video_id = videos.id 
			AND IFNULL(users.genero, (SELECT sexo FROM infos WHERE infos.user_id = users.id)) = 'M'
		) AS male_shares,
		(SELECT 
			count(1)
		FROM 
			shares 
			JOIN users ON users.id = shares.user_id 
		WHERE 
			video_id = videos.id 
			AND IFNULL(users.genero, (SELECT sexo FROM infos WHERE infos.user_id = users.id)) = 'F'
		) AS female_shares,
		(SELECT
			count(shares.id)
		FROM
			shares
		WHERE
			shares.video_id = videos.id
		GROUP BY
			videos.id
		) AS total_shares,
		#
		#
		#			INFORMACION DE CANTIDAD DE USUARIOS
		#
		#
		# * Usuarios @ release
		# * Usuarios activos la semana anterior al lanzamiento
		# * Usuarios activos hasta 2 semanas anteriores al lanzamiento
		# * Usuarios nuevos la semana anteriore al lanzamiento
		# * Usuarios nuevos hasta 2 semanas anteriores al lanzamiento
		#
		(SELECT
			count(users.id)
		FROM
			users
		WHERE
			users.created_at <= videos.created_at
		) AS total_users_at_release,
		(SELECT
			count(distinct shares.user_id)
		FROM
			shares
		WHERE 
			shares.created_at >= DATE_SUB(videos.created_at, INTERVAL 1 WEEK)
			AND shares.created_at <= videos.created_at
		) AS 1_week_active_users_at_release,
		(SELECT
			count(distinct shares.user_id)
		FROM
			shares
		WHERE 
			shares.created_at >= DATE_SUB(videos.created_at, INTERVAL 2 WEEK)
			AND shares.created_at <= videos.created_at
		) AS 2_week_active_users_at_release,
		(SELECT
			count(*)
		FROM
			users
		WHERE 
			users.created_at >= DATE_SUB(videos.created_at, INTERVAL 1 WEEK)
			AND users.created_at <= videos.created_at
		) AS 1_week_new_users_at_release,
		(SELECT
			count(*)
		FROM
			users
		WHERE 
			users.created_at >= DATE_SUB(videos.created_at, INTERVAL 2 WEEK)
			AND users.created_at <= videos.created_at
		) AS 2_week_new_users_at_release,
		#
		#
		#			INFORMACION DE CANTIDADES DE CONCURSOS
		#
		# * Raffles @ release
		# * Raffles primera semana
		# * Raffles primer mes
		#
		(SELECT
			count(*)
		FROM
			productos
		WHERE
			concurso = 1
			AND fecha_concurso >= videos.created_at
			AND productos.created_at <= videos.created_at
		) AS active_raffles_at_release,
		(SELECT
			count(*)
		FROM
			productos
		WHERE
			concurso = 1
			AND fecha_concurso >= videos.created_at
			AND productos.created_at <= DATE_ADD(videos.created_at, INTERVAL 1 WEEK)
		) AS 1_week_active_raffles,
		(SELECT
			count(*)
		FROM
			productos
		WHERE
			concurso = 1
			AND fecha_concurso >= videos.created_at
			AND productos.created_at <= DATE_ADD(videos.created_at, INTERVAL 2 WEEK)
		) AS 2_week_active_raffles
	FROM 
		videos 
		LEFT JOIN categorias ON categorias.id=categoria_id
	WHERE 
		videos.views > 0 
	ORDER BY 
		total_views DESC, release_date ASC"
)
write.table(
	videos, 
	file="/home/jleon/Memoria/Data/videos.csv", 
	append=FALSE, 
	quote=FALSE, 
	sep=";", 
	eol="\n", 
	na="NA", 
	row.names=FALSE, 
	col.names=TRUE, 
	fileEncoding="UTF-8"
)
# QUERY PARA USUARIOS
users <- dbGetQuery(con, 
	"SELECT 
		users.id, 
		puntos_historicos, 
		puntos, 
		users.created_at AS fecha_afiliacion, 
		IFNULL(universidades.nombre,'No especificado') as uni, 
		IFNULL(IFNULL(generos,infos.sexo),'No especificado') as genero, 
		IFNULL(infos.f_nacimiento,'No especificado') as nacimiento 
	FROM 
		users 
		LEFT JOIN universidades ON users.universidad_id=universidades.id 
		LEFT JOIN infos ON infos.user_id = users.id 
	WHERE 
		users.tipo=1 
		AND users.estado=1
		AND users.id != 0
		AND user.id != 8"
)
