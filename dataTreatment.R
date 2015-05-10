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
		(inversion_total - saldo_actual) AS points_given,
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
		u.id, 
		puntos_historicos, 
		puntos, 
		u.created_at AS fecha_afiliacion, 
		IFNULL(universidades.nombre,'NA') as uni, 
		IFNULL(IFNULL(genero,infos.sexo),'NA') as genero, 
		IFNULL(infos.f_nacimiento,'NA') as nacimiento,
		(puntos_historicos - puntos) as puntos_gastados,
		(SELECT
			count(1)
		FROM
			shares
		WHERE
			shares.user_id = u.id
		) AS shares_totales,
		(SELECT
			categoria_id
		FROM
			videos
			JOIN shares ON shares.video_id = videos.id
		WHERE
			shares.user_id = u.id
			AND videos.categoria_id != 0
		GROUP BY
			categoria_id
		ORDER BY
			count(categoria_id)
		LIMIT 
			1
		) AS categoria_dominante,
		(SELECT
			count(1)
		FROM
			users
		WHERE
			referenced_by = u.id
		) AS recruitments,
		(SELECT
			count(distinct canjes.producto_id)
		FROM
			canjes
			JOIN productos ON productos.id = canjes.producto_id
		WHERE
			canjes.user_id = u.id
			AND productos.concurso = 1
		) AS concursos_participados,
		(SELECT
			count(1)
		FROM
			canjes
			JOIN productos ON productos.id = canjes.producto_id
		WHERE
			canjes.user_id = u.id
			AND productos.concurso = 0
		) AS premios_canjeados,
		(SELECT
			sum(canjes.cantidad)
		FROM
			canjes
			JOIN productos ON productos.id = canjes.producto_id
		WHERE
			canjes.user_id = u.id
			AND productos.concurso = 1
		) AS tickets_canjeados
	FROM 
		users AS u
		LEFT JOIN universidades ON u.universidad_id = universidades.id 
		LEFT JOIN infos ON infos.user_id = u.id 
	WHERE 
		u.tipo=1 
		AND u.estado=1
		AND u.id != 0
		AND u.id != 8"
)
write.table(
	users, 
	file="/home/jleon/Memoria/Data/users.csv", 
	append=FALSE, 
	quote=FALSE, 
	sep=";", 
	eol="\n", 
	na="NA", 
	row.names=FALSE, 
	col.names=TRUE, 
	fileEncoding="UTF-8"
)
# QUERY PARA VISTAS
views <- dbGetQuery(con,
	"SELECT
		views.created_at AS Fecha,
		views.locale_country AS Pais,
		views.country_code AS 'Codigo pais',
		views.locale_city AS Ciudad,
		shares.id AS Share,
		shares.video_id AS Video
	FROM
		views
		JOIN shares ON shares.id = views.share_id"
)
write.table(
	views, 
	file="/home/jleon/Memoria/Data/views.csv", 
	append=FALSE, 
	quote=FALSE, 
	sep=";", 
	eol="\n", 
	na="NA", 
	row.names=FALSE, 
	col.names=TRUE, 
	fileEncoding="UTF-8"
)
