# Cargar librerias
# Basicos de UI
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(DT)
library(waiter)

# Manipulacion de datos
library(data.table)
library(dplyr)
library(stringr)
library(lubridate)

# Visualizacion
library(echarts4r)

# Conexion Mongo
library(mongolite)

# Extras utiles
library(readxl)
library(openxlsx)
library(tools)
library(stringi)

library(openxlsx)

library(AzureAuth)
library(httr)

###########################################################################################################################

splash_screen <- waiter::spin_flower()

#########################################################################################################################
###### Funciones
convertir_fecha_segura <- function(fecha_col) {
	suppressWarnings({
		# Prueba varios formatos comunes
		posible <- as.Date(fecha_col, format = "%Y-%m-%d")
		if (all(is.na(posible))) posible <- as.Date(fecha_col, format = "%Y/%m/%d")
		if (all(is.na(posible))) posible <- as.Date(fecha_col, format = "%d/%m/%Y")
		if (all(is.na(posible))) posible <- as.Date(fecha_col, format = "%Y%m%d")
		return(posible)
	})
}

# 🔷 Función general
buscar_pdfs_sharepoint <- function(tenant_id, client_id, client_secret, dominio, sitio, patron) {
	# 🔑 Autenticación
	cat("🔑 Autenticando…\n")
	token <- get_azure_token(
		resource = "https://graph.microsoft.com",
		tenant   = tenant_id,
		app      = client_id,
		password = client_secret
	)

	access_token <- token$credentials$access_token
	headers <- add_headers(
		Authorization = paste("Bearer", access_token),
		`Content-Type` = "application/json"
	)

	# 🌐 site_id
	cat("🌐 Obteniendo site_id…\n")
	site_info <- GET(
		url = paste0("https://graph.microsoft.com/v1.0/sites/", dominio, ":/sites/", sitio),
		headers
	)
	site_id <- content(site_info, as = "parsed", type = "application/json")$id
	cat("✅ site_id: ", site_id, "\n")

	# 💾 drive_id
	cat("💾 Obteniendo drive_id…\n")
	drive_info <- GET(
		url = paste0("https://graph.microsoft.com/v1.0/sites/", site_id, "/drive"),
		headers
	)
	drive_id <- content(drive_info, as = "parsed", type = "application/json")$id
	cat("✅ drive_id: ", drive_id, "\n")

	# 🔍 Buscar PDFs
	cat("🔍 Buscando PDFs con patrón: ", patron, "\n")
	url <- paste0(
		"https://graph.microsoft.com/v1.0/drives/", drive_id,
		"/root/search(q='", patron, "')"
	)
	resp <- GET(url, headers)
	result <- content(resp, as = "parsed", type = "application/json")

	if (!is.null(result$value) && length(result$value) > 0) {
		# capturar id, name y webUrl
		archivos <- lapply(result$value, function(x) {
			list(id = x$id, name = x$name, webUrl = x$webUrl)
		})
		archivos_df <- do.call(rbind, lapply(archivos, as.data.frame))
		pdfs <- archivos_df[grepl("\\.pdf$", archivos_df$name, ignore.case = TRUE), ]
		cat("✅ Se encontraron ", nrow(pdfs), " PDFs.\n")
		return(pdfs)
	} else {
		cat("⚠️ No se encontraron PDFs para el patrón indicado.\n")
		return(NULL)
	}
}


descargar_pdf_sharepoint <- function(tenant_id, client_id, client_secret,dominio, sitio, archivo_id, destino) {
	cat("🔑 Autenticando…\n")
	token <- get_azure_token(
		resource = "https://graph.microsoft.com",
		tenant   = tenant_id,
		app      = client_id,
		password = client_secret
	)

	access_token <- token$credentials$access_token
	headers <- add_headers(
		Authorization = paste("Bearer", access_token)
	)

	# 🌐 site_id
	site_info <- GET(
		url = paste0("https://graph.microsoft.com/v1.0/sites/", dominio, ":/sites/", sitio),
		headers
	)
	site_id <- content(site_info, as = "parsed", type = "application/json")$id
	cat("✅ site_id: ", site_id, "\n")

	# 💾 drive_id
	drive_info <- GET(
		url = paste0("https://graph.microsoft.com/v1.0/sites/", site_id, "/drive"),
		headers
	)
	drive_id <- content(drive_info, as = "parsed", type = "application/json")$id
	cat("✅ drive_id: ", drive_id, "\n")

	# 📥 Descargar archivo
	cat("📥 Descargando archivo con id: ", archivo_id, "\n")
	resp <- GET(
		url = paste0(
			"https://graph.microsoft.com/v1.0/drives/", drive_id,
			"/items/", archivo_id, "/content"
		),
		headers,
		write_disk(destino, overwrite = TRUE)
	)

	if (status_code(resp) == 200) {
		cat("✅ Archivo descargado en: ", destino, "\n")
		return(destino)
	} else {
		cat("❌ Error al descargar archivo\n")
		return(NULL)
	}
}


consulta <- function(Ano = "2025", Mes = c("Mayo"), Administradora = "CCF03") {

	# ---------------- Preparacion ----------------
	meses <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
		"Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre"
	)

	Mes <- intersect(Mes, meses)  # Validar meses validos
	if (length(Mes) == 0) return(data.table())  # No hay meses validos

	connection_string <- 'mongodb://laumar92:wwEVSfQDH1USAJNF@clniyago-shard-00-00.vbdrs.mongodb.net:27017,clniyago-shard-00-01.vbdrs.mongodb.net:27017,clniyago-shard-00-02.vbdrs.mongodb.net:27017/Niyaraky?ssl=true&replicaSet=atlas-mjdt39-shard-0&authSource=admin&retryWrites=true&w=majority'
	mongo_conn <- mongo(collection = "NY_Asignaciones_P", url = connection_string)
	
	# ---------------- Cargar Datos Asignaciones ----------------
	datos_lista <- lapply(Mes, function(mes_unico) {
		query <- sprintf('{"ano_asigna": "%s", "mes_asigna": "%s", "cod_administradora": "%s"}',
		Ano, mes_unico, Administradora)
		resultado <- mongo_conn$find(query)
		if (length(resultado) == 0) return(NULL)
		return(data.table(resultado))
	})

	Datos <- rbindlist(datos_lista, fill = TRUE)

	if (nrow(Datos) == 0) return(Datos)  # Ningun mes tenia datos

	# ---------------- Validar nombres esperados ----------------
	nombres_esperados <- c(
		"numero_de_documento_del_aportante", "accion_de_cobro", "cod_administradora", 
		"nombre_administradora", "fecha_afiliaciion_aportatante", "actividad_economica_del_aportante",
		"tipo_aportante", "tipo_de_documento_del_aportante", "nombre_o_razon_social_del_aportante",
		"nombre_representante_legal", "celular_representante_legal", "correo_representante_legal",
		"persona_de_contacto", "correo_persona_de_contacto", "celular_persona_de_contacto",
		"direccion", "ciudad", "departamento", "telefono_1", "telefono_2", "celular",
		"correo_electronico_1", "correo_electronico_2", "fecha_limite_de_pago",
		"estado_del_aportante", "fecha_ultima_accion_de_cobro", "ultima_accion_de_cobro_cruce",
		"respuesta", "observacion", "tipo_documento_cotizante", "numero_de_documento_del_cotizante",
		"nombre_del_cotizante", "fecha_afiliacion_cotizante", "telefono_del_cotizante",
		"celular_del_cotizante", "estado_del_cotizante", "periodo_de_mora", "valor_de_la_cartera",
		"salario", "estado_de_la_matricula", "fecha_de_cancelacion", "fecha_ultima_actualizacion",
		"telefono_comercial", "telefono_fiscal", "correo_electronico_comercial", "correo_electronico_fiscal",
		"accion", "cantidad_periodos_mora", "periodo_mora_desde", "periodo_mora_hasta",
		"periodo_mora_desde_fecha", "periodo_mora_hasta_fecha", "dias_mora", "edad_mora",
		"ano", "cant_trabajadores_mora", "valor_cartera", "valor_cartera_detalle", "pareto_mora",
		"estrategia", "ano_asigna", "mes_asigna", "razon_social"
	)

	columnas_presentes <- colnames(Datos)

	# Asegurarse de que 'nombres_esperados' contenga solo columnas que existan en 'Datos'
	nombres_esperados_presentes <- intersect(nombres_esperados, columnas_presentes)

	# Reordenar las columnas en 'Datos' solo con las que estan presentes
	Datos <- Datos[, nombres_esperados_presentes, with = FALSE]


	for (nombre in setdiff(nombres_esperados, names(Datos))) {
		Datos[[nombre]] <- NA
	}

	Datos <- Datos[, ..nombres_esperados]

	# ---------------- Datos Gestiones ----------------
	mongo_conn <- mongo(collection = "NY_Gestiones", url = connection_string)

	indices_meses <- match(Mes, meses) - 1
	regex_union <- paste0("^", Ano, indices_meses, collapse = "|")

	consulta_g <- sprintf('{ "fechagestion_2": { "$regex": "%s" } }', regex_union)
	Gestiones <- data.table(mongo_conn$find(consulta_g))


	#############
	Gestiones[, fecha_gestion := convertir_fecha_segura(fecha_gestion)]
	Gestiones[, cod_mejor_gestion := suppressWarnings(as.numeric(cod_mejor_gestion))]

	# Filtra filas con valores válidos en ambas columnas
	Gestiones <- Gestiones[!is.na(fecha_gestion) & !is.na(cod_mejor_gestion)]

	# Agrupa por documento y toma la mejor combinación
	Gestiones <- Gestiones[, .SD[which.min(cod_mejor_gestion)][which.max(fecha_gestion)], by = "nro_documento"]


	############
	Gestiones <- Gestiones[, .SD[which.min(cod_mejor_gestion)][which.max(fecha_gestion)], by = "nro_documento"]
	Gestiones <- Gestiones[, .(nro_documento, fecha_gestion, cod_mejor_gestion, observaciones_gestion, num_gestiones_3_meses)]

	Datos <- merge(Datos, Gestiones, all.x = TRUE,
	by.x = "numero_de_documento_del_aportante", by.y = "nro_documento")

	# ---------------- Correos ----------------
	mongo_conn <- mongo(collection = "NY_Correos", url = connection_string)
	#mongo_conn <- mongo(collection = "NY_Correos", db = "Niyaraky", url = connection_string)
	Correo <- data.table(mongo_conn$find())

	Datos[correo_representante_legal %in% Correo[Estado == "Exitoso", Email], correo_representante_legal_Exitos := "Si"]
	Datos[correo_electronico_comercial %in% Correo[Estado == "Exitoso", Email], correo_electronico_comercial_Exitos := "Si"]
	Datos[correo_electronico_fiscal %in% Correo[Estado == "Exitoso", Email], correo_electronico_fiscal_Exitos := "Si"]

	Datos[correo_representante_legal %in% Correo[Estado == "Fallido", Email], correo_representante_legal_Exitos := "No"]
	Datos[correo_electronico_comercial %in% Correo[Estado == "Fallido", Email], correo_electronico_comercial_Exitos := "No"]
	Datos[correo_electronico_fiscal %in% Correo[Estado == "Fallido", Email], correo_electronico_fiscal_Exitos := "No"]

	# ---------------- Cierres ----------------
	Datos[, ntp := paste0(numero_de_documento_del_aportante, numero_de_documento_del_cotizante, sub("-", "", periodo_de_mora))]
	consulta_ntp <- unique(Datos$ntp)
	
	# ---------------- Recaudos ----------------
	mongo_conn <- mongo(collection = "NY_Recaudos", url = connection_string)
	vals <- paste0('"', consulta_ntp, '"', collapse = ",")

	# Armamos la consulta JSON correctamente: {"ntp": { "$in": ["val1","val2", … ] }}
	query <- paste0('{"ntp": {"$in": [', vals, ']}}')

	# Ejecutamos la consulta
	Recaudos <- data.table(
		mongo_conn$find(
			query = query,
			fields = '{"ntp":1,"mes_asignacion":1,"ano_asignacion":1,"ntp_niyaraky":1,"ntp_niyaraky_2":1}'
		)
	)
	Recaudos <- unique(Recaudos, by = c("ntp", "mes_asignacion", "ano_asignacion"))
	names(Recaudos)[names(Recaudos) == "mes_asignacion"] <- "mes_recuado"
	names(Recaudos)[names(Recaudos) == "ano_asignacion"] <- "ano_recuado"

	# ---------------- Limpieza final ----------------
	Datos[, fecha_ultima_accion_de_cobro := as.character(convertir_fecha_segura(fecha_ultima_accion_de_cobro))]
	Datos[, fecha_afiliaciion_aportatante := as.character(convertir_fecha_segura(fecha_afiliaciion_aportatante))]
	Datos[, fecha_de_cancelacion := as.character(convertir_fecha_segura(fecha_de_cancelacion))]
	Datos[, fecha_ultima_actualizacion := as.character(convertir_fecha_segura(fecha_ultima_actualizacion))]
	Datos[, ciudad := chartr("aeiouaeiou", "aeiouAEIOU", ciudad)]
	Datos[, c("celular", "celular_representante_legal", "telefono_1", "telefono_2",
	"telefono_del_cotizante", "celular_del_cotizante", "telefono_comercial",
	"telefono_fiscal") := lapply(.SD, function(x) ifelse(x == "", 0, x)),
	.SDcols = c("celular", "celular_representante_legal", "telefono_1", "telefono_2",
	"telefono_del_cotizante", "celular_del_cotizante", "telefono_comercial",
	"telefono_fiscal")]
	Datos[, estado_de_la_matricula := stri_trans_general(estado_de_la_matricula, "Latin-ASCII")]

	Datos[, accion_de_cobro_Categoria := fcase(
		accion_de_cobro == 1, "Aviso",
		accion_de_cobro == 2, "Cobro anterior al titulo",
		accion_de_cobro == 3, "Generar titulo",
		accion_de_cobro == 4, "Cobro persuasivo",
		accion_de_cobro == 5, "Cobro judicial/coactivo",
		accion_de_cobro == 6, "Ninguna"
	)]

	return(Datos)
}

consulta_Gestion_Uni <- function(Documento="900790429",Administradora = "CCF03") {
		
	connection_string <- 'mongodb://laumar92:wwEVSfQDH1USAJNF@clniyago-shard-00-00.vbdrs.mongodb.net:27017,clniyago-shard-00-01.vbdrs.mongodb.net:27017,clniyago-shard-00-02.vbdrs.mongodb.net:27017/Niyaraky?ssl=true&replicaSet=atlas-mjdt39-shard-0&authSource=admin&retryWrites=true&w=majority'
	mongo_conn <- mongo(collection = "NY_Gestiones", url = connection_string)
	
	query <- sprintf('{"nro_documento": "%s"}',Documento)
	Gestiones <- data.table(mongo_conn$find(query))

	Gestiones[, fecha_gestion := as.Date(fecha_gestion, format = "%Y-%m-%d")]
	Gestiones[, obs_Gest := paste(unique(observaciones_gestion),collapse=" - "),by="fecha_gestion"]

	setorderv(Gestiones,c("fecha_gestion","grabaciones"),c(-1,-1))

	Gestiones <- Gestiones[, .SD[which.min(cod_mejor_gestion)][which.max(fecha_gestion)], by = "fecha_gestion"]

	Datos <- Gestiones

	# Separar registros con y sin URLs validas
	Datos_con_urls <- Datos[!is.na(grabaciones) & grabaciones != "333" & grepl("https?://", grabaciones)]
	Datos_sin_urls <- Datos[is.na(grabaciones) | grabaciones == "333" | !grepl("https?://", grabaciones)]

	# Expandir grabaciones en multiples filas, conservando todas las columnas
	dt_expandido <- Datos_con_urls[, {
		urls <- unlist(strsplit(grabaciones, "\r\n", fixed = TRUE))
		.SD[rep(1L, length(urls))][, url := urls]
	}, by = seq_len(nrow(Datos_con_urls))][, seq_len := NULL]  # quitar columna auxiliar

	# Para las que no tienen URLs, solo les asignamos su valor en 'grabaciones' a 'url'
	Datos_sin_urls[, url := grabaciones]

	# Unir todo y conservar todas las columnas
	resultado_final <- rbindlist(list(dt_expandido, Datos_sin_urls), use.names = TRUE, fill = TRUE)
	resultado_final[,fecha_gestion:=as.Date(fecha_gestion),]
	setorderv(resultado_final,"fecha_gestion",-1)

	resultado_final	

}

consulta_Gestion <- function() {

	# ---------------- Preparacion ----------------

	connection_string <- 'mongodb://laumar92:wwEVSfQDH1USAJNF@clniyago-shard-00-00.vbdrs.mongodb.net:27017,clniyago-shard-00-01.vbdrs.mongodb.net:27017,clniyago-shard-00-02.vbdrs.mongodb.net:27017/Niyaraky?ssl=true&replicaSet=atlas-mjdt39-shard-0&authSource=admin&retryWrites=true&w=majority'
	mongo_conn <- mongo(collection = "NY_Gestiones", url = connection_string)
	
	Gestiones <- data.table(mongo_conn$find(
	query = '{}',
	fields = '{"nro_documento": 1, "primer_apellido": 1, "fecha_gestion" :1, "_id": 0}'
	))

	Gestiones <- unique(Gestiones, by = "nro_documento")

	Gestiones

}


consulta_Notifica <- function(Documento = "8151083") {
	connection_string <- 'mongodb://laumar92:wwEVSfQDH1USAJNF@clniyago-shard-00-00.vbdrs.mongodb.net:27017,clniyago-shard-00-01.vbdrs.mongodb.net:27017,clniyago-shard-00-02.vbdrs.mongodb.net:27017/Niyaraky?ssl=true&replicaSet=atlas-mjdt39-shard-0&authSource=admin&retryWrites=true&w=majority'
	mongo_conn <- mongo(collection = "NY_Notificacion", url = connection_string)
	
	query <- sprintf('{"identificacion": "%s"}', Documento)
	Notifica <- data.table(mongo_conn$find(query))

	if (nrow(Notifica) == 0) {
		Notifica <- data.table(
		fecha_envio_notificacion = as.Date(NA),
		identificacion = NA_character_,
		estado_aportante = NA_character_,
		notificacion = NA_character_,
		estado_notificacion = NA_character_,
		ultimo_evento = NA_character_,
		mes_procedencia = NA_character_,
		url = NA_character_
		)
	} else {
		columnas_necesarias <- c("fecha_envio_notificacion", "identificacion", "estado_aportante",
			"notificacion", "estado_notificacion", "ultimo_evento","mes_procedencia", "url"
		)
		for (col in columnas_necesarias) {
			if (!(col %in% names(Notifica))) {
				Notifica[[col]] <- NA
			}
		}
		Notifica[, fecha_envio_notificacion := as.Date(fecha_envio_notificacion)]
		setorderv(Notifica, "fecha_envio_notificacion", -1)
	}

	return(Notifica)
}


##########################################################################################################################


# ----------------------- UI ----------------------- #

ui <- dashboardPage(

	dashboardHeader(
		title = "NiyaGO", titleWidth = 240,
		tags$li(
			class = "dropdown",
			actionLink("logout", "Log out", width = "100%", icon = icon("sign-out-alt"))
		)
	),

	dashboardSidebar(
		width = 240,
		sidebarMenu(
			sidebarMenuOutput("Cua")
		)
	),

	dashboardBody(
		#use_theme(mytheme),
		use_waiter(spinners = 3),
		tags$style(HTML("…")),
		shinyjs::useShinyjs(),
		# Estilo global para todos los box
		tags$style(HTML("
			/* Cambiar barra superior */
			.main-header .navbar {
			background-color: #f2923d !important;
			}

			/* Cambiar el logo (texto SEGMENTUM) */
			.main-header .logo {
			background-color: #f2923d !important;
			color: white !important;
			font-weight: bold;
			}

			/* Cambiar sidebar */
			.main-sidebar {
			background-color: #ffffff !important;
			}

			/* Cambiar active tab en sidebar */
			.sidebar-menu > li.active > a {
			background-color: #080808 !important;
			color: white !important;
			}

			/* Cambiar infoBox y valueBox */
			.info-box, .small-box {
			background-color: #f2923d !important;
			color: white !important;
			}

			.info-box-icon, .small-box .icon {
			background-color: #f2923d !important;
			}

			/* Cambiar box headers */
			.box-header, .box-header h3, .box-header h2, .box-header h1 {
			background-color: #f2923d !important;
			color: white !important;
			}

			.box {
			background-color: white !important;
			color: black !important;
			border: 2px solid #cccccc !important;
			}

			/* Cambiar el color del boton de minimizar en los box */
			.box-header .btn,
			.box-header .btn-box-tool,
			.box-header .btn.btn-box-tool,
			.box-header .btn.btn-box-tool > i {
			color: white !important;
			}

			/* Cambiar el color del boton de minimizar al pasar el mouse */
			.box-header .btn:hover,
			.box-header .btn-box-tool:hover,
			.box-header .btn.btn-box-tool:hover,
			.box-header .btn.btn-box-tool > i:hover {
			color: #dddddd !important;
			}

			/* Cambiar color del boton de abrir/cerrar el sidebar */
			.main-header .sidebar-toggle {
			color: white !important;
			background-color: #f2923d !important; /* el fondo normal */
			}

			.main-header .sidebar-toggle:hover {
			color: #dddddd !important;
			background-color: #080808 !important; /* fondo cuando pasas el mouse */
			}
			
			.box-naranja .btn,
			.box-naranja .btn-default,
			.box-naranja .download-btn {
			  background-color: #f8cfa9 !important;
			  color: black !important;
			  border-color: #f8cfa9 !important;
			  border-radius: 6px !important;
			  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
			  transition: all 0.2s ease-in-out;
			}

			/* Hover: más amigable + efecto hundido */
			.box-naranja .btn:hover,
			.box-naranja .btn-default:hover {
			  background-color: #f6b76a !important;
			  border-color: #f6b76a !important;
			  box-shadow: 0 2px 4px rgba(0,0,0,0.15);
			  transform: translateY(1px);
			}
			
			.boton-3d {
				background-color: #080808 !important;
				color: white !important;  /* <- aquí el cambio */
				border-color: #080808 !important;
				border-radius: 6px !important;
				box-shadow: 0 4px 6px rgba(0,0,0,0.1);
				transition: all 0.2s ease-in-out;
			}

			.boton-3d:hover {
				background-color: #8c8b8b !important;
				border-color: #8c8b8b !important;
				color: white !important;  /* <- y aquí también */
				box-shadow: 0 2px 4px rgba(0,0,0,0.15);
				transform: translateY(1px);
			}
			
			.small-box {
				border-radius: 9px !important;
			}
		")),

		shinyjs::useShinyjs(),
		tags$script(HTML("
			$(document).on('click', 'button[id^=btn_detalle_]', function() {
			var btn_id = $(this).attr('id');
			Shiny.setInputValue('last_btn_click', btn_id, {priority: 'event'});
			});

			$(document).on('click', 'button[id^=btn_notifica_]', function() {
			var btn_id = $(this).attr('id');
			Shiny.setInputValue('last_btn_click_notifica', btn_id, {priority: 'event'});
			});
		")),

		uiOutput("Caja"),

		conditionalPanel(
			condition = "input.ID_OBS_CASOS>=1",
			splitLayout(
				br(),
				actionButton("ID_GESTIONAR_CASOS", "Ok", icon = icon("play"), width = "100%"),
				br(),
				cellWidths = c("45%", "10%", "45%")
			)
		),

		br(),

		fluidRow(
			column(4),
			column(4,
				descriptionBlock(
					numberColor = "green",
					img(src = "Logo Plataforma NiyaGo.png", height = 155, width = 410),
					header = div(HTML("<a href='https://www.niyaraky.co/' target='_blank'> www.niyaraky.co </a>")),
					rightBorder = TRUE,
					marginBottom = FALSE
				)
			),
			column(4)
		)
	)
)

# ----------------------- Conexion Mongo ----------------------- #

connection_string <- 'mongodb://laumar92:wwEVSfQDH1USAJNF@clniyago-shard-00-00.vbdrs.mongodb.net:27017,clniyago-shard-00-01.vbdrs.mongodb.net:27017,clniyago-shard-00-02.vbdrs.mongodb.net:27017/Niyaraky?ssl=true&replicaSet=atlas-mjdt39-shard-0&authSource=admin&retryWrites=true&w=majority'
mongo_conn <- mongo(collection = "NY_Clientes", url = connection_string)
mongo_conn_sub <- mongo(collection = "NY_Sub_Clientes", url = connection_string)

Usua <- function() {
	usuarios <- mongo_conn$find('{}')
	return(usuarios)
}


# ----------------------- Server ----------------------- #

Logged = 0

server <- function(input, output, session) {

	observeEvent(input$logout,{
		session$reload()
	})
	
	USER <- reactiveValues(Logged = Logged)
	USER$Id.username <- NULL


	logo_reactivo <- reactiveVal(NULL)
	color_reactivo <- reactiveVal("#FAD7A0")
	nombre_empresa_reactivo <- reactiveVal(NULL)
	ident_empresa_reactivo <- reactiveVal(NULL)
	administradora_empresa_reactivo <- reactiveVal(NULL)
	color_empresa_reactivo <- reactiveVal(NULL)

	US <- data.table(Usua())
	us <- US[, 3]
	co <- US[, 4]

	# ---- LOGIN ----
	observe({
		gc(reset = TRUE)

		if (USER$Logged == 0) {
			if (!is.null(input$Login) && input$Login > 0) {

				Username <- isolate(input$userName)
				Password <- isolate(input$passwd)
				Id.username <- which(us == Username)
				Id.password <- ifelse((co[Id.username] == Password) == 1, Id.username, "1*2@3*@")

				if (length(Id.username) > 0 & length(Id.password) > 0) {
					if (Id.username == Id.password) {
						USER$Logged <- US[Id.username,"Cliente"]
						USER$Id.username <- Id.username

						usuario_logueado <- US[Id.username, ]
						logo_path <- paste0("www/", usuario_logueado$Identificacion, ".png")
						color_empresa <- usuario_logueado$Color
						Nom_Empresa <- usuario_logueado$Nombre
						Ident_Empresa <- usuario_logueado$Identificacion
						Adminitradora_Empresa <- usuario_logueado$Cod_Administradora
						Color_Empresa <- usuario_logueado$Color

						# Validar existencia del logo, si no usar uno por defecto
						if (!file.exists(logo_path)) {
							logo_path <- "www/Logo Plataforma NiyaGo.png"
						}

						logo_reactivo(logo_path)
						color_reactivo(color_empresa)
						nombre_empresa_reactivo(Nom_Empresa)
						ident_empresa_reactivo(Ident_Empresa)
						administradora_empresa_reactivo(Adminitradora_Empresa)
						color_empresa_reactivo(Color_Empresa)
						
					}
				}
			}
		}
	})



	mostrarFichaGestion <- function(fecha_seleccionada, datos) {
		datos_filtrados <- datos[fecha_gestion == fecha_seleccionada]

		urls <- datos_filtrados$url
		urls_validas <- urls[!is.na(urls) & urls != "333" & grepl("^http", urls)]

		contenido_urls <- if (length(urls_validas) == 0) {
			tags$em("Sin URL disponible")
		} else {
			tags$ul(
				lapply(urls_validas, function(u) 
				tags$li(tags$a(href = u, target = "_blank", u, style = "color: #337ab7; text-decoration: underline;")))
			)
		}

		showModal(modalDialog(
			title = div(icon("info-circle"), "Ficha Tecnica del Aportante"),
			easyClose = TRUE,
			footer = modalButton("Cerrar"),
			size = "m",
			tags$div(
				style = "background-color: #fef5e7; padding: 20px; border-radius: 8px;",

				tags$h4(style = "color: #d35400;", paste("Fecha de Gestion:", as.character(fecha_seleccionada))),
				
				tags$p(
					strong("Número de Caso: "),
					unique(datos_filtrados$nro_caso)
				),
				
				tags$hr(style = "border-top: 1px solid #e67e22;"),

				tags$p(
					strong("Observacion:"),
					tags$br(),
					tags$span(style = "color: #2c3e50;", unique(datos_filtrados$observaciones_gestion))
				),

				tags$hr(style = "border-top: 1px solid #e67e22;"),

				tags$p(strong("Grabaciones disponibles:")),
				contenido_urls
			)
		))
	}


	# ---- UI cuando no esta logueado ----
	observe({
		if (USER$Logged == 0) {

			output$Cua <- renderMenu({
				sidebarMenu(id = "Log",
					menuItem(".", tabName = "Id_NiyaGoo", icon = icon("sign-in-alt")),
					menuItem("Autenticacion", tabName = "Id_Autentica", icon = icon("sign-in-alt"))
				)
			})

			isolate({updateTabItems(session, "Log", "Id_Autentica")})

			output$Caja <- renderUI({
				tabItems(
					tabItem(tabName = "Id_Autentica",
						fluidRow(
							column(3),
							column(6,
								box(
									title = div(icon("user"), "Usuario"),
									width = 12,
									status = "warning",
									solidHeader = TRUE,
									class = "box-naranja",
									div(
										textInput("userName", "Username"),
										passwordInput("passwd", "Password"),
										br(),
										actionButton(
											inputId = "Login",
											label = "Log in",
											icon = icon("sign-in-alt"),
											class = "boton-3d"
										)
									)
								)
							),
							column(3)
						)
					),
					tabItem(tabName = "Id_Manual",
						fluidRow(
							tags$iframe(style = "height:600px; width=100%", src = "Manual dash.pdf")
						)
					)
				)
			})
		}

		if (USER$Logged == "1") {

			output$Cua <- renderMenu({
				sidebarMenu(id = "Log",
					menuItem(".", tabName = "Id_Temp"),
					menuItem("Crear Usuario", tabName = "Id_Crear_Usuario", icon = icon("user-plus"))
				)
			})

			isolate({updateTabItems(session, "Log", "Id_Crear_Usuario")})

			output$Caja <- renderUI({
				tabItems(
					tabItem(tabName = "Id_Crear_Usuario",
						fluidRow(
							column(3),
							column(6,
								box(
									title = div(icon("user-plus"), "Crear Nuevo Cliente"),
									width = 12,
									status = "primary",
									solidHeader = TRUE,
									textInput("nuevo_nombre", "Nombre Empresa"),
									textInput("nuevo_identificacion", "Identificación"),
									textInput("nuevo_usuario", "Nombre de Usuario"),
									passwordInput("nuevo_contrasena", "Contraseña"),
									textInput("nuevo_cod_administradora", "Código Administradora"),
									selectInput("nuevo_cliente", "Rol del Cliente",
									choices = c("Cliente" = "2"), selected = "2"),
									textInput("nuevo_color", "Color Hex (#8ED973)"),
									br(),
									actionButton(
										inputId = "crear_usuario_btn",
										label = "Crear Usuario",
										icon = icon("plus"),
										class = "boton-3d"
									)
								)
							),
							column(3)
						)
					)
				)
			})

			observeEvent(input$crear_usuario_btn, {
				req(
					input$nuevo_nombre, input$nuevo_identificacion, input$nuevo_usuario,
					input$nuevo_contrasena, input$nuevo_cod_administradora,
					input$nuevo_cliente, input$nuevo_color
				)

				# Verifica que no exista ya el usuario
				usuario_existente <- mongo_conn$find(
					query = sprintf('{"Usuario":"%s"}', input$nuevo_usuario)
				)

				if (nrow(usuario_existente) > 0) {
					showModal(modalDialog(
						title = "Error",
						paste("El usuario", input$nuevo_usuario, "ya existe."),
						easyClose = TRUE,
						footer = modalButton("Cerrar")
					))
					return()
				}

				nuevo_usuario <- data.frame(
					Nombre              = input$nuevo_nombre,
					Identificacion      = input$nuevo_identificacion,
					Usuario             = input$nuevo_usuario,
					Contrasena          = input$nuevo_contrasena,
					Cod_Administradora  = input$nuevo_cod_administradora,
					Cliente             = input$nuevo_cliente,
					Color               = input$nuevo_color,
					stringsAsFactors    = FALSE
				)

				mongo_conn$insert(nuevo_usuario)

				showModal(modalDialog(
					title = "Usuario Creado",
					paste("El usuario", input$nuevo_usuario, "fue creado exitosamente."),
					easyClose = TRUE,
					footer = modalButton("Cerrar")
				))

				# Limpia los campos
				updateTextInput(session, "nuevo_nombre", value = "")
				updateTextInput(session, "nuevo_identificacion", value = "")
				updateTextInput(session, "nuevo_usuario", value = "")
				updateTextInput(session, "nuevo_contrasena", value = "")
				updateTextInput(session, "nuevo_cod_administradora", value = "")
				updateSelectInput(session, "nuevo_cliente", selected = "2")
				updateTextInput(session, "nuevo_color", value = "")
			})
		}
	
		# ---- UI cuando ya esta logueado ----
		if (USER$Logged == "2") {

			datos_reactivos <- reactiveVal(NULL)
			historico_reactivo <- reactiveVal(NULL)
			resultado_consulta_uni <- reactiveVal(NULL)
			pdfs_encontrados <- reactiveVal(NULL)

			
			datos_hist <- consulta_Gestion()
			historico_reactivo(datos_hist)

			subusuarios <- reactive({
				req(ident_empresa_reactivo())
				US_sub <- US[
					Identificacion == ident_empresa_reactivo() &
					Cliente == "3", # subusuario
					.(Usuario, Contrasena)
				]
				US_sub
			})

			# ---- Sidebar actualizado ----
			output$Cua <- renderMenu({
				sidebarMenu(id = "Log",
					menuItem(".", tabName = "temp_1", icon = icon("sign-in-alt")),
					menuItem("NiyaGO", tabName = "Id_NiyaGo", icon = icon("sign-in-alt")),
					menuItem("Crear Usuario", tabName = "Id_Crear_Sub_Usuario", icon = icon("user-plus")),
					menuItem("Gestion de Cobro", tabName = "Id_Gestion", icon = icon("table")),
					menuItem("Historico Gestiones", tabName = "Id_Historico_Gestion", icon = icon("table"))
				)
			})

			isolate({updateTabItems(session, "Log", "Id_NiyaGo")})

			# ---- Caja principal ----
			output$Caja <- renderUI({

				anios <- as.character(2023:year(Sys.Date()))
				meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
				"Septiembre","Octubre","Noviembre","Diciembre")

				tabItems(
					
					tabItem(tabName = "Id_Crear_Sub_Usuario",
						fluidRow(
							splitLayout(
								br(),
								valueBoxOutput("cantidad_subusuarios",width = 12),
								br(),
								cellWidths=c("35%","30%","35%")
							)
						),
						fluidRow(
						
							splitLayout(
								br(),
								box(
									title = div(icon("user-plus"), "Crear Nuevo Usuario"),
									width = 12,
									status = "primary",
									solidHeader = TRUE,
									textInput("nuevo_sub_usuario", "Nombre de Usuario"),
									passwordInput("nuevo_sub_contrasena", "Contraseña"),
									br(),
									br(),
									actionButton(
										inputId = "crear_sub_usuario_btn",
										label = "Crear Usuario",
										icon = icon("plus"),
										class = "boton-3d"
									)
								),
								box(
									title = "Sub Usuarios Registrados",
									width = 12,
									status = "primary",
									solidHeader = TRUE,
									DTOutput("tabla_subusuarios")
								),
								br(),
								cellWidths=c("10%","40%","40%","10%")	
							)
						)
						
					),
					
					tabItem(tabName = "Id_NiyaGo",
						fluidRow(
							column(12,
								box(
									width = 12,
									status = "primary",
									solidHeader = TRUE,
									title = div(icon("info-circle"), "Bienvenido a NiyaGo"),
									div(
										style = "padding: 30px; font-size: 17px; line-height: 1.6; background-color: #fef5e7; border-radius: 10px;",
										tags$h3("¿Que es NiyaGo?", style = "color: #d35400; font-weight: bold;"),
										tags$p("NiyaGo es una plataforma de trazabilidad diseñada para optimizar la gestion de cobros por aportante. Su objetivo es centralizar la informacion clave del proceso y facilitar la toma de decisiones."),
										tags$br(),
										tags$p("A traves de esta herramienta usted podra:"),
										tags$ul(
											tags$li("Consultar el estado de la cartera por periodo y tipo de aportante."),
											tags$li("Visualizar el historial de gestiones realizadas."),
											tags$li("Acceder a las evidencias recopiladas, como grabaciones y correos electronicos."),
											tags$li("Descargar estados de cuenta detallados.")
										),
										tags$br(),
										tags$p(tags$em("Todo en un solo lugar, de forma agil, segura y eficiente.")),
										tags$br()#,
									)
								)
							)
						)
					),


					tabItem(tabName = "Id_Gestion",
						fluidRow(
							column(3,
								selectInput("ano_sel", "Año Asignacion", choices = anios, selected = max(anios),width = "100%"),
								br(),
								fluidRow(
									column(6,
										actionButton("actualizar", "Actualizar", icon = icon("sync"), class = "boton-3d", width = "100%")
									),
									column(6,
										conditionalPanel(
											condition = "input.actualizar >= 1",
											actionButton("limpiar_filtros", "Limpiar filtros", icon = icon("eraser"), class = "boton-3d", width = "100%")
										)
									)
								)
							),
							column(3,
								selectizeInput("mes_sel", "Mes Asignacion", 
									choices = meses, 
									selected = "Abril", 
									multiple = TRUE,
									options = list(placeholder = 'Seleccione uno o mas meses...'),
									width = "100%"
								),
								uiOutput("filtro_tipo_aportante")

							),
							column(3,
								uiOutput("filtro_ano"),
								uiOutput("filtro_documento")
							),
							column(3,
								uiOutput("filtro_periodo_mora"),
								uiOutput("filtro_estado_aportante")
							)
						),
						br(),
						fluidRow(

							#column(9,
							conditionalPanel(
								condition = "input.actualizar>=1",
								fluidRow(
									box(
										status = "primary", solidHeader = TRUE, width = 12,
										DTOutput("tabla_resultados"),
										br(),
										div(
											class = "text-right",  # Alineación a la derecha
											style = "display: flex; justify-content: flex-end; gap: 10px;",
											uiOutput("download_ecuenta_btn"),
											downloadButton("descarga_tabla_resumen", "Descargar Tabla", icon = icon("file-download"), class = "boton-3d")
										),
										class = "box-naranja"
									)
								),
								br(),
								fluidRow(								
									valueBoxOutput("value_total_cartera", width = 4),
									valueBoxOutput("value_trabajadores_mora", width = 4),
									valueBoxOutput("value_periodos_mora", width = 4)
									
								)
							)	
						)
					),
					tabItem(tabName = "Id_Historico_Gestion",
						fluidRow(
							column(2, 
								style = "border-right: 1px solid #dcdcdc; border-bottom: 1px solid #dcdcdc; padding-right: 20px; padding-bottom: 15px;",
								selectizeInput(
									inputId = "filtro_doc_historico",
									label = "Documento Aportante",
									choices = NULL,
									options = list(placeholder = 'Seleccione un documento...'),
									selected = NULL,
									width = "100%"
								),
								actionButton("btn_aplicar_doc", "Aplicar", icon = icon("search"), class = "boton-3d",width = "100%"),
								actionButton("btn_buscar_pdfs", "Buscar PDFs",icon = icon("search"), class = "boton-3d",width = "100%")
							),
							column(10,
								conditionalPanel(
									condition = "input.btn_aplicar_doc>=1",
									
									box(
										title = div(icon("history"), "Histórico de Gestiones"),
										width = 12,
										status = "warning",
										solidHeader = TRUE,
										div(
											DTOutput("tabla_resultados_historico"),
											br(),
											actionButton(
												inputId = "btn_ver_notificaciones",
												label = "Ver Notificaciones",
												icon = icon("bell"),
												class = "boton-3d"
											)
										),
										class = "box-naranja"
									)
								)
							)
						),
						fluidRow(
							column(12,
								uiOutput("resultado_busqueda_pdfs")
							)
						)						
					)
				)
			})

			# ---- Filtros vacios inicialmente ----
			output$filtro_tipo_aportante <- renderUI({})
			output$filtro_estado_aportante <- renderUI({})
			output$filtro_ano <- renderUI({})
			output$filtro_documento <- renderUI({})
			output$filtro_periodo_mora <- renderUI({})
			
			output$cantidad_subusuarios <- renderValueBox({
				n <- nrow(subusuarios())+1
				valueBox(
					value = HTML(paste0("<span style='font-size: 28px; font-weight: bold;'>", n, "</span> <span style='font-size: 18px;'>Usuarios Registrados</span>")),
					subtitle = paste("1 Principal y",nrow(subusuarios()),"Sub Usuarios"),
					icon = icon("users"),
					color = "aqua"
				)
			})

			output$tabla_subusuarios <- renderDT({
				req(subusuarios())
				datatable(
					subusuarios(),
					options = list(pageLength = 3)
				)
			})

			observeEvent(input$crear_sub_usuario_btn, {
				req(
					input$nuevo_sub_usuario,
					input$nuevo_sub_contrasena
					
				)
				
				# Verifica que no exista ya el usuario
				usuario_existente <- mongo_conn$find(
					query = sprintf('{"Usuario":"%s"}', input$nuevo_sub_usuario)
				)

				if (nrow(usuario_existente) > 0) {
					showModal(modalDialog(
						title = "Error",
						paste("El usuario", input$nuevo_sub_usuario, "ya existe."),
						easyClose = TRUE,
						footer = modalButton("Cerrar")
					))
					return()
				}

				nuevo_usuario <- data.frame(
					Nombre              = nombre_empresa_reactivo(),
					Identificacion      = ident_empresa_reactivo(),
					Usuario             = input$nuevo_sub_usuario,
					Contrasena          = input$nuevo_sub_contrasena,
					Cod_Administradora  = administradora_empresa_reactivo(),
					Cliente             = "3",
					Color               = color_empresa_reactivo(),
					stringsAsFactors    = FALSE
				)

						
				mongo_conn$insert(nuevo_usuario)

				showModal(modalDialog(
					title = "Usuario Creado",
					paste("El usuario", input$nuevo_usuario, "fue creado exitosamente."),
					easyClose = TRUE,
					footer = modalButton("Cerrar")
				))

				# Limpia los campos
				#updateTextInput(session, "nuevo_nombre", value = "")
				#updateTextInput(session, "nuevo_identificacion", value = "")
				updateTextInput(session, "nuevo_sub_usuario", value = "")
				updateTextInput(session, "nuevo_sub_contrasena", value = "")
				#updateTextInput(session, "nuevo_cod_administradora", value = "")
				updateSelectInput(session, "nuevo_sub_cliente", selected = "2")
				#updateTextInput(session, "nuevo_color", value = "")
				
			})
			
			# ---- Evento: Al hacer clic en Actualizar ----
			observeEvent(input$actualizar, {
				cat(">>> Se presiono actualizar\n")
				req(USER$Logged %in% c("2", "3"))
				req(input$ano_sel != "")
				req(length(input$mes_sel) >= 1)

				# ─── Mostrar spinner fullscreen ───────────────────────────────────
				waiter_show(
					html  = spin_flower(),            # o tu splash_screen que definiste arriba
					color = "#f2923d"
				)
				Sys.sleep(0.05)

				# Obtiene la administradora con la que el usuario está logueado
				admin_usuario <- US[USER$Id.username, "Cod_Administradora"]
				req(!is.null(admin_usuario) && admin_usuario != "")

				# ── Aplicar tryCatch para capturar posibles errores de consulta() ──────────
				datos <- NULL
				error_msg <- NULL
				tryCatch({
					datos <- consulta(
						Ano = input$ano_sel,
						Mes = input$mes_sel,
						Administradora = admin_usuario
					)
				}, error = function(e) {
					error_msg <<- e$message
				})
				# ──────────────────────────────────────────────────────────────────────────

				# Oculto el spinner fullscreen (independientemente de si hubo error o no)
				waiter_hide()

				# Si error_msg no es NULL, hubo fallo en consulta(); mostramos modal de error
				if (!is.null(error_msg)) {
					showModal(modalDialog(
						title = "Error al cargar datos",
						paste0("Ocurrió un problema en consulta():\n", error_msg),
						easyClose = TRUE,
						footer = modalButton("Cerrar")
					))
					return()
				}

				# Llegamos aquí solo si consulta() devolvió datos sin error
				datos <- as.data.table(datos)
				datos_reactivos(datos)

				# ── Ahora reconstruimos los filtros dinámicos basados en “datos” ────────────

				# 1) Filtro Tipo Aportante
				tipos <- sort(unique(as.character(datos$tipo_aportante[!is.na(datos$tipo_aportante)])))
				output$filtro_tipo_aportante <- renderUI({
					selectizeInput(
						inputId = "filtro_tipo_aportante",
						label = "Tipo Aportante",
						choices = c("Todos", tipos),
						selected = "Todos",
						options = list(
							placeholder = '...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})

				updateSelectizeInput(
					session,
					inputId = "filtro_tipo_aportante",
					choices = c("Todos", tipos),
					selected = "Todos",
					server = TRUE
				)

				# 2) Filtro Estado del Aportante
				estados <- sort(unique(as.character(datos$estado_del_aportante[!is.na(datos$estado_del_aportante)])))
				output$filtro_estado_aportante <- renderUI({
					selectizeInput(
						inputId = "filtro_estado_aportante",
						label = "Estado del Aportante",
						choices = c("Todos", estados),
						selected = "Todos",
						options = list(
							placeholder = '...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})

				updateSelectizeInput(
					session,
					inputId = "filtro_estado_aportante",
					choices = c("Todos", estados),
					selected = "Todos",
					server = TRUE
				)

				# 3) Filtro Año
				anios_filtro <- as.character(sort(unique(datos$ano[!is.na(datos$ano)])))
				output$filtro_ano <- renderUI({
					selectizeInput(
						inputId = "filtro_ano",
						label = "Año",
						choices = c("Todos", anios_filtro),
						selected = "Todos",
						options = list(
							placeholder = 'Escriba el año...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})
				updateSelectizeInput(
					session,
					inputId = "filtro_ano",
					choices = c("Todos", anios_filtro),
					selected = "Todos",
					server = TRUE
				)

				# 4) Filtro Documento Aportante
				documentos <- sort(unique(as.character(
					datos$numero_de_documento_del_aportante[!is.na(datos$numero_de_documento_del_aportante)]
				)))
				output$filtro_documento <- renderUI({
					selectizeInput(
						inputId = "filtro_documento",
						label = "Documento Aportante",
						choices = NULL,
						selected = "Todos",
						options = list(
							placeholder = 'Escriba para buscar...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})
				updateSelectizeInput(
					session,
					inputId = "filtro_documento",
					choices = c("Todos", documentos),
					selected = "Todos",
					server = TRUE
				)

				# 5) Filtro Periodo de Mora
				periodos_mora <- sort(unique(as.character(datos$periodo_de_mora[!is.na(datos$periodo_de_mora)])))
				output$filtro_periodo_mora <- renderUI({
					selectizeInput(
						inputId = "filtro_periodo_mora",
						label = "Periodo de Mora",
						choices = c("Todos", periodos_mora),
						selected = "Todos",
						options = list(
							placeholder = 'Escriba el periodo...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})

				updateSelectizeInput(
					session,
					inputId = "filtro_periodo_mora",
					choices = c("Todos", periodos_mora),
					selected = "Todos",
					server = TRUE
				)

				# ── Fin del bloque que se ejecuta cuando consulta() fue exitoso ────────────
			})

			observeEvent(input$limpiar_filtros, {
				updateSelectizeInput(session, "filtro_periodo_mora", selected = "Todos")
				updateSelectizeInput(session, "filtro_documento", selected = "Todos")
				updateSelectizeInput(session, "filtro_ano", selected = "Todos")
				updateSelectizeInput(session, "filtro_estado_aportante", selected = "Todos")
				updateSelectizeInput(session, "filtro_tipo_aportante", selected = "Todos")
			})
			
			observeEvent(input$btn_buscar_pdfs, {
				req(input$filtro_doc_historico)

				showModal(modalDialog("Buscando PDFs, por favor espera…", footer = NULL))

				pdfs <- buscar_pdfs_sharepoint(
					tenant_id     = "jkjhkhjk3713e74b",
					client_id     = "jhkjhkj83",
					client_secret = "CjhkhkjjVczy",
					dominio       = "niyarakycomco.sharepoint.com",
					sitio         = "NiyarakyCompartida",
					patron        = input$filtro_doc_historico
				)

				removeModal()

				if (is.null(pdfs) || length(pdfs) == 0) {
					showModal(modalDialog(
						title = "Resultado",
						"⚠️ No se encontraron PDFs para el patrón ingresado.",
						easyClose = TRUE
					))
				} else {
				  showModal(modalDialog(
					title = "PDFs encontrados",
					tagList(
					  tags$div(
						style = "max-height: 400px; overflow-y: auto; word-wrap: break-word; padding: 10px;",
						lapply(1:nrow(pdfs), function(i) {
						  tags$p(
							tags$a(
							  href = pdfs$webUrl[i],
							  download = NA,
							  target = "_blank",
							  style = "color: #337ab7; text-decoration: underline; word-break: break-all;",
							  pdfs$name[i]
							)
						  )
						})
					  )
					),
					easyClose = TRUE
				  ))
				}


			})


			######### Descargar Detalle ##################

			output$descargar_estado_cuenta <- downloadHandler(
				filename = function() {
					doc <- input$filtro_documento
					if (is.null(doc) || doc == "" || doc == "Todos") doc <- "todos"
					paste0("estado_de_cuenta_", doc, "_", Sys.Date(), ".xlsx")
				},
				content = function(file) {
					df_original <- datos_reactivos()
					req(!is.null(df_original))

					if (!is.null(input$filtro_tipo_aportante) && input$filtro_tipo_aportante != "Todos") {
						df_original <- df_original[tipo_aportante == input$filtro_tipo_aportante]
					}
					if (!is.null(input$filtro_documento) && input$filtro_documento != "Todos") {
						df_original <- df_original[numero_de_documento_del_aportante == input$filtro_documento]
					}
					if (!is.null(input$filtro_periodo_mora) && input$filtro_periodo_mora != "Todos") {
						df_original <- df_original[periodo_de_mora == input$filtro_periodo_mora]
					}

					if (nrow(df_original) == 0) {
						showModal(modalDialog(
							title = "Sin datos",
							"No hay registros que coincidan con los filtros seleccionados.",
							easyClose = TRUE,
							footer = modalButton("Cerrar")
						))
						req(FALSE)
					}

					df <- unique(df_original[, .(
						tipo_documento_cotizante,
						numero_de_documento_del_cotizante,
						nombre_del_cotizante,
						periodo_de_mora,
						valor_cartera_detalle,
						salario
					)])

					# Renombrar columnas para mostrar nombres bonitos
					colnames(df) <- c(
						"Tipo Documento Cotizante",
						"Número Documento Cotizante",
						"Nombre del Cotizante",
						"Periodo de Mora",
						"Valor Mora",
						"Salario"
					)
					valor_mora <- sum(df$'Valor Mora', na.rm = TRUE)
					df$'Valor Mora' <- scales::dollar(df$'Valor Mora', big.mark = ".", decimal.mark = ",", prefix = "$ ")

					wb <- createWorkbook()
					addWorksheet(wb, "EstadoCuenta", gridLines = FALSE)

					# Logo centrado en B2:C6 ajustado proporcionalmente
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 2:6)
					insertImage(wb, "EstadoCuenta", file = logo_reactivo(), startRow = 2, startCol = 2,
					width = 304 * 2, height = 304, units = "px")

					# Título centrado en D2:J6
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 2:6)
					writeData(wb, "EstadoCuenta", "ESTADO DE CUENTA", startCol = 4, startRow = 2)
					addStyle(wb, "EstadoCuenta", createStyle(
						fontSize = 16, textDecoration = "bold", halign = "center", valign = "center",
						fgFill = color_reactivo(), border = "TopBottomLeftRight"
					), rows = 2:6, cols = 4:7, gridExpand = TRUE)

					# Razón social
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 8)
					writeData(wb, "EstadoCuenta", "RAZÓN SOCIAL:", startCol = 2, startRow = 8)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold"
					), rows = 8, cols = 2:3, gridExpand = TRUE)

					razon_social <- unique(df_original$nombre_o_razon_social_del_aportante)[1]
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 8)
					writeData(wb, "EstadoCuenta", toupper(razon_social), startCol = 4, startRow = 8)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold",
						fgFill = color_reactivo()
					), rows = 8, cols = 4:7, gridExpand = TRUE)

					# Documento
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 10)
					writeData(wb, "EstadoCuenta", "DOCUMENTO", startCol = 2, startRow = 10)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold"
					), rows = 10, cols = 2:3, gridExpand = TRUE)

					doc_value <- if (!is.null(input$filtro_documento) && input$filtro_documento != "Todos") {
						toupper(as.character(input$filtro_documento))
					} else { "TODOS" }
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 10)
					writeData(wb, "EstadoCuenta", doc_value, startCol = 4, startRow = 10)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold",
						fgFill = color_reactivo()
					), rows = 10, cols = 4:7, gridExpand = TRUE)

					# Tipo documento
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 12)
					writeData(wb, "EstadoCuenta", "TIPO DE DOCUMENTO", startCol = 2, startRow = 12)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold"
					), rows = 12, cols = 2:3, gridExpand = TRUE)

					tipo_doc <- unique(df_original$tipo_de_documento_del_aportante)[1]
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 12)
					writeData(wb, "EstadoCuenta", toupper(tipo_doc), startCol = 4, startRow = 12)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold",
						fgFill = color_reactivo()
					), rows = 12, cols = 4:7, gridExpand = TRUE)

					# Valor mora
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 14)
					writeData(wb, "EstadoCuenta", "VALOR MORA", startCol = 2, startRow = 14)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold"
					), rows = 14, cols = 2:3, gridExpand = TRUE)


					valor_mora_fmt <- scales::dollar(valor_mora, big.mark = ".", decimal.mark = ",", prefix = "$ ")
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 14)
					writeData(wb, "EstadoCuenta", valor_mora_fmt, startCol = 4, startRow = 14)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold",
						fgFill = color_reactivo()
					), rows = 14, cols = 4:7, gridExpand = TRUE)

					# Tabla desde fila 16
					headerStyle <- createStyle(fontSize = 11, textDecoration = "bold", halign = "center", fgFill = color_reactivo())
					writeData(wb, "EstadoCuenta", x = as.data.frame(df), startCol = 2, startRow = 16, colNames = TRUE)
					addStyle(wb, "EstadoCuenta", style = headerStyle, rows = 16, cols = 2:(ncol(df)+1), gridExpand = TRUE)

					bodyStyle <- createStyle(fontSize = 11, halign = "left")
					addStyle(wb, "EstadoCuenta", style = bodyStyle, rows = 17:(nrow(df)+16), cols = 2:(ncol(df)+1), gridExpand = TRUE)

					# Bordes a toda la tabla
					addStyle(wb, "EstadoCuenta", style = createStyle(border = "TopBottomLeftRight"),
					rows = 16:(nrow(df)+16), cols = 2:(ncol(df)+1), gridExpand = TRUE, stack = TRUE)

					setColWidths(wb, "EstadoCuenta", cols = 2:(ncol(df)+1), widths = "auto")
					saveWorkbook(wb, file, overwrite = TRUE)
				}
			)

			# ---- Renderizar tabla ----
			output$descargar_evidencia <- downloadHandler(
				filename = function() {
					"Evidencia prueba.pdf"
				},
				content = function(file) {
					# Ajusta la ruta si tu PDF está en otra carpeta, p.ej. "www/datos/Evidencia prueba.pdf"
					file.copy(from = "datos/Evidencia prueba.pdf", to = file, overwrite = TRUE)
				},
				contentType = "application/pdf"
			)
			
			observe({

				datos <- datos_reactivos()
				req(!is.null(datos))  # Solo procede si hay datos cargados

				datos_filtrados <- datos

				# Filtro: Tipo Aportante
				if (!is.null(input$filtro_tipo_aportante) &&
					input$filtro_tipo_aportante != "" &&
				input$filtro_tipo_aportante != "Todos") {
					datos_filtrados <- datos_filtrados[tipo_aportante == input$filtro_tipo_aportante]
				}

				# Filtro: Estado Aportante
				if (!is.null(input$filtro_estado_aportante) &&
				input$filtro_estado_aportante != "" &&
				input$filtro_estado_aportante != "Todos") {
					datos_filtrados <- datos_filtrados[estado_del_aportante == input$filtro_estado_aportante]
				}

				# Filtro: Año
				if (!is.null(input$filtro_ano) &&
				input$filtro_ano != "" &&
				input$filtro_ano != "Todos") {
					datos_filtrados <- datos_filtrados[ano == input$filtro_ano]
				}

				# Filtro: Documento
				if (!is.null(input$filtro_documento) &&
				input$filtro_documento != "" &&
				input$filtro_documento != "Todos") {
					datos_filtrados <- datos_filtrados[numero_de_documento_del_aportante == input$filtro_documento]
				}

				# Filtro: Periodo de Mora
				if (!is.null(input$filtro_periodo_mora) &&
				input$filtro_periodo_mora != "" &&
				input$filtro_periodo_mora != "Todos") {
					datos_filtrados <- datos_filtrados[periodo_de_mora == input$filtro_periodo_mora]
				}

				############################## Gestiones #####################################

				# --- Renderizar la tabla ---
				output$tabla_resultados <- renderDT({

					if (nrow(datos_filtrados) == 0) {
						return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
					}

					datos_mostrar <- unique(
						datos_filtrados[, .(numero_de_documento_del_aportante,
						mes_asigna,
						razon_social,
						tipo_aportante,
						valor_cartera,
						cant_trabajadores_mora,
						cantidad_periodos_mora)]
					)

					datos_mostrar[, valor_cartera := formatC(valor_cartera, format = "d", big.mark = ".", decimal.mark = ",")]

					setnames(datos_mostrar,
						c("numero_de_documento_del_aportante","mes_asigna", "razon_social", "tipo_aportante",
							"valor_cartera", "cant_trabajadores_mora", "cantidad_periodos_mora"),
						c("Identificacion Aportante","Mes Asignacion", "Razon Social", "Tipo Aportante",
							"Valor Cartera", "Trabajadores en Mora", "Periodos en Mora"
						)
					)

					datatable(datos_mostrar,
						options = list(pageLength = 5, scrollX = TRUE),
					rownames = FALSE)
				})
				
				output$value_total_cartera <- renderValueBox({
					

					if (nrow(datos_filtrados) == 0) {
						return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
					}

					datos_mostrar <- unique(
						datos_filtrados[, .(numero_de_documento_del_aportante,
						mes_asigna,
						razon_social,
						tipo_aportante,
						valor_cartera,
						cant_trabajadores_mora,
						cantidad_periodos_mora)]
					)

					total <- sum(as.numeric(datos_mostrar$valor_cartera), na.rm = TRUE)

					# valueBox(
						# subtitle = "Valor Total de Cartera",
						# value = paste0("$ ", formatC(total, format = "d", big.mark = ".", decimal.mark = ",")),
						# icon = icon("wallet"),
						# color = "orange",
						# width = 4
					# )
					valueBox(
						value = HTML(paste0(
							'<div class="box-redonda" style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 70px;">',
							  '<div style="font-size: 33px; font-weight: bold;">$ ', formatC(total, format = "d", big.mark = ".", decimal.mark = ","), '</div>',
							  '<div style="font-size: 14px; font-weight: normal;">Valor Total de Cartera</div>',
							'</div>'
						)),
						subtitle = " ",
						icon = icon("wallet"),
						color = "orange",
						width = 4
					)
				})
				
				output$value_trabajadores_mora <- renderValueBox({
					

					if (nrow(datos_filtrados) == 0) {
						return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
					}

					datos_mostrar <- unique(
						datos_filtrados[, .(numero_de_documento_del_aportante,
						mes_asigna,
						razon_social,
						tipo_aportante,
						valor_cartera,
						cant_trabajadores_mora,
						cantidad_periodos_mora)]
					)

					total <- sum(as.numeric(datos_mostrar$cant_trabajadores_mora), na.rm = TRUE)

					# valueBox(
						# subtitle = "Total Trabajadores en Mora",
						# value = paste0(formatC(total, format = "d", big.mark = ".", decimal.mark = ",")),
						# icon = icon("users"),
						# color = "navy",
						# width = 4
					# )
					valueBox(
						value = HTML(paste0(
							'<div class="box-redonda" style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 70px;">',
							  '<div style="font-size: 33px; font-weight: bold;"> ', formatC(total, format = "d", big.mark = ".", decimal.mark = ","), '</div>',
							  '<div style="font-size: 14px; font-weight: normal;">Total Trabajadores en Mora</div>',
							'</div>'
						)),
						subtitle = " ",
						icon = icon("users"),
						color = "orange",
						width = 4
					)
				})
				output$value_periodos_mora <- renderValueBox({
					

					if (nrow(datos_filtrados) == 0) {
						return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
					}

					datos_mostrar <- unique(
						datos_filtrados[, .(numero_de_documento_del_aportante,
						mes_asigna,
						razon_social,
						tipo_aportante,
						valor_cartera,
						cant_trabajadores_mora,
						cantidad_periodos_mora)]
					)

					total <- sum(as.numeric(datos_mostrar$cantidad_periodos_mora), na.rm = TRUE)

					# valueBox(
						# subtitle = "Total Periodos en Mora",
						# value = paste0(formatC(total, format = "d", big.mark = ".", decimal.mark = ",")),
						# icon = icon("calendar-alt"),
						# color = "maroon",
						# width = 4
					# )
					valueBox(
						value = HTML(paste0(
							'<div class="box-redonda" style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 70px;">',
							  '<div style="font-size: 33px; font-weight: bold;"> ', formatC(total, format = "d", big.mark = ".", decimal.mark = ","), '</div>',
							  '<div style="font-size: 14px; font-weight: normal;">Total Periodos en Mora</div>',
							'</div>'
						)),
						subtitle = " ",
						icon = icon("calendar-alt"),
						color = "orange",
						width = 4
					)
				})
				
				if (nrow(datos_filtrados) == 0) {
					return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
				}

				datos_mostrar <- unique(
					datos_filtrados[, .(numero_de_documento_del_aportante,
					mes_asigna,
					razon_social,
					tipo_aportante,
					valor_cartera,
					cant_trabajadores_mora,
					cantidad_periodos_mora)]
				)

				datos_mostrar[, valor_cartera := formatC(valor_cartera, format = "d", big.mark = ".", decimal.mark = ",")]

				setnames(datos_mostrar,
					c("numero_de_documento_del_aportante","mes_asigna", "razon_social", "tipo_aportante",
						"valor_cartera", "cant_trabajadores_mora", "cantidad_periodos_mora"),
					c("Identificacion Aportante","Mes Asignacion", "Razon Social", "Tipo Aportante",
						"Valor Cartera", "Trabajadores en Mora", "Periodos en Mora"
					)
				)
				Arch_CP <- data.frame(datos_mostrar)
				
				output$descarga_tabla_resumen <- downloadHandler(
				  filename = function() {
					paste("Datos_Resumen_", input$filtro_documento, ".csv", sep="")
				  },
				  content = function(file) {
					fwrite(Arch_CP,sep=";", file)
				   }
				)

			}) # fin observe que contiene la logica de renderizacion


			output$tabla_resultados_historico <- renderDT({
				datos <- resultado_consulta_uni()
				req(!is.null(datos), nrow(datos) > 0)

				datos_unicos <- unique(datos[, .(nro_caso,fecha_gestion, observaciones_gestion, numero_de_documento_del_aportante)])

				datos_unicos[, id_seguro := gsub("[^a-zA-Z0-9]", "_", as.character(fecha_gestion))]

				# 🔵 Boton gestion
				datos_unicos[, boton_gestion := sprintf(
					'<button id="btn_detalle_%s" class="btn btn-info btn-sm"><i class="fa fa-search"></i></button>',
					id_seguro
				)]

				# 🟢 Boton notificacion
				# datos_unicos[, boton_notifica := sprintf(
				# '<button id="btn_notifica_%s" class="btn btn-success btn-sm"><i class="fa fa-bell"></i></button>',
				# id_seguro
				# )]

				# Columna combinada de acciones
				#datos_unicos[, Acciones := paste(boton_gestion, boton_notifica)]
				datos_unicos[, Acciones := boton_gestion]

				datatable(
					datos_unicos[, .(Caso = nro_caso, Fecha = fecha_gestion, Observacion = observaciones_gestion, Acciones)],
					escape = FALSE,
					selection = "none",
					rownames = FALSE,
					options = list(pageLength = 5, dom = 'tip')
				)
			})



			observeEvent(input$last_btn_click, {
				req(input$last_btn_click)

				datos <- resultado_consulta_uni()
				req(!is.null(datos), nrow(datos) > 0)

				id_raw <- gsub("btn_detalle_", "", input$last_btn_click)

				datos_unicos <- unique(datos[, .(fecha_gestion)])
				datos_unicos[, id_seguro := gsub("[^a-zA-Z0-9]", "_", as.character(fecha_gestion))]

				fecha_seleccionada <- datos_unicos[id_seguro == id_raw, fecha_gestion]
				req(length(fecha_seleccionada) == 1)

				mostrarFichaGestion(fecha_seleccionada = fecha_seleccionada, datos = datos)
			})

			observeEvent(input$btn_ver_notificaciones, {
				req(input$filtro_doc_historico)

				doc <- input$filtro_doc_historico
				req(!is.null(doc), doc != "")

				notifica <- consulta_Notifica(doc)

				if (nrow(notifica) == 0) {
					showModal(modalDialog(
						title = "Notificaciones",
						"No se encontraron notificaciones para este documento.",
						easyClose = TRUE,
						footer = modalButton("Cerrar")
					))
					return()
				}

				# Validacion defensiva
				columnas_necesarias <- c("fecha_envio_notificacion", "identificacion", "estado_aportante",
				"notificacion", "estado_notificacion", "ultimo_evento", "mes_procedencia")
				for (col in columnas_necesarias) {
					if (!(col %in% names(notifica))) {
						notifica[[col]] <- NA
					}
				}

				showModal(modalDialog(
					title = div(icon("bell"), "Detalle de Notificaciones"),
					size  = "l",
					easyClose = TRUE,
					footer = tagList(
						div(
							style = "float: left;",
							downloadLink(
								outputId = "descargar_evidencia",
								label    = "Descargar PDF",
								class    = "btn btn-primary"   # para que se vea como botón sólido
							)
						),
						div(
							style = "float: right;",
							modalButton("Cerrar")
						),
						# Para prevenir que queden flotando, añadimos un clear
						tags$div(style = "clear: both;")
					),
					div(
						style = "background-color: #fef5e7; padding: 15px; border-radius: 8px; overflow-x: auto;",
						tags$table(class = "table table-bordered table-striped",
							tags$thead(
								tags$tr(lapply(columnas_necesarias, tags$th))
							),
							tags$tbody(
								lapply(1:nrow(notifica), function(i) {
									tags$tr(lapply(columnas_necesarias, function(col) tags$td(as.character(notifica[[col]][i]))))
								})
							)
						)
					)
				))

			})


			observe({
				datos_hist <- historico_reactivo()
				req(!is.null(datos_hist), nrow(datos_hist) > 0)

				documentos <- sort(unique(as.character(datos_hist$nro_documento)))

				updateSelectizeInput(
					session,
					inputId = "filtro_doc_historico",
					choices = documentos,
					server = TRUE
				)
			})

			observeEvent(input$btn_aplicar_doc, {
				req(input$filtro_doc_historico)

				admin_usuario <- US[USER$Id.username, "Cod_Administradora"]
				req(!is.null(admin_usuario) && admin_usuario != "")

				# ─── Mostrar spinner full‐screen ───────────────────────────────────
				waiter_show(
					html  = splash_screen,
					color = "rgba(255,255,255,0.8)"
				)

				datos <- consulta_Gestion_Uni(
					Documento      = input$filtro_doc_historico,
					Administradora = admin_usuario
				)

				# ─── Ocultar spinner full‐screen ────────────────────────────────────
				waiter_hide()

				if (nrow(datos) == 0) {
					showModal(modalDialog(
						title = "Aviso",
						"No hay datos disponibles para este aportante.",
						easyClose = TRUE,
						footer = modalButton("Cerrar")
					))
					resultado_consulta_uni(NULL)
				} else {
					# 🔧 Aqui renombramos la columna para que el resto del codigo funcione
					setnames(datos, "nro_documento", "numero_de_documento_del_aportante", skip_absent = TRUE)
					resultado_consulta_uni(as.data.table(datos))
				}
			})

			############################## Historico Gestiones #####################################

		} # fin if USER$Logged == 1

		if (USER$Logged == "3") {

			datos_reactivos <- reactiveVal(NULL)
			historico_reactivo <- reactiveVal(NULL)
			resultado_consulta_uni <- reactiveVal(NULL)


			datos_hist <- consulta_Gestion()
			historico_reactivo(datos_hist)


			# ---- Sidebar actualizado ----
			output$Cua <- renderMenu({
				sidebarMenu(id = "Log",
					menuItem(".", tabName = "Id_temp", icon = icon("sign-in-alt")),
					menuItem("NiyaGO", tabName = "Id_NiyaGo", icon = icon("sign-in-alt")),
					menuItem("Gestion de Cobro", tabName = "Id_Gestion", icon = icon("table")),
					menuItem("Historico Gestiones", tabName = "Id_Historico_Gestion", icon = icon("table"))
				)
			})

			isolate({updateTabItems(session, "Log", "Id_NiyaGo")})

			# ---- Caja principal ----
			output$Caja <- renderUI({

				anios <- as.character(2023:year(Sys.Date()))
				meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio","Agosto",
				"Septiembre","Octubre","Noviembre","Diciembre")

				tabItems(
					
					tabItem(tabName = "Id_NiyaGo",
						fluidRow(
							column(12,
								box(
									width = 12,
									status = "primary",
									solidHeader = TRUE,
									title = div(icon("info-circle"), "Bienvenido a NiyaGo"),
									div(
										style = "padding: 30px; font-size: 17px; line-height: 1.6; background-color: #fef5e7; border-radius: 10px;",
										tags$h3("¿Que es NiyaGo?", style = "color: #d35400; font-weight: bold;"),
										tags$p("NiyaGo es una plataforma de trazabilidad diseñada para optimizar la gestion de cobros por aportante. Su objetivo es centralizar la informacion clave del proceso y facilitar la toma de decisiones."),
										tags$br(),
										tags$p("A traves de esta herramienta usted podra:"),
										tags$ul(
											tags$li("Consultar el estado de la cartera por periodo y tipo de aportante."),
											tags$li("Visualizar el historial de gestiones realizadas."),
											tags$li("Acceder a las evidencias recopiladas, como grabaciones y correos electronicos."),
											tags$li("Descargar estados de cuenta detallados.")
										),
										tags$br(),
										tags$p(tags$em("Todo en un solo lugar, de forma agil, segura y eficiente.")),
										tags$br()#,
									)
								)
							)
						)
					),


					tabItem(tabName = "Id_Gestion",
						fluidRow(
							column(3,
								selectInput("ano_sel", "Año Asignacion", choices = anios, selected = max(anios),width = "100%"),
								br(),
								fluidRow(
									column(6,
										actionButton("actualizar", "Actualizar", icon = icon("sync"), class = "boton-3d", width = "100%")
									),
									column(6,
										conditionalPanel(
											condition = "input.actualizar >= 1",
											actionButton("limpiar_filtros", "Limpiar filtros", icon = icon("eraser"), class = "boton-3d", width = "100%")
										)
									)
								)
							),
							column(3,
								selectizeInput("mes_sel", "Mes Asignacion", 
									choices = meses, 
									selected = "Abril", 
									multiple = TRUE,
									options = list(placeholder = 'Seleccione uno o mas meses...'),
									width = "100%"
								),
								uiOutput("filtro_tipo_aportante")

							),
							column(3,
								uiOutput("filtro_ano"),
								uiOutput("filtro_documento")
							),
							column(3,
								uiOutput("filtro_periodo_mora"),
								uiOutput("filtro_estado_aportante")
							)
						),
						br(),
						fluidRow(

							#column(9,
							conditionalPanel(
								condition = "input.actualizar>=1",
								fluidRow(
									box(
										status = "primary", solidHeader = TRUE, width = 12,
										DTOutput("tabla_resultados"),
										br(),
										div(
											class = "text-right",  # Alineación a la derecha
											style = "display: flex; justify-content: flex-end; gap: 10px;",
											uiOutput("download_ecuenta_btn"),
											downloadButton("descarga_tabla_resumen", "Descargar Tabla", icon = icon("file-download"), class = "boton-3d")
										),
										class = "box-naranja"
									)
								),
								br(),
								fluidRow(								
									valueBoxOutput("value_total_cartera", width = 4),
									valueBoxOutput("value_trabajadores_mora", width = 4),
									valueBoxOutput("value_periodos_mora", width = 4)
									
								)
							)	
						)
					),
					tabItem(tabName = "Id_Historico_Gestion",
						fluidRow(
							column(2, 
								style = "border-right: 1px solid #dcdcdc; border-bottom: 1px solid #dcdcdc; padding-right: 20px; padding-bottom: 15px;",
								selectizeInput(
									inputId = "filtro_doc_historico",
									label = "Documento Aportante",
									choices = NULL,
									options = list(placeholder = 'Seleccione un documento...'),
									selected = NULL,
									width = "100%"
								),
								actionButton("btn_aplicar_doc", "Aplicar", icon = icon("search"), class = "boton-3d",width = "100%")
							),
							column(10,
								conditionalPanel(
									condition = "input.btn_aplicar_doc>=1",
									
									box(
										title = div(icon("history"), "Histórico de Gestiones"),
										width = 12,
										status = "warning",
										solidHeader = TRUE,
										div(
											DTOutput("tabla_resultados_historico"),
											br(),
											actionButton(
												inputId = "btn_ver_notificaciones",
												label = "Ver Notificaciones",
												icon = icon("bell"),
												class = "boton-3d"
											)
										),
										class = "box-naranja"
									)
								)
							)
						)						
					)
				)
			})

			# ---- Filtros vacios inicialmente ----
			output$filtro_tipo_aportante <- renderUI({})
			output$filtro_estado_aportante <- renderUI({})
			output$filtro_ano <- renderUI({})
			output$filtro_documento <- renderUI({})
			output$filtro_periodo_mora <- renderUI({})

			
			
			# ---- Evento: Al hacer clic en Actualizar ----
			observeEvent(input$actualizar, {
				cat(">>> Se presiono actualizar\n")
				req(USER$Logged %in% c("2", "3"))
				req(input$ano_sel != "")
				req(length(input$mes_sel) >= 1)

				# ─── Mostrar spinner fullscreen ───────────────────────────────────
				waiter_show(
					html  = spin_flower(),            # o tu splash_screen que definiste arriba
					color = "#f2923d"
				)
				Sys.sleep(0.05)

				# Obtiene la administradora con la que el usuario está logueado
				admin_usuario <- US[USER$Id.username, "Cod_Administradora"]
				req(!is.null(admin_usuario) && admin_usuario != "")

				# ── Aplicar tryCatch para capturar posibles errores de consulta() ──────────
				datos <- NULL
				error_msg <- NULL
				tryCatch({
					datos <- consulta(
						Ano = input$ano_sel,
						Mes = input$mes_sel,
						Administradora = admin_usuario
					)
				}, error = function(e) {
					error_msg <<- e$message
				})
				# ──────────────────────────────────────────────────────────────────────────

				# Oculto el spinner fullscreen (independientemente de si hubo error o no)
				waiter_hide()

				# Si error_msg no es NULL, hubo fallo en consulta(); mostramos modal de error
				if (!is.null(error_msg)) {
					showModal(modalDialog(
						title = "Error al cargar datos",
						paste0("Ocurrió un problema en consulta():\n", error_msg),
						easyClose = TRUE,
						footer = modalButton("Cerrar")
					))
					return()
				}

				# Llegamos aquí solo si consulta() devolvió datos sin error
				datos <- as.data.table(datos)
				datos_reactivos(datos)

				# ── Ahora reconstruimos los filtros dinámicos basados en “datos” ────────────

				# 1) Filtro Tipo Aportante
				tipos <- sort(unique(as.character(datos$tipo_aportante[!is.na(datos$tipo_aportante)])))
				output$filtro_tipo_aportante <- renderUI({
					selectizeInput(
						inputId = "filtro_tipo_aportante",
						label = "Tipo Aportante",
						choices = c("Todos", tipos),
						selected = "Todos",
						options = list(
							placeholder = '...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})

				updateSelectizeInput(
					session,
					inputId = "filtro_tipo_aportante",
					choices = c("Todos", tipos),
					selected = "Todos",
					server = TRUE
				)

				# 2) Filtro Estado del Aportante
				estados <- sort(unique(as.character(datos$estado_del_aportante[!is.na(datos$estado_del_aportante)])))
				output$filtro_estado_aportante <- renderUI({
					selectizeInput(
						inputId = "filtro_estado_aportante",
						label = "Estado del Aportante",
						choices = c("Todos", estados),
						selected = "Todos",
						options = list(
							placeholder = '...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})

				updateSelectizeInput(
					session,
					inputId = "filtro_estado_aportante",
					choices = c("Todos", estados),
					selected = "Todos",
					server = TRUE
				)

				# 3) Filtro Año
				anios_filtro <- as.character(sort(unique(datos$ano[!is.na(datos$ano)])))
				output$filtro_ano <- renderUI({
					selectizeInput(
						inputId = "filtro_ano",
						label = "Año",
						choices = c("Todos", anios_filtro),
						selected = "Todos",
						options = list(
							placeholder = 'Escriba el año...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})
				updateSelectizeInput(
					session,
					inputId = "filtro_ano",
					choices = c("Todos", anios_filtro),
					selected = "Todos",
					server = TRUE
				)

				# 4) Filtro Documento Aportante
				documentos <- sort(unique(as.character(
					datos$numero_de_documento_del_aportante[!is.na(datos$numero_de_documento_del_aportante)]
				)))
				output$filtro_documento <- renderUI({
					selectizeInput(
						inputId = "filtro_documento",
						label = "Documento Aportante",
						choices = NULL,
						selected = "Todos",
						options = list(
							placeholder = 'Escriba para buscar...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})
				updateSelectizeInput(
					session,
					inputId = "filtro_documento",
					choices = c("Todos", documentos),
					selected = "Todos",
					server = TRUE
				)

				# 5) Filtro Periodo de Mora
				periodos_mora <- sort(unique(as.character(datos$periodo_de_mora[!is.na(datos$periodo_de_mora)])))
				output$filtro_periodo_mora <- renderUI({
					selectizeInput(
						inputId = "filtro_periodo_mora",
						label = "Periodo de Mora",
						choices = c("Todos", periodos_mora),
						selected = "Todos",
						options = list(
							placeholder = 'Escriba el periodo...',
							openOnFocus = FALSE,
							maxOptions = 10
						),
						width = "100%"
					)
				})

				updateSelectizeInput(
					session,
					inputId = "filtro_periodo_mora",
					choices = c("Todos", periodos_mora),
					selected = "Todos",
					server = TRUE
				)

				# ── Fin del bloque que se ejecuta cuando consulta() fue exitoso ────────────
			})

			observeEvent(input$limpiar_filtros, {
				updateSelectizeInput(session, "filtro_periodo_mora", selected = "Todos")
				updateSelectizeInput(session, "filtro_documento", selected = "Todos")
				updateSelectizeInput(session, "filtro_ano", selected = "Todos")
				updateSelectizeInput(session, "filtro_estado_aportante", selected = "Todos")
				updateSelectizeInput(session, "filtro_tipo_aportante", selected = "Todos")
			})
		

			######### Descargar Detalle ##################

			output$descargar_estado_cuenta <- downloadHandler(
				filename = function() {
					doc <- input$filtro_documento
					if (is.null(doc) || doc == "" || doc == "Todos") doc <- "todos"
					paste0("estado_de_cuenta_", doc, "_", Sys.Date(), ".xlsx")
				},
				content = function(file) {
					df_original <- datos_reactivos()
					req(!is.null(df_original))

					if (!is.null(input$filtro_tipo_aportante) && input$filtro_tipo_aportante != "Todos") {
						df_original <- df_original[tipo_aportante == input$filtro_tipo_aportante]
					}
					if (!is.null(input$filtro_documento) && input$filtro_documento != "Todos") {
						df_original <- df_original[numero_de_documento_del_aportante == input$filtro_documento]
					}
					if (!is.null(input$filtro_periodo_mora) && input$filtro_periodo_mora != "Todos") {
						df_original <- df_original[periodo_de_mora == input$filtro_periodo_mora]
					}

					if (nrow(df_original) == 0) {
						showModal(modalDialog(
							title = "Sin datos",
							"No hay registros que coincidan con los filtros seleccionados.",
							easyClose = TRUE,
							footer = modalButton("Cerrar")
						))
						req(FALSE)
					}

					df <- unique(df_original[, .(
						tipo_documento_cotizante,
						numero_de_documento_del_cotizante,
						nombre_del_cotizante,
						periodo_de_mora,
						valor_cartera_detalle,
						salario
					)])

					# Renombrar columnas para mostrar nombres bonitos
					colnames(df) <- c(
						"Tipo Documento Cotizante",
						"Número Documento Cotizante",
						"Nombre del Cotizante",
						"Periodo de Mora",
						"Valor Mora",
						"Salario"
					)
					valor_mora <- sum(df$'Valor Mora', na.rm = TRUE)
					df$'Valor Mora' <- scales::dollar(df$'Valor Mora', big.mark = ".", decimal.mark = ",", prefix = "$ ")

					wb <- createWorkbook()
					addWorksheet(wb, "EstadoCuenta", gridLines = FALSE)

					# Logo centrado en B2:C6 ajustado proporcionalmente
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 2:6)
					insertImage(wb, "EstadoCuenta", file = logo_reactivo(), startRow = 2, startCol = 2,
					width = 304 * 2, height = 304, units = "px")

					# Título centrado en D2:J6
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 2:6)
					writeData(wb, "EstadoCuenta", "ESTADO DE CUENTA", startCol = 4, startRow = 2)
					addStyle(wb, "EstadoCuenta", createStyle(
						fontSize = 16, textDecoration = "bold", halign = "center", valign = "center",
						fgFill = color_reactivo(), border = "TopBottomLeftRight"
					), rows = 2:6, cols = 4:7, gridExpand = TRUE)

					# Razón social
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 8)
					writeData(wb, "EstadoCuenta", "RAZÓN SOCIAL:", startCol = 2, startRow = 8)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold"
					), rows = 8, cols = 2:3, gridExpand = TRUE)

					razon_social <- unique(df_original$nombre_o_razon_social_del_aportante)[1]
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 8)
					writeData(wb, "EstadoCuenta", toupper(razon_social), startCol = 4, startRow = 8)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold",
						fgFill = color_reactivo()
					), rows = 8, cols = 4:7, gridExpand = TRUE)

					# Documento
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 10)
					writeData(wb, "EstadoCuenta", "DOCUMENTO", startCol = 2, startRow = 10)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold"
					), rows = 10, cols = 2:3, gridExpand = TRUE)

					doc_value <- if (!is.null(input$filtro_documento) && input$filtro_documento != "Todos") {
						toupper(as.character(input$filtro_documento))
					} else { "TODOS" }
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 10)
					writeData(wb, "EstadoCuenta", doc_value, startCol = 4, startRow = 10)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold",
						fgFill = color_reactivo()
					), rows = 10, cols = 4:7, gridExpand = TRUE)

					# Tipo documento
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 12)
					writeData(wb, "EstadoCuenta", "TIPO DE DOCUMENTO", startCol = 2, startRow = 12)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold"
					), rows = 12, cols = 2:3, gridExpand = TRUE)

					tipo_doc <- unique(df_original$tipo_de_documento_del_aportante)[1]
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 12)
					writeData(wb, "EstadoCuenta", toupper(tipo_doc), startCol = 4, startRow = 12)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold",
						fgFill = color_reactivo()
					), rows = 12, cols = 4:7, gridExpand = TRUE)

					# Valor mora
					mergeCells(wb, "EstadoCuenta", cols = 2:3, rows = 14)
					writeData(wb, "EstadoCuenta", "VALOR MORA", startCol = 2, startRow = 14)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold"
					), rows = 14, cols = 2:3, gridExpand = TRUE)


					valor_mora_fmt <- scales::dollar(valor_mora, big.mark = ".", decimal.mark = ",", prefix = "$ ")
					mergeCells(wb, "EstadoCuenta", cols = 4:7, rows = 14)
					writeData(wb, "EstadoCuenta", valor_mora_fmt, startCol = 4, startRow = 14)
					addStyle(wb, "EstadoCuenta", createStyle(
						border = "TopBottomLeftRight", halign = "center", textDecoration = "bold",
						fgFill = color_reactivo()
					), rows = 14, cols = 4:7, gridExpand = TRUE)

					# Tabla desde fila 16
					headerStyle <- createStyle(fontSize = 11, textDecoration = "bold", halign = "center", fgFill = color_reactivo())
					writeData(wb, "EstadoCuenta", x = as.data.frame(df), startCol = 2, startRow = 16, colNames = TRUE)
					addStyle(wb, "EstadoCuenta", style = headerStyle, rows = 16, cols = 2:(ncol(df)+1), gridExpand = TRUE)

					bodyStyle <- createStyle(fontSize = 11, halign = "left")
					addStyle(wb, "EstadoCuenta", style = bodyStyle, rows = 17:(nrow(df)+16), cols = 2:(ncol(df)+1), gridExpand = TRUE)

					# Bordes a toda la tabla
					addStyle(wb, "EstadoCuenta", style = createStyle(border = "TopBottomLeftRight"),
					rows = 16:(nrow(df)+16), cols = 2:(ncol(df)+1), gridExpand = TRUE, stack = TRUE)

					setColWidths(wb, "EstadoCuenta", cols = 2:(ncol(df)+1), widths = "auto")
					saveWorkbook(wb, file, overwrite = TRUE)
				}
			)

			# ---- Renderizar tabla ----
			output$descargar_evidencia <- downloadHandler(
				filename = function() {
					"Evidencia prueba.pdf"
				},
				content = function(file) {
					# Ajusta la ruta si tu PDF está en otra carpeta, p.ej. "www/datos/Evidencia prueba.pdf"
					file.copy(from = "datos/Evidencia prueba.pdf", to = file, overwrite = TRUE)
				},
				contentType = "application/pdf"
			)
			
			observe({

				datos <- datos_reactivos()
				req(!is.null(datos))  # Solo procede si hay datos cargados

				datos_filtrados <- datos

				# Filtro: Tipo Aportante
				if (!is.null(input$filtro_tipo_aportante) &&
					input$filtro_tipo_aportante != "" &&
				input$filtro_tipo_aportante != "Todos") {
					datos_filtrados <- datos_filtrados[tipo_aportante == input$filtro_tipo_aportante]
				}

				# Filtro: Estado Aportante
				if (!is.null(input$filtro_estado_aportante) &&
				input$filtro_estado_aportante != "" &&
				input$filtro_estado_aportante != "Todos") {
					datos_filtrados <- datos_filtrados[estado_del_aportante == input$filtro_estado_aportante]
				}

				# Filtro: Año
				if (!is.null(input$filtro_ano) &&
				input$filtro_ano != "" &&
				input$filtro_ano != "Todos") {
					datos_filtrados <- datos_filtrados[ano == input$filtro_ano]
				}

				# Filtro: Documento
				if (!is.null(input$filtro_documento) &&
				input$filtro_documento != "" &&
				input$filtro_documento != "Todos") {
					datos_filtrados <- datos_filtrados[numero_de_documento_del_aportante == input$filtro_documento]
				}

				# Filtro: Periodo de Mora
				if (!is.null(input$filtro_periodo_mora) &&
				input$filtro_periodo_mora != "" &&
				input$filtro_periodo_mora != "Todos") {
					datos_filtrados <- datos_filtrados[periodo_de_mora == input$filtro_periodo_mora]
				}

				############################## Gestiones #####################################

				# --- Renderizar la tabla ---
				output$tabla_resultados <- renderDT({

					if (nrow(datos_filtrados) == 0) {
						return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
					}

					datos_mostrar <- unique(
						datos_filtrados[, .(numero_de_documento_del_aportante,
						mes_asigna,
						razon_social,
						tipo_aportante,
						valor_cartera,
						cant_trabajadores_mora,
						cantidad_periodos_mora)]
					)

					datos_mostrar[, valor_cartera := formatC(valor_cartera, format = "d", big.mark = ".", decimal.mark = ",")]

					setnames(datos_mostrar,
						c("numero_de_documento_del_aportante","mes_asigna", "razon_social", "tipo_aportante",
							"valor_cartera", "cant_trabajadores_mora", "cantidad_periodos_mora"),
						c("Identificacion Aportante","Mes Asignacion", "Razon Social", "Tipo Aportante",
							"Valor Cartera", "Trabajadores en Mora", "Periodos en Mora"
						)
					)

					datatable(datos_mostrar,
						options = list(pageLength = 5, scrollX = TRUE),
					rownames = FALSE)
				})
				
				output$value_total_cartera <- renderValueBox({
					

					if (nrow(datos_filtrados) == 0) {
						return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
					}

					datos_mostrar <- unique(
						datos_filtrados[, .(numero_de_documento_del_aportante,
						mes_asigna,
						razon_social,
						tipo_aportante,
						valor_cartera,
						cant_trabajadores_mora,
						cantidad_periodos_mora)]
					)

					total <- sum(as.numeric(datos_mostrar$valor_cartera), na.rm = TRUE)

					# valueBox(
						# subtitle = "Valor Total de Cartera",
						# value = paste0("$ ", formatC(total, format = "d", big.mark = ".", decimal.mark = ",")),
						# icon = icon("wallet"),
						# color = "orange",
						# width = 4
					# )
					valueBox(
						value = HTML(paste0(
							'<div class="box-redonda" style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 70px;">',
							  '<div style="font-size: 33px; font-weight: bold;">$ ', formatC(total, format = "d", big.mark = ".", decimal.mark = ","), '</div>',
							  '<div style="font-size: 14px; font-weight: normal;">Valor Total de Cartera</div>',
							'</div>'
						)),
						subtitle = " ",
						icon = icon("wallet"),
						color = "orange",
						width = 4
					)
				})
				
				output$value_trabajadores_mora <- renderValueBox({
					

					if (nrow(datos_filtrados) == 0) {
						return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
					}

					datos_mostrar <- unique(
						datos_filtrados[, .(numero_de_documento_del_aportante,
						mes_asigna,
						razon_social,
						tipo_aportante,
						valor_cartera,
						cant_trabajadores_mora,
						cantidad_periodos_mora)]
					)

					total <- sum(as.numeric(datos_mostrar$cant_trabajadores_mora), na.rm = TRUE)

					# valueBox(
						# subtitle = "Total Trabajadores en Mora",
						# value = paste0(formatC(total, format = "d", big.mark = ".", decimal.mark = ",")),
						# icon = icon("users"),
						# color = "navy",
						# width = 4
					# )
					valueBox(
						value = HTML(paste0(
							'<div class="box-redonda" style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 70px;">',
							  '<div style="font-size: 33px; font-weight: bold;"> ', formatC(total, format = "d", big.mark = ".", decimal.mark = ","), '</div>',
							  '<div style="font-size: 14px; font-weight: normal;">Total Trabajadores en Mora</div>',
							'</div>'
						)),
						subtitle = " ",
						icon = icon("users"),
						color = "orange",
						width = 4
					)
				})
				output$value_periodos_mora <- renderValueBox({
					

					if (nrow(datos_filtrados) == 0) {
						return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
					}

					datos_mostrar <- unique(
						datos_filtrados[, .(numero_de_documento_del_aportante,
						mes_asigna,
						razon_social,
						tipo_aportante,
						valor_cartera,
						cant_trabajadores_mora,
						cantidad_periodos_mora)]
					)

					total <- sum(as.numeric(datos_mostrar$cantidad_periodos_mora), na.rm = TRUE)

					# valueBox(
						# subtitle = "Total Periodos en Mora",
						# value = paste0(formatC(total, format = "d", big.mark = ".", decimal.mark = ",")),
						# icon = icon("calendar-alt"),
						# color = "maroon",
						# width = 4
					# )
					valueBox(
						value = HTML(paste0(
							'<div class="box-redonda" style="display: flex; flex-direction: column; align-items: center; justify-content: center; height: 70px;">',
							  '<div style="font-size: 33px; font-weight: bold;"> ', formatC(total, format = "d", big.mark = ".", decimal.mark = ","), '</div>',
							  '<div style="font-size: 14px; font-weight: normal;">Total Periodos en Mora</div>',
							'</div>'
						)),
						subtitle = " ",
						icon = icon("calendar-alt"),
						color = "orange",
						width = 4
					)
				})
				
				if (nrow(datos_filtrados) == 0) {
					return(datatable(data.table(Mensaje = "No hay datos para mostrar con los filtros seleccionados.")))
				}

				datos_mostrar <- unique(
					datos_filtrados[, .(numero_de_documento_del_aportante,
					mes_asigna,
					razon_social,
					tipo_aportante,
					valor_cartera,
					cant_trabajadores_mora,
					cantidad_periodos_mora)]
				)

				datos_mostrar[, valor_cartera := formatC(valor_cartera, format = "d", big.mark = ".", decimal.mark = ",")]

				setnames(datos_mostrar,
					c("numero_de_documento_del_aportante","mes_asigna", "razon_social", "tipo_aportante",
						"valor_cartera", "cant_trabajadores_mora", "cantidad_periodos_mora"),
					c("Identificacion Aportante","Mes Asignacion", "Razon Social", "Tipo Aportante",
						"Valor Cartera", "Trabajadores en Mora", "Periodos en Mora"
					)
				)
				Arch_CP <- data.frame(datos_mostrar)
				
				output$descarga_tabla_resumen <- downloadHandler(
				  filename = function() {
					paste("Datos_Resumen_", input$filtro_documento, ".csv", sep="")
				  },
				  content = function(file) {
					fwrite(Arch_CP,sep=";", file)
				   }
				)

			}) # fin observe que contiene la logica de renderizacion


			output$tabla_resultados_historico <- renderDT({
				datos <- resultado_consulta_uni()
				req(!is.null(datos), nrow(datos) > 0)

				datos_unicos <- unique(datos[, .(nro_caso,fecha_gestion, observaciones_gestion, numero_de_documento_del_aportante)])

				datos_unicos[, id_seguro := gsub("[^a-zA-Z0-9]", "_", as.character(fecha_gestion))]

				# 🔵 Boton gestion
				datos_unicos[, boton_gestion := sprintf(
					'<button id="btn_detalle_%s" class="btn btn-info btn-sm"><i class="fa fa-search"></i></button>',
					id_seguro
				)]

				# 🟢 Boton notificacion
				# datos_unicos[, boton_notifica := sprintf(
				# '<button id="btn_notifica_%s" class="btn btn-success btn-sm"><i class="fa fa-bell"></i></button>',
				# id_seguro
				# )]

				# Columna combinada de acciones
				#datos_unicos[, Acciones := paste(boton_gestion, boton_notifica)]
				datos_unicos[, Acciones := boton_gestion]

				datatable(
					datos_unicos[, .(Caso = nro_caso,Fecha = fecha_gestion, Observacion = observaciones_gestion, Acciones)],
					escape = FALSE,
					selection = "none",
					rownames = FALSE,
					options = list(pageLength = 5, dom = 'tip')
				)
			})



			observeEvent(input$last_btn_click, {
				req(input$last_btn_click)

				datos <- resultado_consulta_uni()
				req(!is.null(datos), nrow(datos) > 0)

				id_raw <- gsub("btn_detalle_", "", input$last_btn_click)

				datos_unicos <- unique(datos[, .(fecha_gestion)])
				datos_unicos[, id_seguro := gsub("[^a-zA-Z0-9]", "_", as.character(fecha_gestion))]

				fecha_seleccionada <- datos_unicos[id_seguro == id_raw, fecha_gestion]
				req(length(fecha_seleccionada) == 1)

				mostrarFichaGestion(fecha_seleccionada = fecha_seleccionada, datos = datos)
			})

			observeEvent(input$btn_ver_notificaciones, {
				req(input$filtro_doc_historico)

				doc <- input$filtro_doc_historico
				req(!is.null(doc), doc != "")

				notifica <- consulta_Notifica(doc)

				if (nrow(notifica) == 0) {
					showModal(modalDialog(
						title = "Notificaciones",
						"No se encontraron notificaciones para este documento.",
						easyClose = TRUE,
						footer = modalButton("Cerrar")
					))
					return()
				}

				# Validacion defensiva
				columnas_necesarias <- c("fecha_envio_notificacion", "identificacion", "estado_aportante",
				"notificacion", "estado_notificacion", "ultimo_evento", "mes_procedencia")
				for (col in columnas_necesarias) {
					if (!(col %in% names(notifica))) {
						notifica[[col]] <- NA
					}
				}

				showModal(modalDialog(
					title = div(icon("bell"), "Detalle de Notificaciones"),
					size  = "l",
					easyClose = TRUE,
					footer = tagList(
						div(
							style = "float: left;",
							downloadLink(
								outputId = "descargar_evidencia",
								label    = "Descargar PDF",
								class    = "btn btn-primary"   # para que se vea como botón sólido
							)
						),
						div(
							style = "float: right;",
							modalButton("Cerrar")
						),
						# Para prevenir que queden flotando, añadimos un clear
						tags$div(style = "clear: both;")
					),
					div(
						style = "background-color: #fef5e7; padding: 15px; border-radius: 8px; overflow-x: auto;",
						tags$table(class = "table table-bordered table-striped",
							tags$thead(
								tags$tr(lapply(columnas_necesarias, tags$th))
							),
							tags$tbody(
								lapply(1:nrow(notifica), function(i) {
									tags$tr(lapply(columnas_necesarias, function(col) tags$td(as.character(notifica[[col]][i]))))
								})
							)
						)
					)
				))

			})


			observe({
				datos_hist <- historico_reactivo()
				req(!is.null(datos_hist), nrow(datos_hist) > 0)

				documentos <- sort(unique(as.character(datos_hist$nro_documento)))

				updateSelectizeInput(
					session,
					inputId = "filtro_doc_historico",
					choices = documentos,
					server = TRUE
				)
			})

			observeEvent(input$btn_aplicar_doc, {
				req(input$filtro_doc_historico)

				admin_usuario <- US[USER$Id.username, "Cod_Administradora"]
				req(!is.null(admin_usuario) && admin_usuario != "")

				# ─── Mostrar spinner full‐screen ───────────────────────────────────
				waiter_show(
					html  = splash_screen,
					color = "rgba(255,255,255,0.8)"
				)

				datos <- consulta_Gestion_Uni(
					Documento      = input$filtro_doc_historico,
					Administradora = admin_usuario
				)

				# ─── Ocultar spinner full‐screen ────────────────────────────────────
				waiter_hide()

				if (nrow(datos) == 0) {
					showModal(modalDialog(
						title = "Aviso",
						"No hay datos disponibles para este aportante.",
						easyClose = TRUE,
						footer = modalButton("Cerrar")
					))
					resultado_consulta_uni(NULL)
				} else {
					# 🔧 Aqui renombramos la columna para que el resto del codigo funcione
					setnames(datos, "nro_documento", "numero_de_documento_del_aportante", skip_absent = TRUE)
					resultado_consulta_uni(as.data.table(datos))
				}
			})

			############################## Historico Gestiones #####################################

		} # fin if USER$Logged == 1


	}) # fin observe general

	output$download_ecuenta_btn <- renderUI({
		doc   <- input$filtro_documento
		texto <- if (is.null(doc) || doc == "" || doc == "Todos") "todos" else doc
		downloadButton(
			outputId = "descargar_estado_cuenta",
			label    = paste0("Descargar estado de cuenta de ", texto)
		)
	})
	
	output$download_ecuenta_btn <- renderUI({
		doc   <- input$filtro_documento
		texto <- if (is.null(doc) || doc == "" || doc == "Todos") "todos" else doc
		downloadButton(
			outputId = "descargar_estado_cuenta",
			label    = paste0("Descargar estado de cuenta de ", texto)
		)
	})

}


# ----------------------- Lanzar app ----------------------- #

shinyApp(ui = ui, server = server)# Cargar librerias
