# ------------------------------------------------------------------------------
# Dashboard: Accidentes de tránsito en México, 2025
#
# Base: ATUS 2025 sin duplicados y solo accidentes causados por el conductor.
# Antes de correr la app, correr una vez preparar_datos.R (genera los .rds).
# Paquetes: install.packages(c("shiny", "bslib", "tidyverse", "scales", "plotly", "sf"))
# ------------------------------------------------------------------------------

library(shiny)
library(bslib)
library(tidyverse)
library(scales)
library(plotly)
library(sf)

datos <- readRDS("datos_app/datos_dashboard.rds")
modelo <- readRDS("datos_app/modelo_dashboard.rds")
mapa <- read_sf("datos_app/mexico_estados.geojson")

# Colores y estilo ----------------------------------------------------------
azul <- "#2a78d6"
naranja <- "#eb6834"
aqua <- "#1baf7a"
amarillo <- "#eda100"
rojo <- "#e34948"
gris <- "#9e9d98"
azules <- c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")
colores_grupo <- c(`Contexto del incidente` = azul, Espacio = naranja, Tiempo = aqua, Conductor = amarillo)
colores_objetivo <- c(`Con víctimas` = naranja, `Sólo daños` = azul)

tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "top")

# Catálogos -----------------------------------------------------------------
regiones <- sort(unique(datos$REGION))
entidades <- sort(unique(datos$ENTIDAD))
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
horas <- paste0(0:23, " h")
nombre_medida <- c(SEVERO = "% con víctimas", FATAL = "% fatales")
umbral <- modelo$simulador$umbral

vehiculos <- c(AUTOMOVIL = "Automóvil", CAMPASAJ = "Camioneta de pasajeros", MICROBUS = "Microbús",
               PASCAMION = "Camión urbano de pasajeros", OMNIBUS = "Ómnibus", TRANVIA = "Trolebús o tranvía",
               CAMIONETA = "Camioneta de carga", CAMION = "Camión de carga", TRACTOR = "Tractor",
               FERROCARRI = "Ferrocarril", MOTOCICLET = "Motocicleta", BICICLETA = "Bicicleta",
               OTROVEHIC = "Otro vehículo")

personas <- tibble(persona = c("Conductor", "Pasajero", "Peatón", "Ciclista", "Otro"),
                   muertos = c("CONDMUERTO", "PASAMUERTO", "PEATMUERTO", "CICLMUERTO", "OTROMUERTO"),
                   heridos = c("CONDHERIDO", "PASAHERIDO", "PEATHERIDO", "CICLHERIDO", "OTROHERIDO"))

# Funciones de apoyo ----------------------------------------------------------

# Convierte un ggplot a gráfica interactiva
a_plotly <- function(p) {
  ggplotly(p, tooltip = "text") %>% config(displayModeBar = FALSE)
}

# Número de accidentes y severidad (% con víctimas o % fatales) por una o más variables
resumen <- function(d, ..., medida = "SEVERO") {
  d %>%
    group_by(...) %>%
    summarise(accidentes = n(), tasa = mean(.data[[medida]]), .groups = "drop")
}

# Barras horizontales.
barras <- function(tabla, categoria, valor, formato = comma, total = NULL, linea = NULL, texto_linea = NULL, ordenar = TRUE) {
  tabla <- tabla %>%
    mutate(cat = as.character({{ categoria }}),
           val = {{ valor }},
           texto = paste0(cat, ": ", formato(val)))
  if (!is.null(total)) tabla$texto <- paste0(tabla$texto, " (", percent(tabla$val / total, accuracy = 0.1), ")")
  if ("accidentes" %in% names(tabla)) tabla$texto <- paste0(tabla$texto, "<br>Accidentes: ", comma(tabla$accidentes))
  tabla$cat <- if (ordenar) fct_reorder(tabla$cat, tabla$val) else fct_rev(fct_inorder(tabla$cat))
  
  p <- ggplot(tabla, aes(x = val, y = cat, text = texto)) +
    geom_col(fill = azul, width = 0.7) +
    scale_x_continuous(labels = formato, breaks = breaks_extended(n = 3)) +
    labs(x = NULL, y = NULL) +
    tema
  
  if (!is.null(linea)) {
    # Usamos el texto dinámico provisto o generamos uno simple como respaldo
    txt <- if (is.null(texto_linea)) paste0("Promedio: ", formato(linea)) else texto_linea
    df_linea <- data.frame(val_linea = linea, texto_linea = txt)
    
    p <- p + geom_vline(
      data = df_linea, 
      aes(xintercept = val_linea, text = texto_linea), 
      color = "grey40"
    )
  }
  a_plotly(p)
}


# Mapa por entidad
mapa_entidades <- function(tabla, escala) {
  p <- mapa %>%
    left_join(tabla, by = c("NOMBRE" = "ENTIDAD")) %>%
    ggplot() +
    geom_sf(aes(fill = val, text = texto), color = "white", linewidth = 0.2) +
    escala +
    theme_void()
  
  a_plotly(p) %>%
    style(hoveron = "fills") %>%
    layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
}

# Mapa de calor
calor <- function(tabla, x, y, valor, formato, nombre) {
  tabla <- tabla %>%
    mutate(val = {{ valor }},
           texto = paste0({{ y }}, ", ", {{ x }}, "<br>", nombre, ": ", formato(val)))
  if ("accidentes" %in% names(tabla)) tabla$texto <- paste0(tabla$texto, "<br>Accidentes: ", comma(tabla$accidentes))
  
  p <- ggplot(tabla, aes(x = {{ x }}, y = {{ y }}, fill = val, text = texto)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_gradientn(colors = azules, labels = formato, name = nombre) +
    labs(x = NULL, y = NULL) +
    tema +
    theme(panel.grid = element_blank())
  a_plotly(p)
}

# Número de vehículos agrupado
num_vehiculos <- function(d) {
  d %>% mutate(numero = if_else(NUM_VEHICULOS >= 4, "4 o más", as.character(NUM_VEHICULOS)))
}

# Variable objetivo con nombre
con_objetivo <- function(d) {
  d %>% mutate(objetivo = factor(if_else(SEVERO == 1, "Con víctimas", "Sólo daños"),
                                 levels = c("Sólo daños", "Con víctimas")))
}

# Tarjeta con una gráfica
tarjeta <- function(titulo, id, nota = NULL, alto = "380px") {
  card(full_screen = TRUE,
       card_header(titulo),
       plotlyOutput(id, height = alto),
       if (!is.null(nota)) card_footer(nota))
}

nota_promedio <- "La línea gris representa el porcentaje general de accidentes con el nivel de severidad seleccionado (con víctimas o fatales) para todos los datos seleccionados con los filtros de región y entidad."

# ------------------------------------------------------------------------------
# Interfaz
# ------------------------------------------------------------------------------
ui <- page_navbar(
  title = "Accidentes de tránsito en México, 2025",
  id = "pagina",
  fillable = FALSE,
  navbar_options = navbar_options(bg = azul),
  theme = bs_theme(version = 5, primary = azul, secondary = "#5f5e5a", warning = naranja, danger = rojo),
  # Sidebar ----
  sidebar = sidebar(
    width = 270, position = "right",
    
    conditionalPanel(
      "input.pagina == 'ocurre' || input.pagina == 'patrones'",
      selectInput("region", "Región", c("Todas", regiones)),
      
      selectInput("entidad", "Entidad", c("Todas", entidades)),
      
      conditionalPanel(
        "input.pagina == 'patrones'",
        radioButtons("medida", "Nivel de severidad",
                     c("% con víctimas (heridos o muertos)" = "SEVERO", "% fatales (muertos en el lugar)" = "FATAL"))
      )
    ),
    helpText(
      "Fuente: Base ATUS 2025 de INEGI. Sin registros duplicados y solo con accidentes causados por el conductor",
      paste0("(", comma(nrow(datos)), " accidentes)."),
      "Las personas muertas son las que fallecieron en el lugar del accidente."
    ),
    
    conditionalPanel(
      "input.pagina == 'modelo' || input.pagina == 'conclusion'",
      helpText("El modelo usa la misma base e incluye todos los tipos de accidente.",
               paste0("Umbral de decisión: ", umbral, "."), "Los filtros no aplican aquí.")
    )
  ),
  
  # ¿Qué ocurre? ---------------------------------------------------------------
  nav_panel(
    "¿Qué ocurrió?", value = "ocurre",
    layout_columns(
      fill = FALSE, height = "125px",
      value_box("Accidentes", textOutput("vb_accidentes"), theme = "primary", showcase = icon("car-burst"), showcase_layout = "top right"), # showcase = bsicons::bs_icon("ev-station-fill"),
      value_box("Con víctimas", textOutput("vb_victimas"), theme = "warning", showcase = icon("user-injured"), showcase_layout = "top right"),
      value_box("Fatales", textOutput("vb_fatales"), theme = "danger", showcase = icon("heart-pulse"), showcase_layout = "top right"),
      value_box("Personas muertas", textOutput("vb_muertos"), theme = "secondary"),
      value_box("Personas heridas", textOutput("vb_heridos"), theme = "secondary")
    ),
    navset_card_underline(
      nav_panel("Generales", layout_columns(
        col_widths = c(6, 6, 6, 6),
        
        card(card_header("Severidad de un accidente"), height = "200px", 
             markdown("
            Para efectos de este análisis, los accidentes fatales (al menos una 
            persona fallecida) y no fatales (al menos una persona herida), se 
            agrupan en una sola categoría denominada accidentes con víctimas, 
            la cual se contrasta contra los accidentes de solo daños 
           ")
        ),
        card(card_header("Datos y cobertura"),
             markdown("
           Fuente: INEGI, ATUS 2025.
           
           Cobertura: accidentres de tránsito en zonas urbanas y suburbanas donde el presunto responsable fue el conductor a nivel nacional.
          ")
        ),
        tarjeta("Clase de accidente", "o_objetivo_clase", alto = "320px"),
        tarjeta("Accidentes por mes", "o_objetivo_mes", alto = "320px")
      )),
      
      nav_panel("Contexto de los accidentes", layout_columns(
        col_widths = c(8, 4),
        tarjeta("¿Qué tipo de accidentes concentran un mayor porcentaje de víctimas?",
                "o_contexto_tipo",
                nota = "Si la barra naranja es más larga que la azul, ese tipo
                concentra un mayor porcentaje de los accidentes con víctimas y un menor
                porcentaje en el total de accidentes. Sin ningún filtro de región o estado,
                la colisión con automóviles representa un mayor porcentaje de todos
                los accidentes, pero los accidentes con víctimas se concentran en
                la colisión con motocicletas.", alto = "460px"),
        # tarjeta("% con víctimas por tipo de accidente", "o_contexto_tasa"),
        tarjeta("% con víctimas por capa de rodamiento", "o_contexto_capa", alto = "460px")
      )),
      
      nav_panel("Víctimas", layout_columns(
        col_widths = c(6, 6, 6, 6),
        tarjeta("¿Quiénes fueron las víctimas?", "o_victimas_persona", alto = "220px"),
        tarjeta("¿Quiénes mueren en cada tipo de accidente?", "o_victimas_quien",
                "Solo tipos de accidente con al menos 20 personas muertas.", alto = "220px"),
        tarjeta("Personas muertas por mes", "o_victimas_muertos", alto = "190px"),
        tarjeta("Personas heridas por mes", "o_victimas_heridos", alto = "190px")
      ))
    )
  ),
  
  # ¿Qué patrones encontramos? --------------------------------------------------
  nav_panel(
    "Patrones encontrados", value = "patrones",
    navset_card_underline(
      nav_panel("Tiempo", layout_columns(
        col_widths = c(12, 6, 6, 12),
        tarjeta("Severidad por hora y día de la semana", "p_tiempo_calor"),
        tarjeta("Severidad por tipo de día", "p_tiempo_dia", alto = "250px"),
        tarjeta("Severidad por franja horaria", "p_tiempo_franja", alto = "250px"),
        tarjeta("Personas muertas por cada 1,000 accidentes, por hora", "p_tiempo_muertos")
      )),
      nav_panel("Espacio", layout_columns(
        col_widths = c(7, 5, 12, 12),
        tarjeta("Severidad por entidad frente al promedio nacional", "p_espacio_mapa",
                "En rojo, arriba del promedio nacional; en azul, abajo."),
        tarjeta("Severidad por región", "p_espacio_region"),
        tarjeta("Severidad por región y tipo de zona", "p_espacio_calor"),
        tarjeta("Severidad en los 10 municipios con más accidentes", "p_espacio_municipios")
      )),
      nav_panel("Vehículos involucrados", layout_columns(
        col_widths = c(7, 5),
        tarjeta("Severidad cuando participa cada tipo de vehículo", "p_vehiculos_tipo",
                "Un accidente cuenta en cada tipo de vehículo que participa."),
        tarjeta("Severidad por número de vehículos", "p_vehiculos_numero")
      )),
      nav_panel("Conductor", layout_columns(
        col_widths = c(12, 6, 6),
        tarjeta("Severidad por sexo y edad del conductor", "p_conductor_edad",
                "Sin los accidentes en los que el conductor se fugó.", alto = "280px"),
        tarjeta("Severidad por sexo y presencia de aliento alcohólico", "p_conductor_aliento", alto = "280px"),
        tarjeta("Severidad por sexo y uso de cinturón de seguridad", "p_conductor_cinturon", alto = "280px")
      ))
    )
  ),
  
  # Modelo de severidad --------------------------------------------------------
  nav_panel(
    "Modelo de severidad", value = "modelo",
    layout_columns(
      fill = FALSE, height = "150px",
      value_box("Recall", percent(modelo$resultado$recall, accuracy = 1),
                "de los accidentes con víctimas fueron detectados", theme = "primary"),
      value_box("Precisión", percent(modelo$resultado$precision, accuracy = 1),
                "de las alertas de accidentes con víctimas fueron reales", theme = "primary"),
      value_box("Especificidad", percent(modelo$resultado$especificidad, accuracy = 1),
                "de los accidentes con sólo daños fueron detectados", theme = "primary"),
      value_box("AUC", round(modelo$resultado$AUC, 2),
                "1 es perfecto y 0.5 indica que el modelo clasifica con volados", theme = "primary")
    ),
    layout_columns(
      col_widths = c(5, 7, 7, 5, 12),
      tarjeta("Matriz de confusión (conjunto de prueba)", "m_confusion",
              paste0("Con umbral de ", umbral, ": si la probabilidad es ", umbral,
                     " o más, el modelo dice \"con víctimas\". ", comma(modelo$n_test), " accidentes de prueba.")),
      tarjeta("¿Qué pasa con otros umbrales?", "m_umbrales",
              paste0("Subir el umbral mejora la precisión pero baja el recall. Con ", umbral,
                     " el modelo detecta 7 de cada 10 accidentes con víctimas.")),
      card(full_screen = TRUE,
           card_header("Factores asociados a accidentes con víctimas (razón de momios)"),
           checkboxGroupInput("m_grupos", NULL, names(colores_grupo), selected = names(colores_grupo), inline = TRUE),
           plotlyOutput("m_or", height = "600px"),
           card_footer("Cada punto se compara con la categoría de referencia de su variable. Mayor a 1: más probabilidad",
                       "de víctimas; menor a 1: menos. La línea es el intervalo de confianza al 95%. Escala logarítmica.",
                       "No se muestran atropellamientos ni caídas de pasajero: el 100% tiene víctimas, así que su",
                       "razón de momios no se puede estimar (es infinita).")),
      tarjeta("¿Qué variables pesan más?", "m_importancia",
              paste("Cuánto empeora el ajuste del modelo si se quita cada variable (prueba de razón de verosimilitudes).",
                    "El tipo de accidente pesa más de 20 veces que la región, la segunda más importante.")),
      card(card_header("Simulador: probabilidad de que un accidente tenga víctimas"),
           layout_columns(
             col_widths = c(8, 4),
             uiOutput("s_controles"),
             value_box("Probabilidad estimada", textOutput("s_prob"),
                       textOutput("s_comparacion"), textOutput("s_clase"), theme = "warning")
           ))
    )
  ),
  
  # Conclusión -------------------------------------------------------------------
  nav_panel(
    "Conclusión", value = "conclusion",
    layout_columns(
      col_widths = c(12, 4, 4, 4, 4, 4, 4),
      card(card_header("Conclusión principal"),
           p("Lo que más se relaciona con que un accidente tenga víctimas es el", strong("tipo de accidente."),
             "Los atropellamientos y las caídas de pasajero siempre tienen víctimas. Entre los demás, aun comparando",
             "accidentes en la misma región, zona y horario, y con conductores parecidos, un choque con ciclista tiene",
             "17.8 veces los momios de tener víctimas de un choque entre vehículos, una volcadura 10.6 veces y un choque",
             "con motocicleta 8.4 veces."),
           p("Después siguen", strong("el lugar"), "(en zona suburbana los momios casi se duplican y en el Noreste y el Centro",
             "son la mitad que en el Noroeste) y", strong("el conductor"), "(con aliento alcohólico 1.7 veces y de 12 a 17 años",
             "1.5 veces).", strong("El momento"), "del accidente (madrugada, noche, domingo o día festivo) también",
             "aumenta el riesgo, pero poco: entre 1.1 y 1.2 veces."),
           p("Con un umbral de 0.2, el modelo detecta 71% de los accidentes con víctimas y, de los que marca con víctimas,",
             "46% sí las tuvieron (AUC de 0.83). Las acciones para reducir víctimas deberían enfocarse en peatones,",
             "motociclistas y ciclistas, en carreteras y caminos suburbanos y en el control de alcohol.")),
      card(card_header("Tiempo"),
           p("En la madrugada hay 20.3 personas muertas por cada 1,000 accidentes, más del doble que en la tarde (7.6).",
             "Los domingos de 17 a 21 h, 24% de los accidentes tienen víctimas; en días festivos, 21%, contra 16.9% entre semana."),
           p(em("Modelo:"), "domingo 1.17, día festivo 1.24, madrugada 1.13 y noche 1.11 veces los momios.")),
      card(card_header("Espacio"),
           p("El Sureste (26.5%) y el Occidente (23.7%) tienen la mayor proporción de accidentes con víctimas y el Noreste",
             "(10.3%) la menor. En carreteras estatales y caminos rurales casi 5% de los accidentes son fatales,",
             "casi ocho veces más que en una intersección urbana (0.6%)."),
           p(em("Modelo:"), "zona suburbana 1.95 veces. El Sureste queda igual que el Noroeste (1.00): su porcentaje alto",
             "se explica por el tipo de accidentes que tiene, como los choques de motocicleta.")),
      card(card_header("Contexto del incidente"),
           p("Los atropellamientos y las caídas de pasajero siempre tienen víctimas. Les siguen los choques con ciclista (54%),",
             "las volcaduras (48%) y los choques con motocicleta (38%). El choque entre vehículos, el más común",
             "(60% de los accidentes), solo 6%."),
           p(em("Modelo:"), "es la variable que más pesa, muy por encima de las demás.")),
      card(card_header("Vehículos involucrados"),
           p("Cuando participa una bicicleta, 53% de los accidentes tienen víctimas, y cuando participa una motocicleta, 43%.",
             "Los accidentes con ferrocarril son los más letales: 7% son fatales."),
           p(em("Modelo:"), "los vehículos entran a través del tipo de accidente (choque con motocicleta 8.4 y con ciclista 17.8).")),
      card(card_header("Conductor"),
           p("Con conductores de 12 a 17 años, 38% de los accidentes tienen víctimas. Con aliento alcohólico es 26% contra",
             "16% sin él, y sin cinturón 25% contra 14% con cinturón."),
           p(em("Modelo:"), "aliento alcohólico 1.69 y de 12 a 17 años 1.52 veces. El sexo casi no influye (mujer 1.08).")),
      card(card_header("Víctimas"),
           p("En 2025 se registraron 4,125 personas muertas y 80,880 heridas en accidentes causados por el conductor.",
             "De los muertos, 61% eran conductores, 22% pasajeros y 14% peatones."),
           p(em("Modelo:"), "predice si un accidente tiene víctimas, no cuántas."))
    )
  )
)

# ------------------------------------------------------------------------------
# Servidor
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Filtros ----------------------------------------------------------------------
  observeEvent(input$region, {
    opciones <- if (input$region == "Todas") entidades else sort(unique(datos$ENTIDAD[datos$REGION == input$region]))
    updateSelectInput(session, "entidad", choices = c("Todas", opciones))
  })
  
  d <- reactive({
    x <- datos
    if (input$region != "Todas") x <- filter(x, REGION == input$region)
    if (input$entidad != "Todas") x <- filter(x, ENTIDAD == input$entidad)
    req(nrow(x) > 0)
    x
  })
  
  medida <- reactive(input$medida)
  promedio <- reactive(mean(d()[[medida()]]))
  promedio_victimas <- reactive(mean(d()$SEVERO))
  
  # Texto dinámico exclusivo para gráficas que siempre muestran "% con víctimas" (pestaña "¿Qué ocurre?")
  texto_promedio_general <- reactive({
    paste0(
      "<b>Porcentaje general de accidentes con víctimas</b><br>",
      "Valor: <b>", percent(promedio_victimas(), accuracy = 1), "</b><br>",
      "Región: ", input$region, "<br>",
      "Entidad: ", input$entidad
    )
  })
  
  # Texto dinámico que cambia si el usuario elige "SEVERO" o "FATAL" (pestaña "¿Qué patrones encontramos?")
  texto_promedio_dinamico <- reactive({
    tipo <- if_else(medida() == "SEVERO", "con víctimas", "fatales")
    paste0(
      "<b>Porcentaje general de accidentes ", tipo, "</b><br>",
      "Valor: <b>", percent(promedio(), accuracy = 1), "</b><br>",
      "Región: ", input$region, "<br>",
      "Entidad: ", input$entidad
    )
  })
  
  # ¿Qué ocurre? ---------------------------------------------------------------
  ## Value Boxes ----
  output$vb_accidentes <- renderText(comma(nrow(d())))
  output$vb_victimas <- renderText(percent(mean(d()$SEVERO), accuracy = 0.1))
  output$vb_fatales <- renderText(percent(mean(d()$FATAL), accuracy = 0.1))
  output$vb_muertos <- renderText(comma(sum(d()$MUERTOS)))
  output$vb_heridos <- renderText(comma(sum(d()$HERIDOS)))
  
  ## Planteamiento ----
  output$o_objetivo_texto <- renderText({
    paste0("En la selección, ", round(100 * mean(d()$SEVERO)), " de cada 100 accidentes tuvieron víctimas (",
           comma(sum(d()$SEVERO)), " de ", comma(nrow(d())), ").")
  })
  
  # output$o_objetivo_clase <- renderPlotly({
  #   d() %>%
  #     count(CLASACC) %>%
  #     mutate(CLASACC = factor(CLASACC, levels = c("Sólo daños", "No fatal", "Fatal"))) %>%
  #     arrange(CLASACC) %>%
  #     barras(CLASACC, n, total = nrow(d()), ordenar = FALSE)
  # })
  
  output$o_objetivo_clase <- renderPlotly({
    g <- d() %>%
      count(CLASACC) %>%
      mutate(
        CLASACC = factor(CLASACC, levels = c("Fatal", "No fatal", "Sólo daños")),
        SEVERIDAD = ifelse(CLASACC == "Sólo daños", "Sólo daños", "Con víctimas"),
        PROP = n / nrow(d())
      ) %>% 
      ggplot(aes(
        y = SEVERIDAD, x = n, fill = CLASACC,
        text = paste0(CLASACC, "<br>", 
                      comma(n), "<br>",
                      percent(PROP, accuracy = 0.1))
      )) +
      geom_bar(stat = "identity") +
      scale_fill_manual(
        values = c(rojo, naranja, azul)
      ) +
      scale_x_continuous(labels = label_comma()) +
      labs(y = NULL, x = NULL, fill = NULL) +
      tema
    
    a_plotly(g)
  })
  
  output$o_objetivo_mes <- renderPlotly({
    x <- d() %>%
      con_objetivo() %>%
      count(MES, objetivo) %>%
      group_by(MES) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      mutate(texto = paste0(meses[MES], "<br>", objetivo, "<br>Accidentes: ", comma(n), " (", percent(prop, accuracy = 0.1), ")"))
    p <- ggplot(x, aes(x = MES, y = n, fill = objetivo, text = texto)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = colores_objetivo) +
      scale_x_continuous(breaks = 1:12, labels = meses) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL, fill = NULL) +
      tema
    a_plotly(p)
  })
  
  ## Contexto del incidente ----
  output$o_contexto_tipo <- renderPlotly({
    x <- d() %>%
      # 1. Contamos los casos totales y los casos con víctimas por tipo de accidente
      group_by(TIPACCID) %>%
      summarise(
        n_tipo = n(),
        n_victimas = sum(SEVERO == 1), # 1 significa con víctimas
        .groups = "drop"
      ) %>%
      # 2. Calculamos los porcentajes relativos al universo total de cada categoría
      mutate(
        pct_total = n_tipo / sum(n_tipo),
        pct_victimas = n_victimas / sum(n_victimas)
      ) %>%
      # 3. Ordenamos de manera descendente (mayor arriba) por el % con víctimas
      mutate(TIPACCID = fct_reorder(TIPACCID, pct_victimas)) %>%
      # 4. Transformamos a formato largo para el position = "dodge"
      pivot_longer(
        cols = c(pct_total, pct_victimas),
        names_to = "medida_cat",
        values_to = "porcentaje"
      ) %>%
      mutate(
        # Creamos etiquetas limpias para la leyenda
        categoria = if_else(medida_cat == "pct_total", "Del total de accidentes", "Del total con víctimas"),
        categoria = factor(categoria, levels = c("Del total de accidentes", "Del total con víctimas")),
        
        # Recuperamos el conteo adecuado para mostrarlo en el tooltip
        conteo = if_else(medida_cat == "pct_total", n_tipo, n_victimas),
        
        # Construimos el tooltip
        texto = paste0(
          "<b>", TIPACCID, "</b><br>",
          categoria, ": <b>", percent(porcentaje, accuracy = 0.1), "</b><br>",
          "Casos: ", comma(conteo)
        )
      )
    
    p <- ggplot(x, aes(x = porcentaje, y = TIPACCID, fill = categoria, text = texto)) +
      geom_col(position = "dodge", width = 0.7) +
      # Reutilizamos los colores globales de tu app
      scale_fill_manual(values = c("Del total de accidentes" = azul, "Del total con víctimas" = naranja)) +
      scale_x_continuous(labels = percent) +
      labs(x = NULL, y = NULL, fill = NULL) +
      tema
    
    a_plotly(p)
  })
  
  output$o_contexto_tasa <- renderPlotly({
    resumen(d(), TIPACCID) %>% barras(TIPACCID, tasa, percent, linea = promedio_victimas(), texto_linea = texto_promedio_general())
  })
  
  output$o_contexto_capa <- renderPlotly({
    resumen(d(), CAPAROD) %>% barras(CAPAROD, tasa, percent, linea = promedio_victimas(), texto_linea = texto_promedio_general())
  })
  
  ## Víctimas ----
  output$o_victimas_persona <- renderPlotly({
    x <- personas %>%
      mutate(`Personas muertas` = map_dbl(muertos, ~ sum(d()[[.x]])),
             `Personas heridas` = map_dbl(heridos, ~ sum(d()[[.x]]))) %>%
      select(persona, `Personas muertas`, `Personas heridas`) %>%
      pivot_longer(-persona, names_to = "tipo", values_to = "n") %>%
      group_by(tipo) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      mutate(persona = factor(persona, levels = personas$persona),
             texto = paste0(persona, "<br>", 
                            tipo, ": ", comma(n), "<br>", 
                            percent(prop, accuracy = 0.1), " del total de ", tolower(tipo)))
    p <- ggplot(x, aes(x = persona, y = prop, fill = tipo, text = texto)) +
      geom_col(position = "dodge", width = 0.7) +
      scale_fill_manual(values = c(`Personas muertas` = rojo, `Personas heridas` = azul)) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = "% del total", fill = NULL) +
      tema
    a_plotly(p)
  })
  
  por_mes <- function(columna) {
    x <- d() %>%
      group_by(MES) %>%
      summarise(personas = sum(.data[[columna]])) %>%
      mutate(texto = paste0(meses[MES], ": ", comma(personas)))
    p <- ggplot(x, aes(x = MES, y = personas)) +
      geom_line(color = azul) +
      geom_point(aes(text = texto), color = azul, size = 2) +
      scale_x_continuous(breaks = 1:12, labels = meses) +
      scale_y_continuous(labels = comma) +
      labs(x = NULL, y = NULL) +
      tema
    a_plotly(p)
  }
  
  output$o_victimas_muertos <- renderPlotly(por_mes("MUERTOS"))
  output$o_victimas_heridos <- renderPlotly(por_mes("HERIDOS"))
  
  output$o_victimas_quien <- renderPlotly({
    x <- d() %>%
      group_by(TIPACCID) %>%
      summarise(across(all_of(personas$muertos), sum)) %>%
      pivot_longer(-TIPACCID, names_to = "columna", values_to = "muertos") %>%
      left_join(personas %>% select(persona, columna = muertos), by = "columna") %>%
      group_by(TIPACCID) %>%
      mutate(total = sum(muertos), prop = muertos / total) %>%
      ungroup() %>%
      filter(total >= 20) %>%
      mutate(persona = factor(persona, levels = personas$persona),
             texto = paste0(TIPACCID, "<br>", persona, ": ", comma(muertos), " (", percent(prop, accuracy = 1), ")"))
    req(nrow(x) > 0)
    p <- ggplot(x, aes(x = prop, y = fct_reorder(TIPACCID, total), fill = persona, text = texto)) +
      geom_col(width = 0.7, position = position_stack(reverse = TRUE)) +
      scale_fill_manual(values = c(azul, naranja, aqua, amarillo, gris)) +
      scale_x_continuous(labels = percent) +
      labs(x = "% de las personas muertas", y = NULL, fill = NULL) +
      tema
    a_plotly(p)
  })
  
  # ¿Qué patrones encontramos? --------------------------------------------------------
  ## Tiempo ----
  output$p_tiempo_calor <- renderPlotly({
    resumen(d(), DIASEMANA, ID_HORA, medida = medida()) %>%
      mutate(DIASEMANA = fct_rev(DIASEMANA), HORA = factor(paste0(ID_HORA, " h"), levels = horas)) %>%
      calor(HORA, DIASEMANA, tasa, \(x) percent(x, accuracy = 0.01), nombre_medida[[medida()]])
  })
  
  output$p_tiempo_dia <- renderPlotly({
    resumen(d(), TIPO_DIA, medida = medida()) %>%
      barras(TIPO_DIA, tasa, percent, linea = promedio(), texto_linea = texto_promedio_dinamico())
  })
  
  output$p_tiempo_franja <- renderPlotly({
    resumen(d(), FRANJA_HORARIA, medida = medida()) %>%
      mutate(FRANJA_HORARIA = factor(FRANJA_HORARIA, levels = c("Madrugada", "Mañana", "Tarde", "Noche"))) %>%
      arrange(FRANJA_HORARIA) %>%
      barras(FRANJA_HORARIA, tasa, percent, linea = promedio(), ordenar = FALSE, texto_linea = texto_promedio_dinamico())
  })
  
  output$p_tiempo_muertos <- renderPlotly({
    x <- d() %>%
      group_by(ID_HORA) %>%
      summarise(tasa = 1000 * sum(MUERTOS) / n()) %>%
      mutate(texto = paste0(ID_HORA, " h<br>Muertos por cada 1,000 accidentes: ", round(tasa, 1)))
    p <- ggplot(x, aes(x = ID_HORA, y = tasa)) +
      geom_hline(yintercept = 1000 * sum(d()$MUERTOS) / nrow(d()), color = "grey40") +
      geom_line(color = azul) +
      geom_point(aes(text = texto), color = azul, size = 2) +
      scale_x_continuous(breaks = seq(0, 21, 3), labels = paste0(seq(0, 21, 3), " h")) +
      labs(x = "Hora del día", y = "Muertos por cada 1,000 accidentes") +
      tema
    a_plotly(p)
  })
  
  ## Espacio ----
  output$p_espacio_mapa <- renderPlotly({
    nacional <- mean(datos[[medida()]])
    tabla <- resumen(d(), ENTIDAD, medida = medida()) %>%
      mutate(val = tasa,
             texto = paste0(ENTIDAD, "<br>", nombre_medida[[medida()]], ": ", percent(val, accuracy = 0.1),
                            "<br>Accidentes: ", comma(accidentes)))
    mapa_entidades(tabla, scale_fill_gradient2(low = azul, mid = "#f0efec", high = rojo, midpoint = nacional,
                                               labels = percent, name = nombre_medida[[medida()]],
                                               na.value = "grey92"))
  })
  
  output$p_espacio_region <- renderPlotly({
    resumen(d(), REGION, medida = medida()) %>%
      barras(REGION, tasa, percent, linea = promedio(), texto_linea = texto_promedio_dinamico())
  })
  
  output$p_espacio_calor <- renderPlotly({
    resumen(d(), REGION, ZONA_DETALLE, medida = medida()) %>%
      mutate(ZONA_DETALLE = factor(ZONA_DETALLE, levels = c("Urbana: intersección", "Urbana: no intersección",
                                                            "Suburbana: carretera estatal", "Suburbana: otro camino",
                                                            "Suburbana: camino rural"))) %>%
      calor(ZONA_DETALLE, REGION, tasa, percent, nombre_medida[[medida()]])
  })
  
  output$p_espacio_municipios <- renderPlotly({
    d() %>%
      mutate(municipio = paste0(NOM_MUNICIPIO, ", ", ENTIDAD)) %>%
      resumen(municipio, medida = medida()) %>%
      slice_max(accidentes, n = 10) %>%
      barras(municipio, tasa, percent, linea = promedio(), texto_linea = texto_promedio_dinamico())
  })
  
  ## Vehículos involucrados ----
  output$p_vehiculos_tipo <- renderPlotly({
    tibble(vehiculo = vehiculos,
           accidentes = map_int(names(vehiculos), ~ sum(d()[[.x]] > 0)),
           tasa = map_dbl(names(vehiculos), ~ mean(d()[[medida()]][d()[[.x]] > 0]))) %>%
      filter(accidentes > 0) %>%
      barras(vehiculo, tasa, percent, linea = promedio(), texto_linea = texto_promedio_dinamico())
  })
  
  output$p_vehiculos_numero <- renderPlotly({
    d() %>%
      num_vehiculos() %>%
      resumen(numero, medida = medida()) %>%
      barras(numero, tasa, percent, linea = promedio(), ordenar = FALSE, texto_linea = texto_promedio_dinamico())
  })
  
  ## Conductor ----
  output$p_conductor_edad <- renderPlotly({
    x <- d() %>%
      filter(SEXO != "Se fugó") %>%
      resumen(GRUPO_EDAD, SEXO, medida = medida()) %>%
      mutate(
        accion = if_else(medida() == "SEVERO", "tuvieron víctimas", "fueron fatales"),
        texto = paste0(
          "<b>", SEXO, " (", GRUPO_EDAD, " años)</b><br>",
          "Total de accidentes: ", comma(accidentes), "<br>",
          "De estos, el <b>", percent(tasa, accuracy = 0.1), "</b> ", accion
        )
      )
    
    # Creamos un data frame auxiliar para el tooltip de la línea gris
    df_prom <- data.frame(
      prom = promedio(), 
      texto_prom = texto_promedio_dinamico()
    )
    
    p <- ggplot(x, aes(x = GRUPO_EDAD, y = tasa, fill = SEXO, text = texto)) +
      geom_col(position = "dodge", width = 0.7) +
      # Modificamos el geom_hline para incluir el tooltip
      geom_hline(
        data = df_prom, 
        aes(yintercept = prom, text = texto_prom), 
        color = "grey40"
      ) +
      scale_fill_manual(values = c(Hombre = azul, Mujer = naranja)) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = nombre_medida[[medida()]], fill = NULL) +
      tema
    
    a_plotly(p)
  })
  
  output$p_conductor_aliento <- renderPlotly({
    x <- d() %>%
      filter(SEXO != "Se fugó") %>%
      resumen(ALIENTO, SEXO, medida = medida()) %>%
      mutate(
        accion = if_else(medida() == "SEVERO", "tuvieron víctimas", "fueron fatales"),
        texto = paste0(
          "<b>", SEXO, " (Aliento: ", ALIENTO, ")</b><br>",
          "Total de accidentes: ", comma(accidentes), "<br>",
          "De estos, el <b>", percent(tasa, accuracy = 0.1), "</b> ", accion
        )
      )
    
    df_prom <- data.frame(
      prom = promedio(), 
      texto_prom = texto_promedio_dinamico()
    )
    
    p <- ggplot(x, aes(x = ALIENTO, y = tasa, fill = SEXO, text = texto)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_hline(
        data = df_prom, 
        aes(yintercept = prom, text = texto_prom), 
        color = "grey40"
      ) +
      scale_fill_manual(values = c(Hombre = azul, Mujer = naranja)) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = nombre_medida[[medida()]], fill = NULL) +
      tema
    
    a_plotly(p)
  })
  
  output$p_conductor_cinturon <- renderPlotly({
    x <- d() %>%
      filter(SEXO != "Se fugó") %>%
      resumen(CINTURON, SEXO, medida = medida()) %>%
      mutate(
        accion = if_else(medida() == "SEVERO", "tuvieron víctimas", "fueron fatales"),
        texto = paste0(
          "<b>", SEXO, " (Cinturón: ", CINTURON, ")</b><br>",
          "Total de accidentes: ", comma(accidentes), "<br>",
          "De estos, el <b>", percent(tasa, accuracy = 0.1), "</b> ", accion
        )
      )
    
    df_prom <- data.frame(
      prom = promedio(), 
      texto_prom = texto_promedio_dinamico()
    )
    
    p <- ggplot(x, aes(x = CINTURON, y = tasa, fill = SEXO, text = texto)) +
      geom_col(position = "dodge", width = 0.7) +
      geom_hline(
        data = df_prom, 
        aes(yintercept = prom, text = texto_prom), 
        color = "grey40"
      ) +
      scale_fill_manual(values = c(Hombre = azul, Mujer = naranja)) +
      scale_y_continuous(labels = percent) +
      labs(x = NULL, y = nombre_medida[[medida()]], fill = NULL) +
      tema
    
    a_plotly(p)
  })
  
  # ¿Qué aporta el modelo? -------------------------------------------------------------
  output$m_confusion <- renderPlotly({
    r <- modelo$resultado
    x <- tibble(real = c("Con víctimas", "Con víctimas", "Sólo daños", "Sólo daños"),
                predicho = c("Con víctimas", "Sólo daños", "Con víctimas", "Sólo daños"),
                n = c(r$VP, r$FN, r$FP, r$VN),
                nombre = c("Verdaderos positivos", "Falsos negativos", "Falsos positivos", "Verdaderos negativos")) %>%
      group_by(real) %>%
      mutate(prop = n / sum(n)) %>%
      ungroup() %>%
      mutate(acierto = if_else(real == predicho, "Acierto", "Error"),
             etiqueta = paste0(comma(n), "\n(", percent(prop, accuracy = 0.1), ")"),
             texto = paste0(nombre, "<br>Real: ", real, "<br>Predicho: ", predicho, "<br>", comma(n),
                            " (", percent(prop, accuracy = 0.1), " de los reales)"))
    p <- ggplot(x, aes(x = predicho, y = fct_rev(real), fill = acierto, text = texto)) +
      geom_tile(color = "white", linewidth = 2) +
      geom_text(aes(label = etiqueta), size = 4.5) +
      scale_fill_manual(values = c(Acierto = "#cde2fb", Error = "#fbd9cc")) +
      labs(x = "Predicho por el modelo", y = "Real", fill = NULL) +
      tema +
      theme(panel.grid = element_blank(), legend.position = "none")
    a_plotly(p)
  })
  
  output$m_umbrales <- renderPlotly({
    x <- modelo$umbrales %>%
      select(umbral, Recall = recall, `Precisión` = precision, Especificidad = especificidad) %>%
      pivot_longer(-umbral, names_to = "metrica", values_to = "valor") %>%
      mutate(texto = paste0("Umbral ", umbral, "<br>", metrica, ": ", percent(valor, accuracy = 0.1)))
    p <- ggplot(x, aes(x = umbral, y = valor, color = metrica)) +
      geom_vline(xintercept = umbral, color = "grey40", linetype = "dashed") +
      geom_line() +
      geom_point(aes(text = texto), size = 0.8) +
      scale_color_manual(values = c(Recall = azul, `Precisión` = naranja, Especificidad = aqua)) +
      scale_y_continuous(labels = percent, limits = c(0, 1)) +
      labs(x = "Umbral de decisión", y = NULL, color = NULL) +
      tema
    a_plotly(p)
  })
  
  output$m_or <- renderPlotly({
    x <- modelo$ors %>%
      filter(!separacion, grupo %in% input$m_grupos) %>%
      mutate(texto = paste0(etiqueta, "<br>Razón de momios: ", round(OR, 2),
                            "<br>IC 95%: ", round(li, 2), " a ", round(ls, 2)))
    req(nrow(x) > 0)
    p <- ggplot(x, aes(x = OR, y = fct_reorder(etiqueta, OR), color = grupo)) +
      geom_vline(xintercept = 1, color = "grey50") +
      geom_segment(aes(x = li, xend = ls, yend = fct_reorder(etiqueta, OR))) +
      geom_point(aes(text = texto), size = 2) +
      scale_x_log10(breaks = c(0.5, 1, 2, 5, 10, 20)) +
      scale_color_manual(values = colores_grupo) +
      labs(x = "Razón de momios", y = NULL, color = NULL) +
      tema
    a_plotly(p)
  })
  
  output$m_importancia <- renderPlotly({
    x <- modelo$importancia %>%
      mutate(texto = paste0(variable, " (", grupo, ")<br>Razón de verosimilitudes: ", comma(LRT, accuracy = 1)))
    p <- ggplot(x, aes(x = LRT, y = fct_reorder(variable, LRT), fill = grupo, text = texto)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = colores_grupo) +
      scale_x_continuous(labels = comma) +
      labs(x = NULL, y = NULL, fill = NULL) +
      tema
    a_plotly(p)
  })
  
  # Simulador
  sim <- modelo$simulador
  etiquetas_sim <- c(TIPO = "Tipo de accidente", REGION = "Región", ZONA = "Zona", FRANJA_HORARIA = "Horario",
                     DIASEMANA = "Día de la semana", FESTIVO = "Día festivo", SEXO = "Sexo del conductor",
                     GRUPO_EDAD = "Edad del conductor", ALIENTO = "Aliento alcohólico")
  
  output$s_controles <- renderUI({
    controles <- map(names(etiquetas_sim), function(v) {
      opciones <- setdiff(sim$niveles[[v]], "Se fugó")
      selectInput(paste0("s_", v), etiquetas_sim[[v]], opciones)
    })
    layout_columns(col_widths = c(4, 4, 4), !!!controles)
  })
  
  prob_sim <- reactive({
    valores <- map(names(etiquetas_sim), ~ input[[paste0("s_", .x)]])
    req(all(!map_lgl(valores, is.null)))
    nuevo <- as.data.frame(set_names(valores, names(etiquetas_sim)))
    X <- model.matrix(sim$terminos, nuevo, xlev = sim$niveles)
    b <- sim$coeficientes
    b[is.na(b)] <- 0
    plogis(sum(X[1, ] * b[colnames(X)]))
  })
  
  output$s_prob <- renderText(percent(prob_sim(), accuracy = 0.1))
  output$s_comparacion <- renderText(
    paste0(round(prob_sim() / sim$promedio, 1), " veces el promedio (", percent(sim$promedio, accuracy = 0.1), ")")
  )
  output$s_clase <- renderText(
    paste0("Con umbral de ", sim$umbral, ", el modelo lo clasifica como: ",
           if_else(prob_sim() >= sim$umbral, "con víctimas", "sólo daños"))
  )
}

shinyApp(ui, server)

# Cambios ----
# 1. output$o_victimas_persona (cambiar tooltip para que porcentaje quede claro). [Sobreescrito por 5 y 6]
# 2. output$p_tiempo_calor: se modifcó argumento de `percent` a `\(x) percent(x, accuracy = 0.01)` para reducir decimales del porcentaje en cada celda del mapa de calor.
# 3. output$p_conductor_edad: Mejorar tooltip. [Sobreescrito por 5 y 6]
# 4. Antes de la última línea de la función barras(). Agregar tooltip a línea promedio y modificarla  un poco. [Sobreescrito por 5 y 6]
# 5. Función barras() para incluir tooltip dinámico de la línea gris (promedio). [Sobreescribe cambios 1, 3 y 4].
# 6. Se añadieron dos variables reactivas texto_promedio_general y texto_promedio_dinamico para crear el tooltip de la línea gris. 
#### *_general es para pestaña "¿Qué ocurre?". *_dinamico para pestaña de Patrones [Sobreescribe cambios 1, 3 y 4].
# 7. Se borraron footers repetitivos.
# 8. Pestaña Contexto del incidente (en ¿Qué ocurre?). Se modificó primera gráfica de acuerdo a las instrucciones de Max.
# 9. Pestaña Conductor (en ¿Qué patrones encontramos?). Se modificó toda la pestaña.

