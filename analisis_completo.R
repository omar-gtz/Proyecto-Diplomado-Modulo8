# ------------------------------------------------------------------------------
# ANÁLISIS DE ACCIDENTES DE TRÁNSITO EN MÉXICO
#
# Paquetes: install.packages(c("tidyverse", "readxl", "scales", "skimr", "pROC"))
# ------------------------------------------------------------------------------

# Elimina los objetos del entorno global previamente creados
rm(list = ls(all.names = TRUE)) 

# Ayuda a liberar la memoria física del sistema operativo
gc() 

# ------------------------------------------------------------------------------
# CARGA DE LIBRERÍAS 
# ------------------------------------------------------------------------------
library(tidyverse)
library(readxl)
library(scales)
library(skimr)
library(pROC)

# ------------------------------------------------------------------------------
# CARGA DE DATOS
# ------------------------------------------------------------------------------

# Datos ATUS 2025
df <- read_csv("datos/atus_anual_2025.csv", show_col_types = FALSE)

# Catálogos de variables categoricas
cat_entidad <- read_csv("datos/tc_entidad.csv", show_col_types = FALSE)
cat_municipios <- read_csv("datos/tc_municipio.csv", show_col_types = FALSE)
cat_region <- read_csv("datos/cat_region.csv", show_col_types = FALSE)

# ------------------------------------------------------------------------------
# EXPLORACIÓN DE DATOS
# ------------------------------------------------------------------------------

# Dimensión de los datos
cat(paste("Número de registros:", comma(dim(df)[1])), "\n")
cat(paste("Número de columnas:", dim(df)[2]), "\n")

# Estructura de los datos
glimpse(df)

# Tipos de variables
table(sapply(df, class))

# Valores faltantes
sum(is.na(df))

# Resumen de los datos
summary_skimr <- skim(df)
summary_skimr

# Tipos de variables

numeric_cols <- df %>% 
  select(where(is.numeric)) %>% colnames()

categorical_cols <- df %>% 
  select(where(~ is.character(.) || is.factor(.) || is.logical(.))) %>% colnames()

cat(paste0("Columnas numéricas: ", length(numeric_cols), "\n"))
cat(paste0("Columnas categóricas: ", length(categorical_cols), "\n"))

# Variables categóricas
categorical_cols

# Resumen de las variables categóricas (de tipo "character")
summary_categorical <- summary_skimr %>% yank("character")
print(summary_categorical)

# Función para visualizar las categorías de las variables categóricas
inspeccionar_categorias <- function(df, cols) {
  
  cols_validas <- cols[cols %in% names(df)]
  
  if (length(cols_validas) == 0) {
    stop("Ninguna de las columnas especificadas existe en el dataframe.")
  }
  
  for (col in cols_validas) {
    cat("\n===", col, "===\n")
    valores <- sort(unique(df[[col]]))
    n_categorias <- length(valores)
    
    if (n_categorias > 100) {
      print(head(valores, 10))
      cat("[... Se ocultaron", n_categorias - 10, "valores...]\n")
    } else {
      print(valores)
    }
  }
}

# COBERTURA y ESTATUS
inspeccionar_categorias(df, c("COBERTURA", "ESTATUS"))

# Claves Geográficas
inspeccionar_categorias(df, c("CVEGEO", "CVE_ENT", "CVE_MUN"))

# Variables temporales
inspeccionar_categorias(df, c("MES","ID_DIA","ID_HORA","ID_MINUTO","DIASEMANA"))

# Características del lugar del accidente
inspeccionar_categorias(df, c("URBANA","SUBURBANA","CAPAROD"))

# Características del accidente
inspeccionar_categorias(df, c("TIPACCID","CAUSAACCI","CLASACC"))

# Sobre el conductor presunto responsable
inspeccionar_categorias(df, c("SEXO","ALIENTO","CINTURON"))

# Variables numéricas
numeric_cols

# Resumen de las variables numéricas
summary_numeric <- summary_skimr %>% yank("numeric")
print(summary_numeric, n=27)

# ------------------------------------------------------------------------------
# LIMPIEZA Y TRANSFORMACIÓN DE DATOS
# ------------------------------------------------------------------------------

# Registros duplicados
total_duplicados <- sum(duplicated(df))
cat(paste("Total de registros completamente duplicados: ", total_duplicados))

# Valores faltantes
sum(is.na(df))

# Analizar la pérdida de datos oculta en las variables de conductor
resumen_datos_faltantes <- df %>%
  distinct() %>%
  summarise(
    edad_se_fugo = sum(ID_EDAD == 0, na.rm = TRUE),
    edad_no_especificada = sum(ID_EDAD == 99, na.rm = TRUE),
    sexo_se_fugo = sum(SEXO == "Se fugó", na.rm = TRUE),
    aliento_se_ignora = sum(ALIENTO == "Se ignora", na.rm = TRUE),
    cinturon_se_ignora = sum(CINTURON == "Se ignora", na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Criterio", values_to = "Casos") %>%
  mutate(Porcentaje = (Casos / nrow(df)) * 100)

resumen_datos_faltantes

# Frecuencia de accidentes por causa del accidente
df %>%
  count(CAUSAACCI) %>%
  mutate(prop = percent(n / sum(n), accuracy = 0.1))

# Catálogos de variables categóricas
glimpse(cat_entidad)
glimpse(cat_municipios)
glimpse(cat_region)

# Días de descanso obligatorio 2025 (Ley Federal del Trabajo, art. 74)
festivos <- as.Date(c("2025-01-01", "2025-02-03", "2025-03-17", "2025-05-01",
                      "2025-09-16", "2025-11-17", "2025-12-25"))

# Tipos de vehículos involucrados 
vehiculos <- c("AUTOMOVIL", "CAMPASAJ", "MICROBUS", "PASCAMION", "OMNIBUS", "TRANVIA", "CAMIONETA",
               "CAMION", "TRACTOR", "FERROCARRI", "MOTOCICLET", "BICICLETA", "OTROVEHIC")

# Limpieza y transformación de variables
datos <- df %>%
  # Eliminar duplicados exactos conservando la primera ocurrencia
  distinct() %>%
  # Filtrar los datos a accidentes causados por el Conductor
  filter(CAUSAACCI == "Conductor") %>%
  # Cruzar los datos con los catálogos 
  left_join(cat_entidad %>% select(CVE_ENT, NOM_ENTIDAD), by = "CVE_ENT") %>%
  left_join(cat_municipios %>% select(CVEGEO, NOM_MUNICIPIO), by = "CVEGEO") %>%
  left_join(cat_region %>% select(CVE_ENT, REGION), by = "CVE_ENT") %>%
  mutate(
    # Corrección de variables temporales
    MES = as.numeric(MES),
    ID_HORA = as.numeric(ID_HORA),
    FECHA = make_date(ANIO, MES, as.numeric(ID_DIA)),
    DIASEMANA = factor(DIASEMANA, levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")),
    # Crear variables 
    FESTIVO = if_else(FECHA %in% festivos, "Sí", "No"),
    TIPO_DIA = case_when(
      FESTIVO == "Sí" ~ "Día festivo",
      DIASEMANA %in% c("Sábado", "Domingo") ~ "Fin de semana",
      TRUE ~ "Entre semana"
    ),
    # Agrupar la hora del accidente en franjas horarias
    FRANJA_HORARIA = case_when(
      ID_HORA >= 0 & ID_HORA < 6 ~ "Madrugada",
      ID_HORA >= 6 & ID_HORA < 12 ~ "Mañana",
      ID_HORA >= 12 & ID_HORA < 18 ~ "Tarde",
      ID_HORA >= 18 & ID_HORA < 24 ~ "Noche"
    ),
    # Variable objetivo: SEVERO = 1 si hay heridos/muertos, 0 si solo hay daños materiales
    SEVERO = if_else(CLASACC == "Sólo daños", 0, 1),
    # FATAL = 1 si hay muertos, 0 si no hay muertos
    FATAL = if_else(CLASACC == "Fatal", 1, 0),
    # Acortar nombre de las Entidades
    ENTIDAD = str_remove(NOM_ENTIDAD, " de Ignacio de la Llave| de Zaragoza| de Ocampo"),
    # Agrupar zona Urbana y Suburbana en una variable
    ZONA = case_when(
      URBANA == "Accidente en intersección" ~ "Urbana en intersección",
      URBANA == "Accidente en no intersección" ~ "Urbana en no intersección",
      URBANA == "Sin accidente en esta zona" ~ "Suburbana"
    ),
    ZONA_DETALLE = case_when(
      URBANA == "Accidente en intersección" ~ "Urbana: intersección",
      URBANA == "Accidente en no intersección" ~ "Urbana: no intersección",
      SUBURBANA == "Accidente en carretera estatal" ~ "Suburbana: carretera estatal",
      SUBURBANA == "Accidente en camino rural" ~ "Suburbana: camino rural",
      SUBURBANA == "Accidentes en otro camino" ~ "Suburbana: otro camino"
    ),
    # Agrupar Edad del conductor en grupos de edad
    GRUPO_EDAD = case_when(
      ID_EDAD == 0 ~ "Se fugó",
      ID_EDAD >= 12 & ID_EDAD < 18 ~ "de 12 a 17",
      ID_EDAD >= 18 & ID_EDAD < 25 ~ "de 18 a 24",
      ID_EDAD >= 25 & ID_EDAD < 45 ~ "de 25 a 44",
      ID_EDAD >= 45 & ID_EDAD < 65 ~ "de 45 a 64",
      ID_EDAD >= 65 & ID_EDAD < 99 ~ "de 65 y más",
      ID_EDAD == 99 ~ "No especificado"
    ),
    GRUPO_EDAD = factor(GRUPO_EDAD, levels = c("de 12 a 17", "de 18 a 24", "de 25 a 44", "de 45 a 64",
                                               "de 65 y más", "No especificado", "Se fugó")),
    # Contar el total de vehículos involucrados en cada accidente
    NUM_VEHICULOS = rowSums(across(all_of(vehiculos))),
    # Contar el total de víctimas involucrados en cada accidente
    MUERTOS = CONDMUERTO + PASAMUERTO + PEATMUERTO + CICLMUERTO + OTROMUERTO + NEMUERTO,
    HERIDOS = CONDHERIDO + PASAHERIDO + PEATHERIDO + CICLHERIDO + OTROHERIDO + NEHERIDO
  ) %>%
  # Elegir las variables a utilizar
  select(
    FECHA, MES, DIASEMANA, ID_HORA, FESTIVO, TIPO_DIA, FRANJA_HORARIA,
    CVE_ENT, ENTIDAD, REGION, CVEGEO, NOM_MUNICIPIO, ZONA, ZONA_DETALLE,
    TIPACCID, CAUSAACCI, CAPAROD, CLASACC, SEVERO, FATAL,
    all_of(vehiculos), NUM_VEHICULOS,
    SEXO, ID_EDAD, GRUPO_EDAD, ALIENTO, CINTURON,
    CONDMUERTO, CONDHERIDO, PASAMUERTO, PASAHERIDO, PEATMUERTO, PEATHERIDO,
    CICLMUERTO, CICLHERIDO, OTROMUERTO, OTROHERIDO, NEMUERTO, NEHERIDO, MUERTOS, HERIDOS
  )

# Conteo de registros en cada paso
tibble(
  paso = c("Base original", "Sin duplicados", "Solo causa conductor"),
  registros = c(nrow(df), nrow(distinct(df)), nrow(datos))
) %>%
  mutate(registros = comma(registros))

# Verificación: las variables nuevas no deben tener faltantes
datos %>%
  select(SEVERO, FATAL, ENTIDAD, REGION, ZONA, ZONA_DETALLE, FRANJA_HORARIA, 
         FECHA, FESTIVO, TIPO_DIA, GRUPO_EDAD, DIASEMANA, NUM_VEHICULOS, 
         MUERTOS, HERIDOS) %>%
  is.na() %>%
  colSums()

# Verificación: Consistencia de Variables de Conteo de Víctimas
datos %>% 
  group_by(CLASACC) %>%
  summarise(
    accidentes = n(),
    con_muertos = sum(MUERTOS > 0),
    con_heridos = sum(HERIDOS > 0)
  )

# ------------------------------------------------------------------------------
# ANÁLISIS EXPLORATORIO DE DATOS
# ------------------------------------------------------------------------------

# Crear tu copia de respaldo para el EDA
datos_eda <- datos

# Primer vistazo rápido a la estructura
glimpse(datos_eda)  

# Revisar si hay valores faltantes (NA)
colSums(is.na(datos_eda))

# Resumen exploratorio
resumen_eda <- skim(datos_eda)
resumen_eda

# Visualizaciones ¿qué accidentes tienen víctimas?

# Colores y estilo de las gráficas
azul <- "#2a78d6"
naranja <- "#eb6834"
aqua <- "#1baf7a"
amarillo <- "#eda100"
rojo <- "#e34948"
gris <- "#9e9d98"

tema <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "grey35"),
        strip.text = element_text(face = "bold", hjust = 0),
        legend.text = element_text(margin = margin(r = 12)),
        plot.margin = margin(10, 20, 10, 10))

# Tasa de accidentes con víctimas
tasa_general <- mean(datos_eda$SEVERO)

cat("Accidentes:", comma(nrow(datos_eda)), "\n")
cat("Con víctimas:", comma(sum(datos_eda$SEVERO)), "-", percent(tasa_general, accuracy = 0.1), "\n")

# Frecuencia de accidentes por clase de accidente
datos_eda %>%
  count(CLASACC) %>%
  mutate(prop = percent(n / sum(n), accuracy = 0.1))

# Frecuencia de accidentes por tipo de accidente
datos_eda %>%
  count(TIPACCID) %>%
  mutate(prop = percent(n / sum(n), accuracy = 0.1)) %>%
  arrange(desc(n))

# % de accidentes con víctimas por Tipo de Accidente
datos_eda %>%
  group_by(TIPACCID) %>%
  summarise(accidentes = n(), con_victimas = mean(SEVERO)) %>%
  mutate(etiqueta = paste0(TIPACCID, " (", comma(accidentes), ")")) %>%
  ggplot(aes(x = con_victimas, y = reorder(etiqueta, con_victimas))) +
  geom_vline(xintercept = tasa_general, color = "grey40") +
  geom_col(fill = azul, width = 0.7) +
  geom_text(aes(label = percent(con_victimas, accuracy = 1)), hjust = -0.2, size = 3.5) +
  scale_x_continuous(labels = percent, limits = c(0, 1.08), expand = c(0, 0)) +
  labs(title = "El tipo de accidente marca la diferencia",
       subtitle = "% de accidentes con heridos o muertos. Entre paréntesis, número de accidentes",
       x = NULL, y = NULL) +
  tema

# % de accidentes con víctimas por grupo de edad del conductor
datos_eda %>%
  group_by(GRUPO_EDAD) %>%
  summarise(accidentes = n(), con_victimas = mean(SEVERO)) %>%
  ggplot(aes(x = GRUPO_EDAD, y = con_victimas)) +
  geom_hline(yintercept = tasa_general, color = "grey40") +
  geom_col(fill = azul, width = 0.7) +
  geom_text(aes(label = percent(con_victimas, accuracy = 0.1)), vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = percent, limits = c(0, 0.45), expand = c(0, 0)) +
  labs(title = "Los conductores de 12 a 24 años tienen más accidentes con víctimas",
       subtitle = "% de accidentes con heridos o muertos por grupo de edad del conductor",
       x = NULL, y = NULL) +
  tema

# Edad continua: solo edades conocidas (sin 0 = se fugó ni 99 = se ignora)
datos_eda %>%
  filter(ID_EDAD >= 12, ID_EDAD <= 99) %>%
  group_by(ID_EDAD) %>%
  summarise(accidentes = n(), con_victimas = mean(SEVERO)) %>%
  ggplot(aes(x = ID_EDAD, y = con_victimas)) +
  geom_vline(xintercept = c(18, 25, 45, 65), color = "grey80") +
  geom_line(color = azul, linewidth = 0.8) +
  geom_point(color = azul, size = 1.5) +
  scale_y_continuous(labels = percent) +
  scale_x_continuous(breaks = seq(10, 100, 10)) +
  labs(title = "La relación con la edad no es una línea recta",
       subtitle = "% de accidentes con víctimas por edad del conductor. Las líneas grises marcan los cortes de los grupos de edad",
       x = "Edad del conductor", y = NULL) +
  tema

# % de accidentes con víctimas por hora del día y día de la semana
datos_eda %>%
  group_by(DIASEMANA, ID_HORA) %>%
  summarise(con_victimas = mean(SEVERO), .groups = "drop") %>%
  ggplot(aes(x = ID_HORA, y = fct_rev(DIASEMANA), fill = con_victimas)) +
  geom_tile(color = "white", linewidth = 0.6) +
  scale_fill_gradientn(colors = c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b"),
                       labels = percent, name = "% con\nvíctimas") +
  scale_x_continuous(breaks = 0:23, expand = c(0, 0)) +
  labs(title = "Las noches y el domingo por la tarde concentran los accidentes más graves",
       subtitle = "% de accidentes con heridos o muertos por hora del día y día de la semana",
       x = "Hora del día", y = NULL) +
  tema +
  theme(panel.grid = element_blank())

# % de accidentes con heridos o muertos por categoría

# descripción de variables
vars_desc <- c(ZONA = "Zona", FRANJA_HORARIA = "Franja horaria", FESTIVO = "Día festivo",
               SEXO = "Sexo del conductor", ALIENTO = "Aliento alcohólico", REGION = "Región")

factores <- map_dfr(names(vars_desc), function(v) {
  datos_eda %>%
    group_by(categoria = .data[[v]]) %>%
    summarise(accidentes = n(), con_victimas = mean(SEVERO)) %>%
    mutate(variable = vars_desc[[v]])
})

# Ordenamos las barras dentro de cada panel
factores %>%
  mutate(categoria = fct_reorder(paste(variable, categoria, sep = "|"), con_victimas)) %>%
  ggplot(aes(x = con_victimas, y = categoria)) +
  geom_col(fill = azul, width = 0.7) +
  geom_text(aes(label = percent(con_victimas, accuracy = 0.1)), hjust = -0.15, size = 3.2) +
  facet_wrap(~ variable, scales = "free_y", ncol = 2) +
  scale_y_discrete(labels = function(x) str_remove(x, ".*\\|")) +
  scale_x_continuous(labels = percent, limits = c(0, 0.36), expand = c(0, 0)) +
  labs(title = "Lugar, momento y conductor",
       subtitle = paste0("% de accidentes con heridos o muertos por categoría. Promedio nacional: ",
                         percent(tasa_general, accuracy = 0.1)),
       x = NULL, y = NULL) +
  tema

# Tasa de accidentes con víctimas por Región
datos_eda %>%
  group_by(REGION) %>%
  summarise(accidentes = n(), con_victimas = mean(SEVERO)) %>%
  arrange(desc(con_victimas)) %>%
  mutate(accidentes = comma(accidentes), con_victimas = percent(con_victimas, accuracy = 0.1))

# % de accidentes con víctimas por entidad, agrupadas por región
datos_eda %>%
  group_by(REGION, ENTIDAD) %>%
  summarise(accidentes = n(), con_victimas = mean(SEVERO), .groups = "drop") %>%
  mutate(ENTIDAD = fct_reorder(ENTIDAD, con_victimas)) %>%
  ggplot(aes(x = con_victimas, y = ENTIDAD)) +
  geom_vline(xintercept = tasa_general, color = "grey40") +
  geom_col(fill = azul, width = 0.7) +
  geom_text(aes(label = percent(con_victimas, accuracy = 1)), hjust = -0.2, size = 3.2) +
  facet_grid(rows = vars(REGION), scales = "free_y", space = "free_y") +
  scale_x_continuous(labels = percent, limits = c(0, 0.55), expand = c(0, 0)) +
  labs(title = "El Sureste y el Occidente tienen la mayor proporción de accidentes con víctimas",
       subtitle = "% de accidentes con heridos o muertos por entidad, agrupadas por región",
       x = NULL, y = NULL) +
  tema +
  theme(strip.text.y = element_text(angle = 0, hjust = 0))

# ------------------------------------------------------------------------------
# MODELADO
# ------------------------------------------------------------------------------

## PREPARACIÓN DE DATOS

df_modelo <- datos %>%
  # Agrupar los tipos de accidentes menos frecuentes como "Otro"
  mutate(
    TIPO = if_else(TIPACCID %in% c("Colisión con animal", "Colisión con ferrocarril", "Incendio", "Otro"), "Otro", TIPACCID)
  ) %>%
  # Seleccionar variables predictoras
  select(
    SEVERO, TIPO, REGION, ZONA, FRANJA_HORARIA, DIASEMANA, 
    FESTIVO, SEXO, GRUPO_EDAD, ALIENTO
  ) %>%
  # Convertir a Factor y Definir la categoría de referencia
  mutate(
    TIPO = relevel(factor(TIPO), ref = "Colisión con vehículo automotor"),
    REGION = relevel(factor(REGION), ref = "Noroeste"),
    ZONA = relevel(factor(ZONA), ref = "Urbana en intersección"),
    FRANJA_HORARIA = relevel(factor(FRANJA_HORARIA), ref = "Mañana"),
    DIASEMANA = relevel(factor(DIASEMANA), ref = "Lunes"),
    FESTIVO = relevel(factor(FESTIVO), ref = "No"),
    SEXO = relevel(factor(SEXO), ref = "Hombre"),
    GRUPO_EDAD = relevel(factor(as.character(GRUPO_EDAD)), ref = "de 25 a 44"),
    ALIENTO = relevel(factor(ALIENTO), ref = "No")
  )

glimpse(df_modelo)

# Proporción de accidentes con víctimas (datos desbalanceados)
df_modelo %>% 
  count(SEVERO) %>% 
  mutate(prop = n / sum(n))

## DIVISIÓN DE LOS DATOS (TRAIN / TEST)

# Entrenamiento (70%) y prueba (30%)
set.seed(123)
idx <- sample(nrow(df_modelo), size = 0.7 * nrow(df_modelo))
train <- df_modelo[idx, ]
test <- df_modelo[-idx, ]

tibble(
  conjunto = c("Entrenamiento", "Prueba"),
  accidentes = comma(c(nrow(train), nrow(test))),
  con_victimas = percent(c(mean(train$SEVERO), mean(test$SEVERO)), accuracy = 0.1)
)

## ENTRENAMIENTO Y AJUSTE 

# Ajuste del modelo: Regresión Logística
modelo <- glm(SEVERO ~ ., data = train, family = binomial)

summary(modelo)

# Porcentaje de accidentes con víctimas por tipo (entrenamiento)
train %>%
  group_by(TIPO) %>%
  summarise(accidentes = n(), con_victimas = mean(SEVERO)) %>%
  arrange(desc(con_victimas)) %>%
  mutate(con_victimas = percent(con_victimas, accuracy = 0.1))

# Coeficientes del tipo de accidente
summary(modelo)$coefficients %>%
  as.data.frame() %>%
  rownames_to_column("termino") %>%
  filter(str_detect(termino, "^TIPO")) %>%
  transmute(termino, beta = Estimate, error_estandar = `Std. Error`, OR = exp(Estimate)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

## EVALUACIÓN DEL RENDIMIENTO

# umbral de decisión: si la probabilidad es >= 0.2, el modelo dice "con víctimas"
umbral <- 0.2

# Probabilidades predichas en prueba
prob_test <- predict(modelo, newdata = test, type = "response")

# Métricas en el conjunto de prueba con el umbral de 0.2
metricas <- function(prob, real, corte) {
  pred <- if_else(prob >= corte, 1, 0)
  VP <- sum(pred == 1 & real == 1)
  FN <- sum(pred == 0 & real == 1)
  FP <- sum(pred == 1 & real == 0)
  VN <- sum(pred == 0 & real == 0)
  tibble(umbral = corte,
         recall = VP / (VP + FN),
         precision = VP / (VP + FP),
         especificidad = VN / (VN + FP),
         VP = VP, FN = FN, FP = FP, VN = VN)
}

resultado <- metricas(prob_test, test$SEVERO, umbral) %>%
  mutate(AUC = as.numeric(auc(roc(test$SEVERO, prob_test, quiet = TRUE))))

resultado %>%
  mutate(across(recall:especificidad, ~ percent(.x, accuracy = 0.1)),
         AUC = round(AUC, 3))

# Matriz de confusión
pred_test <- if_else(prob_test >= umbral, 1, 0)

table(Predicho = pred_test, Real = test$SEVERO)

# Gráfico: Matriz de confusión
confusion <- tibble(Real = test$SEVERO, Predicho = pred_test) %>%
  mutate(
    across(everything(), ~ factor(if_else(.x == 1, "Con víctimas", "Sólo daños"),
                                  levels = c("Con víctimas", "Sólo daños")))
  ) %>%
  count(Real, Predicho) %>%
  group_by(Real) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  mutate(acierto = Real == Predicho)

ggplot(confusion, aes(x = Predicho, y = fct_rev(Real), fill = acierto)) +
  geom_tile(color = "white", linewidth = 2) +
  geom_text(aes(label = paste0(comma(n), "\n(", percent(prop, accuracy = 0.1), ")")), size = 5) +
  scale_fill_manual(values = c(`TRUE` = "#cde2fb", `FALSE` = "#fbd9cc"), guide = "none") +
  labs(title = "Matriz de confusión en el conjunto de prueba",
       subtitle = "Umbral de 0.2. El porcentaje es sobre el total de cada fila (lo real)",
       x = "Predicho por el modelo", y = "Real") +
  tema +
  theme(panel.grid = element_blank())

# Curva ROC
roc_test <- roc(test$SEVERO, prob_test, quiet = TRUE)

ggroc(roc_test, color = azul, linewidth = 0.9) +
  geom_abline(intercept = 1, slope = 1, color = "grey70") +
  annotate("point", x = resultado$especificidad, y = resultado$recall, color = naranja, size = 4) +
  annotate("text", x = resultado$especificidad - 0.03, y = resultado$recall - 0.05, hjust = 0,
           label = paste0("Umbral 0.2\nrecall ", percent(resultado$recall, accuracy = 0.1),
                          "\nespecificidad ", percent(resultado$especificidad, accuracy = 0.1))) +
  labs(title = paste0("Curva ROC (AUC = ", round(resultado$AUC, 3), ")"),
       subtitle = "Entre más se acerca la curva a la esquina superior izquierda, mejor distingue el modelo",
       x = "Especificidad", y = "Sensibilidad (recall)") +
  tema

### ¿Qué pasa con otros umbrales?

# Cómo cambian las métricas con otros umbrales (para justificar 0.2)
umbrales <- map_dfr(seq(0.05, 0.60, by = 0.01), ~ metricas(prob_test, test$SEVERO, .x))

umbrales %>% 
  mutate(
    umbral = round(umbral, 3), 
    across(recall:especificidad, ~ percent(.x, accuracy = 0.1))
  )

# Métricas del modelo según el umbral de decisión
map_dfr(seq(0.05, 0.6, by = 0.01), ~ metricas(prob_test, test$SEVERO, .x)) %>%
  select(umbral, Recall = recall, `Precisión` = precision, Especificidad = especificidad) %>%
  pivot_longer(-umbral, names_to = "metrica", values_to = "valor") %>%
  ggplot(aes(x = umbral, y = valor, color = metrica)) +
  geom_vline(xintercept = umbral, color = "grey40", linetype = "dashed") +
  geom_line(linewidth = 1) +
  scale_color_manual(values = c(Recall = azul, `Precisión` = naranja, Especificidad = aqua)) +
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  labs(title = "Métricas del modelo según el umbral de decisión",
       subtitle = "La línea punteada es el umbral que usamos (0.2)",
       x = "Umbral de decisión", y = NULL, color = NULL) +
  tema +
  theme(legend.position = "top")

# ------------------------------------------------------------------------------
# RESULTADOS CLAVE DEL MODELO
# ------------------------------------------------------------------------------

## ¿Qué factores se relacionan con los accidentes con víctimas?

nombres_vars <- c(TIPO = "Tipo de accidente", REGION = "Región", ZONA = "Zona", FRANJA_HORARIA = "Horario",
                  DIASEMANA = "Día de la semana", FESTIVO = "Día festivo", SEXO = "Sexo",
                  GRUPO_EDAD = "Edad", ALIENTO = "Aliento alcohólico")

grupo_vars <- c(TIPO = "Contexto del incidente", REGION = "Espacio", ZONA = "Espacio",
                FRANJA_HORARIA = "Tiempo", DIASEMANA = "Tiempo", FESTIVO = "Tiempo",
                SEXO = "Conductor", GRUPO_EDAD = "Conductor", ALIENTO = "Conductor")

# Atropellamientos y caídas de pasajero siempre tienen víctimas (100%):
# su razón de momios no se puede estimar (separación completa), por eso se marcan aparte.
siempre_victimas <- c("Colisión con peatón (atropellamiento)", "Caída de pasajero")

# Razones de momios
ors <- exp(cbind(OR = coef(modelo), confint.default(modelo))) %>%
  as.data.frame() %>%
  rownames_to_column("termino") %>%
  rename(li = `2.5 %`, ls = `97.5 %`) %>%
  filter(termino != "(Intercept)", !is.na(OR)) %>%
  mutate(clave = str_extract(termino, paste0("^(", paste(names(nombres_vars), collapse = "|"), ")")),
         categoria = str_remove(termino, clave),
         variable = nombres_vars[clave],
         grupo = grupo_vars[clave],
         etiqueta = paste0(variable, ": ", categoria),
         separacion = clave == "TIPO" & categoria %in% siempre_victimas)

ors %>%
  filter(!categoria %in% siempre_victimas) %>%
  arrange(desc(OR)) %>%
  select(etiqueta, OR, li, ls) %>%
  mutate(across(c(OR, li, ls), ~ round(.x, 2)))

# Gráfico: Factores asociados a accidentes con víctimas
ors %>%
  filter(!categoria %in% siempre_victimas) %>%
  mutate(efecto = case_when(li > 1 ~ "Aumenta la probabilidad",
                            ls < 1 ~ "Disminuye la probabilidad",
                            TRUE ~ "Sin diferencia clara")) %>%
  ggplot(aes(x = OR, y = reorder(etiqueta, OR), color = efecto)) +
  geom_vline(xintercept = 1, color = "grey50") +
  geom_pointrange(aes(xmin = li, xmax = ls), size = 0.35) +
  scale_x_log10(breaks = c(0.5, 0.75, 1, 1.5, 2, 3, 5, 10, 20)) +
  scale_color_manual(values = c(`Aumenta la probabilidad` = rojo,
                                `Disminuye la probabilidad` = azul,
                                `Sin diferencia clara` = gris)) +
  labs(title = "Factores asociados a accidentes con víctimas",
       subtitle = "Razón de momios e intervalo de 95% frente a la categoría de referencia. Escala logarítmica.\nSin atropellamientos ni caídas de pasajero (siempre tienen víctimas)",
       x = "Razón de momios", y = NULL, color = NULL) +
  tema +
  theme(legend.position = "top")

## ¿Qué factores pesan más?

# evalúa la importancia estadística de cada variable explicativa del modelo, 
# eliminando de forma individual cada uno de sus términos y comparando el 
# resultado contra el modelo completo original mediante una Prueba de Razón 
# de Verosimilitud (LRT)

# Importancia de cada variable (tarda uno o dos minutos)
importancia <- drop1(modelo, test = "LRT") %>%
  as.data.frame() %>%
  rownames_to_column("clave") %>%
  filter(clave != "<none>") %>%
  transmute(clave, variable = nombres_vars[clave], grupo = grupo_vars[clave], LRT)

importancia

# Gráfico: Importancia de cada variable 
ggplot(importancia, aes(x = LRT, y = reorder(variable, LRT))) +
  geom_col(fill = azul, width = 0.7) +
  geom_text(aes(label = comma(LRT, accuracy = 1)), hjust = -0.15, size = 3.5) +
  scale_x_continuous(labels = comma, limits = c(0, 63000), expand = c(0, 0)) +
  labs(title = "El tipo de accidente es por mucho el factor que más pesa",
       subtitle = "Cuánto empeora el modelo si se quita cada variable (estadístico de razón de verosimilitudes)",
       x = NULL, y = NULL) +
  tema

## ¿Qué aporta el modelo? 

# Probabilidades para distintos escenarios
escenarios <- tibble(
  escenario = c("Perfil base", "+ con motocicleta", "+ de noche", "+ en domingo",
                "+ conductor de 18 a 24 años", "+ con aliento alcohólico", "+ en zona suburbana"),
  TIPO = c("Colisión con vehículo automotor", rep("Colisión con motocicleta", 6)),
  FRANJA_HORARIA = c(rep("Mañana", 2), rep("Noche", 5)),
  DIASEMANA = c(rep("Lunes", 3), rep("Domingo", 4)),
  GRUPO_EDAD = c(rep("de 25 a 44", 4), rep("de 18 a 24", 3)),
  ALIENTO = c(rep("No", 5), rep("Sí", 2)),
  ZONA = c(rep("Urbana en intersección", 6), "Suburbana"),
  REGION = "Noroeste", FESTIVO = "No", SEXO = "Hombre"
)

escenarios$prob <- predict(modelo, newdata = escenarios, type = "response")

escenarios %>%
  mutate(clasificacion = if_else(prob >= umbral, "Con víctimas", "Sólo daños"),
         prob = percent(prob, accuracy = 0.1)) %>%
  select(escenario, prob, clasificacion)

# Gráfico: Probabilidad de víctimas según el modelo
escenarios %>%
  mutate(escenario = fct_rev(fct_inorder(escenario))) %>%
  ggplot(aes(x = prob, y = escenario)) +
  geom_col(fill = azul, width = 0.7) +
  geom_vline(xintercept = umbral, color = naranja, linetype = "dashed") +
  geom_text(aes(label = percent(prob, accuracy = 0.1)), hjust = -0.15, size = 3.8) +
  scale_x_continuous(labels = percent, limits = c(0, 1), expand = c(0, 0)) +
  labs(title = "Los factores de riesgo se acumulan",
       subtitle = "Probabilidad de víctimas según el modelo; la línea naranja es el umbral de 0.2. Perfil base: choque entre vehículos\nen intersección urbana de la región Noroeste, lunes en la mañana, conductor hombre de 25 a 44 años sin aliento alcohólico",
       x = NULL, y = NULL) +
  tema

