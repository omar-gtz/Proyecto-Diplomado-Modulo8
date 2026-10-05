# ------------------------------------------------------------------------------
# Preparación de datos para el dashboard (correr una sola vez)
#
# Genera dos archivos que usa app.R:
#   datos_dashboard.rds  -> base sin duplicados y solo accidentes causados por el conductor
#   modelo_dashboard.rds -> resultados del modelo (página del modelo)
#
# Los archivos se leen de esta misma carpeta. Abrir primero dashboard.Rproj
# (así R trabaja dentro de esta carpeta) y luego correr este script con "Source".
# Paquetes: install.packages(c("tidyverse", "pROC"))
# ------------------------------------------------------------------------------

library(tidyverse)
library(pROC)

# Datos
df <- read_csv("datos/atus_anual_2025.csv", show_col_types = FALSE)
cat_entidad <- read_csv("datos/tc_entidad.csv", show_col_types = FALSE)
cat_municipios <- read_csv("datos/tc_municipio.csv", show_col_types = FALSE)
cat_region <- read_csv("datos/cat_region.csv", col_types = cols(.default = col_character()))

festivos <- as.Date(c("2025-01-01", "2025-02-03", "2025-03-17", "2025-05-01",
                      "2025-09-16", "2025-11-17", "2025-12-25"))

vehiculos <- c("AUTOMOVIL", "CAMPASAJ", "MICROBUS", "PASCAMION", "OMNIBUS", "TRANVIA", "CAMIONETA",
               "CAMION", "TRACTOR", "FERROCARRI", "MOTOCICLET", "BICICLETA", "OTROVEHIC")

umbral <- 0.2   # umbral de decisión: si la probabilidad es >= 0.2, el modelo dice "con víctimas"

# ------------------------------------------------------------------------------
# 1. Base: sin duplicados y solo causa "Conductor"
# ------------------------------------------------------------------------------
datos <- df %>%
  distinct() %>%
  filter(CAUSAACCI == "Conductor") %>%
  left_join(cat_entidad %>% select(CVE_ENT, NOM_ENTIDAD), by = "CVE_ENT") %>%
  left_join(cat_municipios %>% select(CVEGEO, NOM_MUNICIPIO), by = "CVEGEO") %>%
  left_join(cat_region %>% select(CVE_ENT, REGION), by = "CVE_ENT") %>%
  mutate(
    MES = as.numeric(MES),
    ID_HORA = as.numeric(ID_HORA),
    FECHA = make_date(ANIO, MES, as.numeric(ID_DIA)),
    DIASEMANA = factor(DIASEMANA, levels = c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")),
    FESTIVO = if_else(FECHA %in% festivos, "Sí", "No"),
    TIPO_DIA = case_when(
      FESTIVO == "Sí" ~ "Día festivo",
      DIASEMANA %in% c("Sábado", "Domingo") ~ "Fin de semana",
      TRUE ~ "Entre semana"
    ),
    FRANJA_HORARIA = case_when(
      ID_HORA >= 0 & ID_HORA < 6 ~ "Madrugada",
      ID_HORA >= 6 & ID_HORA < 12 ~ "Mañana",
      ID_HORA >= 12 & ID_HORA < 18 ~ "Tarde",
      ID_HORA >= 18 & ID_HORA < 24 ~ "Noche"
    ),
    SEVERO = if_else(CLASACC == "Sólo daños", 0, 1),
    FATAL = if_else(CLASACC == "Fatal", 1, 0),
    ENTIDAD = str_remove(NOM_ENTIDAD, " de Ignacio de la Llave| de Zaragoza| de Ocampo"),
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
    NUM_VEHICULOS = rowSums(across(all_of(vehiculos))),
    MUERTOS = CONDMUERTO + PASAMUERTO + PEATMUERTO + CICLMUERTO + OTROMUERTO + NEMUERTO,
    HERIDOS = CONDHERIDO + PASAHERIDO + PEATHERIDO + CICLHERIDO + OTROHERIDO + NEHERIDO
  ) %>%
  select(FECHA, MES, DIASEMANA, ID_HORA, FESTIVO, TIPO_DIA, FRANJA_HORARIA,
         CVE_ENT, ENTIDAD, REGION, CVEGEO, NOM_MUNICIPIO, ZONA, ZONA_DETALLE,
         TIPACCID, CAUSAACCI, CAPAROD, CLASACC, SEVERO, FATAL,
         all_of(vehiculos), NUM_VEHICULOS,
         SEXO, ID_EDAD, GRUPO_EDAD, ALIENTO, CINTURON,
         CONDMUERTO, CONDHERIDO, PASAMUERTO, PASAHERIDO, PEATMUERTO, PEATHERIDO,
         CICLMUERTO, CICLHERIDO, OTROMUERTO, OTROHERIDO, NEMUERTO, NEHERIDO, MUERTOS, HERIDOS)

saveRDS(datos, "datos_app/datos_dashboard.rds")

cat("Base para visualizaciones:", nrow(datos), "accidentes\n")

# ------------------------------------------------------------------------------
# 2. Modelo: todos los tipos de accidente
# ------------------------------------------------------------------------------
df_modelo <- datos %>%
  mutate(TIPO = if_else(TIPACCID %in% c("Colisión con animal", "Colisión con ferrocarril", "Incendio", "Otro"),
                        "Otro", TIPACCID)) %>%
  select(SEVERO, TIPO, REGION, ZONA, FRANJA_HORARIA, DIASEMANA, 
         FESTIVO, SEXO, GRUPO_EDAD, ALIENTO) %>%
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

# Entrenamiento (70%) y prueba (30%)
set.seed(123)
idx <- sample(nrow(df_modelo), size = 0.7 * nrow(df_modelo))
train <- df_modelo[idx, ]
test <- df_modelo[-idx, ]

# Ajuste del modelo
modelo <- glm(SEVERO ~ ., data = train, family = binomial)

# Métricas en el conjunto de prueba con el umbral de 0.2
prob_test <- predict(modelo, newdata = test, type = "response")

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

# Cómo cambian las métricas con otros umbrales (para justificar 0.2)
umbrales <- map_dfr(seq(0.05, 0.60, by = 0.01), ~ metricas(prob_test, test$SEVERO, .x))

# Razones de momios
nombres_vars <- c(TIPO = "Tipo de accidente", REGION = "Región", ZONA = "Zona", FRANJA_HORARIA = "Horario",
                  DIASEMANA = "Día de la semana", FESTIVO = "Día festivo", SEXO = "Sexo",
                  GRUPO_EDAD = "Edad", ALIENTO = "Aliento alcohólico")

grupo_vars <- c(TIPO = "Contexto del incidente", REGION = "Espacio", ZONA = "Espacio",
                FRANJA_HORARIA = "Tiempo", DIASEMANA = "Tiempo", FESTIVO = "Tiempo",
                SEXO = "Conductor", GRUPO_EDAD = "Conductor", ALIENTO = "Conductor")

# Atropellamientos y caídas de pasajero siempre tienen víctimas (100%):
# su razón de momios no se puede estimar (separación completa), por eso se marcan aparte.
siempre_victimas <- c("Colisión con peatón (atropellamiento)", "Caída de pasajero")

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

# Importancia de cada variable (tarda uno o dos minutos)
importancia <- drop1(modelo, test = "LRT") %>%
  as.data.frame() %>%
  rownames_to_column("clave") %>%
  filter(clave != "<none>") %>%
  transmute(clave, variable = nombres_vars[clave], grupo = grupo_vars[clave], LRT)

# Lo necesario para calcular probabilidades en el simulador
simulador <- list(
  coeficientes = coef(modelo),
  terminos = delete.response(terms(modelo)),
  niveles = modelo$xlevels,
  promedio = mean(train$SEVERO),
  umbral = umbral
)

elementos_modelo <- list(resultado = resultado, umbrales = umbrales, ors = ors, importancia = importancia,
                         simulador = simulador, n_train = nrow(train), n_test = nrow(test))

saveRDS(elementos_modelo, "datos_app/modelo_dashboard.rds")

cat("Listo: datos_dashboard.rds y modelo_dashboard.rds\n")
