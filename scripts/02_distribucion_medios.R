# =====================================================================
# Ómnibus 1 · Sesgos de confirmación y confianza en los medios
# Distribución de respuestas: número bruto (n) y porcentajes
#   install.packages(c("dplyr","tidyr","ggplot2","forcats"))
# =====================================================================

library(dplyr); library(tidyr); library(ggplot2); library(forcats)

df <- read.csv("data/Omnibus1_medios_expandido.csv", encoding = "UTF-8")

# Nota: la columna 'id' NO es un identificador único (se repite entre
# personas distintas). La unidad de análisis es la fila: 1,163 entrevistas.
nrow(df)          # 1163
sum(df$FE)        # ≈ 83.6 millones (población representada)

# --- variables del módulo y orden de las categorías -------------------
items <- grep("^q_", names(df), value = TRUE)

niveles <- c("No está de acuerdo",
             "Ni de acuerdo ni en desacuerdo",
             "Sí está de acuerdo",
             "No sabe/No responde")


# =====================================================================
# 1. UNA SOLA VARIABLE (lo más básico)
# =====================================================================

# conteo bruto
table(df$q_imposible_distinguir)

# porcentaje sin ponderar
round(prop.table(table(df$q_imposible_distinguir)) * 100, 1)

# conteo y porcentaje juntos
tab <- table(df$q_imposible_distinguir)
data.frame(categoria = names(tab),
           n         = as.integer(tab),
           pct       = round(prop.table(tab) * 100, 1))

# porcentaje PONDERADO (con el factor de expansión)
w <- tapply(df$FE, df$q_imposible_distinguir, sum)
round(w / sum(w) * 100, 1)


# =====================================================================
# 2. TODAS LAS VARIABLES DEL MÓDULO, EN UNA TABLA
# =====================================================================

distribucion <- function(datos, vars, niveles){
  do.call(rbind, lapply(vars, function(v){
    x  <- factor(datos[[v]], levels = niveles)
    ok <- !is.na(x)                      # excluye casos sin respuesta
    n  <- table(x)                       # conteo bruto
    w  <- tapply(datos$FE[ok], x[ok], sum); w[is.na(w)] <- 0
    data.frame(
      variable  = v,
      categoria = niveles,
      n         = as.integer(n),
      pct       = round(as.numeric(n) / sum(n) * 100, 1),          # sin ponderar
      pct_pond  = round(as.numeric(w) / sum(w) * 100, 1),          # ponderado
      row.names = NULL)
  }))
}

tabla <- distribucion(df, items, niveles)
head(tabla, 8)

# formato ancho: una fila por reactivo, columnas = categorías (% ponderado)
tabla |>
  select(variable, categoria, pct_pond) |>
  pivot_wider(names_from = categoria, values_from = pct_pond) |>
  arrange(desc(`Sí está de acuerdo`))

# resumen rápido: solo el % de acuerdo, de mayor a menor
tabla |>
  filter(categoria == "Sí está de acuerdo") |>
  select(variable, n, pct, pct_pond) |>
  arrange(desc(pct_pond))


# =====================================================================
# 3. CRUCES POR UNA VARIABLE SOCIODEMOGRÁFICA
# =====================================================================

# conteo bruto (tabla de contingencia)
table(df$escolaridad, df$q_coincide_no_busca)

# % por fila, sin ponderar  (margin = 1 -> cada fila suma 100)
round(prop.table(table(df$escolaridad, df$q_coincide_no_busca), margin = 1) * 100, 1)

# % de acuerdo por grupo, ponderado
acuerdo_por <- function(datos, var, grupo){
  datos |>
    filter(!is.na(.data[[var]]), !is.na(.data[[grupo]])) |>
    group_by(grupo = .data[[grupo]]) |>
    summarise(
      n        = n(),                                              # casos brutos
      pct      = round(mean(.data[[var]] == "Sí está de acuerdo") * 100, 1),
      pct_pond = round(sum(FE[.data[[var]] == "Sí está de acuerdo"]) / sum(FE) * 100, 1),
      .groups  = "drop")
}

acuerdo_por(df, "q_coincide_no_busca", "escolaridad")
acuerdo_por(df, "q_coincide_no_busca", "age_group")
acuerdo_por(df, "q_molestan_periodistas", "sexo")

# prueba de asociación (chi-cuadrada, sin ponderar)
chisq.test(table(df$escolaridad, df$q_coincide_no_busca))


# =====================================================================
# 4. GRÁFICAS (ggplot2)
# =====================================================================

# etiquetas legibles para cada reactivo (en vez del nombre de la variable)
etiquetas <- c(
  q_imposible_distinguir   = "Es imposible distinguir noticias falsas de verdaderas",
  q_emergencia_redes       = "En una emergencia prefiero X o Facebook",
  q_comparte_desenmascarar = "Comparto información para desenmascarar a los medios",
  q_titular_ineficaces     = "Me interesa el titular: políticas “ineficaces, según expertos”",
  q_confio_abiertos        = "Confío más en periodistas que dicen lo que piensan",
  q_titular_enemigos       = "Me interesa el titular: “los enemigos del pueblo”",
  q_medios_verifican       = "La TV, radio y periódicos verifican antes de publicar",
  q_redes_confiables       = "Quienes informan en redes son más confiables",
  q_coincide_no_busca      = "Si la noticia coincide conmigo, no busco otras opiniones",
  q_no_seguir_noticias     = "Prefiero no seguir las noticias",
  q_molestan_periodistas   = "Me molestan los periodistas que confrontan al gobierno",
  q_discutir_redes         = "Me gusta discutir con quien opina distinto")

# paleta y tema comunes
col_cat <- c("No está de acuerdo"             = "#C3CCD6",
             "Ni de acuerdo ni en desacuerdo" = "#8593A3",
             "Sí está de acuerdo"             = "#1F3864",
             "No sabe/No responde"            = "#E8EBEF")

tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        plot.title.position = "plot",
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(color = "grey35", margin = margin(b = 10)),
        plot.caption  = element_text(color = "grey45", hjust = 0, size = 8),
        legend.position = "top", legend.title = element_blank())

FUENTE <- "Fuente: Ómnibus 1, Universidad Iberoamericana. Porcentajes ponderados (FE); n = 1,163 entrevistas."


# --- 4.1 Ranking: % de acuerdo por reactivo ---------------------------
g1 <- tabla |>
  filter(categoria == "Sí está de acuerdo") |>
  mutate(item = fct_reorder(etiquetas[variable], pct_pond)) |>
  ggplot(aes(pct_pond, item)) +
  geom_col(fill = "#1F3864", width = .7) +
  geom_text(aes(label = paste0(pct_pond, "%")), hjust = -0.15, size = 3.2) +
  scale_x_continuous(limits = c(0, 75), expand = expansion(mult = c(0, .05))) +
  labs(title = "Grado de acuerdo con cada afirmación",
       subtitle = "% que responde “sí está de acuerdo”",
       x = NULL, y = NULL, caption = FUENTE) +
  tema
g1

# --- 4.2 Barras apiladas: distribución completa -----------------------
g2 <- tabla |>
  mutate(item = etiquetas[variable],
         categoria = factor(categoria, levels = rev(niveles))) |>
  group_by(variable) |>
  mutate(orden = pct_pond[categoria == "Sí está de acuerdo"]) |>
  ungroup() |>
  mutate(item = fct_reorder(item, orden)) |>
  ggplot(aes(pct_pond, item, fill = categoria)) +
  geom_col(width = .7) +
  geom_text(aes(label = ifelse(pct_pond >= 8, paste0(round(pct_pond), "%"), "")),
            position = position_stack(vjust = .5), size = 2.9, color = "white") +
  scale_fill_manual(values = col_cat, breaks = niveles) +
  scale_x_continuous(expand = expansion(mult = c(0, .01))) +
  labs(title = "Distribución completa de respuestas",
       subtitle = "% ponderado por categoría; cada barra suma 100",
       x = NULL, y = NULL, caption = FUENTE) +
  tema
g2

# --- 4.3 Un cruce: % de acuerdo por escolaridad -----------------------
orden_esc <- c("Primaria","Secundaria","Preparatoria o bachillerato",
               "Carrera técnica o comercial","Licenciatura o Ingeniería",
               "Maestría o especialidad")

g3 <- acuerdo_por(df, "q_coincide_no_busca", "escolaridad") |>
  filter(grupo %in% orden_esc, n >= 30) |>       # omite grupos con pocos casos
  mutate(grupo = factor(grupo, levels = orden_esc)) |>
  ggplot(aes(grupo, pct_pond)) +
  geom_col(fill = "#1F3864", width = .68) +
  geom_text(aes(label = paste0(pct_pond, "%")), vjust = -0.6, size = 3.2) +
  geom_text(aes(label = paste0("n=", n)), y = 2, color = "white", size = 2.6) +
  scale_x_discrete(labels = function(x) gsub(" o ", "\no ", x)) +
  scale_y_continuous(limits = c(0, 60), expand = expansion(mult = c(0, .05))) +
  labs(title = "El sesgo de confirmación disminuye con la escolaridad",
       subtitle = "“Si la noticia coincide con lo que pienso, no busco otras opiniones” (% de acuerdo)",
       x = NULL, y = NULL, caption = FUENTE) +
  tema + theme(panel.grid.major.y = element_line(color = "grey92"))
g3

# --- 4.4 Comparar dos grupos en varios reactivos ----------------------
g4 <- lapply(names(etiquetas), function(v)
        acuerdo_por(df, v, "age_group") |> mutate(variable = v)) |>
  bind_rows() |>
  filter(grupo %in% c("18-24", "60+")) |>
  mutate(item = fct_reorder(etiquetas[variable], pct_pond, .fun = max)) |>
  ggplot(aes(pct_pond, item, color = grupo)) +
  geom_line(aes(group = item), color = "grey75", linewidth = .8) +
  geom_point(size = 3) +
  scale_color_manual(values = c("18-24" = "#C08422", "60+" = "#1F3864")) +
  scale_x_continuous(limits = c(0, 80)) +
  labs(title = "Jóvenes frente a personas mayores",
       subtitle = "% de acuerdo, por grupo de edad",
       x = NULL, y = NULL, caption = FUENTE) +
  tema
g4

# --- guardar ----------------------------------------------------------
ggsave("outputs/g1_ranking.png",     g1, width = 9, height = 5.5, dpi = 300, bg = "white")
ggsave("outputs/g2_apiladas.png",    g2, width = 9.5, height = 6, dpi = 300, bg = "white")
ggsave("outputs/g3_escolaridad.png", g3, width = 8, height = 5, dpi = 300, bg = "white")
ggsave("outputs/g4_edad.png",        g4, width = 9, height = 5.5, dpi = 300, bg = "white")


# =====================================================================
# 5. EXPORTAR
# =====================================================================
write.csv(tabla, "outputs/distribucion_respuestas.csv", row.names = FALSE, fileEncoding = "UTF-8")

# =====================================================================
# NOTAS
#
# · pct      = porcentaje muestral (sin ponderar); útil para ver el peso
#              real de cada categoría en la muestra.
# · pct_pond = porcentaje ponderado con FE; es el que representa a la
#              población adulta de México y el que debe reportarse.
# · Los conteos brutos (n) siempre van sin ponderar: indican en cuántas
#              entrevistas se apoya cada cifra. Con n bajos (< 30) los
#              porcentajes son inestables y conviene señalarlo.
# · La chi-cuadrada se calcula sin ponderar: indica asociación, pero no
#              incorpora el diseño muestral. Para eso, ver el paquete
#              'survey':  svydesign(ids = ~1, weights = ~FE, data = df)
#              y luego    svychisq(~escolaridad + q_coincide_no_busca, dis)
# =====================================================================
