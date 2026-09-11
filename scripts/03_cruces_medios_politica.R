# =====================================================================
# Cruces: variables políticas × consumo de medios
# Ómnibus 1 · Universidad Iberoamericana
#
# Base: Omnibus1_medios_politica_unido.csv (1,163 entrevistas)
#   install.packages(c("dplyr","tidyr","ggplot2","forcats"))
# =====================================================================

library(dplyr); library(tidyr); library(ggplot2); library(forcats)

df <- read.csv("data/Omnibus1_medios_politica_unido.csv", encoding = "UTF-8")


# ---------------------------------------------------------------------
# 0. Preparación
# ---------------------------------------------------------------------

# ATENCIÓN: las escalas van en sentido OPUESTO entre módulos.
#   Medios   -> "No está de acuerdo" ... "Sí está de acuerdo"
#   Política -> "De acuerdo" ... "En desacuerdo"
AG_MED <- "Sí está de acuerdo"     # respuesta afirmativa en MEDIOS
AG_POL <- "De acuerdo"             # respuesta afirmativa en POLÍTICA
NR     <- "No sabe/No responde"

V_MEDIOS <- c("q_no_seguir_noticias","q_medios_verifican","q_redes_confiables",
              "q_emergencia_redes","q_molestan_periodistas","q_coincide_no_busca",
              "q_confio_abiertos","q_titular_enemigos","q_titular_ineficaces",
              "q_discutir_redes","q_imposible_distinguir","q_comparte_desenmascarar")

V_POLITICA <- c("q_frase_voto1","q_frase_politicos","q_voto_insuficiente",
                "q_otras_formas","q_politicos_preocupan","q_partidos_beneficio",
                "q_acciones_cambio","q_redes_informarse","q_redes_emociones",
                "q_ideologia","q_evita_hablar","q_gusto_hablar")

# etiquetas legibles (para tablas y gráficas)
ETQ <- c(
  q_no_seguir_noticias     = "Prefiere no seguir las noticias",
  q_medios_verifican       = "Los medios verifican antes de publicar",
  q_redes_confiables       = "Quienes informan en redes son más confiables",
  q_emergencia_redes       = "En una emergencia recurre a X o Facebook",
  q_molestan_periodistas   = "Le molestan los periodistas que confrontan",
  q_coincide_no_busca      = "No busca otras opiniones si la noticia coincide",
  q_confio_abiertos        = "Confía en periodistas que declaran su postura",
  q_titular_enemigos       = "Le interesa el titular “enemigos del pueblo”",
  q_titular_ineficaces     = "Le interesa el titular “políticas ineficaces”",
  q_discutir_redes         = "Le gusta discutir con quien opina distinto",
  q_imposible_distinguir   = "Cree imposible distinguir lo falso de lo verdadero",
  q_comparte_desenmascarar = "Comparte para “desenmascarar” a los medios")


# ---------------------------------------------------------------------
# 1. Funciones base
# ---------------------------------------------------------------------

# % ponderado de acuerdo en un reactivo de MEDIOS, por categoría de una
# variable de POLÍTICA. Devuelve también la n bruta de cada grupo.
cruce <- function(datos, var_medios, var_grupo, min_n = 30, excluir_nr = TRUE){
  d <- datos |> filter(!is.na(.data[[var_medios]]), !is.na(.data[[var_grupo]]))
  if (excluir_nr) d <- d |> filter(.data[[var_grupo]] != NR,
                                   .data[[var_medios]] != NR)
  d |>
    group_by(grupo = .data[[var_grupo]]) |>
    summarise(n        = n(),
              pct      = round(mean(.data[[var_medios]] == AG_MED) * 100, 1),
              pct_pond = round(sum(FE[.data[[var_medios]] == AG_MED]) / sum(FE) * 100, 1),
              .groups  = "drop") |>
    filter(n >= min_n) |>
    arrange(desc(pct_pond))
}

# Prueba de asociación 2x2 entre dos categorías.
#
# Se usa "de acuerdo vs. resto" en lugar de la chi-cuadrada sobre la
# tabla completa. Motivo: con 4 categorías, la prueba capta diferencias
# en "ni/ni" y "NS/NR" que no son sustantivas y arroja p < .001 incluso
# cuando el % de acuerdo es idéntico entre grupos. El resultado 2x2
# responde a la pregunta que realmente importa.
prueba_2x2 <- function(datos, var_medios, var_grupo, g1, g2){
  d <- datos |>
    filter(!is.na(.data[[var_medios]]), .data[[var_medios]] != NR,
           .data[[var_grupo]] %in% c(g1, g2))
  t <- table(d[[var_grupo]], d[[var_medios]] == AG_MED)
  if (nrow(t) < 2) return(data.frame(n = nrow(d), chi2 = NA, p = NA))
  r <- suppressWarnings(chisq.test(t))
  data.frame(n = sum(t), chi2 = round(unname(r$statistic), 1),
             p = signif(r$p.value, 3),
             sig = ifelse(r$p.value < .05, "*", "ns"))
}

# Diferencia entre los dos grupos extremos de una variable política,
# para un reactivo de medios. Es la unidad del barrido sistemático.
brecha <- function(datos, var_medios, var_grupo, min_n = 30){
  r <- cruce(datos, var_medios, var_grupo, min_n)
  if (nrow(r) < 2) return(NULL)
  alto <- r[1, ]; bajo <- r[nrow(r), ]
  pr <- prueba_2x2(datos, var_medios, var_grupo, alto$grupo, bajo$grupo)
  data.frame(medios = var_medios, politica = var_grupo,
             grupo_alto = alto$grupo, pct_alto = alto$pct_pond, n_alto = alto$n,
             grupo_bajo = bajo$grupo, pct_bajo = bajo$pct_pond, n_bajo = bajo$n,
             brecha = round(alto$pct_pond - bajo$pct_pond, 1),
             p = pr$p, sig = pr$sig)
}


# ---------------------------------------------------------------------
# 2. Barrido sistemático: 12 × 12 = 144 cruces
# ---------------------------------------------------------------------
# Explora todo el espacio y ordena por tamaño de la brecha. Sirve para
# detectar dónde vale la pena mirar de cerca.

grid <- expand.grid(medios = V_MEDIOS, politica = V_POLITICA,
                    stringsAsFactors = FALSE)

barrido <- do.call(rbind, Map(function(m, p) brecha(df, m, p),
                              grid$medios, grid$politica))

resultados <- barrido |>
  filter(sig == "*") |>
  arrange(desc(brecha))

head(resultados, 20)

write.csv(barrido, "outputs/cruces_todos.csv", row.names = FALSE,
          fileEncoding = "UTF-8")


# ---------------------------------------------------------------------
# 3. Tabla detallada de un cruce
# ---------------------------------------------------------------------
cruce(df, "q_comparte_desenmascarar", "q_redes_emociones")
cruce(df, "q_medios_verifican",       "q_ideologia")
cruce(df, "q_no_seguir_noticias",     "q_evita_hablar")

# distribución completa (no solo el % de acuerdo), ponderada
tabla_cruzada <- function(datos, var_medios, var_grupo){
  datos |>
    filter(!is.na(.data[[var_medios]]), !is.na(.data[[var_grupo]]),
           .data[[var_grupo]] != NR) |>
    group_by(grupo = .data[[var_grupo]], respuesta = .data[[var_medios]]) |>
    summarise(w = sum(FE), n = n(), .groups = "drop_last") |>
    mutate(pct_pond = round(w / sum(w) * 100, 1)) |>
    ungroup() |>
    select(-w) |>
    pivot_wider(names_from = respuesta, values_from = c(n, pct_pond))
}
tabla_cruzada(df, "q_titular_enemigos", "q_ideologia")


# ---------------------------------------------------------------------
# 4. Los cuatro cruces con mayor sustancia
# ---------------------------------------------------------------------

## 4.1 Emocionalidad en redes × sesgo informativo -----------------------
items_emo <- c("q_comparte_desenmascarar","q_titular_enemigos",
               "q_imposible_distinguir","q_emergencia_redes","q_coincide_no_busca")

lapply(items_emo, function(v)
  cbind(reactivo = ETQ[v],
        cruce(df, v, "q_redes_emociones") |>
          filter(grupo %in% c("De acuerdo","En desacuerdo")) |>
          select(grupo, n, pct_pond))) |> bind_rows()

lapply(items_emo, function(v)
  cbind(reactivo = v,
        prueba_2x2(df, v, "q_redes_emociones", "De acuerdo", "En desacuerdo"))) |>
  bind_rows()

## 4.2 Ideología × confianza mediática ---------------------------------
items_ideo <- c("q_medios_verifican","q_titular_enemigos","q_titular_ineficaces",
                "q_molestan_periodistas","q_confio_abiertos")
orden_ideo <- c("Izquierda","Centro","Derecha","Ninguna")

lapply(items_ideo, function(v)
  cruce(df, v, "q_ideologia") |>
    filter(grupo %in% orden_ideo) |>
    mutate(reactivo = ETQ[v]) |>
    select(reactivo, grupo, pct_pond)) |>
  bind_rows() |>
  pivot_wider(names_from = grupo, values_from = pct_pond)

prueba_2x2(df, "q_medios_verifican",   "q_ideologia", "Izquierda", "Centro")
prueba_2x2(df, "q_titular_ineficaces", "q_ideologia", "Izquierda", "Derecha")

## 4.3 Creencia en la política × desconfianza mediática ----------------
orden_pol <- c("Creo en la política y en los políticos",
               "Creo en la política, pero no en los políticos",
               "No creo en la política ni en los políticos")

lapply(c("q_medios_verifican","q_redes_confiables","q_confio_abiertos",
         "q_comparte_desenmascarar"), function(v)
  cruce(df, v, "q_frase_politicos") |>
    mutate(reactivo = ETQ[v]) |> select(reactivo, grupo, n, pct_pond)) |>
  bind_rows()

## 4.4 Evitación: interpersonal e informativa --------------------------
cruce(df, "q_no_seguir_noticias", "q_evita_hablar")
prueba_2x2(df, "q_no_seguir_noticias", "q_evita_hablar", "Sí", "No")

# contraejemplo: compartir para "desenmascarar" NO difiere (p > .05).
# Conviene reportarlo: la evitación no lo explica todo.
cruce(df, "q_comparte_desenmascarar", "q_evita_hablar")
prueba_2x2(df, "q_comparte_desenmascarar", "q_evita_hablar", "Sí", "No")


# ---------------------------------------------------------------------
# 5. Gráficas
# ---------------------------------------------------------------------

tema <- theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        legend.position = "top", legend.title = element_blank(),
        plot.title.position = "plot", plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, color = "grey45", size = 8))

FUENTE <- "Fuente: Ómnibus 1, Universidad Iberoamericana. Porcentajes ponderados (FE); n = 1,163 entrevistas."

## 5.1 Barras agrupadas: dos grupos en varios reactivos ----------------
g1 <- lapply(items_emo, function(v)
        cruce(df, v, "q_redes_emociones") |> mutate(reactivo = v)) |>
  bind_rows() |>
  filter(grupo %in% c("De acuerdo","En desacuerdo")) |>
  mutate(item  = fct_reorder(ETQ[reactivo], pct_pond, .fun = max),
         grupo = recode(grupo,
                        "De acuerdo"    = "Las redes le generan emociones intensas",
                        "En desacuerdo" = "No le generan emociones intensas")) |>
  ggplot(aes(pct_pond, item, fill = grupo)) +
  geom_col(position = position_dodge(width = .7), width = .62) +
  geom_text(aes(label = paste0(pct_pond, "%")),
            position = position_dodge(width = .7), hjust = -0.15, size = 3) +
  scale_fill_manual(values = c("Las redes le generan emociones intensas" = "#B5261E",
                               "No le generan emociones intensas"        = "#8593A3")) +
  scale_x_continuous(limits = c(0, 85), expand = expansion(mult = c(0, .04))) +
  labs(title = "La activación emocional en redes acompaña al sesgo informativo",
       subtitle = "% de acuerdo con cada afirmación",
       x = NULL, y = NULL, caption = FUENTE) + tema
g1

## 5.2 Mapa de calor: ideología × reactivos de medios ------------------
g2 <- lapply(V_MEDIOS, function(v)
        cruce(df, v, "q_ideologia") |> mutate(reactivo = v)) |>
  bind_rows() |>
  filter(grupo %in% orden_ideo) |>
  mutate(grupo = factor(grupo, orden_ideo),
         item  = fct_reorder(ETQ[reactivo], pct_pond, .fun = mean)) |>
  ggplot(aes(grupo, item, fill = pct_pond)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = paste0(pct_pond, "%")), size = 3, color = "grey15") +
  scale_fill_gradient(low = "#EEF1F5", high = "#7FA8C9", guide = "none") +
  labs(title = "Consumo de medios según identificación ideológica",
       subtitle = "% de acuerdo con cada afirmación",
       x = NULL, y = NULL, caption = FUENTE) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(), plot.title.position = "plot",
        plot.title = element_text(face = "bold"),
        plot.caption = element_text(hjust = 0, color = "grey45", size = 8))
g2

ggsave("outputs/cruce_emociones.png", g1, width = 9,   height = 5.5, dpi = 300, bg = "white")
ggsave("outputs/cruce_ideologia.png", g2, width = 8.5, height = 6.5, dpi = 300, bg = "white")


# =====================================================================
# NOTAS
#
# · Ponderación. Los % usan FE (representan población); las n son brutas
#   (indican en cuántas entrevistas se apoya la cifra). Con n < 30 los
#   porcentajes son inestables: por eso min_n = 30 por defecto.
#
# · Prueba estadística. Se usa chi-cuadrada 2x2 sobre "de acuerdo vs.
#   resto", no la tabla completa de 4 categorías (ver comentario en
#   prueba_2x2). Va sin ponderar: indica asociación, no incorpora el
#   diseño muestral. Para eso:
#     library(survey)
#     dis <- svydesign(ids = ~1, weights = ~FE, data = df)
#     svychisq(~q_ideologia + q_medios_verifican, dis)
#
# · Multiplicidad. El barrido de la sección 2 corre 144 pruebas: por azar
#   se esperan ~7 significativas al 5%. Úsalo para EXPLORAR y luego
#   confirma los casos elegidos; o ajusta con p.adjust(p, "BH").
#
# · Causalidad. Todo es transversal: documenta asociación, no dirección.
#   Que la emocionalidad digital y el sesgo informativo vayan juntos no
#   establece cuál antecede a cuál.
#
# · Un camino que no funcionó: un índice aditivo de desafección (0–3)
#   con políticos_preocupan + partidos_beneficio + "votar no sirve" NO
#   guarda relación monotónica con el sesgo de confirmación
#   (30.9% → 47.5% → 39.0% → 49.8%). La desafección genérica no predice
#   linealmente el sesgo; la emocionalidad y la ideología discriminan mejor.
# =====================================================================
