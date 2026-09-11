# =====================================================================
# 01 · Construcción de la base unida (Medios + Política)
#
# Une los dos módulos del Ómnibus 1 y crea un identificador válido (id_2).
# Salida: data/Omnibus1_medios_politica_unido.csv
#
#   install.packages("dplyr")
# =====================================================================

library(dplyr)

medios   <- read.csv("data/Omnibus1_medios_expandido.csv",   encoding = "UTF-8")
politica <- read.csv("data/Omnibus1_politica_expandido.csv", encoding = "UTF-8")


# ---------------------------------------------------------------------
# 1. Verificación: ¿son las mismas personas, en el mismo orden?
# ---------------------------------------------------------------------
# Las dos bases contienen las MISMAS 1,163 entrevistas, fila por fila.
# La unión es POSICIONAL. No se debe usar 'id' como llave: esa columna
# se repite entre personas distintas (ver docs/base-unida.md).

stopifnot(nrow(medios) == nrow(politica))

compartidas <- intersect(names(medios), names(politica))

# Un "conflicto" es una fila donde ambas bases traen un valor presente
# y distinto para la misma variable. Los NA no cuentan como conflicto.
conflictos <- sapply(compartidas, function(v){
  a <- medios[[v]]; b <- politica[[v]]
  sum(!is.na(a) & !is.na(b) & as.character(a) != as.character(b))
})

if (any(conflictos > 0)) {
  print(conflictos[conflictos > 0])
  stop("Las bases no describen a las mismas personas: hay conflictos.")
}
message("Verificación OK: 0 conflictos en ", length(compartidas), " columnas compartidas.")


# ---------------------------------------------------------------------
# 2. Unión
# ---------------------------------------------------------------------
df <- medios

# donde una base tiene dato y la otra lo tiene vacío, conserva el dato
for (v in compartidas) {
  falta <- is.na(df[[v]])
  df[[v]][falta] <- politica[[v]][falta]
}

# reactivos del módulo de política (no hay colisión de nombres)
q_politica <- grep("^q_", names(politica), value = TRUE)
df[q_politica] <- politica[q_politica]


# ---------------------------------------------------------------------
# 3. Identificador único: id_2
# ---------------------------------------------------------------------
df$id_original <- df$id
df$id <- NULL
df$id_2 <- sprintf("OM1-%04d", seq_len(nrow(df)))

stopifnot(!any(duplicated(df$id_2)), !any(is.na(df$id_2)))

# ordena columnas: identificadores, sociodemográficas, medios, política
q_medios <- grep("^q_", names(medios), value = TRUE)
socio    <- setdiff(compartidas, c("id", q_medios))
df <- df[, c("id_2", "id_original", socio, q_medios, q_politica)]


# ---------------------------------------------------------------------
# 4. Comprobaciones finales y guardado
# ---------------------------------------------------------------------
cat("\nFilas:              ", nrow(df), "\n")
cat("Columnas:           ", ncol(df), "\n")
cat("id_2 únicos:        ", length(unique(df$id_2)), "\n")
cat("id_original únicos: ", length(unique(df$id_original)), " <- NO usar como llave\n")
cat("Población (suma FE):", format(sum(df$FE), big.mark = ","), "\n")

write.csv(df, "data/Omnibus1_medios_politica_unido.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
