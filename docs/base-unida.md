# Base unida — Ómnibus 1: Medios + Política

**Archivo:** `Omnibus1_medios_politica_unido.csv`
**Filas:** 1,163 entrevistas · **Columnas:** 54 · **Población representada:** 83,603,443 personas adultas

---

## Cómo se construyó

Las bases `Omnibus1_medios_expandido.csv` y `Omnibus1_politica_expandido.csv` corresponden a **las mismas 1,163 personas, en el mismo orden de filas**. La unión se hizo **por posición de fila**, no por la columna `id`.

Antes de unir se verificó fila a fila la coincidencia de las 29 columnas compartidas (edad, sexo, entidad, escolaridad, zona, FE y demás variables sociodemográficas): **cero conflictos**, es decir, en ninguna fila las dos bases traen valores distintos presentes para la misma variable. Donde una base tenía un dato y la otra lo tenía vacío, se conservó el dato disponible.

## La variable `id_2`

**`id_2` es el identificador único** de la base: formato `OM1-0001` … `OM1-1163`, uno por entrevista, sin repeticiones ni vacíos.

Fue necesario crearlo porque la columna original `id` **no identifica de forma única a cada persona**: el mismo número aparece en encuestados distintos. De los 981 valores de `id`, 813 aparecen una vez, 154 se repiten dos veces y 14 tres veces. Por ejemplo, el `id` 254 corresponde a tres personas diferentes:

| id | edad | sexo | entidad | escolaridad |
|---|---|---|---|---|
| 254 | 39 | Mujer | Chiapas | Licenciatura |
| 254 | 26 | Hombre | Chiapas | Secundaria |
| 254 | 70 | Mujer | Hidalgo | Primaria |

Probablemente el `id` proviene de la numeración del ómnibus completo (su rango va de 9 a 2,495, con muchos números ausentes) y no fue re-secuenciado al extraer cada módulo.

La columna original se conserva como **`id_original`**, por si necesitas rastrear el vínculo con las bases de origen, pero **no debe usarse para deduplicar ni como llave**: hacerlo eliminaría 182 entrevistas reales.

## Estructura de columnas

| Bloque | Columnas | Contenido |
|---|---|---|
| Identificadores | `id_2`, `id_original` | 2 |
| Sociodemográficas | `edad`, `sexo`, `entidad`, `escolaridad`, `zona`, `age_group`, `FE`, y demás variables del hogar | 28 |
| Módulo **Medios** | 12 variables `q_` | sesgos de confirmación y confianza en los medios |
| Módulo **Política** | 12 variables `q_` | desafección política |

No hay colisión de nombres entre los reactivos de ambos módulos, así que todas las variables `q_` conviven sin renombrarse.

### Reactivos por módulo

**Medios:** `q_no_seguir_noticias`, `q_medios_verifican`, `q_redes_confiables`, `q_emergencia_redes`, `q_molestan_periodistas`, `q_coincide_no_busca`, `q_confio_abiertos`, `q_titular_enemigos`, `q_titular_ineficaces`, `q_discutir_redes`, `q_imposible_distinguir`, `q_comparte_desenmascarar`

**Política:** `q_frase_voto1`, `q_frase_politicos`, `q_voto_insuficiente`, `q_otras_formas`, `q_politicos_preocupan`, `q_partidos_beneficio`, `q_acciones_cambio`, `q_redes_informarse`, `q_redes_emociones`, `q_ideologia`, `q_evita_hablar`, `q_gusto_hablar`

Ojo con las **escalas distintas** entre módulos: los reactivos de medios usan `No está de acuerdo / Ni de acuerdo ni en desacuerdo / Sí está de acuerdo`, mientras que los de política usan `De acuerdo / Ni de acuerdo ni en desacuerdo / En desacuerdo`. Al recodificar, verifica la dirección en cada caso.

## Uso

```r
df <- read.csv("Omnibus1_medios_politica_unido.csv", encoding = "UTF-8")

nrow(df)                  # 1163
length(unique(df$id_2))   # 1163  <- identificador válido
length(unique(df$id_original))  # 981  <- NO usar como llave

sum(df$FE)                # 83,603,443

# ejemplo de cruce entre módulos
round(prop.table(table(df$q_redes_emociones, df$q_comparte_desenmascarar), 1) * 100, 1)
```

Todos los análisis deben ponderarse con **`FE`** para representar a la población adulta; los conteos brutos (`n`) van sin ponderar.

---

*El archivo se guardó con codificación UTF-8 con BOM, para que Excel abra correctamente los acentos.*
