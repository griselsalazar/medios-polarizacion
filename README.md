# Ómnibus 1 — Medios y Política

Análisis de dos módulos de la **Encuesta Ómnibus 1** del EQUIDE, Universidad Iberoamericana Ciudad de México:

- **Sesgos de confirmación y confianza en los medios**
- **Desafección política**

La encuesta es representativa a nivel nacional de la población adulta de México: **1,163 entrevistas**, que representan a **83.6 millones de personas** mediante el factor de expansión (`FE`).

---

## Contenido

```
├── data/
│   ├── Omnibus1_medios_expandido.csv          # módulo original: medios
│   ├── Omnibus1_politica_expandido.csv        # módulo original: política
│   └── Omnibus1_medios_politica_unido.csv     # base unida (generada por 01)
├── scripts/
│   ├── 01_unir_bases.R                        # une módulos y crea id_2
│   ├── 02_distribucion_medios.R               # frecuencias y gráficas
│   └── 03_cruces_medios_politica.R            # cruces entre módulos
├── docs/
│   ├── base-unida.md                          # documentación de la base unida
│   ├── reporte-descriptivo.md                 # resultados del módulo de medios
│   ├── Omnibus1_Codebook_Medios.docx          # codebook oficial
│   └── Omnibus1_Codebook_Politica.docx        # codebook oficial
└── outputs/                                   # gráficas y tablas generadas
```

## Cómo reproducir

Desde la raíz del repositorio, en R:

```r
install.packages(c("dplyr", "tidyr", "ggplot2", "forcats"))

source("scripts/01_unir_bases.R")             # reconstruye la base unida
source("scripts/02_distribucion_medios.R")    # distribuciones y gráficas
source("scripts/03_cruces_medios_politica.R") # cruces entre módulos
```

---

## Dos advertencias importantes

### 1. La columna `id` no es un identificador único

El mismo valor de `id` aparece en personas distintas: de los 981 valores, 154 se repiten dos veces y 14 tres veces. Por ejemplo, el `id` 254 corresponde a tres personas con edad, sexo, entidad y respuestas diferentes.

La base unida incluye **`id_2`** (`OM1-0001` … `OM1-1163`), que sí identifica de forma única cada entrevista. El valor original se conserva como `id_original`.

**Nunca deduplicar por `id`**: eliminaría 182 entrevistas reales. La unidad de análisis es la fila; la n correcta es **1,163**.

### 2. Las escalas difieren entre módulos

| Módulo | Categorías |
|---|---|
| Medios | `No está de acuerdo` / `Ni de acuerdo ni en desacuerdo` / `Sí está de acuerdo` |
| Política | `De acuerdo` / `Ni de acuerdo ni en desacuerdo` / `En desacuerdo` |

La dirección se invierte. Verificar al recodificar.

---

## Notas metodológicas

- **Ponderación.** Todos los porcentajes deben calcularse con `FE` para representar a la población adulta. Los conteos brutos (`n`) van sin ponderar e indican en cuántas entrevistas se apoya cada cifra.
- **Pruebas de asociación.** Los scripts usan chi-cuadrada **2×2** ("de acuerdo" vs. resto) en lugar de la tabla completa de cuatro categorías. Esta última detecta diferencias en las categorías intermedias y de no respuesta que no son sustantivas, y produce p-valores significativos incluso cuando el porcentaje de acuerdo es prácticamente idéntico entre grupos.
- **Grupos pequeños.** Los cruces descartan por defecto los grupos con menos de 30 casos. Algunas categorías de escolaridad quedan por debajo de ese umbral (carrera técnica, n = 45; doctorado, n = 17).
- **Alcance.** El análisis es descriptivo y transversal: documenta asociaciones, no relaciones causales. Los reactivos capturan preferencias declaradas, no conducta observada.

---

## Principales hallazgos

**Módulo de medios.** 64% cree que hoy es imposible distinguir noticias falsas de verdaderas; solo 46% confía en que los medios verifican antes de publicar; 55% prefiere periodistas que declaran su postura sobre los que buscan la neutralidad. El sesgo de confirmación disminuye de forma consistente conforme aumenta la escolaridad.

**Cruces entre módulos.** Quienes declaran que los contenidos políticos en redes les generan emociones intensas comparten más información para "desenmascarar" a los medios (65% vs. 51%) y se interesan más por encuadres polarizantes (56% vs. 39%). La identificación ideológica estructura la confianza mediática de forma asimétrica. Más de la mitad de la población no se identifica con ninguna ideología, y ese grupo es el menos polarizado, no el más.

Ver `docs/reporte-descriptivo.md` para el detalle.

---

## Cita sugerida

> Salazar Rebolledo, G. (2026). *Ómnibus 1 — Medios y Política: análisis de sesgos de confirmación, confianza mediática y desafección política en México.* Universidad Iberoamericana Ciudad de México.

## Licencia

Código: [MIT](LICENSE). Los datos y la documentación del EQUIDE se rigen por los términos de la Universidad Iberoamericana.
