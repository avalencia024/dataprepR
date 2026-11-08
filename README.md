# dataprepR

Paquete de R con herramientas básicas para el preprocesado de datasets,
desarrollado para la asignatura *Software Matemático y Estadístico*. Los
algoritmos (discretización, entropía, AUC, correlación e información mutua)
están implementados a mano con las funciones básicas de R y el paquete no
depende de ningún paquete externo.

## Instalación

Desde el fichero `.tar.gz`:

```r
install.packages("dataprepR_1.0.1.tar.gz", repos = NULL, type = "source")
```

## Contenido

| Funcionalidad | Funciones |
|:--------------|:----------|
| Discretización | `discretize`, `discretizeEW`, `discretizeEF`, `discretizeData` |
| Métricas | `entropy`, `calculateAUC`, `computeMetrics` |
| Normalización y estandarización | `normalize`, `standardize`, `normalizeData`, `standardizeData` |
| Filtrado de variables | `filterData` |
| Correlación e información mutua | `correlation`, `mutualInformation`, `computeCorrelations` |
| Visualización | `plotROC`, `plotAUC`, `plotCorrelation`, `plotEntropy` |
| Gestión de datasets | `createDataset`, `isDataset`, `print`, `summary`, `readCSV`, `writeDataset`, `readDataset` |

## Ejemplo rápido

```r
library(dataprepR)

ds <- createDataset(mtcars, class.name = "am", name = "Coches")
computeMetrics(ds)                                  # varianza, AUC y entropía
ds.filtrado <- filterData(ds, metric = "auc", threshold = 0.8)
plotCorrelation(ds)                                 # matriz de correlaciones
```

La viñeta del paquete explica con detalle el uso de todas las funciones y las
decisiones de diseño tomadas:

```r
vignette("dataprepR")
```

## Estructura del paquete

```
dataprepR/
├── DESCRIPTION, NAMESPACE
├── R/            código (organizado por funcionalidades)
├── man/          páginas de ayuda (generadas con roxygen2)
├── tests/        pruebas que se ejecutan con R CMD check
└── vignettes/    viñeta en R Markdown
```
