# =============================================================================
# dataprepR-package.R
# -----------------------------------------------------------------------------
# Documentación general del paquete (página de ayuda ?dataprepR).
# =============================================================================

#' dataprepR: herramientas básicas para el preprocesado de datasets
#'
#' @description Paquete desarrollado para la asignatura Software Matemático y
#' Estadístico. Incluye implementaciones propias (usando solo las funciones
#' básicas de R) de:
#' \itemize{
#'   \item Discretización por igual anchura y por igual frecuencia, de una
#'   variable (\code{\link{discretizeEW}}, \code{\link{discretizeEF}},
#'   \code{\link{discretize}}) o de un dataset completo
#'   (\code{\link{discretizeData}}).
#'   \item Métricas de las variables: varianza, AUC y entropía
#'   (\code{\link{computeMetrics}}, \code{\link{calculateAUC}},
#'   \code{\link{entropy}}).
#'   \item Normalización y estandarización (\code{\link{normalize}},
#'   \code{\link{standardize}}, \code{\link{normalizeData}},
#'   \code{\link{standardizeData}}).
#'   \item Filtrado de variables según sus métricas (\code{\link{filterData}}).
#'   \item Correlación e información mutua por pares
#'   (\code{\link{computeCorrelations}}, \code{\link{correlation}},
#'   \code{\link{mutualInformation}}).
#'   \item Visualización: curva ROC, AUC, matrices de correlación / información
#'   mutua y entropía normalizada (\code{\link{plotROC}},
#'   \code{\link{plotAUC}}, \code{\link{plotCorrelation}},
#'   \code{\link{plotEntropy}}).
#'   \item Gestión de datasets: una clase S3 para los datasets
#'   (\code{\link{createDataset}}, \code{\link{isDataset}}) y funciones de
#'   lectura y escritura (\code{\link{readCSV}}, \code{\link{readDataset}},
#'   \code{\link{writeDataset}}).
#' }
#' La viñeta del paquete muestra cómo usar todas las funciones:
#' \code{vignette("dataprepR")}.
#'
#' @author
#' Alazne Valencia Saez
#'
#' Maintainer: Alazne Valencia Saez <avalencia024@ikasle.ehu.eus>
#'
#' @name dataprepR-package
#' @aliases dataprepR
#'
#' @importFrom stats var sd
#' @importFrom graphics abline axis barplot image par plot text
#' @importFrom grDevices colorRampPalette
#' @importFrom utils head read.csv write.table
"_PACKAGE"
