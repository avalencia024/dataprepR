# =============================================================================
# metrics.R
# -----------------------------------------------------------------------------
# Métricas de las variables de un dataset:
#   - entropy:        entropía de una variable categórica (factor).
#   - calculateAUC:   curva ROC y área bajo la curva (AUC) de una variable
#                     numérica respecto a una clase binaria.
#   - computeMetrics: varianza y AUC de las variables numéricas y entropía de
#                     las categóricas de un dataset, según el tipo de cada una.
# =============================================================================


#' Entropía de una variable categórica
#'
#' @description Calcula la entropía (en bits) de una variable discreta de tipo
#' factor.
#'
#' @param x Factor con los valores de la variable.
#'
#' @details Primero se estima la probabilidad de cada posible valor (cada nivel
#' del factor, aparezca o no en los datos) como el número de apariciones
#' dividido entre el número total de valores. Después se aplica
#' \deqn{H(X) = \sum_{i=1}^{n} -p_i \log_2(p_i)}{H(X) = sum(-p_i * log2(p_i))}
#' Los valores con probabilidad 0 no contribuyen a la suma.
#'
#' Por ejemplo, con los posibles valores a, b y c y el vector (a, a, c, c, c),
#' las probabilidades son 0.4, 0 y 0.6 y la entropía es
#' \code{-0.4 * log2(0.4) - 0.6 * log2(0.6) = 0.971}.
#'
#' @return Valor numérico con la entropía. Es 0 si la variable siempre toma el
#' mismo valor y como máximo \code{log2(k)}, siendo \code{k} el número de
#' niveles.
#'
#' @seealso \code{\link{computeMetrics}}, \code{\link{plotEntropy}}
#'
#' @examples
#' x <- factor(c("a", "a", "c", "c", "c"), levels = c("a", "b", "c"))
#' entropy(x)
#' @export
entropy <- function(x) {

  if (!is.factor(x)) {
    stop("'x' debe ser un factor")
  }

  if (any(is.na(x))) {
    stop("'x' contiene valores NA")
  }

  # Probabilidad estimada de cada posible valor (cada nivel del factor),
  # aparezca o no en los datos.
  probabilities <- sapply(levels(x), FUN = function(level) {
    return(sum(x == level) / length(x))
  })

  # Las probabilidades 0 no contribuyen (además log2(0) = -Inf daría NaN)
  probabilities <- probabilities[probabilities > 0]

  entropy.value <- -sum(probabilities * log2(probabilities))

  return(entropy.value)
}


#' Curva ROC y AUC de una variable numérica
#'
#' @description Calcula los puntos de la curva ROC y el área bajo la curva
#' (AUC) de una variable numérica respecto a una variable clase binaria. Sirve
#' para medir lo bien que la variable, por sí sola, separa las dos clases.
#'
#' @param data \code{data.frame} (o matriz) de dos columnas. La primera es la
#' variable numérica y la segunda la clase: un vector lógico (\code{TRUE} /
#' \code{FALSE}) o una variable con exactamente dos valores (en ese caso se
#' considera positivo el segundo nivel del factor, de los que aparecen en los
#' datos, o el mayor de los dos valores, por ejemplo el 1 en una variable 0/1).
#'
#' @details Se ordenan las filas de mayor a menor según la variable. Para cada
#' punto de corte, las filas con valor mayor o igual que el corte se predicen
#' como positivas y el resto como negativas. Comparando con la clase real se
#' obtienen los verdaderos positivos (TP), falsos positivos (FP), verdaderos
#' negativos (TN) y falsos negativos (FN), y con ellos
#' \code{TPR = TP / (TP + FN)} y \code{FPR = FP / (FP + TN)}.
#'
#' Aunque hay infinitos puntos de corte posibles, solo cambian el TPR y el FPR
#' los que coinciden con los valores de los datos, así que solo se usan esos.
#' La curva empieza en (0, 0) y el área se calcula con la regla del trapecio.
#'
#' Un AUC de 1 indica que, al ordenar los ejemplos por la variable, las dos
#' clases quedan perfectamente separadas; un AUC menor que 0.5, que los valores
#' bajos de la variable se asocian a la clase positiva; y un AUC de 0.5, que
#' ordenar por la variable no separa las clases (como una predicción al azar).
#' Un AUC cercano a 0.5 no significa necesariamente que la variable no tenga
#' relación con la clase: el AUC solo mide la separación al ordenar los
#' valores, así que no detecta, por ejemplo, que los positivos estén en los dos
#' extremos de la variable.
#'
#' @return Una lista con dos elementos:
#' \item{roc.points}{\code{data.frame} con las columnas \code{FPR} y
#' \code{TPR}, un punto de la curva ROC por fila.}
#' \item{auc}{Área bajo la curva ROC.}
#'
#' @seealso \code{\link{plotROC}}, \code{\link{plotAUC}},
#' \code{\link{computeMetrics}}
#'
#' @examples
#' data <- data.frame(value = c(0.1, 0.4, 0.35, 0.8),
#'                    class = c(FALSE, FALSE, TRUE, TRUE))
#' result <- calculateAUC(data)
#' result$roc.points
#' result$auc
#' @export
calculateAUC <- function(data) {

  # Comprobamos primero que sea una tabla (con un vector, ncol() da NULL)
  if (!is.data.frame(data) && !is.matrix(data)) {
    stop("'data' debe ser un data.frame (o una matriz) con dos columnas")
  }

  if (ncol(data) != 2) {
    stop("'data' debe tener dos columnas: la variable numerica y la clase")
  }

  # Ordenamos las filas de mayor a menor según la primera columna
  data <- data[order(data[, 1], decreasing = TRUE), ]

  # Separamos los valores de la variable y las clases
  values <- data[, 1]
  classes <- data[, 2]

  checkNumeric(values, "primera columna de data")

  # Como en el resto del paquete, la clase no puede tener valores NA ni, si es
  # numérica, infinitos. Lo comprobamos antes de pasarla a TRUE / FALSE.
  if (any(is.na(classes)) || (is.numeric(classes) && any(is.infinite(classes)))) {
    stop("La clase contiene valores NA o infinitos")
  }

  labels <- toBinaryClass(classes)

  if (all(labels) || !any(labels)) {
    stop("La clase debe tener ejemplos positivos y negativos para calcular el AUC")
  }

  # Solo los valores presentes en los datos cambian el TPR y el FPR, así que
  # son los únicos puntos de corte que necesitamos (ya ordenados de mayor a
  # menor, porque hemos ordenado las filas).
  cut.points <- unique(values)

  # Calculamos un punto de la curva ROC para cada punto de corte
  roc.points <- lapply(cut.points, FUN = function(cut.point) {

    # Predecimos como positivas las filas con valor >= punto de corte
    prediction <- values >= cut.point

    # Elementos de la matriz de confusión
    TP <- sum(prediction & labels)
    FP <- sum(prediction & !labels)
    TN <- sum(!prediction & !labels)
    FN <- sum(!prediction & labels)

    return(data.frame(FPR = FP / (FP + TN),
                      TPR = TP / (TP + FN)))
  })

  # Unimos todos los puntos en un data.frame (lapply + do.call).
  # La curva ROC empieza siempre en el punto (0, 0).
  roc.points <- rbind(data.frame(FPR = 0, TPR = 0),
                      do.call(rbind, roc.points))

  # Área bajo la curva sumando el área de los trapecios entre puntos
  # consecutivos (regla del trapecio)
  delta.x <- diff(roc.points$FPR)
  mean.y <- (roc.points$TPR[-1] + roc.points$TPR[-nrow(roc.points)]) / 2
  auc.value <- sum(delta.x * mean.y)

  result <- list(roc.points = roc.points, auc = auc.value)

  return(result)
}


#' Métricas de las variables de un dataset
#'
#' @description Calcula, para cada variable de un dataset, las métricas que le
#' corresponden según su tipo: varianza y AUC para las variables numéricas y
#' entropía para las categóricas. La función reconoce el tipo de cada variable
#' y actúa en consecuencia.
#'
#' @param data \code{data.frame}, matriz u objeto de clase \code{dataset}.
#' @param class.name Nombre de la variable clase (binaria) con la que se calcula
#' el AUC de las variables numéricas. Si es \code{NULL} (valor por defecto) no
#' se calcula el AUC. En un objeto \code{dataset} se usa por defecto su propia
#' variable clase.
#'
#' @details Se consideran numéricas las variables \code{numeric} e
#' \code{integer}, y categóricas las de tipo \code{factor} y \code{logical}.
#' Las variables de texto (\code{character}) no se consideran categóricas y no
#' se les calcula ninguna métrica. La variable clase no se incluye en el
#' resultado.
#'
#' El AUC necesita una clase binaria. Si la variable clase tiene más de dos
#' valores (por ejemplo \code{Species} en \code{iris}) se avisa con un
#' \code{warning}, el AUC queda como \code{NA} y el resto de métricas se
#' calculan igualmente.
#'
#' Como el resto del paquete, la función no admite valores \code{NA} ni
#' infinitos en las variables numéricas ni en la clase (da un error). Así, un
#' \code{NA} en el resultado indica que la métrica no se aplica a esa
#' variable, salvo en un caso: con una sola observación la varianza muestral
#' no está definida (necesita al menos dos valores) y también vale \code{NA}.
#'
#' El AUC se calcula con \code{\link{calculateAUC}} y la entropía con
#' \code{\link{entropy}}. La varianza es la varianza muestral (función
#' \code{var}).
#'
#' @return Un \code{data.frame} con una fila por variable (los nombres de fila
#' son los nombres de las variables) y las columnas:
#' \item{variable}{Nombre de la variable.}
#' \item{type}{Tipo: \code{"numeric"}, \code{"categorical"} u otro.}
#' \item{variance}{Varianza (solo variables numéricas).}
#' \item{auc}{AUC (solo variables numéricas y si hay variable clase).}
#' \item{entropy}{Entropía (solo variables categóricas).}
#' Las métricas que no se aplican a una variable valen \code{NA}. Si no hay
#' ninguna variable aparte de la clase, se devuelve una tabla sin filas.
#'
#' @seealso \code{\link{filterData}}, \code{\link{plotAUC}}
#'
#' @examples
#' datos <- mtcars
#' datos$cyl <- factor(datos$cyl)
#' computeMetrics(datos, class.name = "am")
#'
#' # Con un objeto dataset no hace falta indicar la clase
#' ds <- createDataset(datos, class.name = "am")
#' computeMetrics(ds)
#' @export
computeMetrics <- function(data, class.name = NULL) {
  UseMethod("computeMetrics")
}


#' @rdname computeMetrics
#' @export
computeMetrics.default <- function(data, class.name = NULL) {

  data <- toDataFrame(data)

  # El AUC solo se puede calcular si hay una variable clase binaria. Si la
  # clase tiene más de dos valores no se calcula el AUC, pero sí el resto de
  # métricas.
  compute.auc <- FALSE

  if (!is.null(class.name)) {
    if (!(class.name %in% names(data))) {
      stop("'class.name' debe ser el nombre de una de las columnas de 'data'")
    }

    if (isBinaryClass(data[[class.name]])) {
      compute.auc <- TRUE
    } else {
      warning("La variable clase '", class.name, "' no es binaria: ",
              "no se calcula el AUC")
    }
  }

  # Variables a analizar: todas menos la clase
  variables <- setdiff(names(data), class.name)

  # Si no queda ninguna variable (por ejemplo, después de un filtrado que solo
  # ha dejado la clase), devolvemos una tabla vacía con las mismas columnas
  if (length(variables) == 0) {
    return(data.frame(variable = character(0), type = character(0),
                      variance = numeric(0), auc = numeric(0),
                      entropy = numeric(0)))
  }

  # Para cada variable creamos una fila de resultados según su tipo
  metrics <- lapply(variables, FUN = function(variable) {

    x <- data[[variable]]

    # Por defecto las métricas no se aplican (NA)
    variance.value <- NA
    auc.value <- NA
    entropy.value <- NA

    if (is.numeric(x)) {

      type <- "numeric"

      # Como en el resto del paquete, no se admiten valores NA ni infinitos
      checkNumeric(x, variable)
      variance.value <- var(x)

      if (compute.auc) {
        # calculateAUC comprueba la clase y la pasa a TRUE / FALSE
        auc.value <- calculateAUC(data.frame(x, data[[class.name]]))$auc
      }

    } else if (isCategorical(x)) {

      type <- "categorical"

      # Las variables lógicas se tratan como un factor de dos niveles
      if (is.logical(x)) {
        x <- factor(x)
      }
      entropy.value <- entropy(x)

    } else {
      # Otros tipos (por ejemplo character): no se calcula nada
      type <- class(x)[1]
    }

    return(data.frame(variable = variable,
                      type = type,
                      variance = variance.value,
                      auc = auc.value,
                      entropy = entropy.value))
  })

  # Juntamos todas las filas en un único data.frame
  metrics <- do.call(rbind, metrics)
  rownames(metrics) <- metrics$variable

  return(metrics)
}


#' @rdname computeMetrics
#' @export
computeMetrics.dataset <- function(data, class.name = data$class.name) {
  return(computeMetrics.default(data$data, class.name = class.name))
}
