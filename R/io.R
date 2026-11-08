# =============================================================================
# io.R
# -----------------------------------------------------------------------------
# Gestión de datasets: lectura y escritura.
#   - readCSV:      lee un CSV asignando a cada columna el tipo adecuado.
#   - writeDataset: guarda un objeto 'dataset' en un formato propio de texto
#                   que conserva los tipos, los niveles de los factores, la
#                   variable clase y el nombre.
#   - readDataset:  lee los ficheros creados con writeDataset.
#
# Formato propio (extensión recomendada .dts): unas líneas de cabecera que
# empiezan por '#' y después los datos en CSV. La primera línea de la cabecera
# es obligatoria y el resto opcionales salvo #Types:
#
#   #Variables: 4
#   #Name: Ejemplo
#   #Class: grupo
#   #Types: numeric,integer,factor,character
#   #Levels: grupo=A|B
#   "altura","edad","grupo","nombre"
#   1.7,20,"A","Ane"
#   ...
# =============================================================================


#' Leer un fichero CSV detectando el tipo de cada columna
#'
#' @description Lee un fichero CSV y asigna a cada columna el tipo adecuado
#' según los valores que contiene: \code{factor} si tiene pocos valores
#' distintos, \code{integer} si todos los valores son números enteros,
#' \code{numeric} si son números reales y \code{character} en otro caso.
#'
#' @param file.path Ruta del fichero CSV.
#' @param max.categories Una columna se considera categórica (\code{factor}) si
#' tiene menos de \code{max.categories} valores distintos. Por defecto 5.
#' @param sep Carácter separador de columnas. Por defecto \code{","}.
#' @param dec Carácter separador de decimales. Por defecto \code{"."}.
#'
#' @details El fichero se lee con \code{read.csv} sin convertir los textos en
#' factores, y después se decide el tipo de cada columna en este orden:
#' \enumerate{
#'   \item Si tiene menos de \code{max.categories} valores distintos:
#'   \code{factor}.
#'   \item Si es numérica y todos sus valores son enteros: \code{integer}.
#'   \item Si es numérica: \code{numeric}.
#'   \item En otro caso: \code{character}.
#' }
#' Es una regla aproximada: una variable numérica con pocos valores distintos
#' se leerá como \code{factor}, y una categoría codificada con muchos números
#' distintos, como numérica. Si hace falta, el tipo se puede corregir después,
#' por ejemplo con \code{factor()}.
#'
#' @return Un \code{data.frame} con las columnas convertidas al tipo adecuado.
#'
#' @seealso \code{\link{readDataset}}, \code{\link{createDataset}}
#'
#' @examples
#' file <- tempfile(fileext = ".csv")
#' write.csv(mtcars, file, row.names = FALSE)
#' datos <- readCSV(file, max.categories = 5)
#' sapply(datos, class)
#' @export
readCSV <- function(file.path, max.categories = 5, sep = ",", dec = ".") {

  # Leemos el fichero sin convertir automáticamente los textos en factores
  data <- read.csv(file.path, sep = sep, dec = dec, stringsAsFactors = FALSE)

  # Decidimos el tipo de cada columna. Usamos data[] <- lapply(...) en lugar
  # de as.data.frame(lapply(...)) para que no se modifiquen los nombres de
  # las columnas.
  data[] <- lapply(data, FUN = detectType, max.categories = max.categories)

  return(data)
}


#' Guardar un dataset en un fichero con formato propio
#'
#' @description Guarda un objeto \code{dataset} (o un \code{data.frame}) en un
#' fichero de texto con un formato propio que, a diferencia de un CSV normal,
#' conserva el tipo de cada variable, los niveles de los factores, la variable
#' clase y el nombre del dataset.
#'
#' @param data Objeto de clase \code{dataset} o \code{data.frame}.
#' @param file.path Ruta del fichero (se recomienda la extensión \code{.dts}).
#'
#' @details El fichero tiene una cabecera con líneas que empiezan por
#' \code{#} y, a continuación, los datos en formato CSV:
#' \itemize{
#'   \item \code{#Variables: n} (obligatoria): número de variables.
#'   \item \code{#Name: nombre} (opcional): nombre del dataset.
#'   \item \code{#Class: variable} (opcional): variable clase.
#'   \item \code{#Types: tipo1,tipo2,...}: tipo de cada variable
#'   (\code{numeric}, \code{integer}, \code{factor}, \code{logical} o
#'   \code{character}).
#'   \item \code{#Levels: variable=nivel1|nivel2|...}: una línea por cada
#'   factor, con sus niveles en orden.
#' }
#' Los nombres de los niveles no deberían contener el carácter \code{|}. Los
#' nombres de las filas no se guardan. Como el resto del paquete, el formato no
#' admite valores \code{NA}: si los datos tienen alguno se da un error.
#'
#' @return De forma invisible, la ruta del fichero.
#'
#' @seealso \code{\link{readDataset}}
#'
#' @examples
#' datos <- mtcars
#' datos$am <- factor(datos$am, labels = c("automatico", "manual"))
#' ds <- createDataset(datos, class.name = "am", name = "Coches")
#' file <- tempfile(fileext = ".dts")
#' writeDataset(ds, file)
#' head(readLines(file))
#' @export
writeDataset <- function(data, file.path) {

  # Aceptamos también un data.frame (lo convertimos en dataset)
  if (!isDataset(data)) {
    data <- createDataset(data)
  }

  df <- data$data

  # Tipo de cada columna
  types <- sapply(df, FUN = function(column) {
    return(class(column)[1])
  })

  valid.types <- c("numeric", "integer", "factor", "logical", "character")
  if (!all(types %in% valid.types)) {
    stop("Solo se pueden guardar variables de tipo: ",
         paste(valid.types, collapse = ", "))
  }

  # Como en el resto del paquete, no se admiten valores NA. Así, al leer el
  # fichero, un texto "NA" nunca se confunde con un valor perdido.
  if (any(is.na(df))) {
    stop("Los datos contienen valores NA: no se pueden guardar en este formato")
  }

  # Abrimos la conexión. Con on.exit nos aseguramos de cerrarla siempre,
  # aunque se produzca un error a mitad de la escritura.
  con <- file(description = file.path, open = "w", encoding = "UTF-8")
  on.exit(close(con))

  # Cabecera
  writeLines(paste("#Variables: ", ncol(df), sep = ""), con = con)

  if (!is.null(data$name)) {
    writeLines(paste("#Name: ", data$name, sep = ""), con = con)
  }

  if (!is.null(data$class.name)) {
    writeLines(paste("#Class: ", data$class.name, sep = ""), con = con)
  }

  writeLines(paste("#Types: ", paste(types, collapse = ","), sep = ""),
             con = con)

  # Una línea con los niveles de cada factor, para no perder su orden ni los
  # niveles que no aparecen en los datos
  for (column in names(df)[types == "factor"]) {
    writeLines(paste("#Levels: ", column, "=",
                     paste(levels(df[[column]]), collapse = "|"), sep = ""),
               con = con)
  }

  # Datos en formato CSV (write.table acepta una conexión ya abierta).
  # qmethod = "double" escribe las comillas que haya dentro de un texto
  # duplicadas (""), que es como las entiende read.csv al leer el fichero.
  write.table(df, file = con, sep = ",", row.names = FALSE, col.names = TRUE,
              qmethod = "double")

  invisible(file.path)
}


#' Leer un dataset guardado con formato propio
#'
#' @description Lee un fichero creado con \code{\link{writeDataset}} y
#' devuelve un objeto de clase \code{dataset} con los mismos tipos de
#' variables, niveles de los factores, variable clase y nombre que el
#' original.
#'
#' @param file.path Ruta del fichero.
#'
#' @details Se comprueba que el fichero siga el formato descrito en
#' \code{\link{writeDataset}}: la primera línea debe ser \code{#Variables: n},
#' debe existir la línea \code{#Types:} y el número de columnas de los datos
#' debe coincidir con \code{n}.
#'
#' @return Un objeto de clase \code{dataset}.
#'
#' @seealso \code{\link{writeDataset}}, \code{\link{readCSV}}
#'
#' @examples
#' datos <- mtcars
#' datos$am <- factor(datos$am, labels = c("automatico", "manual"))
#' ds <- createDataset(datos, class.name = "am", name = "Coches")
#' file <- tempfile(fileext = ".dts")
#' writeDataset(ds, file)
#' ds2 <- readDataset(file)
#' ds2
#' levels(ds2$data$am)
#' @export
readDataset <- function(file.path) {

  con <- file(description = file.path, open = "r", encoding = "UTF-8")
  lines <- readLines(con = con)
  close(con)

  # Las líneas de cabecera son las primeras que empiezan por '#'
  is.header <- substr(lines, 1, 1) == "#"
  num.header.lines <- which(!is.header)[1] - 1

  if (is.na(num.header.lines) || num.header.lines == 0) {
    stop("El fichero no tiene el formato de writeDataset (falta la cabecera)")
  }

  header <- lines[1:num.header.lines]
  body <- lines[-(1:num.header.lines)]

  # La primera línea es obligatoria
  if (substr(header[1], 1, 12) != "#Variables: ") {
    stop("La primera linea tiene que tener el formato:\n",
         "#Variables: numero_de_variables")
  }
  num.variables <- as.numeric(gsub("#Variables: ", "", header[1]))

  # Información opcional de la cabecera (NULL si no aparece)
  name <- getHeaderValue(header, "#Name: ")
  class.name <- getHeaderValue(header, "#Class: ")

  types <- getHeaderValue(header, "#Types: ")
  if (is.null(types)) {
    stop("Falta la linea '#Types:' en la cabecera")
  }
  types <- unlist(strsplit(types, split = ","))

  # Niveles de los factores: lista con un elemento por factor. Cada línea es
  # del tipo "#Levels: variable=nivel1|nivel2|...", así que separamos primero
  # por "=" (nombre de la variable y niveles) y después los niveles por "|".
  levels.lines <- header[substr(header, 1, 9) == "#Levels: "]
  levels.lines <- gsub("#Levels: ", "", levels.lines)
  factor.levels <- list()

  for (line in levels.lines) {
    parts <- unlist(strsplit(line, split = "=", fixed = TRUE))
    factor.name <- parts[1]
    # Volvemos a unir el resto por si algún nivel contiene un "="
    levels.text <- paste(parts[-1], collapse = "=")
    factor.levels[[factor.name]] <- unlist(strsplit(levels.text, split = "|",
                                                    fixed = TRUE))
  }

  # Leemos los datos. read.csv puede leer de un vector de líneas (argumento
  # text) en lugar de un fichero. Con colClasses = "character" todas las
  # columnas se leen como texto, y con check.names = FALSE no se cambian los
  # nombres de las columnas. Por defecto read.csv convierte el texto "NA" en
  # un valor perdido; con na.strings = character(0) no convierte nada (el
  # fichero no tiene valores NA, porque writeDataset no los admite). Después
  # convertimos cada columna a su tipo.
  data <- read.csv(text = body, colClasses = "character", check.names = FALSE,
                   na.strings = character(0))

  if (ncol(data) != num.variables || length(types) != num.variables) {
    stop("El numero de variables no coincide con el indicado en la cabecera")
  }

  for (i in 1:num.variables) {
    data[[i]] <- convertType(data[[i]], type = types[i],
                             levels = factor.levels[[names(data)[i]]])
  }

  return(createDataset(data, class.name = class.name, name = name))
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: detectType
# -----------------------------------------------------------------------------
# Decide el tipo de una columna leída de un CSV (ver readCSV).
#
# Entradas:
#   x               Vector con los valores de la columna.
#   max.categories  Si hay menos valores distintos que este número, la
#                   columna se considera categórica.
#
# Salida:
#   El vector convertido a factor, integer, numeric o character.
# -----------------------------------------------------------------------------

detectType <- function(x, max.categories) {

  # Número de valores diferentes de la variable
  num.values <- length(unique(x))

  # Si tiene pocos valores posibles, la consideramos categórica
  if (num.values < max.categories) {
    return(factor(x))
  }

  # Si es numérica, miramos si todos sus valores son enteros
  if (is.numeric(x)) {
    if (all(x == round(x), na.rm = TRUE)) {
      return(as.integer(x))
    } else {
      return(as.numeric(x))
    }
  }

  # Si no es numérica ni categórica, la dejamos como texto
  return(as.character(x))
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: convertType
# -----------------------------------------------------------------------------
# Convierte un vector de texto al tipo indicado (ver readDataset).
# -----------------------------------------------------------------------------

convertType <- function(x, type, levels = NULL) {

  result <- switch(type,
    numeric = as.numeric(x),
    integer = as.integer(x),
    logical = as.logical(x),
    character = x,
    factor = factor(x, levels = levels),
    stop("Tipo de variable no reconocido: ", type)
  )

  return(result)
}


# -----------------------------------------------------------------------------
# FUNCIÓN INTERNA: getHeaderValue
# -----------------------------------------------------------------------------
# Busca en las líneas de cabecera la que empieza por 'prefix' y devuelve el
# resto de la línea, o NULL si no existe.
# -----------------------------------------------------------------------------

getHeaderValue <- function(header, prefix) {

  line <- header[substr(header, 1, nchar(prefix)) == prefix]

  if (length(line) == 0) {
    return(NULL)
  }

  return(gsub(prefix, "", line[1], fixed = TRUE))
}
