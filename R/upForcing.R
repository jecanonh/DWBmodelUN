#' @name 
#' upForcing
#'
#' @title
#' Upload Forcings
#'
#' @description This function loads the precipitation and evapotranspiration estimates that will be used
#' to run or force the DWB model (\code{\link{DWBCalculator}}). If files are in raster format, it saves a variable 
#' cointaining the inputs in table format.
#'
#' @param path_p is a character string that specifies the directory where the precipitation rasters or
#' the csv file are stored. The csv file must have nrows = N° of cells and ncol= N° of time steps.
#' @param path_pet is a character string that specifies the location of the potential evapotranspiration rasters or
#' the csv file are stored. The csv file must have nrows= N° of cells and ncol= N° of time steps.
#' @param file_type Character string that specifies the forcing file formats, it should be "raster" or "csv",
#' the default value is "raster".
#' @param format Character string that specifies the format file of the Rasters, possible values are "GTiff"
#' and "NetCDF". Default value is "GTiff".
#' 
#' @details The character strings that control the location of the forcing files are as default "\emph{./precip/}"
#' and "\emph{./pet/}" for precipitation and potential evapotranspiration, but can be change to other directories.
#' However, if one's intention is to upload them from NetCDF files, the \bold{strings must be completely changed} to
#' a complete path that includes the name and extension of the file.
#'
#' @return a list containing the two objects (P and PET).
#' 
#' @author
#' Nicolas Duque Gardeazabal <nduqueg@unal.edu.co> \cr
#' Pedro Felipe Arboleda <pfarboledao@unal.edu.co> \cr
#' Carolina Vega Viviescas <cvegav@unal.edu.co> \cr
#' David Zamora <dazamoraa@unal.edu.co> \cr
#' 
#' Water Resources Engineering Research Group - GIREH
#' Universidad Nacional de Colombia - sede Bogota
#' 
#' @export
#' 
#' 
upForcing <- function(path_p = tempdir(), path_pet = tempdir(), file_type = "raster", format = "GTiff"){
  
  if (missing(path_pet) || is.null(path_pet) || !nzchar(path_pet)) {
    stop("Not filepath to read evapotranspiration data")
  }
  
  if (missing(path_p) || is.null(path_p) || !nzchar(path_p)) {
    stop("Not filepath to read precipitation data")
  }
  
  if (!file_type %in% c("raster", "csv")) {
    stop("file_type must be 'raster' or 'csv'")
  }
  
  if (file_type == "raster") {
    
    if (!requireNamespace("terra", quietly = TRUE)) {
      stop("The 'terra' package is required. Install it with install.packages('terra').")
    }
    
    format <- toupper(format)
    
    raster_to_table <- function(x) {
      as.data.frame(x, xy = TRUE, na.rm = TRUE)
    }
    
    get_files <- function(path, pattern) {
      if (file.exists(path) && !dir.exists(path)) {
        return(path)
      }
      
      files <- list.files(path, pattern = pattern, full.names = TRUE)
      
      if (length(files) == 0) {
        return(character(0))
      }
      
      sort(files)
    }
    
    if (format == "GTIFF") {
      
      pet_files <- get_files(path_pet, "\\.tif$|\\.tiff$")
      p_files <- get_files(path_p, "\\.tif$|\\.tiff$")
      
      if (length(pet_files) == 0 || length(p_files) == 0) {
        stop("Not available data of precipitation or evapotranspiration")
      }
      
      pet <- terra::rast(pet_files)
      p <- terra::rast(p_files)
      
    } else if (format == "NCDF") {
      
      pet_files <- get_files(path_pet, "\\.nc$")
      p_files <- get_files(path_p, "\\.nc$")
      
      if (length(pet_files) == 0 || length(p_files) == 0) {
        stop("Not available data of precipitation or evapotranspiration")
      }
      
      pet <- terra::rast(pet_files)
      p <- terra::rast(p_files)
      
    } else {
      stop("format must be 'GTiff' or 'NCDF'")
    }
    
    p_v <- raster_to_table(p)
    pet_v <- raster_to_table(pet)
    
  } else {
    
    get_csv <- function(path) {
      if (file.exists(path) && !dir.exists(path)) {
        return(path)
      }
      
      files <- list.files(path, pattern = "\\.csv$", full.names = TRUE)
      
      if (length(files) == 0) {
        stop("Not available csv data")
      }
      
      sort(files)[1]
    }
    
    p_v <- read.csv(get_csv(path_p))
    pet_v <- read.csv(get_csv(path_pet))
  }
  
  meteo <- list(PET = pet_v, Prec = p_v)
  return(meteo)
}