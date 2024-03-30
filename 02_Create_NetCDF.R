
# *************************************************************************
# CREATION OF A NETCDF FILE OF CHIRPS PRECIPITATION DATA
# @autor: Kevin Traverso
# *************************************************************************

# Install and load libraries

if(!require(raster)) install.packages("raster")
if(!require(ncdf4)) install.packages("ncdf4")
if(!require(rgdal)) install.packages("rgdal")
if(!require(sp)) install.packages("sp")
if(!require(tictoc)) install.packages("tictoc")
if(!require(abind)) install.packages("abind")

# Funcion de construccion de archivo NC para PISCO OPERATIVO DE PRECIPITACIONES

Create_CHIRPS_nc <- function(Path, Date_ini, Name_nc){
  
  
  # Path = Tif file addres
  # Date_ini = Data initial date "1981-01-02"
  # Name_nc = Name nc file
  
  setwd(Path)
  getwd()
  
  filenames <- list.files("./", pattern = ".tif")
  length(filenames)
  
  ras  <- raster(paste0(getwd(),"/",filenames[1]))
  mtx  <- as.matrix(ras)
  time <- 1:length(filenames)
  pp   <- array(NA, dim=c(nrow(mtx), ncol(mtx), length(time)))
  
  tic()
  for (i in 1:length(time)){                           
    pp[,,i] <- as.matrix(raster(filenames[i]))
    cat('\f')
    message("Reading TIF Data'")
    message(paste0("Building NC .............................. ",
                   round(100*i/length(time),1),'%'))
    # message("Please wait...")
  }
  toc()
  

  Name  <- Name_nc
  
  # Generar variables
  Data <- aperm(pp, c(2,1,3))
  ext  <- extent(ras)
  lon  <- seq(ext[1], ext[2] - 0.01 , 0.05)
  lat  <- rev(seq(ext[3], ext[4] - 0.01, 0.05))
  VarOut  <- 'pr'
  NameVar <- "Daily precipitation"
  Units   <- 'mm'
  
  # Definir dimensiones
  londim  <- ncdim_def("lon","degrees_east", as.double(lon))
  latdim  <- ncdim_def("lat","degrees_north", as.double(lat))
  timedim <- ncdim_def("time", paste0('days since ', Date_ini), as.double(time))
  
  # Definir variables
  fillvalue <- NA
  VarDef    <- ncvar_def(VarOut,
                         Units,
                         list(londim, latdim, timedim), 
                         fillvalue, 
                         NameVar, 
                         prec="single")
  
  # Crear archivo netCDF
  ncout <- nc_create(Name,
                     list(VarDef), 
                     force_v4=TRUE)
  
  # Asignar variables
  ncvar_put(ncout, VarDef, Data)
  
  # Adicionar atributos
  ncatt_put(ncout,"lon","axis","X")
  ncatt_put(ncout,"lat","axis","Y")
  ncatt_put(ncout,"time","axis","T")
  
  # Cerrar y guardar archivo netCDF
  nc_close(ncout)
  message('Completo!')
  
}


Create_CHIRPS_nc(Path = "D:/Developer/Chirps_data_download/Test_data_CHIRPS/", 
                 Date_ini = "2000-01-01",
                 Name_nc = "D:/Developer/Chirps_data_download/CHIRPS_Peru_2000.nc")
