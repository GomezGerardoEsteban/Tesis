#################################################################################################################################################
# Descripción: Ejecutando este código es posible descargar las bases necesarias para correr el script **baseEnergia.R** el cual es la base
#              para varios de los graficos que se generan en el documento.
#              La estructura para la descarga de las bases es la misma, primero se guerda en un objeto la url y el nombre de la base y 
#              posteriormente se ejecuta la función 'dowload.file()' que descarga la respectiva base en el directorio de trabajo.
#              Las bases descargadas utilizando este codigo son:
#                - OWiD
#                - IDH historico desde 1990 para varios paises y por componente de IDH
#                - WDI (World Development Indicators) del Banco Mundial
#                - La base de climate watch sobre emisiones es necesario descargarla directamente desde la pagina.
# Author: Gerardo Esteban Gomez-Santiago                                                                                                        #
# Fecha: 3 de Julio de 2024                                                                                                                     #
#################################################################################################################################################



rm(list = ls())

# El comando options(timeout = "1000") es para extender el tiempo de espera en la descarga de los datos desde API
# por definición viene con 60 segundos de espera 
# getOption("timeout") # ejecutando este comando puedes ver que viene con 60 segundos de espera

options(timeout = "1000")

# Descarga via API de la base de energía de OWiD --------------------------

url_owid <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
filename_owid <- basename(url_owid)

download.file(url = url_owid,
              destfile = filename_owid, # Puedes crear una carpeta en tu directorio que conserve estas bases con 'str_c("nombreCarpetaNueva/", filename_owid, sep = "")' en lugar de solamente 'filename_owid'
              mode = "wb")


# Descarga via API de la base de IDH --------------------------------------

url_idh <- "https://hdr.undp.org/sites/default/files/2021-22_HDR/HDR21-22_Composite_indices_complete_time_series.csv"
filename_idh <- basename(url_idh)
download.file(url = url_idh,
              destfile = filename_idh, # Puedes crear una carpeta en tu directorio que conserve estas bases con 'str_c("nombreCarpetaNueva/", filename_owid, sep = "")' en lugar de solamente 'filename_owid'
              mode = "wb")

# Descarga base total de World Development Indicators del banco mundial ---

# Lo que se descarga a partir del comando 'download.file(...)' es un archivo comprimido (.zip) que contiene
# todos los indicadores para todos los países y regiones.
# con 'unzip(filename_bm)' descomprimimos el archivo y ya es posible leerlo con el siguiente comando:
# readxl::read_excel(path = "WDIEXCEL.xlsx", sheet = 1) 

url_bm <- "https://databank.worldbank.org/data/download/WDI_EXCEL.zip"
filename_bm <- basename(url_bm)

download.file(url = url_bm,
              destfile = filename_bm, 
              mode = "wb")

unzip(filename_bm)

# indicadores de la base WDIEXCEL que se usan en la tesis.

indicators.code <- c("NV.IND.TOTL.ZS", # Industrial total
                     "NV.IND.MANF.ZS", # Industria manufacturera
                     "NY.GDP.PCAP.KD", # PIB per capita
                     "SP.URB.TOTL.IN.ZS", # % poblacion urban
                     "NY.GDP.MKTP.KD") # PIB


# La base de Climate Watch presenta algunas complicaciones adicionales para poder descargarla como
# las bases anteriores, yo descargue todos los datos desde el boton superior derecho (DOWNLOAD BULK DATA)
# al que se accede desde a siguiente url:
# "https://www.climatewatchdata.org/data-explorer/historical-emissions?historical-emissions-data-sources=climate-watch&historical-emissions-gases=all-ghg&historical-emissions-regions=All%20Selected&historical-emissions-sectors=total-including-lucf%2Ctotal-including-lucf&page=1"

