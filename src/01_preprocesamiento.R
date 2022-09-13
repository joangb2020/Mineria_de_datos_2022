###################################################################################################
# # 01_juntar_bases-R
# # R Versions: 4.0.2
# # 
# #
# # Script: Ricardo 
# #
# # Inputs: Saber_11__2019-2.csv
# #
# # Outputs: base depurada
# #
# #
# # File history:
# #   20201006: Creación
################################################################################
rm(list=ls(all=TRUE))

setwd("C:/Users/rduplat/Documents/maestria/redes-neuronales/preprocesamiento/Src")

inPath  <- file.path("..","Input")
srcPath <- file.path("..","Src")
outPath <- file.path("..", "Output")

################################################################################
# # Packages
################################################################################

library(readxl)
library(data.table)
library(purrr)
library(xlsx)
library(dplyr)
library(ggplot2)

################################################################################
# # Functions
################################################################################

################################################################################
# # lectura de datos
################################################################################

baseOri <- fread(file.path(inPath, "Saber_11__2019-2.csv"),
  encoding="UTF-8")

# unique(apply(baseOri[ ,  .(ESTU_FECHANACIMIENTO )], 1, nchar))
# 22

baseOri[ , ESTU_FECHANACIMIENTO := strptime( substr(ESTU_FECHANACIMIENTO,1, 19), format="%m/%d/%Y %H:%M:%S")  ]

#> dim(baseOri)
#[1] 546212     82


borrar <- c("PERIODO", "ESTU_COD_RESIDE_DEPTO", 
  "ESTU_COD_RESIDE_MCPIO", "COLE_NOMBRE_ESTABLECIMIENTO",
  "COLE_COD_DANE_ESTABLECIMIENTO", "COLE_NOMBRE_ESTABLECIMIENTO",
  "COLE_COD_MCPIO_UBICACION", "COLE_COD_DEPTO_UBICACION",
  "COLE_NOMBRE_SEDE",
  "ESTU_COD_MCPIO_PRESENTACION", "ESTU_MCPIO_PRESENTACION",
  "ESTU_DEPTO_PRESENTACION", "ESTU_COD_DEPTO_PRESENTACION")

conservar<- names(baseOri)[!names(baseOri) %in% borrar]

baseNew <- copy(baseOri)

baseNew <-baseNew[, ..conservar]

#> dim(baseNew)
#[1] 546212     70
fwrite(baseNew, file= file.path(inPath, "Saber_11__2019-2_NEW.csv"))
#variables de estudiante

grep("ESTU",names(baseNew), value=T)

 [1] "ESTU_TIPODOCUMENTO"          
 [2] "ESTU_NACIONALIDAD"           
 [3] "ESTU_GENERO"                 
 [4] "ESTU_FECHANACIMIENTO"        
 [5] "ESTU_CONSECUTIVO"            
 [6] "ESTU_ESTUDIANTE"             
 [7] "ESTU_TIENEETNIA"             
 [8] "ESTU_PAIS_RESIDE"            
 [9] "ESTU_ETNIA"                  
[10] "ESTU_DEPTO_RESIDE"           
[11] "ESTU_MCPIO_RESIDE"           
[12] "ESTU_DEDICACIONLECTURADIARIA"
[13] "ESTU_DEDICACIONINTERNET"     
[14] "ESTU_HORASSEMANATRABAJA"     
[15] "ESTU_TIPOREMUNERACION"       
[16] "ESTU_PRIVADO_LIBERTAD"       
[17] "ESTU_INSE_INDIVIDUAL"        
[18] "ESTU_NSE_INDIVIDUAL"         
[19] "ESTU_NSE_ESTABLECIMIENTO"  # esta es de colegio
[20] "ESTU_ESTADOINVESTIGACION"    
[21] "ESTU_GENERACION-E" 

# en total son 21 variables de estudiante, 
# 4 de ellas son postaplicacion: "ESTU_ESTADOINVESTIGACION"
# "ESTU_GENERACION-E", "ESTU_NSE_INDIVIDUAL", "ESTU_INSE_INDIVIDUAL"
# una es de identificacion ESTU_CONSECUTIVO

#variables de colegio

grep("COLE",names(baseNew), value=T)

 [1] "COLE_CODIGO_ICFES"    "COLE_GENERO"         
 [3] "COLE_NATURALEZA"      "COLE_CALENDARIO"     
 [5] "COLE_BILINGUE"        "COLE_CARACTER"       
 [7] "COLE_COD_DANE_SEDE"   "COLE_SEDE_PRINCIPAL" 
 [9] "COLE_AREA_UBICACION"  "COLE_JORNADA"        
[11] "COLE_MCPIO_UBICACION" "COLE_DEPTO_UBICACION"
"ESTU_NSE_ESTABLECIMIENTO"
# 13 VARIABES, 2 son de identificacion
#COLE_CODIGO_ICFES, COLE_COD_DANE_SEDE

#variables de familia

grep("FAMI",names(baseNew), value=T)

 [1] "FAMI_ESTRATOVIVIENDA"         
 [2] "FAMI_PERSONASHOGAR"           
 [3] "FAMI_CUARTOSHOGAR"            
 [4] "FAMI_EDUCACIONPADRE"          
 [5] "FAMI_EDUCACIONMADRE"          
 [6] "FAMI_TRABAJOLABORPADRE"       
 [7] "FAMI_TRABAJOLABORMADRE"       
 [8] "FAMI_TIENEINTERNET"           
 [9] "FAMI_TIENESERVICIOTV"         
[10] "FAMI_TIENECOMPUTADOR"         
[11] "FAMI_TIENELAVADORA"           
[12] "FAMI_TIENEHORNOMICROOGAS"     
[13] "FAMI_TIENEAUTOMOVIL"          
[14] "FAMI_TIENEMOTOCICLETA"        
[15] "FAMI_TIENECONSOLAVIDEOJUEGOS" 
[16] "FAMI_NUMLIBROS"               
[17] "FAMI_COMELECHEDERIVADOS"      
[18] "FAMI_COMECARNEPESCADOHUEVO"   
[19] "FAMI_COMECEREALFRUTOSLEGUMBRE"
[20] "FAMI_SITUACIONECONOMICA"   

#VARIABLES DE PUNTAJE

> grep("(PUNT|DESE|PERC)",names(baseNew), value=T)
 [1] "PUNT_LECTURA_CRITICA"         
 [2] "PERCENTIL_LECTURA_CRITICA"    
 [3] "DESEMP_LECTURA_CRITICA"       
 [4] "PUNT_MATEMATICAS"             
 [5] "PERCENTIL_MATEMATICAS"        
 [6] "DESEMP_MATEMATICAS"           
 [7] "PUNT_C_NATURALES"             
 [8] "PERCENTIL_C_NATURALES"        
 [9] "DESEMP_C_NATURALES"           
[10] "PUNT_SOCIALES_CIUDADANAS"     
[11] "PERCENTIL_SOCIALES_CIUDADANAS"
[12] "DESEMP_SOCIALES_CIUDADANAS"   
[13] "PUNT_INGLES"                  
[14] "PERCENTIL_INGLES"             
[15] "DESEMP_INGLES"                
[16] "PUNT_GLOBAL"                  
[17] "PERCENTIL_GLOBAL"             



