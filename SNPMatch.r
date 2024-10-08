# Programa para fusionar los ficheros .snp y summary statistics 
# despues de un PRS para obtener un fichero con los LeadSNP con su rsID

# Sintaxis: Rscript SNPMatch.R --snp archivo.snp --summary archivo_summary_statistics_con_rsID_y_posiciones_hg19.txt --out salida

args <- commandArgs(trailingOnly = TRUE)

fichero_snp <- NULL
fichero_summarystats <- NULL
out <- NULL

for (i in 1:length(args)){
  if(args[i] == "--snp" && !is.null(args[i+1])){
    fichero_snp <- args[i+1]
  }else if(args[i] == "--out" && !is.null(args[i+1])){
    out <- args[i+1]
  }else if(args[i] == "--summary" && !is.null(args[i+1])){
    fichero_summarystats <- args[i+1]
  }
}


if(is.null(fichero_snp)){
  stop("! Advertencia: No se ha proporcionado el archivo .snp")
}
if(is.null(out)){
  stop("! Advertencia: No se ha proporcionado nombre para la salida")
}
if(is.null(fichero_summarystats)){
  stop("! Advertencia: No se ha proporcionado el archivo summary statistics")
}



fusionar <- function(fichero_snp, fichero_summarystats, out){
  datos_snp <- read.table(fichero_snp, header = TRUE, sep = "\t", stringsAsFactors = FALSE, colClasses = "character")[, "SNP", drop = FALSE]
  datos_summarystats <- read.table(fichero_summarystats, header = TRUE, sep = "\t", stringsAsFactors = FALSE, colClasses = "character")
  
  if (!("SNP" %in% colnames(datos_snp))) {
    stop(paste("La columna 'SNP' no se encuentra en el archivo", fichero_snp))
  }
  if (!("SNP" %in% colnames(datos_summarystats))) {
    stop(paste("La columna 'SNP' no se encuentra en el archivo", fichero_summarystats))
  }
  
  fusion <- merge(datos_snp, datos_summarystats, by = "SNP") # Básicamente fusiona ambos ficheros usando la columna SNP (CHR:BP:A1:A2) como clave primaria, y despues...
  fusion_filtrada <- fusion[,c(10,2, 4)] # ... selecciona solo las columnas que requiere FUMA
  write.table(fusion_filtrada, file = paste0(out, ".txt"), sep = "\t", quote = FALSE, row.names = FALSE)
  
}

fusionar(fichero_snp, fichero_summarystats, out)



