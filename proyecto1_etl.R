##### Instalación y carga de paqueterías

pack_u<-c("rstudioapi","readxl","dplyr","knitr","kableExtra","ggplot2","tidyr")
pack_u_installed<-pack_u %in% installed.packages()
pack_u_uninstalled<-pack_u[pack_u_installed==FALSE]
install.packages(pack_u_uninstalled)
lapply(pack_u, require, character.only = TRUE)

##### Establecer directorio de trabajo y variables de ETL

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dir.archivo<-paste(getwd(),"/","refugios_nayarit.xlsx",sep="")
row.skip<-6
col.names<-c("no","refugio","municipio","direccion","uso_inmueble","servicios","capacidad_personas","latitud","longitud","altitud","responsable","telefono")

##### Extracción de la información a partir del libro de Excel             

sheets<-excel_sheets(dir.archivo)
for(i in 1:length(sheets))
{
  base<-read_excel(dir.archivo,sheet=sheets[i],skip=row.skip,col_names = FALSE)
  if(i==1){df<-base} else{df<-rbind(df,base)}
}
names(df)<-col.names

##### Eliminación de registros inválidos

df<-df %>% filter(rowSums(is.na(df))!=ncol(df)-1)

##### Coordenadas geográficas

df<-df %>% 
  
  filter(!(is.na(latitud) | is.na(longitud))) %>%
  
  separate(latitud,into=c("A","B","C","D"),remove=FALSE,extra="drop",fill="right") %>%
  mutate(D=ifelse(D=="",NA,D)) %>%
  mutate(latitud_val=as.numeric(A)+as.numeric(B)/60+(as.numeric(C)+as.numeric(paste("0",D,sep=".")))/3600) %>%
  select(-A,-B,-C,-D) %>%
  
  separate(longitud,into=c("A","B","C","D"),remove=FALSE,extra="drop",fill="right") %>%
  mutate(D=ifelse(D=="",NA,D)) %>%
  mutate(longitud_val=as.numeric(A)+as.numeric(B)/60+(as.numeric(C)+as.numeric(paste("0",D,sep=".")))/3600) %>%
  select(-A,-B,-C,-D) %>%
  
  mutate(latitud1=latitud_val,
         latitud_val=ifelse(no %in% c(434),longitud_val,latitud_val),
         longitud_val=ifelse(no %in% c(434),latitud1,longitud_val)) %>%
  select(-latitud1) %>%
  
  filter(!(is.na(latitud_val) | is.na(longitud_val)))

##### Uso del inmueble

df<-df %>% mutate(uso_inmueble=ifelse(uso_inmueble %in% c("RELIGIOSO","RELIGIOSOS"),"RELIGIOSO",uso_inmueble))
df<-df %>% mutate(uso_inmueble=ifelse(uso_inmueble %in% c("GOBIERNO MUNICIPAL","MUNICIPAL"),"MUNICIPAL",uso_inmueble))

##### Teléfonos

df<-df %>%
  mutate(tel=gsub("-","",telefono)) %>%
  mutate(tel=gsub("\\*","",tel)) %>%
  separate(tel,into=c("A","B","C","D","E","FF","G","H","I","J"),fill="right") %>%
  mutate(A=as.numeric(A),B=as.numeric(B),C=as.numeric(C),D=as.numeric(D),E=as.numeric(E)
         ,FF=as.numeric(FF),G=as.numeric(G),H=as.numeric(H),I=as.numeric(I),J=as.numeric(J)   ) %>%
  mutate(telefonos=paste(A,B,C,D,E,FF,G,H,I,J,sep=" // ") ) %>%
  mutate(telefonos=gsub("NA // ","",telefonos)) %>%
  mutate(telefonos=gsub(" // NA","",telefonos)) %>%
  mutate(telefonos=gsub("NA","",telefonos)) %>%
  select(-A,-B,-C,-D,-E,-FF,-G,-H,-I,-J)

##### Imputación de NA's

df$no[is.na(df$no)]<-0
df$refugio[is.na(df$refugio)]<-""
df$municipio[is.na(df$municipio)]<-""
df$direccion[is.na(df$direccion)]<-""
df$uso_inmueble[is.na(df$uso_inmueble)]<-""
df$servicios[is.na(df$servicios)]<-""
df$capacidad_personas[is.na(df$capacidad_personas)]<-0
df$latitud[is.na(df$latitud)]<-""
df$longitud[is.na(df$longitud)]<-""
df$altitud[is.na(df$altitud)]<-0
df$responsable[is.na(df$responsable)]<-""
df$telefono[is.na(df$telefono)]<-""
df$latitud_val[is.na(df$latitud_val)]<-0
df$longitud_val[is.na(df$longitud_val)]<-0
df$telefonos[is.na(df$telefonos)]<-""








 
  








