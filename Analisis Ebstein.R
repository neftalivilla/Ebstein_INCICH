#Protocolo de Ebstein
#Analisis: Marzo 2022

setwd("/Users/nefoantonio/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/PROYECTOS/CARDIOLOGIA/Cardiologia Nuclear/Protocolo Ebstein")

#----Library managment-----

library(readxl)
library(tidyverse)
library(ggthemes)
library(ggalt)
library(ggpubr)
library(janitor)
library(epiR)
library(survminer)
library(timeROC)
library(rms)

#----Dataset Managment-----

#base <- read_excel("Base_Ebstein_FINAL.xlsx")
base <- read_excel("Base_Ebstein FINAL RepCor.xlsx", sheet = "Hoja2")
base <- janitor::clean_names(base)
base <- base%>%dplyr::filter(tf==0)

#----Multiple Imputation-----

base<-base%>%mutate(base, id = rownames(base))%>%mutate(id=as.numeric(id))
base1.1<-base%>%dplyr::select(id,celermajer,adosamiento_vt_septal,adosamiento_vt_posterior, 
                              porcion_atrializada_vd_percent,
                              porcion_atrializada_vd_mm,
                              vtd_vi,vtd_vi_index,          
                              vts_vi,vts_vi_index,masa_vi,
                              masa_vi_index,fevi_49,vtd_v_df_a,
                              vtd_v_df_a_index,vts_v_df_a,vts_v_df_a_index,fevd)
base1.1<-sapply(base1.1, as.numeric)
base1_imp<-mice::mice(base1.1, m=5, maxit=5,seed = 123)
base1_imp_2<-complete(base1_imp,1)

#mice::stripplot(base1_imp, pch = 20, cex = 1.2)#ver como quedan las imputaciones junto a cada una de las variables observadas
#mice::densityplot(base1_imp)
base <- base%>% dplyr::select(-c(celermajer,adosamiento_vt_septal,adosamiento_vt_posterior, 
                                 porcion_atrializada_vd_percent,
                                 porcion_atrializada_vd_mm,
                                 vtd_vi,vtd_vi_index,          
                                 vts_vi,vts_vi_index,masa_vi,
                                 masa_vi_index,fevi_49,vtd_v_df_a,
                                 vtd_v_df_a_index,vts_v_df_a,vts_v_df_a_index,fevd))
base <- base%>%left_join(base1_imp_2,by="id")

#----Recoding Variables-----

#Sexo
base$sexo[base$sexo=="M"]<-1
base$sexo[base$sexo=="F"]<-0
base$sexo<-factor(base$sexo)

#EDAD AL DIAGNOSTICO
base$TIEMPO_AL_DIAGNOSTICO<-round(as.numeric(as.Date(base$ingreso_inc,tryFormats = c("%Y-%m-%d"))-as.Date(base$fn,tryFormats = c("%Y-%m-%d")))/365,2)

#EDAD A LA ULTIMA CONSULTA
base$EDAD_A_UC<-round(as.numeric(as.Date(base$ultima_cita_inc,tryFormats = c("%Y-%m-%d"))-as.Date(base$fn,tryFormats = c("%Y-%m-%d")))/365,2)

#TIEMPO DE TRATAMIENTO
base$TIEMPO_TRATAMIENTO<-round(as.numeric(as.Date(base$ultima_cita_inc,tryFormats = c("%Y-%m-%d"))-as.Date(base$ingreso_inc,tryFormats = c("%Y-%m-%d")))/365,2)

#TIEMPO A RESONANCIA
base$TIEMPO_A_RESONANCIA<-round(as.numeric(as.Date(base$ano_resonancia_magnetica,tryFormats = c("%Y-%m-%d"))-as.Date(base$fn,tryFormats = c("%Y-%m-%d")))/365,2)

#Diferencia
base$TIEMPO_DIF_RESONANCIA<-abs(base$TIEMPO_A_RESONANCIA-base$TIEMPO_AL_DIAGNOSTICO)

#Tiempo UC a Resonancia
base$SEGUIMIENTO_UC_RM<-round(as.numeric(as.Date(base$ultima_cita_inc,tryFormats = c("%Y-%m-%d"))-as.Date(base$ano_resonancia_magnetica,tryFormats = c("%Y-%m-%d")))/365,2)


#Num Comorbilidades

base$NUM_COMORBILIDADES<-as.numeric(base$diabetes)+as.numeric(base$hipertension_arterial)+as.numeric(base$evc)+as.numeric(base$otra)+
  as.numeric(base$wolff_parkinson_white)+as.numeric(base$bloqueo_av)+as.numeric(base$fa)+as.numeric(base$otra_arritmia)+
  as.numeric(base$comunicacion_ia)+as.numeric(base$comunicacion_iv)+as.numeric(base$persistencia_del_ca)+as.numeric(base$foramen_oval_permeable)+as.numeric(base$otra_anomalia_cardiaca)

#Log Masa VI
base$LOG_MASA_VI<-log(base$masa_vi_index)

#Remplazar NA por Ceros

#Camaras
base$vd_pared_libre<-na.tools::na.replace(base$vd_pared_libre,0)
base$ai<-na.tools::na.replace(base$ai,0)
base$ad<-na.tools::na.replace(base$ad,0)
base$vi<-na.tools::na.replace(base$vi,0)

#Segmentos
base$basal_anterior<-na.tools::na.replace(base$basal_anterior,0)
base$basal_anterolateral<-na.tools::na.replace(base$basal_anterolateral,0)
base$basal_anteroseptal<-na.tools::na.replace(base$basal_anteroseptal,0)
base$basal_inferior<-na.tools::na.replace(base$basal_inferior,0)
base$basal_inferolateral<-na.tools::na.replace(base$basal_inferolateral,0)
base$basal_inferoseptal<-na.tools::na.replace(base$basal_inferoseptal,0)

base$medio_anterior<-na.tools::na.replace(base$medio_anterior,0)
base$medio_anterolateral<-na.tools::na.replace(base$medio_anterolateral,0)
base$medio_anteroseptal<-na.tools::na.replace(base$medio_anteroseptal,0)
base$medio_inferior<-na.tools::na.replace(base$medio_inferior,0)
base$medio_inferolateral<-na.tools::na.replace(base$medio_inferolateral,0)
base$medio_inferoseptal<-na.tools::na.replace(base$medio_inferoseptal,0)

base$apical_anterior<-na.tools::na.replace(base$apical_anterior,0)
base$apical_lateral<-na.tools::na.replace(base$apical_lateral,0)
base$apical_septal<-na.tools::na.replace(base$apical_septal,0)
base$apical_inferior<-na.tools::na.replace(base$apical_inferior,0)

base$apex<-na.tools::na.replace(base$apex,0)

#Segmentos
base$localizacion_intramiocardico<-na.tools::na.replace(base$localizacion_intramiocardico,0)
base$localizacion_subendocardica<-na.tools::na.replace(base$localizacion_subendocardica,0)
base$localizacion_subepicardico<-na.tools::na.replace(base$localizacion_subepicardico,0)
base$localizacion_transmural<-na.tools::na.replace(base$localizacion_transmural,0)

#Reforzamiento DIC

base$REFORZAMIENTO_DIC<-NULL
base$REFORZAMIENTO_DIC[base$zonas_de_reforzamiento_segmentos>=1]<-1
base$REFORZAMIENTO_DIC[base$zonas_de_reforzamiento_segmentos==0]<-0

table(base$vi)

#Reforzamiento CAT
base$REFORZAMIENTO_CAT<-NULL
base$REFORZAMIENTO_CAT[base$zonas_de_reforzamiento_segmentos==0]<-0
base$REFORZAMIENTO_CAT[base$zonas_de_reforzamiento_segmentos>=1 & base$zonas_de_reforzamiento_segmentos<=3]<-1
base$REFORZAMIENTO_CAT[base$zonas_de_reforzamiento_segmentos>=4]<-2
base$REFORZAMIENTO_CAT<-factor(base$REFORZAMIENTO_CAT,labels = c("0","1-3","≥4"))

#Reforzamiento Camaras
base$REFORZAMIENTO_DIC_CAMARA<-NULL
base$REFORZAMIENTO_DIC_CAMARA[base$zonas_de_reforzamiento_segmentos>=1]<-1
base$REFORZAMIENTO_DIC_CAMARA[base$zonas_de_reforzamiento_segmentos==0]<-0
base$REFORZAMIENTO_DIC_CAMARA<-factor(base$REFORZAMIENTO_DIC_CAMARA,labels = c("0","≥1"))

#VD
base$vd_pared_libre<-na.tools::na.replace(base$vd_pared_libre,0)

#VI_VD
base$VI_VD<-NULL
base$VI_VD[base$vd_pared_libre==1]<-1
base$VI_VD[base$vi==1]<-1
base$VI_VD<-na.tools::na.replace(base$VI_VD,0)

#Segmentos afectados 

base$SEPTAL[base$basal_anteroseptal==1 | base$basal_inferoseptal==1 | base$medio_anteroseptal==1 | base$medio_inferoseptal==1 | base$apical_septal==1]<-1
base$SEPTAL<-na.tools::na.replace(base$SEPTAL,0)

base$LATERAL[base$basal_anterolateral==1 | base$basal_inferolateral==1 | base$medio_anterolateral==1 | base$medio_inferolateral==1 | base$apical_lateral==1]<-1
base$LATERAL<-na.tools::na.replace(base$LATERAL,0)

base$ANTERIOR[base$basal_anterior==1 | base$medio_anterior==1 | base$apical_anterior==1]<-1
base$ANTERIOR<-na.tools::na.replace(base$ANTERIOR,0)

base$INFERIOR[base$basal_inferior==1 | base$medio_inferior==1 | base$apical_inferior==1 |base$apex==1]<-1
base$INFERIOR<-na.tools::na.replace(base$INFERIOR,0)

#Segmentos

base$BASAL[base$basal_anterior==1 | base$basal_anteroseptal==1 | base$basal_inferoseptal==1 | base$basal_inferior==1 | base$basal_inferolateral==1 | base$basal_anterolateral==1]<-1
base$BASAL<-na.tools::na.replace(base$BASAL,0)

base$MEDIOS[base$medio_anterior==1 | base$medio_anteroseptal==1 | base$medio_inferoseptal==1 | base$medio_inferior==1 | base$medio_inferolateral==1 | base$medio_anterolateral==1]<-1
base$MEDIOS<-na.tools::na.replace(base$MEDIOS,0)

base$APICAL[base$apical_anterior==1 | base$apical_septal==1 | base$apical_inferior==1 | base$apical_lateral==1 | base$apex==1]<-1
base$APICAL<-na.tools::na.replace(base$APICAL,0)

#----Descriptive All Population (Table 0)#####

columns <- c('Parameter',"All-Population (n=60)")

#Sexo
sexo <- table(base$sexo,useNA = "always")[c(2)]
sexoprop <- round(prop.table(table(base$sexo,useNA = "always")),4)[c(2)]*100
Sexo<-`names<-`(data.frame("Men (%)",
                           matrix(c(paste(sexo,paste0('(',sexoprop,')'))),ncol = 1)),
                columns)

#Edad

num1<-c(paste(round(median(base$EDAD_A_UC,na.rm = T ),2),
              paste0('(',round(quantile(base$EDAD_A_UC,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$EDAD_A_UC,na.rm = T,probs = c(0.75)),2),')')))

Edad<-`names<-`(data.frame(matrix(c("Age (Years)",num1),ncol = 2)),columns)

#Edad al Diagnostic

nortest::ad.test(base$TIEMPO_AL_DIAGNOSTICO)
num1<-c(paste(round(median(base$TIEMPO_AL_DIAGNOSTICO,na.rm = T ),4),
              paste0('(',round(quantile(base$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.75)),2),')')))

Edad_Dx<-`names<-`(data.frame(matrix(c("Age at Diagnosis (Years)",num1),ncol = 2)),columns)

#Tiempo de Tratamiento


num1<-c(paste(round(median(base$TIEMPO_TRATAMIENTO,na.rm = T ),2),
              paste0('(',round(quantile(base$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.75)),2),')')))

Tiempo_Tx<-`names<-`(data.frame(matrix(c("Time of Treatment (Years)",num1),ncol = 2)),columns)


#IMC

IMC.media<-round(mean(base$imc,na.rm = T),2)
IMC.sd<-round(sd(base$imc,na.rm = T),2)
IMC<-`names<-`(data.frame(matrix(c("BMI (Kg/m2)",paste(IMC.media,"(","±",IMC.sd,")")),ncol = 2,byrow = T)),columns)

#Underweight
Underweight.CAT <- table((base$imc<18.5),useNA = "always")[c(2)]
Underweight.CAT.prop <- round(prop.table(table((base$imc<18.5),useNA = "always")),4)[c(2)]*100
Infrapeso<-`names<-`(data.frame("Underweight  (%)",
                               matrix(c(paste(Underweight.CAT,paste0('(',Underweight.CAT.prop,')'))),ncol = 1)),
                    columns)

#Normopeso
Normopeso.CAT <- table((base$imc>=18.5  & base$imc<25),useNA = "always")[c(2)]
Normopeso.CAT.prop <- round(prop.table(table((base$imc>=18.5  & base$imc<25),useNA = "always")),4)[c(2)]*100
Normopeso<-`names<-`(data.frame("Normalweight  (%)",
                               matrix(c(paste(Normopeso.CAT,paste0('(',Normopeso.CAT.prop,')'))),ncol = 1)),
                    columns)

#Sobrepeso
Sobrepeso.CAT <- table((base$imc>=25  & base$imc<30),useNA = "always")[c(2)]
Sobrepeso.CAT.prop <- round(prop.table(table((base$imc>=25  & base$imc<30),useNA = "always")),4)[c(2)]*100
Sobrepso<-`names<-`(data.frame("Overweight  (%)",
                             matrix(c(paste(Sobrepeso.CAT,paste0('(',Sobrepeso.CAT.prop,')'))),ncol = 1)),
                  columns)

#Obesidad
Obesidad.CAT <- table((base$imc>30),useNA = "always")[c(2)]
Obesidad.CAT.prop <- round(prop.table(table((base$imc>30),useNA = "always")),4)[c(2)]*100
Obesidad<-`names<-`(data.frame("Obesity  (%)",
                               matrix(c(paste(Obesidad.CAT,paste0('(',Obesidad.CAT.prop,')'))),ncol = 1)),
                    columns)

#Diabetes
Diabetes <- table(base$diabetes,useNA = "always")[c(2)]
Diabetes.prop <- round(prop.table(table(base$diabetes,useNA = "always")),4)[c(2)]*100
Diabetes<-`names<-`(data.frame("Diabetes (%)",
                                  matrix(c(paste(Diabetes,paste0('(',Diabetes.prop,')'))),ncol = 1)),
                       columns)

#Arterial Hypertension
HAS <- table(base$hipertension_arterial,useNA = "always")[c(2)]
HAS.prop <- round(prop.table(table(base$hipertension_arterial,useNA = "always")),4)[c(2)]*100
Hipertension<-`names<-`(data.frame("Arterial Hypertension (%)",
                               matrix(c(paste(HAS,paste0('(',HAS.prop,')'))),ncol = 1)),
                    columns)

#EVC
EVC <- table(base$evc,useNA = "always")[c(2)]
EVC.prop <- round(prop.table(table(base$evc,useNA = "always")),4)[c(2)]*100
Stroke<-`names<-`(data.frame("Stroke (%)",
                                   matrix(c(paste(EVC,paste0('(',EVC.prop,')'))),ncol = 1)),
                        columns)

#Otra Comorbilidad
Otra.Com <- table(base$otra,useNA = "always")[c(2)]
Otra.Com.prop <- round(prop.table(table(base$otra,useNA = "always")),4)[c(2)]*100
Otra.Comorb<-`names<-`(data.frame("Other Comorbidity (%)",
                             matrix(c(paste(Otra.Com,paste0('(',Otra.Com.prop,')'))),ncol = 1)),
                  columns)


#Wolff-Parkinson White
Wold.P <- table(base$wolff_parkinson_white,useNA = "always")[c(2)]
Wold.P.prop <- round(prop.table(table(base$wolff_parkinson_white,useNA = "always")),4)[c(2)]*100
Wold.P.Comorb<-`names<-`(data.frame("Wolff-Parkinson White (%)",
                                  matrix(c(paste(Wold.P,paste0('(',Wold.P.prop,')'))),ncol = 1)),
                       columns)

#Atrio-Ventricular Block
AV.B <- table(base$bloqueo_av,useNA = "always")[c(2)]
AV.B.prop <- round(prop.table(table(base$bloqueo_av,useNA = "always")),4)[c(2)]*100
Bloqueo_AV<-`names<-`(data.frame("Atrio-Ventricular Block (%)",
                                    matrix(c(paste(AV.B,paste0('(',AV.B.prop,')'))),ncol = 1)),
                         columns)

#Atrial Fibrilation
FA <- table(base$fa,useNA = "always")[c(2)]
FA.prop <- round(prop.table(table(base$fa,useNA = "always")),4)[c(2)]*100
Fibrilacion_Atrial<-`names<-`(data.frame("Atrial Fibrilation (%)",
                                 matrix(c(paste(FA,paste0('(',FA.prop,')'))),ncol = 1)),
                      columns)

#Bloqueo Rama Derecho
Bloqueo.RD <- table(base$bloqueo_rama_derecha,useNA = "always")[c(2)]
Bloqueo.RD.prop <- round(prop.table(table(base$bloqueo_rama_derecha,useNA = "always")),4)[c(2)]*100
Bloqueo_RAMA_DERECHO<-`names<-`(data.frame("Right Bundle Branch Block (%)",
                                         matrix(c(paste(Bloqueo.RD,paste0('(',Bloqueo.RD.prop,')'))),ncol = 1)),
                              columns)

#Otra Arritmia
Otra.Arritmia <- table(base$bloqueo_rama_derecha,useNA = "always")[c(2)]
Otra.Arritmia.prop <- round(prop.table(table(base$bloqueo_rama_derecha,useNA = "always")),4)[c(2)]*100
Otra.Arri<-`names<-`(data.frame("Other Athrhymia (%)",
                                           matrix(c(paste(Otra.Arritmia,paste0('(',Otra.Arritmia.prop,')'))),ncol = 1)),
                                columns)

#Comunicacion IA
Com_IA <- table(base$comunicacion_ia,useNA = "always")[c(2)]
Com_IA.prop <- round(prop.table(table(base$comunicacion_ia,useNA = "always")),4)[c(2)]*100
Comunicacion_IA<-`names<-`(data.frame("Interatial-Communication (%)",
                                   matrix(c(paste(Com_IA,paste0('(',Com_IA.prop,')'))),ncol = 1)),
                        columns)

#Comunicacion IV
Com_IV <- table(base$comunicacion_iv,useNA = "always")[c(2)]
Com_IV.prop <- round(prop.table(table(base$comunicacion_iv,useNA = "always")),4)[c(2)]*100
Comunicacion_IV<-`names<-`(data.frame("Interventricular-Communication (%)",
                                      matrix(c(paste(Com_IV,paste0('(',Com_IV.prop,')'))),ncol = 1)),
                           columns)

#Persistencia del Ductus
DAP <- table(base$persistencia_del_ca,useNA = "always")[c(2)]
DAP.prop <- round(prop.table(table(base$persistencia_del_ca,useNA = "always")),4)[c(2)]*100
Ductus_Arterial<-`names<-`(data.frame("Patent Ductus Arteriosus (%)",
                                   matrix(c(paste(DAP,paste0('(',DAP.prop,')'))),ncol = 1)),
                        columns)

#Foramen Oval
FOP <- table(base$foramen_oval_permeable,useNA = "always")[c(2)]
FOP.prop <- round(prop.table(table(base$foramen_oval_permeable,useNA = "always")),4)[c(2)]*100
Foramen_Oval<-`names<-`(data.frame("Patent Foramen Ovale (%)",
                                   matrix(c(paste(FOP,paste0('(',FOP.prop,')'))),ncol = 1)),
                        columns)

#Otra
Otra <- table(base$otra,useNA = "always")[c(2)]
Otra.prop <- round(prop.table(table(base$otra,useNA = "always")),4)[c(2)]*100
Otra.Anomalia.Fisica<-`names<-`(data.frame("Other Structural Impairment (%)",
                             matrix(c(paste(Otra,paste0('(',Otra.prop,')'))),ncol = 1)),
                  columns)

#ASC

ASC.media<-round(mean(base$sct_m2_1_73,na.rm = T),2)
ASC.sd<-round(sd(base$sct_m2_1_73,na.rm = T),2)
ASC<-`names<-`(data.frame(matrix(c("Superfitial Corporal Area (m2/1.73",paste(ASC.media,"(","±",ASC.sd,")")),ncol = 2,byrow = T)),columns)


#Disnea
disnea <- table(base$disnea,useNA = "always")[c(2)]
disneaprop <- round(prop.table(table(base$disnea,useNA = "always")),4)[c(2)]*100
Disnea<-`names<-`(data.frame("Dysnea (%)",
                           matrix(c(paste(disnea,paste0('(',disneaprop,')'))),ncol = 1)),
                columns)

#Palpitaciones
Palpita <- table(base$palpitaciones,useNA = "always")[c(2)]
Palpitaprop <- round(prop.table(table(base$palpitaciones,useNA = "always")),4)[c(2)]*100
Palpitaciones<-`names<-`(data.frame("Palpitations (%)",
                             matrix(c(paste(Palpita,paste0('(',Palpitaprop,')'))),ncol = 1)),
                  columns)


#Edema
Edema <- table(base$edema,useNA = "always")[c(2)]
Edemaprop <- round(prop.table(table(base$edema,useNA = "always")),4)[c(2)]*100
Edema<-`names<-`(data.frame("Edema (%)",
                                    matrix(c(paste(Edema,paste0('(',Edemaprop,')'))),ncol = 1)),
                         columns)

#Fatiga
Fatigue <- table(base$fatiga,useNA = "always")[c(2)]
Fatigueprop <- round(prop.table(table(base$fatiga,useNA = "always")),4)[c(2)]*100
Fatiga<-`names<-`(data.frame("Fatigue (%)",
                            matrix(c(paste(Fatigue,paste0('(',Fatigueprop,')'))),ncol = 1)),
                 columns)

#Taquicardia
Taquicardia <- table(base$taquicardia,useNA = "always")[c(2)]
Taquicardiaprop <- round(prop.table(table(base$taquicardia,useNA = "always")),4)[c(2)]*100
Taquicardia<-`names<-`(data.frame("Tachycardia (%)",
                             matrix(c(paste(Taquicardia,paste0('(',Taquicardiaprop,')'))),ncol = 1)),
                  columns)

#Cianosis
Cianos <- table(base$cianosis,useNA = "always")[c(2)]
Cianosprop <- round(prop.table(table(base$cianosis,useNA = "always")),4)[c(2)]*100
Cianosis<-`names<-`(data.frame("Cyanosis (%)",
                                  matrix(c(paste(Cianos,paste0('(',Cianosprop,')'))),ncol = 1)),
                       columns)

#Functional Class
Clase.Funcional <- table(base$clase_funcional,useNA = "always")[c(1:3)]
Clase.Funcional.prop <- round(prop.table(table(base$clase_funcional,useNA = "always")),4)[c(1:3)]*100
NYHA<-`names<-`(data.frame(c("I (%)","II (%)","III (%)"),
                               matrix(c(paste(Clase.Funcional,paste0('(',Clase.Funcional.prop,')'))),ncol = 1)),
                    columns)

#HB
base$hb<-as.numeric(base$hb)
num1<-c(paste(round(median(base$hb,na.rm = T ),2),
              paste0('(',round(quantile(base$hb,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$hb,na.rm = T,probs = c(0.75)),2),')')))

Hemoglobina<-`names<-`(data.frame(matrix(c("Hemoglobin (gr/dl)",num1),ncol = 2)),columns)

#HB
base$hto<-as.numeric(base$hto)
num1<-c(paste(round(median(base$hto,na.rm = T ),2),
              paste0('(',round(quantile(base$hto,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$hto,na.rm = T,probs = c(0.75)),2),')')))

Hematocrito<-`names<-`(data.frame(matrix(c("Hematocrit (%)",num1),ncol = 2)),columns)

#Celemajer
base$celermajer<-as.numeric(base$celermajer)
num1<-c(paste(round(median(base$celermajer,na.rm = T ),2),
              paste0('(',round(quantile(base$celermajer,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$celermajer,na.rm = T,probs = c(0.75)),2),')')))

Celermajer<-`names<-`(data.frame(matrix(c("Celermajer Index (XXX)",num1),ncol = 2)),columns)


#Adosamiento VT Septal
base$adosamiento_vt_septal<-as.numeric(base$adosamiento_vt_septal)
num1<-c(paste(round(median(base$adosamiento_vt_septal,na.rm = T ),2),
              paste0('(',round(quantile(base$adosamiento_vt_septal,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$adosamiento_vt_septal,na.rm = T,probs = c(0.75)),2),')')))

Septal_Endorsement <-`names<-`(data.frame(matrix(c("Septal Endorsement (XXX)",num1),ncol = 2)),columns)

#Adosamiento VT Posterior
base$adosamiento_vt_posterior<-as.numeric(base$adosamiento_vt_posterior)
num1<-c(paste(round(median(base$adosamiento_vt_posterior,na.rm = T ),2),
              paste0('(',round(quantile(base$adosamiento_vt_posterior,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$adosamiento_vt_posterior,na.rm = T,probs = c(0.75)),2),')')))

Posterior_Endorsement <-`names<-`(data.frame(matrix(c("Posterior Endorsement (XXX)",num1),ncol = 2)),columns)

#Right_Ventricle_Atrialization (mm)
base$porcion_atrializada_vd_mm<-as.numeric(base$porcion_atrializada_vd_mm)
num1<-c(paste(round(median(base$porcion_atrializada_vd_mm,na.rm = T ),2),
              paste0('(',round(quantile(base$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.75)),2),')')))

Right_Ventricle_Atrialization <-`names<-`(data.frame(matrix(c("Right Ventricle - Atrialization (mm)",num1),ncol = 2)),columns)


#Right_Ventricle_Atrialization (%)
base$porcion_atrializada_vd_percent<-as.numeric(base$porcion_atrializada_vd_percent)
num1<-c(paste(round(median(base$porcion_atrializada_vd_percent,na.rm = T ),2),
              paste0('(',round(quantile(base$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.75)),2),')')))

Right_Ventricle_Atrialization_percent <-`names<-`(data.frame(matrix(c("Right Ventricle - Atrialization (%)",num1),ncol = 2)),columns)

#LVEF

num1<-c(paste(round(median(base$fevi_49,na.rm = T ),2),
              paste0('(',round(quantile(base$fevi_49,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$fevi_49,na.rm = T,probs = c(0.75)),2),')')))

LVEF <-`names<-`(data.frame(matrix(c("LVEF (%)",num1),ncol = 2)),columns)

#RVEF
base$fevd<-as.numeric(base$fevd)
num1<-c(paste(round(median(base$fevd,na.rm = T ),2),
              paste0('(',round(quantile(base$fevd,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$fevd,na.rm = T,probs = c(0.75)),2),')')))

RVEF <-`names<-`(data.frame(matrix(c("RVEF (%)",num1),ncol = 2)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vtd_vi_index)
num1<-c(paste(round(median(base$vtd_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base$vtd_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$vtd_vi_index,na.rm = T,probs = c(0.75)),2),')')))

LV_EDV_Ind <-`names<-`(data.frame(matrix(c("LV-EDV Indexed (%)",num1),ncol = 2)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vts_vi_index)
num1<-c(paste(round(median(base$vts_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base$vts_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$vts_vi_index,na.rm = T,probs = c(0.75)),2),')')))

LV_ESV_Ind <-`names<-`(data.frame(matrix(c("LV-ESV Indexed (%)",num1),ncol = 2)),columns)


#LV-Mass Indexed
nortest::ad.test(base$masa_vi_index)
num1<-c(paste(round(median(base$masa_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base$masa_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$masa_vi_index,na.rm = T,probs = c(0.75)),2),')')))

LV_Mass_Ind <-`names<-`(data.frame(matrix(c("LV-Mass Indexed (%)",num1),ncol = 2)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vtd_v_df_a_index)
num1<-c(paste(round(median(base$vtd_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base$vtd_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$vtd_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

RV_EDV_Ind <-`names<-`(data.frame(matrix(c("RV-EDV Indexed (%)",num1),ncol = 2)),columns)

#LV-ESV Indexed
nortest::ad.test(base$vts_v_df_a_index)
num1<-c(paste(round(median(base$vts_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base$vts_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$vts_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

RV_ESV_Ind <-`names<-`(data.frame(matrix(c("RV-ESV Indexed (%)",num1),ncol = 2)),columns)


#AI
AI <- table(base$ai,useNA = "always")[c(2)]
AI.prop <- round(prop.table(table(base$ai,useNA = "always")),4)[c(2)]*100
AI<-`names<-`(data.frame(c("Left-Atrium (%)"),
                           matrix(c(paste(AI,paste0('(',AI.prop,')'))),ncol = 1)),
                columns)

#AI
AD <- table(base$ad,useNA = "always")[c(2)]
AD.prop <- round(prop.table(table(base$ad,useNA = "always")),4)[c(2)]*100
AD<-`names<-`(data.frame(c("Right-Atrium (%)"),
                         matrix(c(paste(AD,paste0('(',AD.prop,')'))),ncol = 1)),
              columns)

#VI
VI <- table(base$vi,useNA = "always")[c(2)]
VI.prop <- round(prop.table(table(base$vi,useNA = "always")),4)[c(2)]*100
VI<-`names<-`(data.frame(c("Left-Ventricle (%)"),
                         matrix(c(paste(VI,paste0('(',VI.prop,')'))),ncol = 1)),
              columns)

#VD
VD <- table(base$vd_pared_libre,useNA = "always")[c(2)]
VD.prop <- round(prop.table(table(base$vd_pared_libre,useNA = "always")),4)[c(2)]*100
VD<-`names<-`(data.frame(c("Right-Ventricle (%)"),
                         matrix(c(paste(VD,paste0('(',VD.prop,')'))),ncol = 1)),
              columns)

#Reinforced Areas

num1<-c(paste(round(median(base$zonas_de_reforzamiento_segmentos,na.rm = T ),2),
              paste0('(',round(quantile(base$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.75)),2),')')))

Reinforced_Areas <-`names<-`(data.frame(matrix(c("Reinforced Areas (n)",num1),ncol = 2)),columns)

#Localizacion Intramiocardio
Intra.myo <- table(base$localizacion_intramiocardico,useNA = "always")[c(2)]
Intra.myo.prop <- round(prop.table(table(base$localizacion_intramiocardico,useNA = "always")),4)[c(2)]*100
Intramiocardio<-`names<-`(data.frame(c("Intra-myocardial (%)"),
                         matrix(c(paste(Intra.myo,paste0('(',Intra.myo.prop,')'))),ncol = 1)),
              columns)

#Localizacion Subendocardico
Subendo.myo <- table(base$localizacion_subendocardica,useNA = "always")[c(2)]
Subendo.myo.prop <- round(prop.table(table(base$localizacion_subendocardica,useNA = "always")),4)[c(2)]*100
Subendo.myocardico<-`names<-`(data.frame(c("Subendocardial (%)"),
                                     matrix(c(paste(Subendo.myo,paste0('(',Subendo.myo.prop,')'))),ncol = 1)),
                          columns)

#Localizacion Subepicardico
Subepi.myo <- table(base$localizacion_subepicardico,useNA = "always")[c(2)]
Subepi.myo.prop <- round(prop.table(table(base$localizacion_subepicardico,useNA = "always")),4)[c(2)]*100
Subepi.myocardico<-`names<-`(data.frame(c("Subepicardial (%)"),
                                         matrix(c(paste(Subepi.myo,paste0('(',Subepi.myo.prop,')'))),ncol = 1)),
                              columns)

#Localizacion Transmural
Trans.myo <- table(base$localizacion_transmural,useNA = "always")[c(2)]
Trans.myo.prop <- round(prop.table(table(base$localizacion_transmural,useNA = "always")),4)[c(2)]*100
Trans.myocardico<-`names<-`(data.frame(c("Transepicardical (%)"),
                                        matrix(c(paste(Trans.myo,paste0('(',Trans.myo.prop,')'))),ncol = 1)),
                             columns)

Table_0<-rbind(Sexo,Edad,Edad_Dx,Tiempo_Tx,ASC,IMC,Infrapeso,Normopeso,Sobrepso,Obesidad,Diabetes,Hipertension,Stroke,Otra.Comorb,
               Wold.P.Comorb,Bloqueo_AV,Fibrilacion_Atrial,Bloqueo_RAMA_DERECHO,Otra.Arri,
               Comunicacion_IA,Comunicacion_IV,Ductus_Arterial,Foramen_Oval,Otra.Anomalia.Fisica,
               Disnea,Palpitaciones,Edema,Fatiga,Taquicardia,Cianosis,NYHA,
               Hemoglobina,Hematocrito,
               Celermajer,Septal_Endorsement,Posterior_Endorsement,Right_Ventricle_Atrialization,Right_Ventricle_Atrialization_percent,
               LVEF,RVEF,LV_EDV_Ind,LV_ESV_Ind,LV_Mass_Ind,RV_EDV_Ind,RV_ESV_Ind,
               Reinforced_Areas,
               AI,AD,VI,VD,
               Intramiocardio,Subendo.myocardico,Subepi.myocardico,Trans.myocardico)

#----Descriptive Between LGE + (Table 1)-----

columns <- c('Parameter',"LGE (-) (n=40)","LGE (+) (n=17)")

#Sexo
sexo <- table(base$sexo,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
sexoprop <- round(prop.table(table(base$sexo,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Sexo<-`names<-`(data.frame("Men (%)",
                           matrix(c(paste(sexo,paste0('(',sexoprop,')'))),ncol = 2)),
                columns)
#Edad

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$EDAD_A_UC,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$EDAD_A_UC,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$EDAD_A_UC,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$EDAD_A_UC,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$EDAD_A_UC,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$EDAD_A_UC,na.rm = T,probs = c(0.75)),2),')')))

Edad<-`names<-`(data.frame(matrix(c("Age (Years)",num1,num2),ncol = 3)),columns)

#Edad al Diagnostic

nortest::ad.test(base$TIEMPO_AL_DIAGNOSTICO)
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.75)),2),')')))

Edad_Dx<-`names<-`(data.frame(matrix(c("Age at Diagnosis (Years)",num1,num2),ncol = 3)),columns)

#Tiempo de Tratamiento

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$TIEMPO_TRATAMIENTO,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$TIEMPO_TRATAMIENTO,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.75)),2),')')))

Tiempo_Tx<-`names<-`(data.frame(matrix(c("Time of Treatment (Years)",num1,num2),ncol = 3)),columns)


#IMC

IMC.media<-round(mean(base[base$REFORZAMIENTO_DIC==0,]$imc,na.rm = T),2)
IMC.sd<-round(sd(base[base$REFORZAMIENTO_DIC==0,]$imc,na.rm = T),2)
num1<-paste(IMC.media,"(","±",IMC.sd,")")

IMC.media<-round(mean(base[base$REFORZAMIENTO_DIC==1,]$imc,na.rm = T),2)
IMC.sd<-round(sd(base[base$REFORZAMIENTO_DIC==1,]$imc,na.rm = T),2)
num2<-paste(IMC.media,"(","±",IMC.sd,")")

IMC<-`names<-`(data.frame(matrix(c("BMI (Years)",num1,num2),ncol = 3)),columns)

#Underweight
Underweight.CAT <- table((base$imc<18.5),base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Underweight.CAT.prop <- round(prop.table(table((base$imc<18.5),base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Infrapeso<-`names<-`(data.frame("Underweight  (%)",
                                matrix(c(paste(Underweight.CAT,paste0('(',Underweight.CAT.prop,')'))),ncol = 2)),
                     columns)

#Normopeso
Normopeso.CAT <- table((base$imc>=18.5  & base$imc<25),base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Normopeso.CAT.prop <- round(prop.table(table((base$imc>=18.5  & base$imc<25),base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Normopeso<-`names<-`(data.frame("Normalweight  (%)",
                                matrix(c(paste(Normopeso.CAT,paste0('(',Normopeso.CAT.prop,')'))),ncol = 2)),
                     columns)

#Sobrepeso
Sobrepeso.CAT <- table((base$imc>=25  & base$imc<30),base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Sobrepeso.CAT.prop <- round(prop.table(table((base$imc>=25  & base$imc<30),base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Sobrepso<-`names<-`(data.frame("Overweight  (%)",
                               matrix(c(paste(Sobrepeso.CAT,paste0('(',Sobrepeso.CAT.prop,')'))),ncol = 2)),
                    columns)

#Obesidad
Obesidad.CAT <- table((base$imc>30),base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Obesidad.CAT.prop <- round(prop.table(table((base$imc>30),base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Obesidad<-`names<-`(data.frame("Obesity  (%)",
                               matrix(c(paste(Obesidad.CAT,paste0('(',Obesidad.CAT.prop,')'))),ncol = 2)),
                    columns)

#Diabetes
Diabetes <- table(base$diabetes,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Diabetes.prop <- round(prop.table(table(base$diabetes,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Diabetes<-`names<-`(data.frame("Diabetes (%)",
                               matrix(c(paste(Diabetes,paste0('(',Diabetes.prop,')'))),ncol = 2)),
                    columns)

#Arterial Hypertension
HAS <- table(base$hipertension_arterial,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
HAS.prop <- round(prop.table(table(base$hipertension_arterial,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Hipertension<-`names<-`(data.frame("Arterial Hypertension (%)",
                                   matrix(c(paste(HAS,paste0('(',HAS.prop,')'))),ncol = 2)),
                        columns)

#EVC
EVC <- table(base$evc,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
EVC.prop <- round(prop.table(table(base$evc,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Stroke<-`names<-`(data.frame("Stroke (%)",
                             matrix(c(paste(EVC,paste0('(',EVC.prop,')'))),ncol = 2)),
                  columns)

#Otra Comorbilidad
Otra.Com <- table(base$otra,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Otra.Com.prop <- round(prop.table(table(base$otra,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Otra.Comorb<-`names<-`(data.frame("Other Comorbidity (%)",
                                  matrix(c(paste(Otra.Com,paste0('(',Otra.Com.prop,')'))),ncol = 2)),
                       columns)


#Wolff-Parkinson White
Wold.P <- table(base$wolff_parkinson_white,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Wold.P.prop <- round(prop.table(table(base$wolff_parkinson_white,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Wold.P.Comorb<-`names<-`(data.frame("Wolff-Parkinson White (%)",
                                    matrix(c(paste(Wold.P,paste0('(',Wold.P.prop,')'))),ncol = 2)),
                         columns)

#Atrio-Ventricular Block
AV.B <- table(base$bloqueo_av,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
AV.B.prop <- round(prop.table(table(base$bloqueo_av,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Bloqueo_AV<-`names<-`(data.frame("Atrio-Ventricular Block (%)",
                                 matrix(c(paste(AV.B,paste0('(',AV.B.prop,')'))),ncol = 2)),
                      columns)

#Atrial Fibrilation
FA <- table(base$fa,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
FA.prop <- round(prop.table(table(base$fa,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Fibrilacion_Atrial<-`names<-`(data.frame("Atrial Fibrilation (%)",
                                         matrix(c(paste(FA,paste0('(',FA.prop,')'))),ncol = 2)),
                              columns)

#Bloqueo Rama Derecho
Bloqueo.RD <- table(base$bloqueo_rama_derecha,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Bloqueo.RD.prop <- round(prop.table(table(base$bloqueo_rama_derecha,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Bloqueo_RAMA_DERECHO<-`names<-`(data.frame("Right Bundle Branch Block (%)",
                                           matrix(c(paste(Bloqueo.RD,paste0('(',Bloqueo.RD.prop,')'))),ncol = 2)),
                                columns)

#Otra Arritmia
Otra.Arritmia <- table(base$bloqueo_rama_derecha,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Otra.Arritmia.prop <- round(prop.table(table(base$bloqueo_rama_derecha,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Otra.Arri<-`names<-`(data.frame("Other Athrhymia (%)",
                                matrix(c(paste(Otra.Arritmia,paste0('(',Otra.Arritmia.prop,')'))),ncol = 2)),
                     columns)

#Comunicacion IA
Com_IA <- table(base$comunicacion_ia,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Com_IA.prop <- round(prop.table(table(base$comunicacion_ia,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Comunicacion_IA<-`names<-`(data.frame("Interatial-Communication (%)",
                                      matrix(c(paste(Com_IA,paste0('(',Com_IA.prop,')'))),ncol = 2)),
                           columns)

#Comunicacion IV
Com_IV <- table(base$comunicacion_iv,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Com_IV.prop <- round(prop.table(table(base$comunicacion_iv,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Comunicacion_IV<-`names<-`(data.frame("Interventricular-Communication (%)",
                                      matrix(c(paste(Com_IV,paste0('(',Com_IV.prop,')'))),ncol = 2)),
                           columns)

#Persistencia del Ductus
DAP <- table(base$persistencia_del_ca,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
DAP.prop <- round(prop.table(table(base$persistencia_del_ca,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Ductus_Arterial<-`names<-`(data.frame("Patent Ductus Arteriosus (%)",
                                      matrix(c(paste(DAP,paste0('(',DAP.prop,')'))),ncol = 2)),
                           columns)

#Foramen Oval
FOP <- table(base$foramen_oval_permeable,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
FOP.prop <- round(prop.table(table(base$foramen_oval_permeable,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Foramen_Oval<-`names<-`(data.frame("Patent Foramen Ovale (%)",
                                   matrix(c(paste(FOP,paste0('(',FOP.prop,')'))),ncol = 2)),
                        columns)

#Otra
Otra <- table(base$otra,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Otra.prop <- round(prop.table(table(base$otra,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Otra.Anomalia.Fisica<-`names<-`(data.frame("Other Structural Impairment (%)",
                                           matrix(c(paste(Otra,paste0('(',Otra.prop,')'))),ncol = 2)),
                                columns)

#ASC

ASC.media<-round(mean(base[base$REFORZAMIENTO_DIC==0,]$sct_m2_1_73,na.rm = T),2)
ASC.sd<-round(sd(base[base$REFORZAMIENTO_DIC==0,]$sct_m2_1_73,na.rm = T),2)
num1<-paste(ASC.media,"(","±",ASC.sd,")")

ASC.media<-round(mean(base[base$REFORZAMIENTO_DIC==1,]$sct_m2_1_73,na.rm = T),2)
ASC.sd<-round(sd(base[base$REFORZAMIENTO_DIC==1,]$sct_m2_1_73,na.rm = T),2)
num2<-paste(ASC.media,"(","±",ASC.sd,")")

ASC<-`names<-`(data.frame(matrix(c("Superfitial Corporal Area (m2/1.73)",num1,num2),ncol = 3)),columns)

#Disnea
disnea <- table(base$disnea,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
disneaprop <- round(prop.table(table(base$disnea,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Disnea<-`names<-`(data.frame("Dysnea (%)",
                             matrix(c(paste(disnea,paste0('(',disneaprop,')'))),ncol = 2)),
                  columns)

#Palpitaciones
Palpita <- table(base$palpitaciones,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Palpitaprop <- round(prop.table(table(base$palpitaciones,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Palpitaciones<-`names<-`(data.frame("Palpitations (%)",
                                    matrix(c(paste(Palpita,paste0('(',Palpitaprop,')'))),ncol = 2)),
                         columns)


#Edema
Edema <- table(base$edema,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Edemaprop <- round(prop.table(table(base$edema,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Edema<-`names<-`(data.frame("Edema (%)",
                            matrix(c(paste(Edema,paste0('(',Edemaprop,')'))),ncol = 2)),
                 columns)

#Fatiga
Fatigue <- table(base$fatiga,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Fatigueprop <- round(prop.table(table(base$fatiga,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Fatiga<-`names<-`(data.frame("Fatigue (%)",
                             matrix(c(paste(Fatigue,paste0('(',Fatigueprop,')'))),ncol = 2)),
                  columns)

#Taquicardia
Taquicardia <- table(base$taquicardia,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Taquicardiaprop <- round(prop.table(table(base$taquicardia,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Taquicardia<-`names<-`(data.frame("Tachycardia (%)",
                                  matrix(c(paste(Taquicardia,paste0('(',Taquicardiaprop,')'))),ncol = 2)),
                       columns)

#Cianosis
Cianos <- table(base$cianosis,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Cianosprop <- round(prop.table(table(base$cianosis,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Cianosis<-`names<-`(data.frame("Cyanosis (%)",
                               matrix(c(paste(Cianos,paste0('(',Cianosprop,')'))),ncol = 2)),
                    columns)

#Functional Class
Clase.Funcional <- table(base$clase_funcional,base$REFORZAMIENTO_DIC,useNA = "always")[c(1:3,5:7)]
Clase.Funcional.prop <- round(prop.table(table(base$clase_funcional,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(1:3,5:7)]*100
NYHA<-`names<-`(data.frame(c("I (%)","II (%)","III (%)"),
                           matrix(c(paste(Clase.Funcional,paste0('(',Clase.Funcional.prop,')'))),ncol = 2)),
                columns)

#HB
base$hb<-as.numeric(base$hb)
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$hb,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$hb,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$hb,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$hb,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$hb,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$hb,na.rm = T,probs = c(0.75)),2),')')))

Hemoglobina<-`names<-`(data.frame(matrix(c("Hemoglobin (gr/dl)",num1,num2),ncol = 3)),columns)

#HB
base$hto<-as.numeric(base$hto)
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$hto,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$hto,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$hto,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$hto,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$hto,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$hto,na.rm = T,probs = c(0.75)),2),')')))

Hematocrito<-`names<-`(data.frame(matrix(c("Hematocrit (%)",num1,num2),ncol = 3)),columns)

#Celemajer
base$celermajer<-as.numeric(base$celermajer)
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$celermajer,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$celermajer,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$celermajer,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$celermajer,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$celermajer,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$celermajer,na.rm = T,probs = c(0.75)),2),')')))

Celermajer<-`names<-`(data.frame(matrix(c("Celermajer Index (XXX)",num1,num2),ncol = 3)),columns)


#Adosamiento VT Septal
base$adosamiento_vt_septal<-as.numeric(base$adosamiento_vt_septal)

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$adosamiento_vt_septal,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$adosamiento_vt_septal,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$adosamiento_vt_septal,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$adosamiento_vt_septal,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$adosamiento_vt_septal,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$adosamiento_vt_septal,na.rm = T,probs = c(0.75)),2),')')))

Septal_Endorsement <-`names<-`(data.frame(matrix(c("Septal Endorsement (XXX)",num1,num2),ncol = 3)),columns)

#Adosamiento VT Posterior
base$adosamiento_vt_posterior<-as.numeric(base$adosamiento_vt_posterior)
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$adosamiento_vt_posterior,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$adosamiento_vt_posterior,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$adosamiento_vt_posterior,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$adosamiento_vt_posterior,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$adosamiento_vt_posterior,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$adosamiento_vt_posterior,na.rm = T,probs = c(0.75)),2),')')))


Posterior_Endorsement <-`names<-`(data.frame(matrix(c("Posterior Endorsement (XXX)",num1,num2),ncol = 3)),columns)

#Right_Ventricle_Atrialization (mm)
base$porcion_atrializada_vd_mm<-as.numeric(base$porcion_atrializada_vd_mm)

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$porcion_atrializada_vd_mm,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$porcion_atrializada_vd_mm,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.75)),2),')')))


Right_Ventricle_Atrialization <-`names<-`(data.frame(matrix(c("Right Ventricle - Atrialization (mm)",num1,num2),ncol = 3)),columns)


#Right_Ventricle_Atrialization (%)
base$porcion_atrializada_vd_percent<-as.numeric(base$porcion_atrializada_vd_percent)

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$porcion_atrializada_vd_percent,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$porcion_atrializada_vd_percent,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.75)),2),')')))

Right_Ventricle_Atrialization_percent <-`names<-`(data.frame(matrix(c("Right Ventricle - Atrialization (%)",num1,num2),ncol = 3)),columns)

#LVEF
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$fevi_49,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$fevi_49,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$fevi_49,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$fevi_49,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$fevi_49,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$fevi_49,na.rm = T,probs = c(0.75)),2),')')))

LVEF <-`names<-`(data.frame(matrix(c("LVEF (%)",num1,num2),ncol = 3)),columns)

#RVEF
base$fevd<-as.numeric(base$fevd)

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$fevd,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$fevd,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$fevd,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$fevd,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$fevd,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$fevd,na.rm = T,probs = c(0.75)),2),')')))

RVEF <-`names<-`(data.frame(matrix(c("RVEF (%)",num1,num2),ncol = 3)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vtd_vi_index)
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$vtd_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$vtd_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$vtd_vi_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$vtd_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$vtd_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$vtd_vi_index,na.rm = T,probs = c(0.75)),2),')')))


LV_EDV_Ind <-`names<-`(data.frame(matrix(c("LV-EDV Indexed (%)",num1,num2),ncol = 3)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vts_vi_index)

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$vts_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$vts_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$vts_vi_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$vts_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$vts_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$vts_vi_index,na.rm = T,probs = c(0.75)),2),')')))

LV_ESV_Ind <-`names<-`(data.frame(matrix(c("LV-ESV Indexed (%)",num1,num2),ncol = 3)),columns)


#LV-Mass Indexed
nortest::ad.test(base$masa_vi_index)
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$masa_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$masa_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$masa_vi_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$masa_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$masa_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$masa_vi_index,na.rm = T,probs = c(0.75)),2),')')))

LV_Mass_Ind <-`names<-`(data.frame(matrix(c("LV-Mass Indexed (%)",num1,num2),ncol = 3)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vtd_v_df_a_index)

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$vtd_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$vtd_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$vtd_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$vtd_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$vtd_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$vtd_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

RV_EDV_Ind <-`names<-`(data.frame(matrix(c("RV-EDV Indexed (%)",num1,num2),ncol = 3)),columns)

#LV-ESV Indexed
nortest::ad.test(base$vts_v_df_a_index)
num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$vts_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$vts_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$vts_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$vts_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$vts_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$vts_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

RV_ESV_Ind <-`names<-`(data.frame(matrix(c("RV-ESV Indexed (%)",num1,num2),ncol = 3)),columns)


#AI
AI <- table(base$ai,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
AI.prop <- round(prop.table(table(base$ai,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
AI<-`names<-`(data.frame(c("Left-Atrium (%)"),
                         matrix(c(paste(AI,paste0('(',AI.prop,')'))),ncol = 2)),
              columns)

#AI
AD <- table(base$ad,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
AD.prop <- round(prop.table(table(base$ad,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
AD<-`names<-`(data.frame(c("Right-Atrium (%)"),
                         matrix(c(paste(AD,paste0('(',AD.prop,')'))),ncol = 2)),
              columns)

#VI
VI <- table(base$vi,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
VI.prop <- round(prop.table(table(base$vi,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
VI<-`names<-`(data.frame(c("Left-Ventricle (%)"),
                         matrix(c(paste(VI,paste0('(',VI.prop,')'))),ncol = 2)),
              columns)

#VD
VD <- table(base$vd_pared_libre,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
VD.prop <- round(prop.table(table(base$vd_pared_libre,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
VD<-`names<-`(data.frame(c("Right-Ventricle (%)"),
                         matrix(c(paste(VD,paste0('(',VD.prop,')'))),ncol = 2)),
              columns)

#Reinforced Areas

num1<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==0,]$zonas_de_reforzamiento_segmentos,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==0,]$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==0,]$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$REFORZAMIENTO_DIC==1,]$zonas_de_reforzamiento_segmentos,na.rm = T ),2),
              paste0('(',round(quantile(base[base$REFORZAMIENTO_DIC==1,]$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$REFORZAMIENTO_DIC==1,]$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.75)),2),')')))

Reinforced_Areas <-`names<-`(data.frame(matrix(c("Reinforced Areas (n)",num1,num2),ncol = 3)),columns)

#Localizacion Intramiocardio
Intra.myo <- table(base$localizacion_intramiocardico,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Intra.myo.prop <- round(prop.table(table(base$localizacion_intramiocardico,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Intramiocardio<-`names<-`(data.frame(c("Intra-myocardial (%)"),
                                     matrix(c(paste(Intra.myo,paste0('(',Intra.myo.prop,')'))),ncol = 2)),
                          columns)

#Localizacion Subendocardico
Subendo.myo <- table(base$localizacion_subendocardica,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Subendo.myo.prop <- round(prop.table(table(base$localizacion_subendocardica,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Subendo.myocardico<-`names<-`(data.frame(c("Subendocardial (%)"),
                                         matrix(c(paste(Subendo.myo,paste0('(',Subendo.myo.prop,')'))),ncol = 2)),
                              columns)

#Localizacion Subepicardico
Subepi.myo <- table(base$localizacion_subepicardico,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Subepi.myo.prop <- round(prop.table(table(base$localizacion_subepicardico,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Subepi.myocardico<-`names<-`(data.frame(c("Subepicardial (%)"),
                                        matrix(c(paste(Subepi.myo,paste0('(',Subepi.myo.prop,')'))),ncol = 2)),
                             columns)

#Localizacion Transmural
Trans.myo <- table(base$localizacion_transmural,base$REFORZAMIENTO_DIC,useNA = "always")[c(2,5)]
Trans.myo.prop <- round(prop.table(table(base$localizacion_transmural,base$REFORZAMIENTO_DIC,useNA = "always"),2),4)[c(2,5)]*100
Trans.myocardico<-`names<-`(data.frame(c("Transepicardical (%)"),
                                       matrix(c(paste(Trans.myo,paste0('(',Trans.myo.prop,')'))),ncol = 2)),
                            columns)


Table_1<-rbind(Sexo,Edad,Edad_Dx,Tiempo_Tx,ASC,IMC,Infrapeso,Normopeso,Sobrepso,Obesidad,Diabetes,Hipertension,Stroke,Otra.Comorb,
               Wold.P.Comorb,Bloqueo_AV,Fibrilacion_Atrial,Bloqueo_RAMA_DERECHO,Otra.Arri,
               Comunicacion_IA,Comunicacion_IV,Ductus_Arterial,Foramen_Oval,Otra.Anomalia.Fisica,
               Disnea,Palpitaciones,Edema,Fatiga,Taquicardia,Cianosis,NYHA,
               Hemoglobina,Hematocrito,
               Celermajer,Septal_Endorsement,Posterior_Endorsement,Right_Ventricle_Atrialization,Right_Ventricle_Atrialization_percent,
               LVEF,RVEF,LV_EDV_Ind,LV_ESV_Ind,LV_Mass_Ind,RV_EDV_Ind,RV_ESV_Ind,
               Reinforced_Areas,
               AI,AD,VI,VD,
               Intramiocardio,Subendo.myocardico,Subepi.myocardico,Trans.myocardico)


Table1.FINAL<-cbind(Table_0,Table_1[,-1])
Table1_Flex<-flextable::align(flextable::flextable(Table1.FINAL,cwidth=4),align="center",part="all")%>%flextable::autofit()
flextable::save_as_docx(Table1_Flex,path="Table_1.docx")


chisq.test(table(base$ai,base$REFORZAMIENTO_DIC))
fisher.test(table(base$localizacion_transmural,base$REFORZAMIENTO_DIC))
wilcox.test(base$vtd_v_df_a_index~base$vivo_0_muerto_1)
wilcox.test(base$hto~base$vivo_0_muerto_1)
t.test(base$porcion_atrializada_vd_percent~base$REFORZAMIENTO_DIC,var.equal = T)

table(base$imc_cat,base$vivo_0_muerto_1)
prop.table(table(base$edad,base$vivo_0_muerto_1),2)*100

#----Descriptive Between Death and Alive (Table 2)-----

columns <- c('Parameter',"Alive (n=45)","Death (n=12)")

#Sexo
sexo <- table(base$sexo,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
sexoprop <- round(prop.table(table(base$sexo,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Sexo<-`names<-`(data.frame("Men (%)",
                           matrix(c(paste(sexo,paste0('(',sexoprop,')'))),ncol = 2)),
                columns)
#Edad

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$EDAD_A_UC,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$EDAD_A_UC,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$EDAD_A_UC,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$EDAD_A_UC,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$EDAD_A_UC,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$EDAD_A_UC,na.rm = T,probs = c(0.75)),2),')')))

Edad<-`names<-`(data.frame(matrix(c("Age (Years)",num1,num2),ncol = 3)),columns)

#Edad al Diagnostic

nortest::ad.test(base$TIEMPO_AL_DIAGNOSTICO)
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$TIEMPO_AL_DIAGNOSTICO,na.rm = T,probs = c(0.75)),2),')')))

Edad_Dx<-`names<-`(data.frame(matrix(c("Age at Diagnosis (Years)",num1,num2),ncol = 3)),columns)

#Tiempo de Tratamiento

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$TIEMPO_TRATAMIENTO,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$TIEMPO_TRATAMIENTO,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$TIEMPO_TRATAMIENTO,na.rm = T,probs = c(0.75)),2),')')))

Tiempo_Tx<-`names<-`(data.frame(matrix(c("Time of Treatment (Years)",num1,num2),ncol = 3)),columns)


#IMC

IMC.media<-round(mean(base[base$vivo_0_muerto_1==0,]$imc,na.rm = T),2)
IMC.sd<-round(sd(base[base$vivo_0_muerto_1==0,]$imc,na.rm = T),2)
num1<-paste(IMC.media,"(","±",IMC.sd,")")

IMC.media<-round(mean(base[base$vivo_0_muerto_1==1,]$imc,na.rm = T),2)
IMC.sd<-round(sd(base[base$vivo_0_muerto_1==1,]$imc,na.rm = T),2)
num2<-paste(IMC.media,"(","±",IMC.sd,")")

IMC<-`names<-`(data.frame(matrix(c("BMI (Years)",num1,num2),ncol = 3)),columns)

#Underweight
Underweight.CAT <- table((base$imc<18.5),base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Underweight.CAT.prop <- round(prop.table(table((base$imc<18.5),base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Infrapeso<-`names<-`(data.frame("Underweight  (%)",
                                matrix(c(paste(Underweight.CAT,paste0('(',Underweight.CAT.prop,')'))),ncol = 2)),
                     columns)

#Normopeso
Normopeso.CAT <- table((base$imc>=18.5  & base$imc<25),base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Normopeso.CAT.prop <- round(prop.table(table((base$imc>=18.5  & base$imc<25),base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Normopeso<-`names<-`(data.frame("Normalweight  (%)",
                                matrix(c(paste(Normopeso.CAT,paste0('(',Normopeso.CAT.prop,')'))),ncol = 2)),
                     columns)

#Sobrepeso
Sobrepeso.CAT <- table((base$imc>=25  & base$imc<30),base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Sobrepeso.CAT.prop <- round(prop.table(table((base$imc>=25  & base$imc<30),base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Sobrepso<-`names<-`(data.frame("Overweight  (%)",
                               matrix(c(paste(Sobrepeso.CAT,paste0('(',Sobrepeso.CAT.prop,')'))),ncol = 2)),
                    columns)

#Obesidad
Obesidad.CAT <- table((base$imc>30),base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Obesidad.CAT.prop <- round(prop.table(table((base$imc>30),base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Obesidad<-`names<-`(data.frame("Obesity  (%)",
                               matrix(c(paste(Obesidad.CAT,paste0('(',Obesidad.CAT.prop,')'))),ncol = 2)),
                    columns)

#Diabetes
Diabetes <- table(base$diabetes,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Diabetes.prop <- round(prop.table(table(base$diabetes,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Diabetes<-`names<-`(data.frame("Diabetes (%)",
                               matrix(c(paste(Diabetes,paste0('(',Diabetes.prop,')'))),ncol = 2)),
                    columns)

#Arterial Hypertension
HAS <- table(base$hipertension_arterial,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
HAS.prop <- round(prop.table(table(base$hipertension_arterial,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Hipertension<-`names<-`(data.frame("Arterial Hypertension (%)",
                                   matrix(c(paste(HAS,paste0('(',HAS.prop,')'))),ncol = 2)),
                        columns)

#EVC
EVC <- table(base$evc,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
EVC.prop <- round(prop.table(table(base$evc,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Stroke<-`names<-`(data.frame("Stroke (%)",
                             matrix(c(paste(EVC,paste0('(',EVC.prop,')'))),ncol = 2)),
                  columns)

#Otra Comorbilidad
Otra.Com <- table(base$otra,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Otra.Com.prop <- round(prop.table(table(base$otra,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Otra.Comorb<-`names<-`(data.frame("Other Comorbidity (%)",
                                  matrix(c(paste(Otra.Com,paste0('(',Otra.Com.prop,')'))),ncol = 2)),
                       columns)


#Wolff-Parkinson White
Wold.P <- table(base$wolff_parkinson_white,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Wold.P.prop <- round(prop.table(table(base$wolff_parkinson_white,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Wold.P.Comorb<-`names<-`(data.frame("Wolff-Parkinson White (%)",
                                    matrix(c(paste(Wold.P,paste0('(',Wold.P.prop,')'))),ncol = 2)),
                         columns)

#Atrio-Ventricular Block
AV.B <- table(base$bloqueo_av,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
AV.B.prop <- round(prop.table(table(base$bloqueo_av,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Bloqueo_AV<-`names<-`(data.frame("Atrio-Ventricular Block (%)",
                                 matrix(c(paste(AV.B,paste0('(',AV.B.prop,')'))),ncol = 2)),
                      columns)

#Atrial Fibrilation
FA <- table(base$fa,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
FA.prop <- round(prop.table(table(base$fa,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Fibrilacion_Atrial<-`names<-`(data.frame("Atrial Fibrilation (%)",
                                         matrix(c(paste(FA,paste0('(',FA.prop,')'))),ncol = 2)),
                              columns)

#Bloqueo Rama Derecho
Bloqueo.RD <- table(base$bloqueo_rama_derecha,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Bloqueo.RD.prop <- round(prop.table(table(base$bloqueo_rama_derecha,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Bloqueo_RAMA_DERECHO<-`names<-`(data.frame("Right Bundle Branch Block (%)",
                                           matrix(c(paste(Bloqueo.RD,paste0('(',Bloqueo.RD.prop,')'))),ncol = 2)),
                                columns)

#Otra Arritmia
Otra.Arritmia <- table(base$bloqueo_rama_derecha,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Otra.Arritmia.prop <- round(prop.table(table(base$bloqueo_rama_derecha,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Otra.Arri<-`names<-`(data.frame("Other Athrhymia (%)",
                                matrix(c(paste(Otra.Arritmia,paste0('(',Otra.Arritmia.prop,')'))),ncol = 2)),
                     columns)

#Comunicacion IA
Com_IA <- table(base$comunicacion_ia,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Com_IA.prop <- round(prop.table(table(base$comunicacion_ia,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Comunicacion_IA<-`names<-`(data.frame("Interatial-Communication (%)",
                                      matrix(c(paste(Com_IA,paste0('(',Com_IA.prop,')'))),ncol = 2)),
                           columns)

#Comunicacion IV
Com_IV <- table(base$comunicacion_iv,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Com_IV.prop <- round(prop.table(table(base$comunicacion_iv,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Comunicacion_IV<-`names<-`(data.frame("Interventricular-Communication (%)",
                                      matrix(c(paste(Com_IV,paste0('(',Com_IV.prop,')'))),ncol = 2)),
                           columns)

#Persistencia del Ductus
DAP <- table(base$persistencia_del_ca,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
DAP.prop <- round(prop.table(table(base$persistencia_del_ca,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Ductus_Arterial<-`names<-`(data.frame("Patent Ductus Arteriosus (%)",
                                      matrix(c(paste(DAP,paste0('(',DAP.prop,')'))),ncol = 2)),
                           columns)

#Foramen Oval
FOP <- table(base$foramen_oval_permeable,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
FOP.prop <- round(prop.table(table(base$foramen_oval_permeable,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Foramen_Oval<-`names<-`(data.frame("Patent Foramen Ovale (%)",
                                   matrix(c(paste(FOP,paste0('(',FOP.prop,')'))),ncol = 2)),
                        columns)

#Otra
Otra <- table(base$otra,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Otra.prop <- round(prop.table(table(base$otra,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Otra.Anomalia.Fisica<-`names<-`(data.frame("Other Structural Impairment (%)",
                                           matrix(c(paste(Otra,paste0('(',Otra.prop,')'))),ncol = 2)),
                                columns)

#ASC

ASC.media<-round(mean(base[base$vivo_0_muerto_1==0,]$sct_m2_1_73,na.rm = T),2)
ASC.sd<-round(sd(base[base$vivo_0_muerto_1==0,]$sct_m2_1_73,na.rm = T),2)
num1<-paste(ASC.media,"(","±",ASC.sd,")")

ASC.media<-round(mean(base[base$vivo_0_muerto_1==1,]$sct_m2_1_73,na.rm = T),2)
ASC.sd<-round(sd(base[base$vivo_0_muerto_1==1,]$sct_m2_1_73,na.rm = T),2)
num2<-paste(ASC.media,"(","±",ASC.sd,")")

ASC<-`names<-`(data.frame(matrix(c("Superfitial Corporal Area (m2/1.73)",num1,num2),ncol = 3)),columns)

#Disnea
disnea <- table(base$disnea,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
disneaprop <- round(prop.table(table(base$disnea,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Disnea<-`names<-`(data.frame("Dysnea (%)",
                             matrix(c(paste(disnea,paste0('(',disneaprop,')'))),ncol = 2)),
                  columns)

#Palpitaciones
Palpita <- table(base$palpitaciones,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Palpitaprop <- round(prop.table(table(base$palpitaciones,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Palpitaciones<-`names<-`(data.frame("Palpitations (%)",
                                    matrix(c(paste(Palpita,paste0('(',Palpitaprop,')'))),ncol = 2)),
                         columns)


#Edema
Edema <- table(base$edema,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Edemaprop <- round(prop.table(table(base$edema,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Edema<-`names<-`(data.frame("Edema (%)",
                            matrix(c(paste(Edema,paste0('(',Edemaprop,')'))),ncol = 2)),
                 columns)

#Fatiga
Fatigue <- table(base$fatiga,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Fatigueprop <- round(prop.table(table(base$fatiga,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Fatiga<-`names<-`(data.frame("Fatigue (%)",
                             matrix(c(paste(Fatigue,paste0('(',Fatigueprop,')'))),ncol = 2)),
                  columns)

#Taquicardia
Taquicardia <- table(base$taquicardia,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Taquicardiaprop <- round(prop.table(table(base$taquicardia,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Taquicardia<-`names<-`(data.frame("Tachycardia (%)",
                                  matrix(c(paste(Taquicardia,paste0('(',Taquicardiaprop,')'))),ncol = 2)),
                       columns)

#Cianosis
Cianos <- table(base$cianosis,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Cianosprop <- round(prop.table(table(base$cianosis,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Cianosis<-`names<-`(data.frame("Cyanosis (%)",
                               matrix(c(paste(Cianos,paste0('(',Cianosprop,')'))),ncol = 2)),
                    columns)

#Functional Class
Clase.Funcional <- table(base$clase_funcional,base$vivo_0_muerto_1,useNA = "always")[c(1:3,5:7)]
Clase.Funcional.prop <- round(prop.table(table(base$clase_funcional,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(1:3,5:7)]*100
NYHA<-`names<-`(data.frame(c("I (%)","II (%)","III (%)"),
                           matrix(c(paste(Clase.Funcional,paste0('(',Clase.Funcional.prop,')'))),ncol = 2)),
                columns)

#HB
base$hb<-as.numeric(base$hb)
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$hb,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$hb,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$hb,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$hb,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$hb,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$hb,na.rm = T,probs = c(0.75)),2),')')))

Hemoglobina<-`names<-`(data.frame(matrix(c("Hemoglobin (gr/dl)",num1,num2),ncol = 3)),columns)

#HB
base$hto<-as.numeric(base$hto)
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$hto,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$hto,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$hto,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$hto,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$hto,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$hto,na.rm = T,probs = c(0.75)),2),')')))

Hematocrito<-`names<-`(data.frame(matrix(c("Hematocrit (%)",num1,num2),ncol = 3)),columns)

#Celemajer
base$celermajer<-as.numeric(base$celermajer)
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$celermajer,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$celermajer,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$celermajer,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$celermajer,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$celermajer,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$celermajer,na.rm = T,probs = c(0.75)),2),')')))

Celermajer<-`names<-`(data.frame(matrix(c("Celermajer Index (XXX)",num1,num2),ncol = 3)),columns)


#Adosamiento VT Septal
base$adosamiento_vt_septal<-as.numeric(base$adosamiento_vt_septal)

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$adosamiento_vt_septal,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$adosamiento_vt_septal,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$adosamiento_vt_septal,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$adosamiento_vt_septal,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$adosamiento_vt_septal,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$adosamiento_vt_septal,na.rm = T,probs = c(0.75)),2),')')))

Septal_Endorsement <-`names<-`(data.frame(matrix(c("Septal Endorsement (XXX)",num1,num2),ncol = 3)),columns)

#Adosamiento VT Posterior
base$adosamiento_vt_posterior<-as.numeric(base$adosamiento_vt_posterior)
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$adosamiento_vt_posterior,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$adosamiento_vt_posterior,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$adosamiento_vt_posterior,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$adosamiento_vt_posterior,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$adosamiento_vt_posterior,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$adosamiento_vt_posterior,na.rm = T,probs = c(0.75)),2),')')))


Posterior_Endorsement <-`names<-`(data.frame(matrix(c("Posterior Endorsement (XXX)",num1,num2),ncol = 3)),columns)

#Right_Ventricle_Atrialization (mm)
base$porcion_atrializada_vd_mm<-as.numeric(base$porcion_atrializada_vd_mm)

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$porcion_atrializada_vd_mm,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$porcion_atrializada_vd_mm,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$porcion_atrializada_vd_mm,na.rm = T,probs = c(0.75)),2),')')))


Right_Ventricle_Atrialization <-`names<-`(data.frame(matrix(c("Right Ventricle - Atrialization (mm)",num1,num2),ncol = 3)),columns)


#Right_Ventricle_Atrialization (%)
base$porcion_atrializada_vd_percent<-as.numeric(base$porcion_atrializada_vd_percent)

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$porcion_atrializada_vd_percent,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$porcion_atrializada_vd_percent,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$porcion_atrializada_vd_percent,na.rm = T,probs = c(0.75)),2),')')))

Right_Ventricle_Atrialization_percent <-`names<-`(data.frame(matrix(c("Right Ventricle - Atrialization (%)",num1,num2),ncol = 3)),columns)

#LVEF
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$fevi_49,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$fevi_49,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$fevi_49,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$fevi_49,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$fevi_49,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$fevi_49,na.rm = T,probs = c(0.75)),2),')')))

LVEF <-`names<-`(data.frame(matrix(c("LVEF (%)",num1,num2),ncol = 3)),columns)

#RVEF
base$fevd<-as.numeric(base$fevd)

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$fevd,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$fevd,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$fevd,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$fevd,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$fevd,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$fevd,na.rm = T,probs = c(0.75)),2),')')))

RVEF <-`names<-`(data.frame(matrix(c("RVEF (%)",num1,num2),ncol = 3)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vtd_vi_index)
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$vtd_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$vtd_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$vtd_vi_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$vtd_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$vtd_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$vtd_vi_index,na.rm = T,probs = c(0.75)),2),')')))


LV_EDV_Ind <-`names<-`(data.frame(matrix(c("LV-EDV Indexed (%)",num1,num2),ncol = 3)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vts_vi_index)

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$vts_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$vts_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$vts_vi_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$vts_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$vts_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$vts_vi_index,na.rm = T,probs = c(0.75)),2),')')))

LV_ESV_Ind <-`names<-`(data.frame(matrix(c("LV-ESV Indexed (%)",num1,num2),ncol = 3)),columns)


#LV-Mass Indexed
nortest::ad.test(base$masa_vi_index)
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$masa_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$masa_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$masa_vi_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$masa_vi_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$masa_vi_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$masa_vi_index,na.rm = T,probs = c(0.75)),2),')')))

LV_Mass_Ind <-`names<-`(data.frame(matrix(c("LV-Mass Indexed (%)",num1,num2),ncol = 3)),columns)

#LV-EDV Indexed
nortest::ad.test(base$vtd_v_df_a_index)

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$vtd_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$vtd_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$vtd_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$vtd_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$vtd_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$vtd_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

RV_EDV_Ind <-`names<-`(data.frame(matrix(c("RV-EDV Indexed (%)",num1,num2),ncol = 3)),columns)

#LV-ESV Indexed
nortest::ad.test(base$vts_v_df_a_index)
num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$vts_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$vts_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$vts_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$vts_v_df_a_index,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$vts_v_df_a_index,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$vts_v_df_a_index,na.rm = T,probs = c(0.75)),2),')')))

RV_ESV_Ind <-`names<-`(data.frame(matrix(c("RV-ESV Indexed (%)",num1,num2),ncol = 3)),columns)


#AI
AI <- table(base$ai,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
AI.prop <- round(prop.table(table(base$ai,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
AI<-`names<-`(data.frame(c("Left-Atrium (%)"),
                         matrix(c(paste(AI,paste0('(',AI.prop,')'))),ncol = 2)),
              columns)

#AI
AD <- table(base$ad,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
AD.prop <- round(prop.table(table(base$ad,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
AD<-`names<-`(data.frame(c("Right-Atrium (%)"),
                         matrix(c(paste(AD,paste0('(',AD.prop,')'))),ncol = 2)),
              columns)

#VI
VI <- table(base$vi,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
VI.prop <- round(prop.table(table(base$vi,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
VI<-`names<-`(data.frame(c("Left-Ventricle (%)"),
                         matrix(c(paste(VI,paste0('(',VI.prop,')'))),ncol = 2)),
              columns)

#VD
VD <- table(base$vd_pared_libre,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
VD.prop <- round(prop.table(table(base$vd_pared_libre,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
VD<-`names<-`(data.frame(c("Right-Ventricle (%)"),
                         matrix(c(paste(VD,paste0('(',VD.prop,')'))),ncol = 2)),
              columns)

#Reinforced Areas

num1<-c(paste(round(median(base[base$vivo_0_muerto_1==0,]$zonas_de_reforzamiento_segmentos,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==0,]$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==0,]$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.75)),2),')')))

num2<-c(paste(round(median(base[base$vivo_0_muerto_1==1,]$zonas_de_reforzamiento_segmentos,na.rm = T ),2),
              paste0('(',round(quantile(base[base$vivo_0_muerto_1==1,]$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.25)),2),"-",
                     round(quantile(base[base$vivo_0_muerto_1==1,]$zonas_de_reforzamiento_segmentos,na.rm = T,probs = c(0.75)),2),')')))

Reinforced_Areas <-`names<-`(data.frame(matrix(c("Reinforced Areas (n)",num1,num2),ncol = 3)),columns)

#Localizacion Intramiocardio
Intra.myo <- table(base$localizacion_intramiocardico,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Intra.myo.prop <- round(prop.table(table(base$localizacion_intramiocardico,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Intramiocardio<-`names<-`(data.frame(c("Intra-myocardial (%)"),
                                     matrix(c(paste(Intra.myo,paste0('(',Intra.myo.prop,')'))),ncol = 2)),
                          columns)

#Localizacion Subendocardico
Subendo.myo <- table(base$localizacion_subendocardica,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Subendo.myo.prop <- round(prop.table(table(base$localizacion_subendocardica,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Subendo.myocardico<-`names<-`(data.frame(c("Subendocardial (%)"),
                                         matrix(c(paste(Subendo.myo,paste0('(',Subendo.myo.prop,')'))),ncol = 2)),
                              columns)

#Localizacion Subepicardico
Subepi.myo <- table(base$localizacion_subepicardico,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Subepi.myo.prop <- round(prop.table(table(base$localizacion_subepicardico,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Subepi.myocardico<-`names<-`(data.frame(c("Subepicardial (%)"),
                                        matrix(c(paste(Subepi.myo,paste0('(',Subepi.myo.prop,')'))),ncol = 2)),
                             columns)

#Localizacion Transmural
Trans.myo <- table(base$localizacion_transmural,base$vivo_0_muerto_1,useNA = "always")[c(2,5)]
Trans.myo.prop <- round(prop.table(table(base$localizacion_transmural,base$vivo_0_muerto_1,useNA = "always"),2),4)[c(2,5)]*100
Trans.myocardico<-`names<-`(data.frame(c("Transepicardical (%)"),
                                       matrix(c(paste(Trans.myo,paste0('(',Trans.myo.prop,')'))),ncol = 2)),
                            columns)

Table_2<-rbind(Sexo,Edad,Edad_Dx,Tiempo_Tx,ASC,IMC,Infrapeso,Normopeso,Sobrepso,Obesidad,Diabetes,Hipertension,Stroke,Otra.Comorb,
               Wold.P.Comorb,Bloqueo_AV,Fibrilacion_Atrial,Bloqueo_RAMA_DERECHO,Otra.Arri,
               Comunicacion_IA,Comunicacion_IV,Ductus_Arterial,Foramen_Oval,Otra.Anomalia.Fisica,
               Disnea,Palpitaciones,Edema,Fatiga,Taquicardia,Cianosis,NYHA,
               Hemoglobina,Hematocrito,
               Celermajer,Septal_Endorsement,Posterior_Endorsement,Right_Ventricle_Atrialization,Right_Ventricle_Atrialization_percent,
               LVEF,RVEF,LV_EDV_Ind,LV_ESV_Ind,LV_Mass_Ind,RV_EDV_Ind,RV_ESV_Ind,
               Reinforced_Areas,
               AI,AD,VI,VD,
               Intramiocardio,Subendo.myocardico,Subepi.myocardico,Trans.myocardico)


Table2.FINAL<-cbind(Table_0,Table_2[,-1])
Table2_Flex<-flextable::align(flextable::flextable(Table2.FINAL,cwidth=4),align="center",part="all")%>%flextable::autofit()
flextable::save_as_docx(Table2_Flex,path="Table_2.docx")


chisq.test(table(base$ai,base$vivo_0_muerto_1))

fisher.test(table(base$localizacion_transmural,base$vivo_0_muerto_1))
wilcox.test(base$zonas_de_reforzamiento_segmentos~base$vivo_0_muerto_1)
t.test(base$edad~base$vivo_0_muerto_1,var.equal = T)

table(base$imc_cat,base$vivo_0_muerto_1)
prop.table(table(base$edad,base$vivo_0_muerto_1),2)*100


#----Figure 1 (LGE +)-----

base$LGE<-NULL
base$LGE[base$ai==1]<-1
base$LGE[base$vi==1]<-1
base$LGE[base$ad==1]<-1
base$LGE[base$vd_pared_libre==1]<-1

ncas <- table(base$LGE==1,useNA = "always")[1]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

base$COR_DER<-NULL
base$COR_DER[base$ad==1]<-1
base$COR_DER[base$vd_pared_libre==1]<-1

ncas <- table(base$COR_DER==1,useNA = "always")[1]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
          conf.level = 0.95) * 100)[1:4]

ncas <- table(base$ai==1,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
AI.df.1<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                conf.level = 0.95) * 100)[1:4]

ncas <- table(base$vi==1,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
VI.df.1<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

ncas <- table(base$ad==1,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
AD.df.1<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

ncas <- table(base$vd_pared_libre==1,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
VD.df.1<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

fig.1.df.1<-round(rbind(AI.df.1,VI.df.1,AD.df.1,VD.df.1),2)
fig.1.df.1$group<-c("Left-Atrium","Left-Ventricle","Right-Atrium","Right-Ventricle")
fig.1.df.1$group<-factor(fig.1.df.1$group,levels = c("Left-Atrium","Left-Ventricle","Right-Atrium","Right-Ventricle"))


Fig.1.A<-ggplot(fig.1.df.1,aes(group, est)) +
  geom_col(fill=c("#002d9c" ,"#005d5d" ,"#ffa300", "#b30000")) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  theme_classic()+
  coord_flip()+
  xlab("")+
  ylab("Percentage, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 19),
    position = position_dodge(0.9),
    vjust = 0.5)+
  scale_y_continuous(limits = c(-.1,50))+
  labs(fill="Type")+
  ggtitle("Fibrosis by Chambers")+
  theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))

#By Segments
#BASAL
ncas <- table(base$basal_anterior,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
BA.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

ncas <- table(base$basal_anterolateral,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
BAL.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

ncas <- table(base$basal_anteroseptal,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
BAS.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

ncas <- table(base$basal_inferior,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
BI.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

ncas <- table(base$basal_inferolateral,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
BIL.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

ncas <- table(base$basal_inferoseptal,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
BIS.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

fig.2.df.2.1<-round(rbind(BA.df.2,BAL.df.2,BAS.df.2,BI.df.2,BIL.df.2,BIS.df.2),2)
fig.2.df.2.1$subgroup<-c("Basal-Anterior","Basal-Anterolateral","Basal-Anteroseptal","Basal-Inferior","Basal-Inferolateral","Basal-Inferoseptal")
fig.2.df.2.1$group<-c("Basal")

#Medio
ncas <- table(base$medio_anterior,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
MA.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

ncas <- table(base$medio_anterolateral,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
MAL.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

ncas <- table(base$medio_anteroseptal,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
MAS.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

ncas <- table(base$medio_inferior,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
MI.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

ncas <- table(base$medio_inferolateral,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
MIL.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

ncas <- table(base$medio_inferoseptal,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
MIS.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

fig.2.df.2.2<-round(rbind(MA.df.2,MAL.df.2,MAS.df.2,MI.df.2,MIL.df.2,MIS.df.2),2)
fig.2.df.2.2$subgroup<-c("Mid-Anterior","Mid-Anterolateral","Mid-Anteroseptal","Mid-Inferior","Mid-Inferolateral","Mid-Inferoseptal")
fig.2.df.2.2$group<-c("Mid")

fig.2.df.2<-rbind(fig.2.df.2.1,fig.2.df.2.2)

#Apical

ncas <- table(base$apical_anterior,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
AA.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

ncas <- table(base$apical_septal,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
AS.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

ncas <- table(base$apical_inferior,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
AI.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

ncas <- table(base$apical_lateral,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
AL.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

ncas <- table(base$apex,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
Apex.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

fig.2.df.2.3<-round(rbind(AA.df.2,AS.df.2,AI.df.2,AL.df.2,Apex.df.2),2)
fig.2.df.2.3$subgroup<-c("Apical-Anterior","Apical-Septal","Apical-Inferior","Apical-Lateral","Apex")
fig.2.df.2.3$group<-c("Apex")

#Merge Dataset
fig.2.df.2<-rbind(fig.2.df.2.1,fig.2.df.2.2,fig.2.df.2.3)
fig.2.df.2$group<-factor(fig.2.df.2$group,levels = c("Basal","Mid","Apex"))

Fig.1.B<-fig.2.df.2 %>%
  mutate(subgroup = fct_reorder(subgroup, est)) %>%
  ggplot( aes(x=subgroup, y=est,fill=group))+
  geom_col() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  coord_flip()+
  theme_classic()+
  xlab("")+
  ylab("Percentage, (%)")+
  geom_text(
    aes(label = paste0(est,"%"," ","(",lower,"-",upper,")"), y = est + 23.0),
    position = position_dodge(0.9),
    vjust = 0.5)+
  scale_y_continuous(limits = c(-.1,50))+
  labs(fill="Segment")+
  ggtitle("Fibrosis by Segment in Left-Ventricle")+
  scale_fill_gdocs()+
  theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))

#By Strat
#Subendocardica
ncas <- table(base$localizacion_subendocardica,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
SUB.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                   conf.level = 0.95) * 100)[1:4]

#Intramiocardico
ncas <- table(base$localizacion_intramiocardico,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
INTRA.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                    conf.level = 0.95) * 100)[1:4]

#Subepicardica
ncas <- table(base$localizacion_subepicardico,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
SUBEPI.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                      conf.level = 0.95) * 100)[1:4]

#Transmural
ncas <- table(base$localizacion_transmural,useNA = "always")[2]; npop <- nrow(base)
tmp <- as.matrix(cbind(ncas, npop))
TRANS.df.2<-(epi.conf(tmp, ctype = "prevalence", method = "wilson", N = 100, design = 1, 
                       conf.level = 0.95) * 100)[1:4]

fig.2.df.3<-round(rbind(SUB.df.2,INTRA.df.2,SUBEPI.df.2,TRANS.df.2),2)
fig.2.df.3$group<-c("Sub-Endocardial","Mid-Wall","Sup-Epicardiac","Transmural")
fig.2.df.3$group<-factor(fig.2.df.3$group,levels = c("Mid-Wall","Transmural","Sub-Endocardial","Sup-Epicardiac"))

Fig.1.C<-ggplot(fig.2.df.3,aes(group, est,fill=group)) +
  geom_col() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  coord_flip()+
  theme_classic()+
  xlab("")+
  ylab("Percentage, (%)")+
  geom_text(
    aes(label = paste0(est,"%","\n","(",lower,"-",upper,")"), y = est + 20.0),
    position = position_dodge(0.9),
    vjust = 0.5)+
  scale_y_continuous(limits = c(-.1,50))+
  labs(fill="Location")+
  scale_fill_colorblind()+
  theme(legend.position = "none")+
  ggtitle("Fibrosis by Location")+
  theme(axis.text.x = element_text(size = 14),axis.text.y = element_text(size = 14))

Fig.1.LEFT<-ggarrange(Fig.1.A,Fig.1.C,ncol = 1, nrow = 2,labels = c("A","B"))
Fig.1<-ggarrange(Fig.1.LEFT,Fig.1.B,ncol = 2, nrow = 1,labels = c("","C"))

ggsave(Fig.1,
       filename = "Figure1_V2.pdf", 
       width = 55, 
       height = 20,
       units=c("cm"),
       dpi = 600,
       limitsize = FALSE)

#----Logistic and Poisson Regression Model#####

m1<-glm(REFORZAMIENTO_DIC~fevi_49+fevd+vd_pared_libre+vtd_vi_index+vtd_v_df_a_index,family = "binomial",data = base)
summary(m1)
car::vif(m1)


m1<-MASS::glm.nb(zonas_de_reforzamiento_segmentos~fevi_49+fevd+vd_pared_libre+vtd_vi_index+vtd_v_df_a_index,data = base)
summary(m1)
library(pscl)
odTest(m1)


#----Cox Regresion Model (Tabla 3)-----

#VD

m0<-coxph(Surv(EDAD_A_UC, vivo_0_muerto_1)~ vd_pared_libre+TIEMPO_TRATAMIENTO, data=base)
summary(m0);BIC(m0)
cox.m1.ph<-cox.zph(m0)
survminer::ggcoxzph(cox.m1.ph)


#VI Modelo 1
v1.m1<-coxph(Surv(EDAD_A_UC, vivo_0_muerto_1)~ vi, data=base)
summary(v1.m1);BIC(v1.m1)
cox.m1.ph<-cox.zph(v1.m1)
survminer::ggcoxzph(cox.m1.ph)
cph(Surv(EDAD_A_UC, vivo_0_muerto_1) ~ vi, data=base)


#VI Modelo 2
v1.m2<-coxph(Surv(EDAD_A_UC, vivo_0_muerto_1)~ vi+sexo+sct_m2_1_73, data=base)
summary(v1.m2);BIC(v1.m2)
cox.m2.ph<-cox.zph(v1.m2)
survminer::ggcoxzph(cox.m2.ph)[1]
cph(Surv(EDAD_A_UC, vivo_0_muerto_1) ~ vi+sexo+sct_m2_1_73, data=base)

#VI Modelo 3

v1.m3<-coxph(Surv(EDAD_A_UC, vivo_0_muerto_1)~ vi+sexo+NUM_COMORBILIDADES+sct_m2_1_73+masa_vi_index+porcion_atrializada_vd_percent+TIEMPO_TRATAMIENTO, data=base)
summary(v1.m3);BIC(v1.m3);car::vif(v1.m3)
cox.m3.ph<-cox.zph(v1.m3)
survminer::ggcoxzph(cox.m3.ph)[1]
cph(Surv(EDAD_A_UC, vivo_0_muerto_1) ~ vi+sexo+NUM_COMORBILIDADES+sct_m2_1_73+masa_vi_index+porcion_atrializada_vd_percent, data=base)

#----Figure 2 (Survival Analysis)-----

library(flexsurv)
gomp1<-flexsurvreg(Surv(EDAD_A_UC,vivo_0_muerto_1)~factor(vi),dist="gompertz",data=base)
ggflexsurvplot(gomp1,data = base,conf.int = T,
               risk.table = T,
               xlim = c(0,70),
               ylim= c(0,1.0),palette="jama")+
  theme_survminer(base_size = 9,base_family = "Arial")+
  guides(colour = guide_legend(nrow = 1))

#VI
mod1_km<-survfit(Surv(EDAD_A_UC, vivo_0_muerto_1) ~factor(vi), data = base)
KM_fig1<-ggsurvplot(mod1_km, data = base, size = 1,palette ="jama",conf.int = T,
                    risk.table = T,
                    ggtheme = theme_classic(),
                    xlab="Age (Years)",
                    ylab="Survival Probability, (%)",
                    title="Death by Any Cause",
                    legend.labs = c("LV-LGE (-)",
                                    "LV-LGE (+)"),
                    xlim = c(0,40),
                    ylim= c(0,1.0),
                    break.y.by= c(0.1),
                    break.x.by= c(10),
                    pval = TRUE, 
                    pval.method = TRUE,
                    log.rank.weights = "S1", 
                    pval.method.size = 3,
                    pval.coord = c(10, 0.20),
                    pval.method.coord = c(10, 0.15))+
  theme_survminer(base_size = 9,base_family = "Arial")+
  guides(colour = guide_legend(nrow = 1))

KM_fig1 <-KM_fig1 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))

KM1<-ggarrange(KM_fig1$plot, KM_fig1$table, heights = c(2, 0.7),
               ncol = 1, nrow = 2);KM1


#VD
mod2_km<-survfit(Surv(EDAD_A_UC, vivo_0_muerto_1) ~factor(vd_pared_libre), data = base)
KM_fig2<-ggsurvplot(mod2_km, data = base, size = 1,palette ="jama",conf.int = T,
                    risk.table = T,
                    ggtheme = theme_classic(),
                    xlab="Age (Years)",
                    ylab="Survival Probability, (%)",
                    title="Death by Any Cause",
                    legend.labs = c("RV-LGE (-)",
                                    "RV-LGE (+)"),
                    xlim = c(0,40),
                    ylim= c(0,1.0),
                    break.y.by= c(0.1),
                    break.x.by= c(10),
                    pval = TRUE, 
                    pval.method = TRUE,
                    log.rank.weights = "S1", 
                    pval.method.size = 3,
                    pval.coord = c(10, 0.20),
                    pval.method.coord = c(10, 0.15))+
  theme_survminer(base_size = 9,base_family = "Arial")+
  guides(colour = guide_legend(nrow = 1))

KM_fig2 <-KM_fig2 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))

KM2<-ggarrange(KM_fig2$plot, KM_fig2$table, heights = c(2, 0.7),
               ncol = 1, nrow = 2);KM2

#VD+VI
mod3_km<-survfit(Surv(EDAD_A_UC, vivo_0_muerto_1) ~factor(VI_VD), data = base)
KM_fig3<-ggsurvplot(mod3_km, data = base, size = 1,palette ="jama",conf.int = T,
                    risk.table = T,
                    ggtheme = theme_classic(),
                    xlab="Age (Years)",
                    ylab="Survival Probability, (%)",
                    title="Death by Any Cause",
                    legend.labs = c("RV+LV - LGE (-)",
                                    "RV+LV - LGE (+)"),
                    xlim = c(0,40),
                    ylim= c(0,1.0),
                    break.y.by= c(0.1),
                    break.x.by= c(10),
                    pval = TRUE, 
                    pval.method = TRUE,
                    log.rank.weights = "S1", 
                    pval.method.size = 3,
                    pval.coord = c(10, 0.20),
                    pval.method.coord = c(10, 0.15))+
  theme_survminer(base_size = 9,base_family = "Arial")+
  guides(colour = guide_legend(nrow = 1))


KM_fig3 <-KM_fig3 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))

KM3<-ggarrange(KM_fig3$plot, KM_fig3$table, heights = c(2, 0.7),
               ncol = 1, nrow = 2);KM3


Figure3<-ggpubr::ggarrange(KM1,KM2,ncol = 2,nrow = 1,labels =LETTERS[1:3],common.legend = T)
ggsave(Figure3,
       filename = "Figure3_V2.png", 
       width = 25, 
       height = 12.5,
       units=c("cm"),
       dpi = 800,
       limitsize = FALSE)

# Execute test
PetoPeto.1 <- survdiff(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ factor(vd_pared_libre), rho = 1, data = base)$chisq
1 - pchisq(PetoPeto.1,1)

# Execute test
PetoPeto.2 <- survdiff(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ factor(vi), rho = 1, data = base)$chisq
1 - pchisq(PetoPeto.2,1)

#----Figure 3 (HR Plots)-----

#VI - Subepicardico
m1.1<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ localizacion_subepicardico, data=base)
summary(m1.1);BIC(m1.1)

#VI - Subendocardico
m1.2<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ localizacion_subendocardica+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m1.2);BIC(m1.2)

#VI -  Transmural

m1.3<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ localizacion_transmural+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m1.3);BIC(m1.3)

#VI - Intramiocardico
base$localizacion_intramiocardico<-na.tools::na.replace(base$localizacion_intramiocardico,0)
m1.4<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ localizacion_intramiocardico+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m1.4);BIC(m1.4)

Figure3A<-jtools::plot_summs(m1.1,m1.2,m1.3,m1.4, legend.title = "Location",
                             colors = "Qual1", 
                             coefs = c("lel1"="localizacion_subepicardico",
                                       "lel2"="localizacion_subendocardica",
                                       "lel3"="localizacion_transmural",
                                       "lel4"="localizacion_intramiocardico"),
                             exp=TRUE,
                             model.names = c("Sup-Epicardiac","Sub-Endocardial", "Transmural","Mid-Wall"))+
  xlab("Cox Proportional Hazard Regresion Model\nHazard Ratio (95% CI)")+ylab("")+
  theme_classic()+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position = "top")+
  labs(title = "Location of Fibrosis in Left-Ventricle",caption = "")+
  guides(colour = guide_legend(nrow = 1))+
  geom_text(inherit.aes = FALSE, aes(x = 50, y = 1.1, label = "HR: 28.72 (95% CI: 3.42-240.75, p=0.002)", vjust = -0.5))+
  geom_text(inherit.aes = FALSE, aes(x = 50, y = 2.1, label = "HR: 0.18 (95% CI: 0.01-6.13, p=0.347)", vjust = -0.5))+
  geom_text(inherit.aes = FALSE, aes(x = 50, y = 3.1, label = "HR: 0.18 (95% CI: 0.01-6.11, p=0.345)", vjust = -0.5))+
  geom_text(inherit.aes = FALSE, aes(x = 50, y = 4.1, label = "HR: 0.01 (95% CI: 0.01-250, p=0.999)", vjust = -0.5))+
  scale_x_continuous(limits = c(0,250))


#VI - Septal
m2.1<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ SEPTAL+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m2.1);BIC(m2.1)

#VI - Lateral
m2.2<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ LATERAL+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m2.2);BIC(m2.2)

#VI - Anterior
m2.3<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ ANTERIOR+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m2.3);BIC(m2.3)

#VI - Inferior
m2.4<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ INFERIOR+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m2.4);BIC(m2.4)


Figure3B<-jtools::plot_summs(m2.1,m2.2,m2.3,m2.4, legend.title = "Region",
                             colors = "Qual2", 
                             coefs = c("lel1"="SEPTAL",
                                       "lel2"="LATERAL",
                                       "lel3"="ANTERIOR",
                                       "lel4"="INFERIOR"),
                             exp=TRUE,
                             model.names = c("Septal","Lateral", "Anterior","Inferior"))+
  xlab("Cox Proportional Hazard Regresion Model\nHazard Ratio (95% CI)")+ylab("")+
  theme_classic()+
  theme(axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank(),legend.position = "top")+
  labs(title = "Region of Fibrosis in Left-Ventricle",caption = "Models adjusted for: Sex, BSA and Comorbidities, LVEF, LV Mass and Right Ventricle Atrialization")+
  guides(colour = guide_legend(nrow = 1))+
  geom_text(inherit.aes = FALSE, aes(x = 35, y = 1.1, label = "HR: 1.39 (95% CI: 0.103-18.85, p=0.803)", vjust = -0.5))+
  geom_text(inherit.aes = FALSE, aes(x = 35, y = 2.1, label = "HR: 1.06 (95% CI: 0.117-9.44, p=0.964)", vjust = -0.5))+
  geom_text(inherit.aes = FALSE, aes(x = 35, y = 3.1, label = "HR: 12.73 (95% CI: 1.92-84.35, p=0.008)", vjust = -0.5))+
  geom_text(inherit.aes = FALSE, aes(x = 35, y = 4.1, label = "HR: 0.87 (95% CI: 0.06-12.11, p=0.922)", vjust = -0.5))


#VI - Basal
m3.1<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ BASAL+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m3.1);BIC(m3.1)

#VI - Medio
m3.2<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ MEDIOS+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m3.2);BIC(m3.2)

#VI - Apical
m3.3<-coxph(Surv(TIEMPO_TRATAMIENTO, vivo_0_muerto_1)~ APICAL+imc+NUM_COMORBILIDADES+sexo+celermajer, data=base)
summary(m3.3);BIC(m3.3)

Figure4<-ggarrange(Figure3A,Figure3B,ncol = 2,nrow = 1,common.legend = F)

ggsave(Figure4,
       filename = "Figure5.pdf", 
       width = 40, 
       height = 13,
       units=c("cm"),
       dpi = 600,
       limitsize = FALSE)

#----Predictive Model----

base$sexo_2<-NULL
base$sexo_2[base$sexo==0]<-0
base$sexo_2[base$sexo==1]<-1

base$vi_2<-NULL
base$vi_2[base$vi==0]<-0
base$vi_2[base$vi==1]<-1

#Modelo Basal

pred.1<-coxph(Surv(EDAD_A_UC, vivo_0_muerto_1)~ vi+sexo+fevd+sct_m2_1_73, data=base)
summary(pred.1);BIC(pred.1);car::vif(pred.1)
cph(Surv(EDAD_A_UC, vivo_0_muerto_1) ~ vi+sexo+fevd+sct_m2_1_73, data=base)
round(coef(pred.1)/min(abs(coef(pred.1))),1)
round(coef(pred.1))

#Puntaje Version Continua
base$score<-base$vi_2*2+base$sexo_2*4+base$sct_m2_1_73*(-6)+base$fevd*(-0.1)
base$score_3<-(base$score-round(min(base$score)))
cph(Surv(EDAD_A_UC, vivo_0_muerto_1) ~ score_3, data=base)

mod1_pts<-coxph(Surv(EDAD_A_UC, vivo_0_muerto_1)~score_3, data=base)
summary(mod1_pts);cox.zph(mod1_pts);BIC(mod1_pts)
(1-predict(mod1_pts,type="survival"))*100
cox.m1.ph<-cox.zph(mod1_pts)
survminer::ggcoxzph(cox.m1.ph)

#Categorical Model
base$score_cat_3<-NULL
base$score_cat_3[base$score_3<=6.5]<-1;base$score_cat_3[base$score_3>6.5]<-2
base$score_cat_3<-factor(base$score_cat_3)
table(base$score_cat_3,base$vivo_0_muerto_1)


mod2_pts<-coxph(Surv(EDAD_A_UC, vivo_0_muerto_1)~score_cat_3, data=base)
summary(mod2_pts);cox.zph(mod2_pts);BIC(mod2_pts)
cph(Surv(EDAD_A_UC, vivo_0_muerto_1) ~ score_cat_3, data=base)
(1-predict(mod2_pts,type="survival"))*100
cox.m1.ph<-cox.zph(mod2_pts)
survminer::ggcoxzph(mod2_pts)


#Kaplan Meier Curve Analisis
mod2_pts<-coxph(Surv(EDAD_A_UC, vivo_0_muerto_1)~factor(score_cat_3), data=base)
summary(mod2_pts);cox.zph(mod2_pts)

mod1_km<-survfit(Surv(EDAD_A_UC, vivo_0_muerto_1) ~factor(score_cat_3), data = base)
KM_fig1<-ggsurvplot(mod1_km, data = base, size = 1,palette ="jama",conf.int = T,
                    risk.table = T,
                    ggtheme = theme_classic(),
                    xlab="Age (Years)",
                    ylab="Survival Probability, (%)",
                    title="Death by Any Cause",
                    legend.labs = c("Mortality Score <6.5",
                                    "Mortality Score >6.5"),
                    xlim = c(0,75),
                    ylim= c(0,1.0),
                    break.y.by= c(0.1),
                    break.x.by= c(10),
                    pval = TRUE, 
                    pval.method = TRUE,
                    log.rank.weights = "1", 
                    pval.method.size = 3,
                    pval.coord = c(10, 0.20),
                    pval.method.coord = c(10, 0.15))+
  theme_survminer(base_size = 9,base_family = "Arial")+
  guides(colour = guide_legend(nrow = 1))

KM_fig1 <-KM_fig1 + theme_survminer(base_size = 8,
                                    base_family = "Arial",
                                    font.x = c(8, "plain" ), 
                                    font.y = c(8, "plain"),
                                    font.main = c(10, "plain"),
                                    font.caption = c(8, "plain"), 
                                    font.legend = c(8, "plain"),
                                    font.tickslab = c(8, "plain"))

KM1<-ggarrange(KM_fig1$plot, KM_fig1$table, heights = c(2, 0.7),
               ncol = 1, nrow = 2);KM1

ggsave(KM1,
       filename = "Supplementary_Figure_2.png", 
       width = 15, 
       height = 12.5,
       units=c("cm"),
       dpi = 800,
       limitsize = FALSE)

