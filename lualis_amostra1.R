#RE-INFECTION SCREENING ALGORITHM
#Algorithm created by Timothy Cavazzotto 
#Started 22/10/2022 final version at: 04/12/2022
#Purpose:  screening covid19 reinfection by personal document and valid exam
#excluding duplication and errors 

#### Packages #########
library(readxl)
library(dplyr)
library(lubridate)

#### Data #############
data1 <- read_excel("amostra1_d1.xlsx")
data2 <- read_excel("amostra1_d2.xlsx")
data <- rbind(data1, data2) #merge data to 1 df

#frequencies of duplicates
max(table(data$cpf))

#create subset to detect duplicates and covid_positives
#attributing classes of data
data$cpf<-as.numeric(data$cpf)
data$cns<-as.numeric(data$cns)

#### clean data #######

#clean rows with missing in selected variables
df <- data[!is.na(data$cpf),]
df <- df[!is.na(df$data_coleta),]
df <- df[!is.na(df$resultado),]
df <- df[!is.na(df$exame),]
df <- df[order(df$cpf),]
df$cpf[df$cpf==0]<-NA
df<- df[!is.na(df$cpf),]

#frequencies of duplicates
max(table(df$cpf))

datab<-df

#### Create Boolean variables ####

#positive cases by results
datab$resultado_pos<-0
datab$resultado_pos[datab$resultado== "Coronavírus (SARS-COV2)"]<-1
datab$resultado_pos[datab$resultado== "Reagente"]<-1
datab$resultado_pos[datab$resultado_pos!= 1]<-0

#positive cases by list of valid exam
datab$exame_pos<-0
datab$exame_pos[datab$exame== "Celer Sansure Kit de Detecção por PCR em Tempo Real para SARS-CoV-2"]<-1
datab$exame_pos[datab$exame== "COBAS SARS-CoV-2"]<-1
datab$exame_pos[datab$exame== "COVID-19 Ag ECO Teste"]<-1
datab$exame_pos[datab$exame== "COVID-19 Ag Rapid Test Device - Panbio"]<-1
datab$exame_pos[datab$exame== "COVID-19 Real-Time PCR Kit"]<-1
datab$exame_pos[datab$exame== "COVID-19, Biologia Molecular"]<-1
datab$exame_pos[datab$exame== "DETECT SARS-CoV-2 RT-PCR"]<-1
datab$exame_pos[datab$exame== "Diagnostic Kit for Novel-Coronavirus(2019-nCoV) RNA"]<-1
datab$exame_pos[datab$exame== "Diagnóstico Molecular CORONÁVIRUS COVID-19"]<-1
datab$exame_pos[datab$exame== "ECO F COVID-19 Ag"]<-1
datab$exame_pos[datab$exame== "EURORealTime SARS-CoV-2	RT-PCR"]<-1
datab$exame_pos[datab$exame== "Família Abbott RealTime SARS-CoV-2"]<-1
datab$exame_pos[datab$exame== "Família Abbott RealTime SARS-CoV-2 EUA"]<-1
datab$exame_pos[datab$exame== "FAMÍLIA BIO GENE COVID-19 PCR"]<-1
datab$exame_pos[datab$exame== "Família cobas SARS-CoV-2"]<-1
datab$exame_pos[datab$exame== "Família Kit de Detecção por PCR em Tempo Real VIASURE SARS-CoV-2"]<-1
datab$exame_pos[datab$exame== "FAMÍLIA KIT XGEN MASTER COVID-19"]<-1
datab$exame_pos[datab$exame== "GeneFinderTM COVID-19 PLUS RealAmp Kit"]<-1
datab$exame_pos[datab$exame== "Kit MOLECULAR SARS-CoV2 (E/P1)"]<-1
datab$exame_pos[datab$exame== "Kit Molecular SARS-CoV2 (E/RP)"]<-1
datab$exame_pos[datab$exame== "KIT XGEN MASTER COVID-19"]<-1
datab$exame_pos[datab$exame== "Novel Coronavirus (2019-nCoV) Nucleic Acid Detection Kit PCR-Fluorescence Probing"]<-1
datab$exame_pos[datab$exame== "Panbio COVID-19 Ag Rapid Test"]<-1
datab$exame_pos[datab$exame== "RealStar® SARS-CoV-2 RT-PCR Kit 1.0"]<-1
datab$exame_pos[datab$exame== "SARS-COV-2 RT-PCR KIT"]<-1
datab$exame_pos[datab$exame== "SARS-CoV-2 S gene for BD Max"]<-1
datab$exame_pos[datab$exame== "TaqPath™ COVID-19 CE-IVD RT PCR Kit"]<-1
datab$exame_pos[datab$exame== "Teste Molecular (RT-PCR)"]<-1
datab$exame_pos[datab$exame== "Teste Rápido (Molecular)"]<-1
datab$exame_pos[datab$exame== "Teste Rápido Antígeno (Imunocromatografia)"]<-1
datab$exame_pos[datab$exame== "Teste Rápido Antígeno (Imunofluorescência)"]<-1
datab$exame_pos[datab$exame== "TR COVID-19 Ag - Bio-Manguinhos"]<-1
datab$exame_pos[datab$exame== "TR DPP® COVID-19 AG- Bio-Manguinhos"]<-1
datab$exame_pos[datab$exame== "TR SARS COV 2 AG - Bio-Manguinhos"]<-1
datab$exame_pos[datab$exame== "Xpert Xpress SARS-CoV-2"]<-1

datab$resultado_pos<-as.numeric(datab$resultado_pos)
datab$exame_pos<-as.numeric(datab$exame_pos)
datab$covid_positivo_valido<-datab$exame_pos + datab$resultado_pos

datab$data_coleta_n<-as.Date(datab$data_coleta, format = "%d/%m/%Y")

#creating dataset with olny positive cases
datac <- datab %>%
  filter(covid_positivo_valido > 0)

#frequencies of duplicates
max(table(datab$cpf))

#### reinfection screening algorithm ########
datab$data_coleta_n<-as.Date(datab$data_coleta_n, format = "%d/%m/%Y")
datab <- datab[order(datab$cpf, datab$data_coleta_n),]
datab$coleta1<-datab$data_coleta_n
datab$cpf1<-datab$cpf
max(table(datab$cpf)) #show the highest value of replication by a "cpf" 

datab$cpf_dupli<-0
for (i in 1:length(datab$cpf1)){
ifelse (datab$cpf1[i]==datab$cpf1[i+1], datab$cpf_dupli[i]<- 1,0)
ifelse (datab$cpf1[i]==datab$cpf1[i-1], datab$cpf_dupli[i]<- 1,0)}

datab$cpf_dupli<-as.factor(datab$cpf_dupli) #create a vector to count cpf dupli
summary(datab$cpf_dupli) #count cpf dupli

datab <- datab[order(datab$cpf, datab$data_coleta_n),]
summary(datab$data_coleta_n)



str(datab$data_coleta_n)

datab$onda<-0
for (i in 1:length(datab$cpf1)) { 
  ifelse (
    datab$data_coleta_n[i] > as.Date("11/03/2020", format = "%d/%m/%Y") && 
    datab$data_coleta_n[i] < as.Date("18/10/2020", format = "%d/%m/%Y"), 
    datab$onda[i] <- 1,
    datab$onda[i]
  )
}

table(datab$onda)

#creating Covid database based on first wave 
datab <- datab %>%
  filter(onda == 1,
         resultado_pos==1)


datac<-datab

datab$ordemcoleta<-1
#create variable with order of exam collection
for (i in 1:length(datab$cpf1)) {
  ifelse (datab$cpf1[i]==datab$cpf1[i+1], 
          datab$ordemcoleta[i+1]<-datab$ordemcoleta[i]+1, 
          datab$ordemcoleta[i+1])}


datab$diffdays_exame<-0
#create variable with exam days diff
for (i in 1:length(datab$cpf1)) {
  ifelse (
    datab$ordemcoleta[i + 1] > 1 &&
      datab$cpf1[i] == datab$cpf1[i + 1],
    datab$diffdays_exame[i + 1] <-
      as.numeric(
        difftime(datab$data_coleta_n[i + 1],
                 datab$data_coleta_n[i], units = "days")
      ),
    datab$diffdays_exame[i + 1]
  )
}

#create variable with order of infection
datab$ordem_infec<-datab$resultado_pos

datab<-datab %>%
  group_by(cpf1) %>%
  arrange(cpf1, ordemcoleta) %>%
  mutate(
    ordem_infec2= ifelse(covid_positivo_valido==2,
                        cumsum(ordem_infec), ordem_infec))

table(datab$ordem_infec2)

#create variable with time diff and cum time diff
datab<-datab %>%
  group_by(cpf1) %>%
  arrange(cpf1, ordem_infec2) %>%
  mutate(
    tcumsum = cumsum(diffdays_exame)) %>%
  ungroup()

table(datab$tcumsum)

datab$ordem_coleta_infect<-paste(datab$ordemcoleta,datab$ordem_infec2)




#create variable with valid reinfection
datab$reinfectec<-0


for (i in 1:length(datab$cpf1)) {
  ifelse(datab$ordem_infec2[i]>1 &&
           datab$tcumsum[i]>44,
         datab$reinfectec[i]<-1,0)
}

table(datab$reinfectec)

write.csv2(datab, file = "dados_datab.csv")

























