##
## Downloading Filiados Data
## 21/05/2018
##

#working directory
setwd("C:\\Users\\mauricio\\Desktop\\dados_filiados\\")

#create directory
dir.create("original_files")
dir.create("original_unzipped")

#################
## Downloading ##
#################

#Downloading UFs
url <- "http://agencia.tse.jus.br/estatistica/sead/eleitorado/filiados/uf/filiados_"
dfolder <- "C:\\Users\\mauricio\\Desktop\\dados_filiados\\"

ufs <- c("ac", "al","ap","am", "ba",
         "ce","df","es", "go","ma","mt","ms",
         "mg","pa","pb","pr","pe","pi","rj",
         "rn","rs","ro","rr","sc","sp", "se","to")

#Getting parties
get_parties <- function(url){
	temp <- readLines(url)
	temp <- temp[(grep("Partido:", temp) + 1):(grep("UF:", temp) - 1)]
	temp <- gsub('.*<option value=\"', "", temp)
	temp <- gsub('\".*', "", temp)
	temp
}

pparties <- get_parties("http://filiaweb.tse.jus.br/filiaweb/portal/relacoesFiliados.xhtml")

#Downloading all states per party
for(i in 1:length(pparties)){
	for(j in 1:length(ufs)){
		download.file(url = paste0(url, pparties[i], "_", ufs[j], ".zip"),
				  destfile = paste0(dfolder, "original_files\\",
							  "filiados_", pparties[i],"_", ufs[j],".zip"))
	}
}

#Unzipping
for(i in 1:length(pparties)){
	for(j in 1:length(ufs)){
		unzip(zipfile = paste0(dfolder, "original_files\\",
					     "filiados_", pparties[i],"_", ufs[j],".zip"),
			exdir = paste0(dfolder, "original_unzipped\\", pparties[i],"\\",ufs[j]))
	}
}

#Checking of all expected files downloaded and unzipped
#1. Downloads
length(list.files(paste0(dfolder, "original_files/")))==length(pparties)*length(ufs)
#2. Unzips
length(list.files(paste0(dfolder, "original_unzipped/")))==length(pparties)

#Binding all party affiliates
#Binding filiados
all <- list(NULL)
k <- 0
for(i in 1:length(pparties)){
	for(j in 1:length(ufs)){
		k <- k + 1
		temp <- paste0("C:\\Users\\mauricio\\Desktop\\dados_filiados\\original_unzipped\\", pparties[i], "\\",
								   ufs[j], "\\aplic\\sead\\lista_filiados\\uf", sep = "")
		all[[k]] <- read.csv(paste0(temp, "\\", list.files(temp, pattern = "filiados"), sep = ""),
					   sep = ";",
					   colClasses = c("NULL","NULL","character","character",
								NA,"NULL",NA,NA,
								"NULL","NULL","NULL",NA,
								"NULL","NULL","NULL","NULL",
								"NULL","NULL","NULL"))
		print(k)
	}
}

all <- do.call("rbind", all)
write.csv(all, "filiados_may2018.csv", row.names = F)

#Binding sobjudice
all_sobj <- list(NULL)
k <- 0
for(i in 1:length(pparties)){
	for(j in 1:length(ufs)){
		k <- k + 1
		temp <- paste0("C:\\Users\\mauricio\\Desktop\\dados_filiados\\original_unzipped\\", pparties[i], "\\",
								   ufs[j], "\\aplic\\sead\\lista_filiados\\uf", sep = "")
		all_sobj[[k]] <- read.csv(paste0(temp, "\\", list.files(temp, pattern = "fil_sub_jud"), sep = ""),
					   sep = ";",
					   colClasses = c("NULL","NULL","character","character",
								NA,"NULL",NA,NA,
								"NULL","NULL","NULL",NA,
								"NULL","NULL","NULL","NULL",
								"NULL","NULL","NULL"))
		print(k)
	}
}

all_sobj <- do.call("rbind", all_sobj)
write.csv(all_sobj, "sobjudice_may2018.csv", row.names = F)

save.image("filiados.RData")
















#Basic checks on number of states and number of parties
#1. Number of states
length(table(filiados_jan2017$UF))==length(ufs)
length(table(sobjudice_jan2017$UF))==length(ufs) #missing DF (this is an error)
#2. Number of parties
length(table(filiados_jan2017$`SIGLA DO PARTIDO`))==length(pparties)
length(table(sobjudice_jan2017$`SIGLA DO PARTIDO`))==length(pparties) #missing two parties #missing NOVO and PCO
#3. Any state without any party affiliates for any political party? (smell check)
filiados_jan2017 %>% group_by(`SIGLA DO PARTIDO`, UF) %>% summarize(n_obs = n()) %>% arrange(n_obs)
#if so, which state(s) and which party (ies)?
filiados_jan2017 %>% group_by(`SIGLA DO PARTIDO`, UF) %>% summarize(n_obs = n()) %>% filter(n_obs == 0)
#None

save(filiados_jan2017, file=paste0(dfolder, "combined_data/filiados_jan2017.Rda"))
save(sobjudice_jan2017, file=paste0(dfolder, "combined_data/sobjudice_jan2017_tobecorrected.Rda"))
write.csv(filiados_jan2017, file = paste0(dfolder, "combined_data/filiados_jan2017.csv"), fileEncoding = "UTF-8")

############### ADD IBGE CODES TO FILIADOS DATA

codes <- read_excel(paste0(dfolder, "IBGE_TSE_files/Munic2016_IBGE_TSE.xlsx"))
codes <- codes %>% mutate(TSECod1 = as.numeric(TSECod))
filiados_jan2017 <- filiados_jan2017 %>% mutate(TSECod1 = as.numeric(`CODIGO DO MUNICIPIO`))

filiados_jan2017 <- filiados_jan2017 %>% left_join(codes, by = "TSECod1") %>% select(-UF.y) %>% rename(UF = UF.x)

save(filiados_jan2017, file=paste0(dfolder, "combined_data/filiados_jan2017_ibge.Rda"))