setwd("C:\\Users\\mauricio.izumi\\Desktop\\join_teste\\")

cand <- read.csv("unique_cand_fed.csv")

##
## Standardizing título
## https://pt.wikipedia.org/wiki/T%C3%ADtulo_eleitoral
##

#to character
cand$NUMERO.DA.INSCRICAO <- as.character(cand$NUMERO.DA.INSCRICAO)

teste <- c("123456789", "111111111111")

#check 12 digits
for(i in 1:length(cand$NUMERO.DA.INSCRICAO)){
	while(nchar(cand$NUMERO.DA.INSCRICAO[i]) < 12){
		cand$NUMERO.DA.INSCRICAO[i] <- paste0("0", cand$NUMERO.DA.INSCRICAO[i])
	}
}

#check UF
cand$teste_uf <- ifelse(cand$UF == "ZZ" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "28", 1,
	ifelse(cand$UF == "TO" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "27", 1,
	ifelse(cand$UF == "RR" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "26", 1,
	ifelse(cand$UF == "AP" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "25", 1,
	ifelse(cand$UF == "AC" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "24", 1,
	ifelse(cand$UF == "RO" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "23", 1,
	ifelse(cand$UF == "AM" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "22", 1,
	ifelse(cand$UF == "SE" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "21", 1,
	ifelse(cand$UF == "DF" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "20", 1,
	ifelse(cand$UF == "MS" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "19", 1,
	ifelse(cand$UF == "MT" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "18", 1,
	ifelse(cand$UF == "AL" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "17", 1,
	ifelse(cand$UF == "RN" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "16", 1,
	ifelse(cand$UF == "PI" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "15", 1,
	ifelse(cand$UF == "ES" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "14", 1,
	ifelse(cand$UF == "PA" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "13", 1,
	ifelse(cand$UF == "PB" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "12", 1,
	ifelse(cand$UF == "MA" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "11", 1,
	ifelse(cand$UF == "GO" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "10", 1,
	ifelse(cand$UF == "SC" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "09", 1,
	ifelse(cand$UF == "PE" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "08", 1,
	ifelse(cand$UF == "CE" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "07", 1,
	ifelse(cand$UF == "PR" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "06", 1,
	ifelse(cand$UF == "BA" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "05", 1,
	ifelse(cand$UF == "RS" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "04", 1,
	ifelse(cand$UF == "RJ" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "03", 1,
	ifelse(cand$UF == "MG" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "02", 1,
	ifelse(cand$UF == "SP" & substr(cand$NUMERO.DA.INSCRICAO, 9, 10) == "01", 1, 0))))))))))))))))))))))))))))

#digito verificador
	cand$x1 <- 2 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 1, 1))
	cand$x2 <- 3 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 2, 2))
	cand$x3 <- 4 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 3, 3))
	cand$x4 <- 5 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 4, 4))
	cand$x5 <- 6 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 5, 5))
	cand$x6 <- 7 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 6, 6))
	cand$x7 <- 8 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 7, 7))
	cand$x8 <- 9 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 8, 8))

	cand$v1 <- rowSums(cand[,c("x1","x2","x3","x4","x5","x6","x7","x8")]) %% 11
	cand$v1 <- ifelse(cand$v1 == 10, 0, cand$v1)

	cand$x9 <- 7 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 9, 9))
	cand$x10 <- 8 * as.numeric(substr(cand$NUMERO.DA.INSCRICAO, 10, 10))
	cand$x11 <- 9 * cand$v1

	cand$v2 <- rowSums(cand[,c("x9","x10","x11")]) %% 11
	cand$v2 <- ifelse(cand$v2 == 10, 0, cand$v2)

	cand$verif <- paste0(cand$v1, cand$v2)

	cand$teste_verif <- ifelse(cand$verif == substr(cand$NUMERO.DA.INSCRICAO, 11, 12), 1, 0)

#number of valid ID's
table(cand$teste_verif == 1 & cand$teste_uf == 1)



