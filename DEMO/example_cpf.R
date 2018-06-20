#Example
#standardizing cpfs
inscritos2$cpf2 <- as.character(inscritos2$cpf2) #making sure it's character
inscritos2$cpf3 <- as.numeric(inscritos2$cpf2) #From character to numeric #NAs from unusable
inscritos2$cpf3[inscritos2$cpf3 == 0] <- NA #NAs from unusable (these are 0 but should be NA)
inscritos2$cpfstd <- sprintf("%011.0f", inscritos2$cpf3)#From numeric to character with fixed number of digits
inscritos2$ind_notstd <- ifelse(nchar(inscritos2$cpfstd) > 11, 1, 0)
temp <- inscritos2 %>% filter(ind_notstd == 0 & unusable == 0)

inscritos_temp <- validate_cpf(cpfs = temp$cpfstd, data = temp)
inscritos_temp$final_invalid <- ifelse(inscritos_temp$first_digit_valid == 0 | inscritos_temp$second_digit_valid == 0, 1, 0)

#Functions created to clean, merge, combine, and analyze data

#Create a clean version of the names
clean.accent<-function(x){ 
    y<-toupper(x)
    y<-gsub("Â","A", y) 
    y<-gsub("Á","A", y)
    y<-gsub("Ã","A", y)
    y<-gsub("Ä","A", y)
    y<-gsub("À","A", y)
    y<-gsub("È","E", y)
    y<-gsub("É","E", y)
    y<-gsub("Ê","E", y)
    y<-gsub("Ë","E", y)
    y<-gsub("Í","I", y)
    y<-gsub("Ì","I", y)
    y<-gsub("Ó","O", y)
    y<-gsub("Ô","O", y)
    y<-gsub("Õ","O", y)
    y<-gsub("Ö","O", y)
	 y<-gsub("Ò","O", y)
    y<-gsub("Ú","U", y)
    y<-gsub("Ü","U", y)
    y<-gsub("Ù","U", y)
    y<-gsub("Ç","C", y)  
    y<-gsub("´","'", y) 
    y<-gsub("Ñ","N", y)     
    return(y)
}

#Function to check valid CPFs
validate_cpf <- function(cpfs, data){
  split <- str_split_fixed(cpfs, "", 11)
  split <- apply(split, 2, function(x) as.numeric(x))
  weights <- seq(10, 2)
  fd <- sweep(split[,1:9], MARGIN = 2, weights, `*`)
  sum <- rowSums(fd)
  remainder <- sum %% 11
  first_digit <- 11 - remainder
  first_digit <- ifelse(first_digit > 9, 0, first_digit)
  split_fd <- cbind(split, first_digit, deparse.level = 0)
  weights <- c(11, weights)
  sd <- sweep(split_fd[,c(1:9, 12)], MARGIN = 2, weights, `*`)
  sums <- rowSums(sd)
  remainders <- sums %% 11
  second_digit <- 11 - remainders
  second_digit <- ifelse(second_digit > 9, 0, second_digit)
  cod_validate <- cbind(first_digit, second_digit, deparse.level = 0)
  cod_disponivel <-  split[,10:11]
  valid_code <- ifelse(cod_validate == cod_disponivel, 1, 0)
  colnames(valid_code) <- c("first_digit_valid", "second_digit_valid")
  valid_code <- as_tibble(valid_code)
  return(bind_cols(data, valid_code))	
}