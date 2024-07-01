###################################################################################################
## Skript für Nowcasting der BevStatG-Daten
## Ivo Foppa 26.06.2024
###################################################################################################
# rm(list = ls())
pacman::p_load(tidyverse,data.table,R2jags)

datei1agg <- datei1 %>%
  filter(wday(sterbedatum)==1) %>%
  mutate(lag = as.integer(eingang - sterbedatum)) %>%
  filter(lag < 45 & lag >= 0) %>%
  group_by(lag) %>%
  summarise(n1 = n()) %>%
  tidyr::complete(lag = c(0:44)) %>%
  left_join((datei1 %>%
               filter(wday(sterbedatum)==2) %>%
               mutate(lag = as.integer(eingang - sterbedatum)) %>%
               filter(lag < 45 & lag >= 0) %>%
               group_by(lag) %>%
               summarise(n2 = n()) %>%
               tidyr::complete(lag = c(0:44))),by = "lag") %>%
  left_join((datei1 %>%
               filter(wday(sterbedatum)==3) %>%
               mutate(lag = as.integer(eingang - sterbedatum)) %>%
               filter(lag < 45 & lag >= 0) %>%
               group_by(lag) %>%
               summarise(n3 = n()) %>%
               tidyr::complete(lag = c(0:44))),by = "lag") %>%
  left_join((datei1 %>%
               filter(wday(sterbedatum)==4) %>%
               mutate(lag = as.integer(eingang - sterbedatum)) %>%
               filter(lag < 45 & lag >= 0) %>%
               group_by(lag) %>%
               summarise(n4 = n()) %>%
               tidyr::complete(lag = c(0:44))),by = "lag") %>%
  left_join((datei1 %>%
               filter(wday(sterbedatum)==5) %>%
               mutate(lag = as.integer(eingang - sterbedatum)) %>%
               filter(lag < 45 & lag >= 0) %>%
               group_by(lag) %>%
               summarise(n5 = n()) %>%
               tidyr::complete(lag = c(0:44))),by = "lag") %>%
  left_join((datei1 %>%
               filter(wday(sterbedatum)==6) %>%
               mutate(lag = as.integer(eingang - sterbedatum)) %>%
               filter(lag < 45 & lag >= 0) %>%
               group_by(lag) %>%
               summarise(n6 = n()) %>%
               tidyr::complete(lag = c(0:44))),by = "lag") %>%
  left_join((datei1 %>%
               filter(wday(sterbedatum)==7) %>%
               mutate(lag = as.integer(eingang - sterbedatum)) %>%
               filter(lag < 45 & lag >= 0) %>%
               group_by(lag) %>%
               summarise(n7 = n()) %>%
               tidyr::complete(lag = c(0:44))),by = "lag") %>%
  replace(is.na(.),0) %>% select(-lag)

total <- datei1agg %>% summarise_at(c("n1","n2","n3","n4","n5","n6","n7"),sum) %>% unlist() %>% as.vector()
###################################################################################################
### Bayes'sche Analyse ############################################################################
###################################################################################################
### Erstellen der Analysedatei
datei2agg <-datei %>%
  filter(sterbedatum > (maxsdatum - 45),sterbedatum<=maxsdatum ) %>%
  group_by(sterbedatum) %>%
  summarise(n = n()) %>%
  # filter(sterbedatum <= heute1) %>%
  complete(sterbedatum = seq.Date((maxsdatum - 45+1),maxsdatum,by = "day")) %>%
  replace(is.na(.),0) %>%
  arrange(rev(sterbedatum)) %>%
  mutate(tag = wday(sterbedatum)) %>%
  arrange(sterbedatum)

###################################################################################################
###################################################################################################
###################################################################################################
### Zusammestellen der Daten für di 
n <- t(datei1agg)
K <- nrow(datei1agg)

m <- rev(datei2agg$n)
wtag <- rev(datei2agg$tag)
M <- nrow(datei2agg)

data <- list("m" = rev(datei2agg$n),
             "n" = t(datei1agg),
             "total" = total,
             "wtag" = rev(datei2agg$tag),
             "K" = nrow(datei1agg),
             "M" = nrow(datei2agg))

deltainit <- n + 0.001
pinit <- (deltainit/total)/rowSums(deltainit/total)

qinit <- NULL
for (k in 1:M) {
  qinit <- c(qinit,sum(pinit[wtag[k],1:k]))
}

muinit <- m/qinit
sinit <- round(muinit)

nadapt <- 10000
niter <- 10000

variables <- c("s")

inits <- function(){
  list(
    delta = deltainit,
    mu = muinit,
    s = sinit
  )}

model.file <- file.path(maindir,"R-Skripte","model.txt")

j.model <- jags.model(file=model.file,data=data, inits=inits, n.adapt=nadapt, n.chains=3)
j.samples<-coda.samples(j.model, variable.names=variables, n.iter=niter, thin = 5) 

# sm <- summary(j.samples)
codals <- data.frame(rbind(j.samples[[1]],j.samples[[2]],j.samples[[3]],deparse.level=0)) 

outfname2 <- file.path(maindir,"Resultate","Coda",paste0("coda_",heute2,".csv"))
write.csv2(codals,file = outfname2,row.names = FALSE)

sterbedatenvek <- unique(datei2agg$sterbedatum) %>% sort()

smedian <- as.vector(unlist(sapply(1:M, function(k) quantile(codals[,k],probs = 0.5,type = 4)))) %>% rev()
sll <- as.vector(unlist(sapply(1:M, function(k) quantile(codals[,k],probs = 0.025,type = 4)))) %>% rev()
sul <- as.vector(unlist(sapply(1:M, function(k) quantile(codals[,k],probs = 0.975,type = 4)))) %>% rev()

# nowcastdaten <- tibble(sterbedatum=sterbedatenvek,sm = smedian,sll,sul) %>%
#   mutate(across(c(sm,sll,sul),~case_when(row_number()%in%c(M) ~ NA_real_, TRUE ~ .x)))
# 
nowcastdaten <- tibble(sterbedatum=sterbedatenvek,sm = smedian,sll,sul) 

write.csv2(nowcastdaten,file.path(maindir,"Resultate","Nowcast",paste0("Nowcast_Daten_",heute0,".csv")),row.names = FALSE)
