# Pre-processing data (Synthetic control DATA) - Leonardo Biazoli

# Packages --------------------------------------------------------------

library(tidyverse)
library(sidrar)
library(dplyr)

# Pre-processing data ---------------------------------------------------

df <- read.csv("mg_data_scm.csv", header = T, sep = ',', encoding = 'UTF-8')

mg <- df %>%
  select(!c(part_micro, part_meso, X, UF))

mg$afetadas[which(mg$mun == "mariana" & mg$ano > 2014)] = 1

# Deflating GDP with IPCA accumulated per year --------------------------

series_ipca <- search_sidra("IPCA")
tail(series_ipca)
info_sidra(1737, wb = T)
ipca <- get_sidra(1737,
                  variable = 69,
                  period = "200112-201812",
                  format = 2)
ip = ipca %>%
  filter(str_detect(Mês, "dezembro")) %>%
  select(c(Valor, Mês))

ip$ano = 2001:2018

# index numbers

ip$indice <- 100

for (i in 1:17) {
  ip[i+1,"indice"] <- (ip[i+1,"Valor"]/100+1)*
    ip[i,"indice"]
}

ip$precos_2018 <- c(NA,rep(1,17))

# Price in terms of 2018

for (i in 0:16) {
  ip[18-i,"precos_2018"] <- ip[18,"indice"]/
    ip[18-i-1,"indice"]
}

# Merging the data ------------------------------------------------------

mg2 = merge(mg, ip, by="ano")

# Create variables ------------------------------------------------------

mg2$PIBreal = mg2$PIB * mg2$precos_2018
mg2$PIBreal_pc = (mg2$PIBreal)/mg2$pop
mg2$dens_pop = mg2$pop/mg2$area_mun_2010
mg2$trab_analfabeto = mg2$analfabeto/mg2$total
mg2$trab_sup = mg2$superior_completo/mg2$total

# Data processed --------------------------------------------------------

mg3 <- mg2 %>%
  select(!c(municipio2, analfabeto, ate5_incompleto, X5completo,
            X6a9fund, fund_completo, medio_incompleto,
            medio_completo, superior_incompleto,
            superior_completo, mestrado, doutorado, total, Valor,
            Mês, indice, precos_2018))

write.csv(mg3, "data_proc_scm.csv")
