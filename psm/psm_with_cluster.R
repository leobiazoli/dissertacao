# PSM with cluster analysis - Leonardo Biazoli

# Packages ----------------------------------------------------------------

library(MatchIt)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sidrar)
library(dplyr)
library(plotly)
library(factoextra)
library(dendextend)
library(cluster)
library(NbClust)

# Loading data ------------------------------------------------------------

df = read.csv("data_proc_psm.csv", dec = ".", sep = ",", encoding = "UTF-8")

mg_2016 = df %>%
  filter(ano == 2016)

mg_2017 = df %>%
  filter(ano == 2017)

mg_2018 = df %>%
  filter(ano == 2018)

# Model 2016 ------------------------------------------------------------

x <- mg_2016 %>%
  select(c(mun, part_agro, part_administracao, part_industria, 
           part_servicos, PIBreal_pc, taxa_estab, dens_pop,
           trab_analfabeto, trab_sup
  )) %>%
  na.omit()

row.names(x) <- x$mun

x$mun <- NULL

z = scale(x)

# k-means

fviz_nbclust(x, kmeans, method = "wss")+  
  geom_vline(xintercept = 4, linetype = 2)

nb <- NbClust(z, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

cluster2 <- kmeans(x, 2)

print(cluster2)

x2=cbind(x, cluster=cluster2$cluster)

x3 = x2 %>%
  filter(cluster==1)

mun4 = row.names(x3)

mg_c4 = mg2 %>%
  filter(mun %in% mun4)

# k-means: 2 groups 
# G1: 845 and G2: 5

# Ward's Method

z = scale(x)

dista=dist(z, method="euclidean")

dista.hc=hclust(d=dista, method="ward.D")

fviz_dend(dista.hc, cex=0.5)

den <- as.dendrogram(dista.hc)

plot(den, ylab = "Distância")
rect.hclust(dista.hc, k = 7, border = "red")

nb <- NbClust(z, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "ward.D")

fviz_nbclust(nb)

c = data.frame(cutree(dista.hc, k=3))

# 3 groups
# G1: 394 mun (with treated)
# G2: 132 mun
# G3: 324 mun (with treated)

# nrow(filter(c, cutree.dista.hc..k...3. == 3))

c3 = c %>%
  filter(cutree.dista.hc..k...3. != 2)

mun_hie = row.names(c3)

mg_c3 = mg_2016 %>%
  filter(mun %in% mun_hie)

# Balancing before matching

mg_c3 %>%
  group_by(afetadas) %>%
  select(one_of(cov1)) %>%
  summarise_all(funs(sd(., na.rm=T)))

lapply(cov1, function(v){
  t.test(mg_c3[,v] ~ mg_c3[,'afetadas'])
})

# Logit

mps = glm(afetadas ~ part_agro + part_industria + part_servicos +
            part_administracao + taxa_estab + trab_sup +
            trab_analfabeto + dens_pop, family = binomial(), 
          data = mg_c3)
summary(mps)

ps_df = data.frame(p_s = predict(mps, type = "response"), 
                   afetadas = mps$model$afetadas)

# Histogram of Propensity Score
labs = paste("Rompimento em Mariana:", c("sim", "não"))

ps_df %>%
  mutate(afetadas = ifelse(afetadas == 1, labs[1], labs[2])) %>%
  ggplot(aes(x=p_s)) + geom_histogram(color='white') +
  facet_wrap(~afetadas) + #, scales = "free_y"
  xlab("Propensity Score") + 
  theme_classic()

# Matching algorithm

row.names(mg_c3) = mg_c3$mun

df_nomiss = mg_c3 %>%
  select(PIBreal_pc, afetadas, one_of(cov1)) %>%
  na.omit()

model = matchit(afetadas ~ part_agro + part_industria + part_servicos +
                  part_administracao + taxa_estab + trab_sup +
                  trab_analfabeto + dens_pop,
                method = "nearest", data = df_nomiss)

summary(model)
plot(model)

pards = match.data(model)

# Checking balancing after matching

lapply(cov1, function(v){
  t.test(pards[,v] ~ pards$afetadas)
})

# Average treatment effect

with(pards, t.test(PIBreal_pc ~ afetadas))

# Model 2017 ------------------------------------------------------------

x17 <- mg_2017 %>%
  select(c(mun, part_agro, part_administracao, part_industria, 
           part_servicos, PIBreal_pc, taxa_estab, dens_pop,
           trab_analfabeto, trab_sup
  )) %>%
  na.omit()

row.names(x17) <- x17$mun

x17$mun <- NULL

z17 = scale(x17)

# k-means

fviz_nbclust(x17, kmeans, method = "wss")+  
  geom_vline(xintercept = 4, linetype = 2)

nb <- NbClust(z17, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

cluster2 <- kmeans(x, 2)

print(cluster2)

# k-means: 2 groups 
# G1: 845 and G2: 5

# Ward's Method

dista=dist(z17, method="euclidean")

dista.hc=hclust(d=dista, method="ward.D")

fviz_dend(dista.hc, cex=0.5)

den <- as.dendrogram(dista.hc)

plot(den, ylab = "Distância")
rect.hclust(dista.hc, k = 7, border = "red")

# Gap statistic for hierarchical clustering

nb <- NbClust(z17, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "ward.D")

fviz_nbclust(nb)

c = data.frame(cutree(dista.hc, k=3))

# 3 groups
# G1: 342 mun (with treated)
# G2: 392 mun
# G3: 116 mun (with treated)
# nrow(filter(c, cutree.dista.hc..k...3. == 1))

c3 = c %>%
  filter(cutree.dista.hc..k...3. != 3)

mun_hie = row.names(c3)

mg_c3_17 = mg_2017 %>%
  filter(mun %in% mun_hie)

# Balancing before matching

mg_c3_17 %>%
  group_by(afetadas) %>%
  select(one_of(cov1)) %>%
  summarise_all(funs(sd(., na.rm=T)))

lapply(cov1, function(v){
  t.test(mg_c3[,v] ~ mg_c3[,'afetadas'])
})

# Logit

mps = glm(afetadas ~ part_agro + part_industria + part_servicos +
            part_administracao + taxa_estab + trab_sup +
            trab_analfabeto + dens_pop, family = binomial(), 
          data = mg_c3_17)
summary(mps)

ps_df = data.frame(p_s = predict(mps, type = "response"), 
                   afetadas = mps$model$afetadas)

# Matching algorithm

row.names(mg_c3_17) = mg_c3_17$mun

df_nomiss = mg_c3_17 %>%
  select(PIBreal_pc, afetadas, one_of(cov1)) %>%
  na.omit()

model = matchit(afetadas ~ part_agro + part_industria + part_servicos +
                  part_administracao + taxa_estab + trab_sup +
                  trab_analfabeto + dens_pop,
                method = "nearest", data = df_nomiss)

summary(model)
plot(model)

pards = match.data(model)

# Checking balancing after matching

lapply(cov1, function(v){
  t.test(pards[,v] ~ pards$afetadas)
})

# Average treatment effect

with(pards, t.test(PIBreal_pc ~ afetadas))


# Model 2018 ------------------------------------------------------------

x18 <- mg_2018 %>%
  select(c(mun, part_agro, part_administracao, part_industria, 
           part_servicos, PIBreal_pc, taxa_estab, dens_pop,
           trab_analfabeto, trab_sup
  )) %>%
  na.omit()

row.names(x18) <- x18$mun

x18$mun <- NULL

z18 = scale(x18)

# k-means

fviz_nbclust(x18, kmeans, method = "wss")+  
  geom_vline(xintercept = 4, linetype = 2)

nb <- NbClust(z18, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

cluster2 <- kmeans(x, 2)

print(cluster2)

# k-means: 2 groups 
# G1: 845 and G2: 5

x2=cbind(x, cluster=cluster2$cluster)

x3 = x2 %>%
  filter(cluster==1)

mun4 = row.names(x3)

mg_c4 = mg2 %>%
  filter(mun %in% mun4)

mg_c4$lnPIBreal = log(mg_c4$PIBreal)

# Ward's Method

dista=dist(z18, method="euclidean")

dista.hc=hclust(d=dista, method="ward.D")

fviz_dend(dista.hc, cex=0.5)

den <- as.dendrogram(dista.hc)

plot(den, ylab = "Distância")
rect.hclust(dista.hc, k = 7, border = "red")

nb <- NbClust(z, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "ward.D")

fviz_nbclust(nb)

c = data.frame(cutree(dista.hc, k=3))

# 3 groups
# G1: 412 mun (with treated)
# G2: 248 mun (with treated)
# G3: 190 mun (with treated)
nrow(filter(c, cutree.dista.hc..k...3. == 3))

c3 = c %>%
  filter(cutree.dista.hc..k...3. != 3)

mun_hie = row.names(c3)

mg_c3_18 = mg_2018 %>%
  filter(mun %in% mun_hie)
nrow(filter(mg_c3_18, afetadas == 1))

