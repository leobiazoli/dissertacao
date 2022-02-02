# PSM without cluster analysis - Leonardo Biazoli

# Packages ----------------------------------------------------------------

library(MatchIt)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(sidrar)
library(dplyr)
library(plotly)

# Loading data ------------------------------------------------------------

df = read.csv("data_proc_psm.csv", dec = ".", sep = ",", encoding = "UTF-8")

# Descriptive analysis ----------------------------------------------------

mg_gr = df %>% 
  filter(mun %in% c("mariana", 'aimores', 'alpercata', 'barra longa', 'belo oriente', 'bom jesus do galho', 'bugre', 'caratinga', 'conselheiro pena', 'corrego novo', 
                    'dionisio', 'fernandes tourinho', 'galileia', 'governador valadares', 'iapu', 'ipaba', 'ipatinga', 'itueta', 'marlieria',
                    'naque', 'periquito', "pingo d'agua", 'raul soares', 'resplendor', 'rio casca', 'rio doce', 'santa cruz do escalvado',
                    'santana do paraiso', 'sao domingos do prata', 'sao jose do goiabal', 'sao pedro dos ferros', 'sem-peixe', 'sobralia', 
                    'timoteo', 'tumiritinga')) %>%
  select(c(ano, mun, PIBreal, PIBreal_pc))

mg_gr$especial <- ifelse(mg_gr$mun == 'mariana', 'mariana', 'municipios_mineiros_atingidos')


gg1 <-  ggplot(data = mg_gr, aes(x=ano, y=PIBreal_pc, colours=mun))+
  geom_line(aes(color=especial))+
  geom_vline(xintercept = 2015, color  = "black", 
             linetype = "dashed")+
  labs(y = expression(paste("PIB real",italic(" per capita"))),
       x = "ano")+ 
  theme_classic() + 
  scale_color_manual(labels= c("Mariana", "Municípios mineiros impactados") ,values = c(mariana = 'red', municipios_mineiros_atingidos = 'grey'))+
  guides(color=guide_legend(NULL)) + theme(legend.position = "bottom")
gg1

# Model 2016 -------------------------------------------------------------

mg_2016 = df %>%
  filter(ano == 2016)

# T-test

with(mg_2016, t.test(PIBreal_pc ~ afetadas))
# As médias entre os grupos não são iguais antes de parear

# Covariates

cov1 = c('part_agro', 'part_industria', 'part_servicos', 
         'part_administracao', 'taxa_estab', 'trab_sup', 
         'trab_analfabeto', 'dens_pop')

nrow(filter(mg_2016, afetadas == 1))


mg_2016 %>%
  group_by(afetadas) %>%
  select(one_of(cov1)) %>%
  summarise_all(funs(mean(., na.rm=T)))

lapply(cov1, function(v){
  t.test(mg_2016[,v] ~ mg_2016[,'afetadas'])
})

# Covariates: 2,3,4,6,7 e 8 equal means

# Balancing before matching

row.names(mg_2016) = mg_2016$mun

df_nomiss = mg_2016 %>%
  select(PIBreal_pc, afetadas, one_of(cov1)) %>%
  na.omit()

model = matchit(afetadas ~ part_agro + part_industria + part_servicos +
                  part_administracao + taxa_estab + trab_sup +
                  trab_analfabeto + dens_pop,
                method = NULL, distance = "glm", data = df_nomiss)

summary(model)

# Propensity score matching

mps = glm(afetadas ~ part_agro + part_industria + part_servicos +
            part_administracao + taxa_estab + trab_sup +
            trab_analfabeto + dens_pop, family = binomial(), 
          data = mg_2016)
summary(mps)

ps_df = data.frame(p_s = predict(mps, type = "response"), 
                   afetadas = mps$model$afetadas)

# Histogram of Propensity Score
labs = paste("Rompimento em Mariana:", c("sim", "não"))

ps_df %>%
  mutate(afetadas = ifelse(afetadas == 1, labs[1], labs[2])) %>%
  ggplot(aes(x=p_s)) + geom_histogram(color='white') +
  facet_wrap(~afetadas) + 
  xlab("Propensity Score") + 
  theme_classic()

# Matching algorithm

model = matchit(afetadas ~ part_agro + part_industria + part_servicos +
                  part_administracao + taxa_estab + trab_sup +
                  trab_analfabeto + dens_pop,
                method = "nearest", data = df_nomiss)

summary(model)
plot(model)

plot(model, type = "jitter", interactive = FALSE, legend="nada")

pards = match.data(model)

pards %>%
  group_by(afetadas) %>%
  select(one_of(cov1)) %>%
  summarise_all(funs(mean))

# Checking balancing

lapply(cov1, function(v){
  t.test(pards[,v] ~ pards$afetadas)
})

# Average treatment effect

with(pards, t.test(PIBreal_pc ~ afetadas))


# Model 2017 -------------------------------------------------------------

mg_2017 = df %>%
  filter(ano == 2017)

# T-test 
with(mg_2017, t.test(PIBreal_pc ~ afetadas))
# As médias entre os grupos são iguais antes de parear

# Covariates

cov1 = c('part_agro', 'part_industria', 'part_servicos', 
         'part_administracao', 'taxa_estab', 'trab_sup', 
         'trab_analfabeto', 'dens_pop')


mg_2017 %>%
  group_by(afetadas) %>%
  select(one_of(cov1)) %>%
  summarise_all(funs(sd(., na.rm=T)))

lapply(cov1, function(v){
  t.test(mg_2017[,v] ~ mg_2017[,'afetadas'])
})

# Propensity score matching

mps = glm(afetadas ~ part_agro + part_industria + part_servicos +
            part_administracao + taxa_estab + trab_sup +
            trab_analfabeto + dens_pop, family = binomial(), 
          data = mg_2017)
summary(mps)


ps_df = data.frame(p_s = predict(mps, type = "response"), 
                   afetadas = mps$model$afetadas)

# Matching algorithm

row.names(mg_2017) = mg_2017$mun

df_nomiss = mg_2017 %>%
  select(PIBreal_pc, afetadas, one_of(cov1)) %>%
  na.omit()

model = matchit(afetadas ~ part_agro + part_industria + part_servicos +
                  part_administracao + taxa_estab + trab_sup +
                  trab_analfabeto + dens_pop,
                method = "nearest", data = df_nomiss)

summary(model)
plot(model)

pards = match.data(model)

# Checking balancing

lapply(cov1, function(v){
  t.test(pards[,v] ~ pards$afetadas)
})

# Average treatment effect

with(pards, t.test(PIBreal_pc ~ afetadas))


# Modelo 2018 -------------------------------------------------------------

mg_2018 = df %>%
  filter(ano == 2018)

# T-test 
with(mg_2018, t.test(PIBreal_pc ~ afetadas))

# Covariates

cov1 = c('part_agro', 'part_industria', 'part_servicos', 
         'part_administracao', 'taxa_estab', 'trab_sup', 
         'trab_analfabeto', 'dens_pop')


mg_2018 %>%
  group_by(afetadas) %>%
  select(one_of(cov1)) %>%
  summarise_all(funs(sd(., na.rm=T)))

lapply(cov1, function(v){
  t.test(mg_2018[,v] ~ mg_2018[,'afetadas'])
})

# Propensity score matching

mps = glm(afetadas ~ part_agro + part_industria + part_servicos +
            part_administracao + taxa_estab + trab_sup +
            trab_analfabeto + dens_pop, family = binomial(), 
          data = mg_2018)
summary(mps)

ps_df = data.frame(p_s = predict(mps, type = "response"), 
                   afetadas = mps$model$afetadas)

# Matching algorithm

row.names(mg_2018) = mg_2018$mun

df_nomiss = mg_2018 %>%
  select(PIBreal_pc, afetadas, one_of(cov1)) %>%
  na.omit()

model = matchit(afetadas ~ part_agro + part_industria + part_servicos +
                  part_administracao + taxa_estab + trab_sup +
                  trab_analfabeto + dens_pop,
                method = "nearest", data = df_nomiss)

summary(model)
plot(model)

pards = match.data(model)

# Checking balancing

lapply(cov1, function(v){
  t.test(pards[,v] ~ pards$afetadas)
})

# Average treatment effect

with(pards, t.test(PIBreal_pc ~ afetadas))