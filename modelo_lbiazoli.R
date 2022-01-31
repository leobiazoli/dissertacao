# Synthetic Control - Leonardo Biazoli


# Packages ----------------------------------------------------------------

library(tidysynth)
require(tidysynth)
library(microsynth)
library(gsynth)
library(Synth)
library(tidyverse)
library(dplyr)


# Loading data ------------------------------------------------------------

df <- read.csv("dados_biazoli.csv", header = T, sep = ',', encoding = 'UTF-8')

mg = df %>%
  drop_na(part_agro, part_industria, part_servicos,
          part_administracao, taxa_estab, dens_pop)

# Model 1 -----------------------------------------------------------------

pib_out_1 <-
  
  mg %>%
  
  # synthetic control object
  synthetic_control(outcome = PIBreal_pc, # variável resposta
                    unit = mun, # agregação
                    time = ano, # tempo
                    i_unit = "mariana", # unidade que sofreu intervenção
                    i_time = 2015, # período que a intervenção ocorreu
                    generate_placebos=T # teste de placebo
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  generate_predictor(time_window = 2002:2014,
                     part_agro = mean(part_agro, na.rm = T),
                     part_industria = mean(part_industria, na.rm = T),
                     part_servicos = mean(part_servicos, na.rm = T),
                     part_administracao = mean(part_administracao, na.rm = T),
                     dens_pop = mean(dens_pop, na.rm = T),
                     taxa_estab = mean(taxa_estab, na.rm = T)
  ) %>%
  
  generate_predictor(time_window = 2010,
                     taxa_alfab = mean(taxa_alfab, na.rm = T)) %>%
  
  generate_predictor(time_window = 2006:2014,
                     trab_sup = mean(trab_sup, na.rm = T),
                     trab_analfabeto = mean(trab_analfabeto, na.rm = T)
  ) %>%
  
  generate_weights(optimization_window = 2002:2014, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

pib_out_1 %>% plot_trends()

pib_out_1 %>% plot_differences()

pib_out_1 %>% grab_predictor_weights() 

pib_out_1 %>% grab_balance_table()

pib_out_1 %>% plot_mspe_ratio ()

pib_out_1 %>% grab_signficance()

pib_out_1 %>% grab_outcome()

pib_out_1 %>% grab_predictors()

pib_out_1 %>% grab_synthetic_control()

pib_out_1 %>% plot_placebos()

# Extract respective predictor matrices

pib_out_1 %>% grab_predictors(type = "treated")

pib_out_1 %>% grab_predictors(type = "controls")

pib_out_1 %>% grab_loss()

pib_out_1 %>% grab_predictor_weights()

pib_out_1 %>% grab_unit_weights() %>% arrange(desc(weight))


# Model 2 -----------------------------------------------------------------

pib_out_2 <-
  
  mg %>%
  
  # synthetic control object
  synthetic_control(outcome = PIBreal_pc, # variável resposta
                    unit = mun, # agregação
                    time = ano, # tempo
                    i_unit = "mariana", # unidade que sofreu intervenção
                    i_time = 2015, # período que a intervenção ocorreu
                    generate_placebos=T # teste de placebo
  ) %>%
  
  # Generate the aggregate predictors used to fit the weights
  
  generate_predictor(time_window = 2002:2014,
                     part_agro = mean(part_agro, na.rm = T),
                     part_industria = mean(part_industria, na.rm = T),
                     part_servicos = mean(part_servicos, na.rm = T),
                     part_administracao = mean(part_administracao, na.rm = T),
                     taxa_estab = mean(taxa_estab, na.rm = T)
  ) %>%
  
  generate_predictor(time_window = 2010,
                     taxa_alfab = mean(taxa_alfab, na.rm = T)) %>%
  
  generate_predictor(time_window = 2006:2014,
                     trab_sup = mean(trab_sup, na.rm = T),
                     trab_analfabeto = mean(trab_analfabeto, na.rm = T)
  ) %>%
  
  generate_weights(optimization_window = 2002:2014, # time to use in the optimization task
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6 # optimizer options
  ) %>%
  
  # Generate the synthetic control
  generate_control()

pib_out_2 %>% plot_trends()

pib_out_2 %>% plot_differences()

pib_out_2 %>% grab_predictor_weights() 

pib_out_2 %>% grab_balance_table()

pib_out_2 %>% plot_mspe_ratio()

pib_out_2 %>% grab_signficance() %>% head(20)

pib_out_2 %>% grab_outcome()

pib_out_2 %>% grab_predictors()

pib_out_2 %>% grab_synthetic_control()

pib_out_2 %>% plot_placebos()

# Extract respective predictor matrices

pib_out_2 %>% grab_predictors(type = "treated")

pib_out_2 %>% grab_predictors(type = "controls")

pib_out_2 %>% grab_loss()

pib_out_2 %>% grab_predictor_weights()

pib_out_2 %>% grab_unit_weights() %>% arrange(desc(weight))

