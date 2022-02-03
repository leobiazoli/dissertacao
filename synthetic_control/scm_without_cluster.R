# Modelo 1 - Synthetic Control - BIAZOLI, L.
# PIB real per capita sem agrupamento

# Packages ----------------------------------------------------------------

library(tidysynth)
require(tidysynth)
library(microsynth)
library(gsynth)
library(Synth)
library(tidyverse)
library(dplyr)


# Loading data ------------------------------------------------------------

df <- read.csv("data_proc_scm.csv", header = T, sep = ',', encoding = 'UTF-8')

mg = df %>%
  drop_na(part_agro, part_industria, part_servicos,
          part_administracao, taxa_estab, dens_pop)

# Model 1 -----------------------------------------------------------------

start.time <- Sys.time()

pib_out_1 <-
  
  mg %>%
  
  # synthetic control object
  synthetic_control(outcome = PIBreal_pc, # outcome
                    unit = mun, # aggregation
                    time = ano, # time
                    i_unit = "mariana", # unit treated
                    i_time = 2015, # period of intervention
                    generate_placebos=T # placebo test
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

save(pib_out_1,file="pib_out_1.rda")

end.time <- Sys.time()

time.taken <- end.time - start.time

time.taken

pib_out_1 %>% plot_trends()

pib_out_1 %>% plot_differences()

pib_out_1 %>% grab_unit_weights() %>% arrange(desc(weight))

a = pib_out_1 %>% grab_signficance()

a %>%
  filter(type!="Donor")

pib_out_1 %>% grab_synthetic_control()

pib_out_1 %>% grab_balance_table()

pib_out_1 %>% plot_placebos()

pib_out_1 %>% plot_mspe_ratio()

pib_out_1 %>% grab_predictor_weights()


plot_trends <- function(data,time_window=NULL){
  
  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}
  
  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  
  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}
  
  # Generate plot
  data %>%
    grab_synthetic_control(placebo = FALSE) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::rename(Sintetico= synth_y,
                  Observado= real_y) %>%
    tidyr::pivot_longer(cols = c(Observado,Sintetico)) %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,value,color=name,linetype=name)) +
    ggplot2::geom_vline(xintercept = trt_time,color="black",linetype=2) +
    ggplot2::geom_line(size=1,alpha=.7) +
    ggplot2::geom_point() +
    ggplot2::scale_color_manual(values=c("grey50","black")) +
    ggplot2::scale_linetype_manual(values=c(1,4)) +
    ggplot2::labs(color="",linetype="",y=expression(paste("PIB real",italic(" per capita"))),x=time_index,
                  title=paste0(""),
                  caption = "") +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")
}

plot_differences <- function(data,time_window=NULL){
  
  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}
  
  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  treatment_unit <- data$.meta[[1]]$treatment_unit[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  
  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}
  
  # Generate plot
  data %>%
    grab_synthetic_control(placebo = FALSE) %>%
    dplyr::mutate(diff = real_y-synth_y) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,diff)) +
    ggplot2::geom_hline(yintercept = 0,color="black",linetype=2) +
    ggplot2::geom_vline(xintercept = trt_time,color="black") +
    ggplot2::geom_line(size=1,alpha=.75,color="black") +
    ggplot2::geom_point(color="black") +
    ggplot2::labs(color="",linetype="",y=expression(paste("Diferença no PIB real",italic(" per capita"))),x=time_index,
                  title=paste0("")) +
    ggplot2::theme_classic()
}

plot_placebos <- function(data,time_window=NULL,prune=TRUE){
  
  # Check if .meta is in data.
  if(!(".meta" %in% colnames(data))){stop("`.meta` column has been removed. `.meta` column needs to be included for `generte_control()` to work.")}
  
  # Grab meta data
  trt_time <- data$.meta[[1]]$treatment_time[1]
  time_index <- data$.meta[[1]]$time_index[1]
  treatment_unit <- data$.meta[[1]]$treatment_unit[1]
  unit_index <- data$.meta[[1]]$unit_index[1]
  outcome_name <- data$.meta[[1]]$outcome[1]
  
  # If no time window is specified for the plot, plot the entire series
  if(is.null(time_window)){ time_window <- unique(data$.original_data[[1]][[time_index]])}
  
  # Generate plot data
  plot_data <-
    data %>%
    grab_synthetic_control(placebo = TRUE) %>%
    dplyr::mutate(diff = real_y-synth_y) %>%
    dplyr::filter(time_unit %in% time_window) %>%
    dplyr::mutate(type_text = ifelse(.placebo==0,treatment_unit,"unidades controle"),
                  type_text = factor(type_text,levels=c(treatment_unit,"unidades controle")))
  
  
  # Pruning implementation-- if one of the donors falls outside two standard
  # deviations of the rest of the pool, it's dropped.
  caption <- ""
  if (prune){
    
    # Gather significance field
    sig_data = data %>% grab_signficance(time_window = time_window)
    
    # Treated units Pre-Period RMSPE
    thres <-
      sig_data %>%
      dplyr::filter(type=="Treated") %>%
      dplyr::pull(pre_mspe) %>%
      sqrt(.)
    
    # Only retain units that are 2 times the treated unit RMSPE.
    retain_ <-
      sig_data %>%
      dplyr::select(unit_name,pre_mspe) %>%
      dplyr::filter(sqrt(pre_mspe) <= thres*2) %>%
      dplyr::pull(unit_name)
    
    plot_data <- plot_data %>% dplyr::filter(.id %in% retain_)
    caption <- ""
  }
  
  # Generate plot
  plot_data %>%
    ggplot2::ggplot(ggplot2::aes(time_unit,diff,group=.id,
                                 color=type_text,
                                 alpha=type_text,
                                 size=type_text)) +
    ggplot2::geom_hline(yintercept = 0,color="black",linetype=2) +
    ggplot2::geom_vline(xintercept = trt_time,color="black",linetype=3) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(values=c("red","grey60")) +
    ggplot2::scale_alpha_manual(values=c(1,.4)) +
    ggplot2::scale_size_manual(values=c(1,.5)) +
    ggplot2::labs(color="",alpha="",size="",y=expression(paste("Diferença no PIB real",italic(" per capita"))),x=time_index,
                  title=paste0(""),
                  caption = caption) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position="bottom")
}
