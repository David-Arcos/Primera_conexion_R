cc_reps_1500 <- reps %>% 
  group_by(Codigo, Fecha, Num_ID_Asegurado) %>% 
  dplyr::summarise(Beneficiarios = n(), 
                   Valor_liq = sum(Valor_Siniestro)) %>% 
  ungroup() %>% 
  mutate(Cobertura = ifelse(Valor_liq > 1500, 1500, Valor_liq),
         NumBenf = ifelse(Valor_liq > 0 & Num_ID_Asegurado != "NN", 1, 0)) %>%
  group_by(Fecha, Codigo) %>% 
  dplyr::summarise(Valor_Liquidado = sum(Valor_liq, na.rm = T),
                   Cobertura = sum(Cobertura, na.rm = T),
                   Beneficiarios = sum(NumBenf)) %>% 
  ungroup()