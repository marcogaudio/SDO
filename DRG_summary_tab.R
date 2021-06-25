# Table for Acuti-regime ordinario with degenza >= 7 days

DRG_table <- SDO_ordinario %>% 
  dplyr::filter(`DEGENZA MEDIA (giorni)` >= 7) %>% 
  dplyr::select(TIPO, DRG, DIMISSIONI, `DEGENZA MEDIA (giorni)`, CLASSE_MDC)

