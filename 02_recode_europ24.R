#### RECODAGES RESULTATS ELECTORAUX EUROP 24 ----

# VARIABLES GENERALES ----

europ24 <- europ24_brut %>% 
  mutate(
    code_com = `Code commune`,
    nom_com = `Libellé commune`,
    inscrits = as.numeric(Inscrits),
    votants = as.numeric(Votants))

# VARIABLES RESULTATS ----

## Enlever le signe %, les virgules et recoder en numérique les variables de pourcentage ----

europ24 <- europ24 %>% 
  mutate_all(~ str_replace_all(., "%", "")) %>% 
  mutate_all(~ str_replace_all(., ",", "\\."))

percent_cols <- names(europ24) %>% 
  grep("^%", ., value = TRUE)

voix_cols <- names(europ24) %>% 
  grep("^Voix", ., value = TRUE)

europ24 <- europ24 %>%
  mutate(across(all_of(percent_cols), as.numeric),
         across(all_of(voix_cols), as.numeric),
         abstention_n = as.numeric(Abstentions),
         abstention_pourc = as.numeric(`% Abstentions`))

## création des variables résultats ----

europ24 <- europ24 %>% 
  mutate(
    # Reconquête
    res_reconquete_n = `Voix 3`,
    res_reconquete_pourc = `% Voix/exprimés 3`,
    res_reconquete_pourc_insc = `% Voix/inscrits 3`,
    
    # LFI
    res_lfi_n = `Voix 4`,
    res_lfi_pourc = `% Voix/exprimés 4`,
    res_lfi_pourc_insc = `% Voix/inscrits 4`,
    
    # RN
    res_rn_n = `Voix 5`,
    res_rn_pourc = `% Voix/exprimés 5`,
    res_rn_pourc_insc = `% Voix/inscrits 5`,
    
    # EELV
    res_eelv_n = `Voix 6`,
    res_eelv_pourc = `% Voix/exprimés 6`,
    res_eelv_pourc_insc = `% Voix/inscrits 6`,
    
    # MACRONISTES
    res_macron_n = `Voix 11`,
    res_macron_pourc = `% Voix/exprimés 11`,
    res_macron_pourc_insc = `% Voix/inscrits 11`,
    
    # LR
    res_lr_n = `Voix 18`,
    res_lr_pourc = `% Voix/exprimés 18`,
    res_lr_pourc_insc = `% Voix/inscrits 18`,
    
    # L0
    res_lo_n = `Voix 19`,
    res_lo_pourc = `% Voix/exprimés 19`,
    res_lo_pourc_insc = `% Voix/inscrits 19`, 
    
    # PS - Place publique
    res_ps_n = `Voix 27`,
    res_ps_pourc = `% Voix/exprimés 27`,
    res_ps_pourc_insc = `% Voix/inscrits 27`,
    
    # NPA-R
    res_npar_n = `Voix 22`,
    res_npar_pourc = `% Voix/exprimés 22`,
    res_npar_pourc_insc = `% Voix/inscrits 22`,
    
    # PT
    res_pt_n = `Voix 23`,
    res_pt_pourc = `% Voix/exprimés 23`,
    res_pt_pourc_insc = `% Voix/inscrits 23`,
    
    # PCF
    res_pcf_n = `Voix 33`,
    res_pcf_pourc = `% Voix/exprimés 33`,
    res_pcf_pourc_insc = `% Voix/inscrits 33`,
    
    # Autre Ext Droite
    # 15 = Asselineau ; 21 = "Kuzmanovic Nous le Peuple" ; 24 = Philippot ; 26 = FORTERESSE EUROPE ; 29 = Alliance rurale
    res_extd_n = `Voix 15` + `Voix 21` + `Voix 24` + 
      `Voix 26` + `Voix 29`,
    res_extd_pourc = `% Voix/exprimés 15` + `% Voix/exprimés 21` + 
      `% Voix/exprimés 24` + `% Voix/exprimés 26` + `% Voix/exprimés 29`,
    res_extd_insc = `% Voix/inscrits 15` + `% Voix/inscrits 21` + 
      `% Voix/inscrits 24` + `% Voix/inscrits 26` + `% Voix/inscrits 29`, 
    
    # Autres listes
    res_autres_n = `Voix 1` + `Voix 2` + `Voix 7` + 
      `Voix 8` + `Voix 9` + `Voix 10` + `Voix 12` + `Voix 13` +
      `Voix 14` + `Voix 16` + `Voix 17` + `Voix 20` + `Voix 25` + `Voix 28` +
      `Voix 30` + `Voix 31` + `Voix 32` + `Voix 34` + `Voix 35` + `Voix 36` + 
      `Voix 37` + `Voix 38`,
    res_autres_pourc = `% Voix/exprimés 1` + `% Voix/exprimés 2` + `% Voix/exprimés 7` + 
      `% Voix/exprimés 8` + `% Voix/exprimés 9` + `% Voix/exprimés 10` + 
      `% Voix/exprimés 12` + `% Voix/exprimés 13` +
      `% Voix/exprimés 14` + `% Voix/exprimés 16` + `% Voix/exprimés 17` + 
      `% Voix/exprimés 20` + `% Voix/exprimés 25` + `% Voix/exprimés 28` +
      `% Voix/exprimés 30` + `% Voix/exprimés 31` + `% Voix/exprimés 32` + 
      `% Voix/exprimés 34` + `% Voix/exprimés 35` + `% Voix/exprimés 36` + 
      `% Voix/exprimés 37` + `% Voix/exprimés 38`,
    res_autres_insc = `% Voix/inscrits 1` + `% Voix/inscrits 2` + `% Voix/inscrits 7` + 
      `% Voix/inscrits 8` + `% Voix/inscrits 9` + `% Voix/inscrits 10` + 
      `% Voix/inscrits 12` + `% Voix/inscrits 13` +
      `% Voix/inscrits 14` + `% Voix/inscrits 16` + `% Voix/inscrits 17` + 
      `% Voix/inscrits 20` + `% Voix/inscrits 25` + `% Voix/inscrits 28` +
      `% Voix/inscrits 30` + `% Voix/inscrits 31` + `% Voix/inscrits 32` + 
      `% Voix/inscrits 34` + `% Voix/inscrits 35` + `% Voix/inscrits 36` + 
      `% Voix/inscrits 37` + `% Voix/inscrits 38`) %>% 
  
  mutate(res_extg_pourc = res_lo_pourc + res_npar_pourc + res_pt_pourc)

# VARIABLES ECARTS A LA MOYENNE NATIONALE

rn <- 31.37
macron <- 14.6
ps <- 13.83
lfi <- 9.89
lr <- 7.25
eelv <- 5.5
recon <- 5.47
pcf <- 2.36
extg <- 0.49 + 0.15 + 0.02
autre_extd <- 1.02 + 0.93 + 0.06 + 0.02 + 2.35
abstention <- 48.51

europ24 <- europ24 %>% 
  mutate(
    ecarts_rn = res_rn_pourc - rn,
    ecarts_macron = res_macron_pourc - macron,
    ecarts_ps = res_ps_pourc - ps,
    ecarts_lfi = res_lfi_pourc - lfi,
    ecarts_lr = res_lr_pourc - lr,
    ecarts_eelv = res_eelv_pourc - eelv,
    ecarts_reconquete = res_reconquete_pourc - recon,
    ecarts_pcf = res_pcf_pourc - pcf,
    ecarts_extg = res_extg_pourc - extg,
    ecarts_extd = res_extd_pourc - autre_extd,
    ecarts_abstention = abstention_pourc - abstention)

# CARACTERISATION DE CONFIGURATION DEUX A DEUX ----

## LFI / RN ----

europ24 <- europ24 %>% 
  mutate(
    config_lfi_rn = case_when(
      res_lfi_pourc > lfi & res_rn_pourc > rn ~ "LFI + et RN +",
      res_lfi_pourc > lfi & res_rn_pourc < rn ~ "LFI + et RN -",
      res_lfi_pourc < lfi & res_rn_pourc < rn ~ "LFI - et RN -",
      res_lfi_pourc < lfi & res_rn_pourc > rn ~ "LFI - et RN +",
      res_lfi_pourc == lfi & res_rn_pourc == rn ~ "LFI = RN"))

europ24 %>% 
  tabyl(config_lfi_rn) |> 
  adorn_pct_formatting()

## LR / RN

europ24 <- europ24 %>% 
  mutate(
    config_lr_rn = case_when(
      res_lr_pourc > lr & res_rn_pourc > rn ~ "LR + et RN +",
      res_lr_pourc > lr & res_rn_pourc < rn ~ "LR + et RN -",
      res_lr_pourc < lr & res_rn_pourc < rn ~ "LR - et RN -",
      res_lr_pourc < lr & res_rn_pourc > rn ~ "LR - et RN +",
      res_lr_pourc == lr & res_rn_pourc == rn ~ "LR = RN"))

europ24 %>% 
  tabyl(config_lr_rn) %>% 
  adorn_pct_formatting()


# EXPORT ----

europ24 <- europ24 %>% 
  select(ends_with("com"), 
         inscrits, 
         votants,
         starts_with("abstention_"), 
         starts_with("res"), 
         starts_with("ecarts"),
         starts_with("config"))
