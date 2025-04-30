#### RECODAGES FICHIERS RECENSEMENT EAR2020 ----

## STATUT D'EMPLOI ----

# EMPL : Conditions d'emploi
# 11 : En contrat d'apprentissage ou de professionnalisation
# 12 : Placés par une agence d'intérim
# 13 : En emplois jeunes, CES (contrats emploi solidarité), contrats de qualification ou autres emplois aidés
# 14 : Stagiaires rémunérés en entreprise
# 15 : Autres emplois à durée limitée, CDD (contrats à durée déterminée), contrats courts, saisonniers, vacataires...
# 16 : Emplois sans limite de durée, CDI (contrats à durée indéterminée), titulaires de la fonction publique
# 21 : Non salariés : Indépendants
# 22 : Non salariés : Employeurs
# 23 : Non salariés : Aides familiaux

# census20_act2b |> 
#   names()

ear20_emploi <- census20_act2b |> 
  # sommer les variables par catégorie de variable EMPLXX
  mutate(empl11 = rowSums(across(contains("EMPL11")), na.rm = TRUE),
         empl12 = rowSums(across(contains("EMPL12")), na.rm = TRUE),
         empl13 = rowSums(across(contains("EMPL13")), na.rm = TRUE),
         empl14 = rowSums(across(contains("EMPL14")), na.rm = TRUE),
         empl15 = rowSums(across(contains("EMPL15")), na.rm = TRUE),
         empl16 = rowSums(across(contains("EMPL16")), na.rm = TRUE),
         empl21 = rowSums(across(contains("EMPL21")), na.rm = TRUE),
         empl22 = rowSums(across(contains("EMPL22")), na.rm = TRUE),
         empl23 = rowSums(across(contains("EMPL23")), na.rm = TRUE),
         empl_tot = rowSums(across(starts_with("EMPL")), na.rm = TRUE),
         # catégorisation : précaires / stables / inépendants / employeurs : effectifs       
         empl_precaire_n = 
           empl11 + empl12 + empl13 + 
           empl14 + empl15,
         empl_stable_n = empl16,
         empl_indep_n = empl21,
         empl_employeurs_n = empl22,
         # catégorisation : précaires / stables / indépendants / employeurs : pourcentages        
         empl_precaire_pourc = 
           100 * empl_precaire_n / empl_tot,
         empl_stable_pourc = 
           100 * empl_stable_n / empl_tot,
         empl_indep_pourc = 
           100 * empl_indep_n / empl_tot,
         empl_employeurs_pourc = 
           100 * empl_employeurs_n / empl_tot
  ) |> 
  select(CODGEO, LIBGEO, starts_with("empl")) |> 
  rename_all(tolower)

# summary(ear20_emploi$empl_precaire_pourc)

# PCS REGROUPEE ----

#  `ACT4` --> CS1_6
# CS1_6 : Catégorie socioprofessionnelle regroupée (6 postes)
# 1 : Agriculteurs exploitants
# 2 : Artisans, commerçants, chefs entreprise
# 3 : Cadres et professions intellectuelles supérieures
# 4 : Professions intermédiaires
# 5 : Employés
# 6 : Ouvriers

# census20_act4 |> 
#   names()

ear20_pcs <- census20_act4 |> 
  mutate(cs_agri_n = rowSums(across(contains("CS1_61")), na.rm = TRUE),
         cs_acce_n = rowSums(across(contains("CS1_62")), na.rm = TRUE),
         cs_cpis_n = rowSums(across(contains("CS1_63")), na.rm = TRUE),
         cs_pi_n = rowSums(across(contains("CS1_64")), na.rm = TRUE),
         cs_empl_n = rowSums(across(contains("CS1_65")), na.rm = TRUE),
         cs_ouvr_n = rowSums(across(contains("CS1_66")), na.rm = TRUE),
         cs_tot = cs_agri_n + cs_acce_n + cs_cpis_n +
           cs_pi_n + cs_empl_n + cs_ouvr_n,
         
         cs_ouvr_pourc = 
           100 * cs_ouvr_n / cs_tot,
         cs_agri_pourc = 
           100 * cs_agri_n / cs_tot,
         cs_acce_pourc = 
           100 * cs_acce_n / cs_tot,
         cs_pi_pourc = 
           100 * cs_pi_n / cs_tot,
         cs_empl_pourc = 
           100 * cs_empl_n / cs_tot,
         cs_cpis_pourc = 
           100 * cs_cpis_n / cs_tot,
         
         cs_ouvr_quartile = 
           quant.cut(cs_ouvr_pourc, 4,
                     labels = c("Ouvr. Q1",
                                "Ouvr. Q2",
                                "Ouvr. Q3",
                                "Ouvr. Q4")),
         cs_empl_quartile = 
           quant.cut(cs_empl_pourc, 4,
                     labels = c("Empl. Q1",
                                "Empl. Q2",
                                "Empl. Q3",
                                "Empl. Q4")),
         cs_cpis_quartile = 
           quant.cut(cs_cpis_pourc, 4,
                     labels = c("CPIS Q1",
                                "CPIS Q2",
                                "CPIS Q3",
                                "CPIS Q4"))
  ) |> 
  select(CODGEO, LIBGEO, starts_with("cs_")) |> 
  rename_all(tolower)



# CS DETAILLEE ----

# EMP3 --> CS3_29 

# CS3_29 : Catégorie socioprofessionnelle détaillée (29 postes)
# 10 : Agriculteurs exploitants
# 21 : Artisans
# 22 : Commerçants et assimilés
# 23 : Chefs d'entreprise de 10 salariés ou plus
# 31 : Professions libérales
# 33 : Cadres de la fonction publique
# 34 : Professeurs, professions scientifiques
# 35 : Professions de l'information, des arts et des spectacles
# 37 : Cadres administratifs et commerciaux d'entreprise
# 38 : Ingénieurs et cadres techniques d'entreprise
# 42 : Professeurs des écoles, instituteurs et assimilés
# 43 : Professions intermédiaires de la santé et du travail social
# 44 : Clergé, religieux
# 45 : Professions intermédiaires de la fonction publique
# 46 : Professions intermédiaires administratives et commerciales des entreprises
# 47 : Techniciens
# 48 : Contremaîtres, agents de maîtrise
# 52 : Employés civile et agents de service fonction publique
# 53 : Policiers et militaires
# 54 : Employés administratifs d'entreprise
# 55 : Employés de commerce
# 56 : Personnels des services aux particuliers
# 62 : Ouvriers qualifiés de type industriel
# 63 : Ouvriers qualifiés de type artisanal
# 64 : Chauffeurs
# 65 : Ouvriers qualifiés de la manutention, du magasinage et du transport
# 67 : Ouvriers non qualifiés de type industriel
# 68 : Ouvriers non qualifiés de type artisanal
# 69 : Ouvriers agricoles

# census20_emp3 |> 
#   names()

ear20_cs29 <- census20_emp3 |> 
  mutate(cs_10 = rowSums(across(contains("CS3_2910")), na.rm = TRUE),
         cs_21 = rowSums(across(contains("CS3_2921")), na.rm = TRUE),
         cs_22 = rowSums(across(contains("CS3_2922")), na.rm = TRUE),
         cs_23 = rowSums(across(contains("CS3_2923")), na.rm = TRUE),
         cs_31 = rowSums(across(contains("CS3_2931")), na.rm = TRUE),
         cs_33 = rowSums(across(contains("CS3_2933")), na.rm = TRUE),
         cs_34 = rowSums(across(contains("CS3_2934")), na.rm = TRUE),
         cs_35 = rowSums(across(contains("CS3_2935")), na.rm = TRUE),
         cs_37 = rowSums(across(contains("CS3_2937")), na.rm = TRUE),
         cs_38 = rowSums(across(contains("CS3_2938")), na.rm = TRUE),
         cs_42 = rowSums(across(contains("CS3_2942")), na.rm = TRUE),
         cs_43 = rowSums(across(contains("CS3_2943")), na.rm = TRUE),
         cs_44 = rowSums(across(contains("CS3_2944")), na.rm = TRUE),
         cs_45 = rowSums(across(contains("CS3_2945")), na.rm = TRUE),
         cs_46 = rowSums(across(contains("CS3_2946")), na.rm = TRUE),
         cs_47 = rowSums(across(contains("CS3_2947")), na.rm = TRUE),
         cs_48 = rowSums(across(contains("CS3_2948")), na.rm = TRUE),
         cs_52 = rowSums(across(contains("CS3_2952")), na.rm = TRUE),
         cs_53 = rowSums(across(contains("CS3_2953")), na.rm = TRUE),
         cs_54 = rowSums(across(contains("CS3_2954")), na.rm = TRUE),
         cs_55 = rowSums(across(contains("CS3_2955")), na.rm = TRUE),
         cs_56 = rowSums(across(contains("CS3_2956")), na.rm = TRUE),
         cs_62 = rowSums(across(contains("CS3_2962")), na.rm = TRUE),
         cs_63 = rowSums(across(contains("CS3_2963")), na.rm = TRUE),
         cs_64 = rowSums(across(contains("CS3_2964")), na.rm = TRUE),
         cs_65 = rowSums(across(contains("CS3_2965")), na.rm = TRUE),
         cs_67 = rowSums(across(contains("CS3_2967")), na.rm = TRUE),
         cs_68 = rowSums(across(contains("CS3_2968")), na.rm = TRUE),
         cs_69 = rowSums(across(contains("CS3_2969")), na.rm = TRUE)) |> 
  select(CODGEO, LIBGEO, starts_with("cs_")) |> 
  rename_all(tolower)

# TYPES DE LOGEMENTS ----
# LOG1 --> CATL

# CATL : Catégorie de logement
# 1 : Résidences principales
# 2 : Logements occasionnels
# 3 : Résidences secondaires
# 4 : Logements vacants


# census20_log1 |> 
#   names()

ear20_log <- census20_log1 |> 
  mutate(log_resprinc = rowSums(across(contains("CATL1")), na.rm = TRUE),
         log_logoccas = rowSums(across(contains("CATL2")), na.rm = TRUE),
         log_ressecond = rowSums(across(contains("CATL3")), na.rm = TRUE),
         log_vacants = rowSums(across(contains("CATL4")), na.rm = TRUE),
         
         log_vacants_pourc = 
           100 * (log_vacants / (log_resprinc +
                                   log_logoccas +
                                   log_ressecond +
                                   log_ressecond)),
         log_vacants_pourc = if_else(
           log_resprinc +
             log_logoccas +
             log_ressecond +
             log_ressecond == 0,
           NA_integer_,
           log_vacants_pourc),
         log_vacants_pourc = if_else(
           log_vacants_pourc > 100,
           NA_integer_,
           log_vacants_pourc)) |>
  select(CODGEO, LIBGEO, starts_with("log_")) |> 
  rename_all(tolower)

# summary(ear20_log$log_vacants)
# summary(ear20_log$log_vacants_pourc)
# 
# ggplot(ear20_log) + 
#   aes(x = log_vacants_pourc) + 
#   geom_histogram(col = "white")
# 

# IMMIGRES ----
# IMG1 --> IMMI

# IMMI : Situation quant à l'immigration
# 1 : Immigrés
# 2 : Non immigrés


# census20_img1A |> 
  # names()

ear20_immi <- census20_img1A |> 
  mutate(immi_oui = rowSums(across(contains("IMMI1")), na.rm = TRUE),
         immi_non = rowSums(across(contains("IMMI2")), na.rm = TRUE)) |> 
  select(CODGEO, LIBGEO, starts_with("immi_")) |> 
  rename_all(tolower)

# ETRANGERS ----
# NAT1 --> INATC

# INATC : Indicateur de nationalité condensé (Français/Étranger)
# 1 : Français
# 2 : Etrangers

# census20_nat1 |> 
  # names()

ear20_natio <- census20_nat1 |> 
  mutate(nat_fr = rowSums(across(contains("INATC1")), na.rm = TRUE),
         nat_etr = rowSums(across(contains("INATC2")), na.rm = TRUE)) |> 
  select(CODGEO, LIBGEO, starts_with("nat_")) |> 
  rename_all(tolower)

# summary(ear20_natio$nat_fr)

# AGE 

ear20_age <- census20_pop1B |>
  mutate (Age_q1_5 = rowSums(across(any_of(c(ends_with('001'),ends_with('002'),ends_with('003'),ends_with('004'),
                               ends_with('005')))), na.rm=TRUE),
          Age_q6_10 = rowSums(across(any_of(c(ends_with('006'),ends_with('007'),ends_with('008'),ends_with('009'),
                               ends_with('0010')))), na.rm=TRUE),
          Age_q11_15 = rowSums(across(any_of(c(ends_with('011'),ends_with('012'),ends_with('013'),ends_with('014'),
                               ends_with('015')))), na.rm=TRUE),
          Age_q16_20 = rowSums(across(any_of(c(ends_with('016'),ends_with('017'),ends_with('018'),ends_with('019'),
                                  ends_with('020')))), na.rm=TRUE),
          Age_q21_25 = rowSums(across(any_of(c(ends_with('021'),ends_with('022'),ends_with('023'),ends_with('024'),
                                  ends_with('025')))), na.rm=TRUE),
          Age_q26_30 = rowSums(across(any_of(c(ends_with('026'),ends_with('027'),ends_with('028'),ends_with('029'),
                                  ends_with('030')))), na.rm=TRUE),
          Age_q31_35 = rowSums(across(any_of(c(ends_with('031'),ends_with('032'),ends_with('033'),ends_with('034'),
                                  ends_with('035')))), na.rm=TRUE),
          Age_q36_40 = rowSums(across(any_of(c(ends_with('036'),ends_with('037'),ends_with('038'),ends_with('039'),
                                  ends_with('040')))), na.rm=TRUE),
          Age_q41_45 = rowSums(across(any_of(c(ends_with('041'),ends_with('042'),ends_with('043'),ends_with('044'),
                                  ends_with('045')))), na.rm=TRUE),
          Age_q46_50 = rowSums(across(any_of(c(ends_with('046'),ends_with('047'),ends_with('048'),ends_with('049'),
                                  ends_with('050')))), na.rm=TRUE),
          Age_q51_55 = rowSums(across(any_of(c(ends_with('051'),ends_with('052'),ends_with('053'),ends_with('054'),
                                  ends_with('055')))), na.rm=TRUE),
          Age_q56_60 = rowSums(across(any_of(c(ends_with('056'),ends_with('057'),ends_with('058'),ends_with('059'),
                                  ends_with('060')))), na.rm=TRUE),
          Age_q61_65 = rowSums(across(any_of(c(ends_with('061'),ends_with('062'),ends_with('063'),ends_with('064'),
                                  ends_with('065')))), na.rm=TRUE),
          Age_q66_70 = rowSums(across(any_of(c(ends_with('066'),ends_with('067'),ends_with('068'),ends_with('069'),
                                  ends_with('070')))), na.rm=TRUE),
          Age_q71_75 = rowSums(across(any_of(c(ends_with('071'),ends_with('072'),ends_with('073'),ends_with('074'),
                                  ends_with('075')))), na.rm=TRUE),
          Age_q76_80 = rowSums(across(any_of(c(ends_with('076'),ends_with('077'),ends_with('078'),ends_with('079'),
                                  ends_with('080')))), na.rm=TRUE),
          Age_q81_85 = rowSums(across(any_of(c(ends_with('081'),ends_with('082'),ends_with('083'),ends_with('084'),
                                  ends_with('085')))), na.rm=TRUE),
          Age_q86_90 = rowSums(across(any_of(c(ends_with('086'),ends_with('087'),ends_with('088'),ends_with('089'),
                                  ends_with('090')))), na.rm=TRUE),
          Age_q91_95 = rowSums(across(any_of(c(ends_with('091'),ends_with('092'),ends_with('093'),ends_with('094'),
                                  ends_with('095')))), na.rm=TRUE),
          Age_q96_100 = rowSums(across(any_of(c(ends_with('096'),ends_with('097'),ends_with('098'),ends_with('099'),
                                  ends_with('100')))), na.rm=TRUE),)|> 
  select(CODGEO, LIBGEO, starts_with("Age_q")) |> 
  rename_all(tolower)
#Valeur moyenne de l'âge de la population et répartition en quartiles   
age<- census20_pop1B%>% 
  pivot_longer(cols=starts_with("SEXE"), names_to = "Age",values_to = "Nombre")%>%
  mutate(sexe=substr(Age,5,5),
         Age=as.numeric(substr(Age, 14,16)))%>%
  group_by(CODGEO,LIBGEO,Age)%>%
  summarise(Nombre=sum(Nombre, na.rm=TRUE))%>%
  filter(Nombre!=0)%>%
  group_by(CODGEO,LIBGEO)%>%
  summarise(age_moy=weighted.mean(Age,Nombre, na.rm=TRUE),pop=sum(Nombre,na.rm=TRUE), 
            age_q1=wtd.quantile(Age,Nombre,p=c(0.25,0.5,0.75), na.rm = TRUE )[1],
            age_q2=wtd.quantile(Age,Nombre,p=c(0.25,0.5,0.75), na.rm = TRUE )[2],
            age_q3=wtd.quantile(Age,Nombre,p=c(0.25,0.5,0.75), na.rm = TRUE )[3])%>%
  rename_all(tolower)

# DENSITE URBAINE ----
# LIBDENS

densite_communes <- densite_communes |> 
  select(CODGEO, LIBGEO, DENS, LIBDENS, PMUN21) |> 
  rename_all(tolower)

# FUSION DES TABLES ----

communes <- ear20_emploi |> 
  left_join(ear20_pcs) |> 
  left_join(ear20_cs29) |> 
  left_join(ear20_log) |> 
  left_join(ear20_immi) |> 
  left_join(ear20_natio) |> 
  left_join(age)|>
  left_join(densite_communes)

communes<- communes%>% mutate (across(c(age_moy,pop, age_q1,age_q2,age_q3), ~replace_na(.x,0)))

write_csv(communes, "recensement_communes.csv")
