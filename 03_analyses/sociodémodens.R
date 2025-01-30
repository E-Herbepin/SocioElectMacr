# Tableaux selon le niveau de densité 
dens7 <- filter (communes, dens==7)
dens6 <- filter (communes, dens==6)
dens5 <- filter (communes, dens==5)
communes_rurales <- filter (communes, dens %in% c(5,6,7))

#Sur la taille des communes 
print(ggplot(communes_rurales, aes(x = pmun21))+geom_histogram(aes(y = ..density..), position = 'identity', bins=100)+facet_wrap(~libdens)+coord_cartesian(xlim = c(0, 10000)))
print(ggplot(communes_rurales, aes(x = pmun21))+geom_histogram(aes(y = ..density..), position = 'identity', bins=300)+facet_wrap(~libdens)+coord_cartesian(xlim = c(0, 3000)))

#Sur les types d'emplois et CSP:
grouped <- communes_rurales%>%
  group_by(dens, libdens)%>% 
  summarise(emp_prec=mean(empl_precaire_pourc, na.rm = TRUE), emp_stable=mean(empl_stable_pourc, na.rm=TRUE), 
            emp_indep=mean(empl_indep_pourc, na.rm = TRUE), 
            emp_employeurs=mean(empl_employeurs_pourc, na.rm = TRUE),
            cs_agri=mean(cs_agri_pourc, na.rm = TRUE),
            cs_acce=mean(cs_acce_pourc, na.rm=TRUE),
            cs_pi=mean(cs_pi_pourc, na.rm = TRUE),
            cs_cpis=mean(cs_cpis_pourc, na.rm = TRUE),
            cs_empl=mean(cs_empl_pourc, na.rm = TRUE),
            cs_ouvr=mean(cs_ouvr_pourc, na.rm = TRUE))
grouped_long <- grouped %>%
  pivot_longer(cols = starts_with("emp"), 
               names_to = "Type_emploi", 
               values_to = "Pourcentage")
grouped_long$Type_emploi <- factor(grouped_long$Type_emploi, levels = c("emp_employeurs", "emp_indep", "emp_stable", "emp_prec"))  

ggplot(grouped_long, aes(x = dens, y = Pourcentage, fill = Type_emploi)) +
  geom_bar(stat = "identity", position = "stack")
grouped_long2 <- grouped %>%
  pivot_longer(cols = starts_with("cs"), 
               names_to = "CSP", 
               values_to = "Pourcentage")
ggplot(grouped_long2, aes(x = dens, y = Pourcentage, fill = CSP)) +
  geom_bar(stat = "identity", position = "stack")

ggplot(communes_rurales, aes(x = cs_agri_pourc))+geom_histogram(aes(y = ..density..), position = 'identity', bins=50)+facet_wrap(~libdens)+coord_cartesian(xlim = c(0, 100))
ggplot(communes_rurales[communes_rurales$empl_precaire_n!=0,], aes(x = empl_precaire_pourc))+geom_histogram(aes(y = ..density..), position = 'identity', bins=50)+facet_wrap(~libdens)+coord_cartesian(xlim = c(0, 100))

