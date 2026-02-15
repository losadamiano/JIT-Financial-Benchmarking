library(readxl)

#Carico il dataset con all'interno dati trovati su aida, nel quale è presente 
#anche una variabile dummy che rappresenta il momento di inizio produzione
#(1 se ha cominciato subito, 0 se ha cominciato in un secondo momento)
dataset <- read_excel("data_finale.xlsx")


library(tidyverse)
## calcolo gli indici mancanti e li aggiungo al dataset 

dataset <- dataset %>% 
  mutate(current_ratio = attivo_circolante / debiti_breve ,
         quick_ratio = ( attivo_circolante - tot_rimanenze  ) / debiti_breve ,
         immobiliz_su_tot_attivo = tot_immobilizzazioni / ( tot_immobilizzazioni +
                                                              attivo_circolante  ))
library(FactoMineR)

## rimuovo le variabili che non mi servono ai fini della pca

dataset_pca <- dataset %>% 
  select(codice_fiscale, ricavi_vendite, ROA, ROS, EBITDAMargin, CCN, PFN, utile_netto,
         current_ratio, quick_ratio, immobiliz_su_tot_attivo )

#CONTROLLI VARI
# Controlla i valori NA
sapply(dataset_pca, function(x) sum(is.na(x)))

# Controlla i valori Inf
sapply(dataset_pca, function(x) sum(is.infinite(x)))


library(dplyr)

#Rendo la variabile codice fiscale il nome delle righe così non impatta sul calcolo
#della pca
dataset_finale <- dataset_pca %>% 
  group_by(codice_fiscale) %>% 
  column_to_rownames("codice_fiscale")

#Ora che il dataset è protno eseguo la pca e ne verifico gli output 
pca_dati <- PCA(dataset_finale, scale.unit = T, graph = F)
plot(pca_dati)

#La maggior parte dei punti è addensata attorno all'origine (0,0),
#suggerendo che molte aziende sono simili nei principali indicatori.
#Il grafico non mostra una separazione netta in cluster visibili

pca_dati$eig

library(factoextra) # Assicurati di aver caricato questo pacchetto

# Grafico del Cerchio di Correlazione (Variabili Factor Map)
fviz_pca_var(pca_dati,
             col.var = "cos2",       # Colora le variabili in base alla loro qualità di rappresentazione (cos2)
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), # Palette di colori per la qualità
             repel = TRUE,           # Evita la sovrapposizione delle etichette delle variabili
             ggtheme = theme_minimal() # Usa un tema grafico minimalista
)

#Dallo scree plot si capisce che i primi 3 componenti principali sono 
#i più significativi e catturano la maggior parte della varianza utile nei nostri dati,
#per via del "gomito" (o "elbow") pronunciato tra il terzo e il quarto componente.

pca_dati$var$cor
pca_dati$eig
pca_dati$var$coord
var_coord <- as.data.frame(pca_dati$var$coord)
#install.packages("writexl")
library(writexl)
write_xlsx(var_coord, path = "var_coord.xlsx")
#scegliamo quindi le prime 3 componenti principali e carichiamo la tabella su excel
write_xlsx(var_coord[1:3], path = "var_coord2.xlsx")
#La tabella mostra la correlazione tra le componenti e gli indici, e sembrano 
#andare in ordine di correlazione decrescente, inoltre la prima componente è
#correlata quasi del tutto positivamente 
#Valori alti (positivi o negativi): Indicano una forte associazione tra la variabile e il componente.
#Segno (+ o -): Indica la direzione della relazione. Variabili con lo stesso 
#segno sui caricamenti contribuiscono allo stesso modo al componente, mentre quelle con segni opposti contribuiscono in direzioni opposte.
## su excel verifco tramite la tabella se è il caso di effettuare la rotazione

### effettuo la rotazione 
install.packages("psych") 
library(psych)           
### ruoto
# Estrai le coordinate delle variabili per le prime 3 componenti principali
# Le 'coord' rappresentano i loadings (coefficienti di correlazione tra variabili originali e componenti)
# Scegli il numero di colonne corrispondente alle componenti che vuoi ruotare (nfactors = 3)
#La rotazione la effettuo sulla tabella originale di dati usata anche per la pca
# Ora applica la funzione principal() ai loadings estratti
# nfactors deve corrispondere al numero di colonne che hai estratto
rc <- principal(dataset_finale, nfactors = 3, rotate = 'varimax', scores = TRUE)

# Puoi poi stampare i risultati della rotazione
print(rc)

## questi sono i punti del piano delle varibili ruotate 
rc$scores
data_scores <-  as.data.frame( rc$scores )
plot(data_scores$RC1, data_scores$RC2, col= "white", xlab = "RC1", ylab= "RC2")
text(data_scores$RC1, data_scores$RC2, rownames(data_scores), cex = 0.6)
abline(h=0, lty=2)
abline(v=0, lty=2)

#Il graficoindividua la posizione di ogni azienda nello spazio delle componenti,
#attraversi la rotazione
#Questo grafico mostra come le nostre variabili originali (i vettori che partono dall'origine) 
#si allineano con i nuovi assi (i componenti principali ruotati). 
#La rotazione ha lo scopo di "semplificare" la struttura dei caricamenti, 
#rendendo più chiaro quali variabili contribuiscono fortemente a ciascun componente e quali no. 
#In un buon risultato di rotazione, ci aspetteremmo che i vettori delle variabili
#si allineino il più possibile con uno degli assi, indicando che quella variabile 
#ha un carico alto su quel componente e basso sugli altri. Questo facilita l'interpretazione 
#del significato di ciascun componente.

# Creiamo un databse con tutte le componenti principali
df <- pca_dati$ind$coord 
df <- as.data.frame(df)

#Ricarichiamo il dataset con i codici fiscali non numerati per gli anni,
#in modo che codici che si riferiscano alla stessa azienda siano uguali
dataset_codici_non_numerati <- read_excel("dataset_codici_non_numerati.xlsx")
dataset_codici_non_numerati <- dataset_codici_non_numerati %>% 
  mutate(current_ratio = attivo_circolante / debiti_breve ,
         quick_ratio = ( attivo_circolante - tot_rimanenze  ) / debiti_breve ,
         immobiliz_su_tot_attivo = tot_immobilizzazioni / ( tot_immobilizzazioni +
                                                              attivo_circolante  ))


# Così da inserire nel dataset anche le prime due componenti principali
dataset_codici_non_numerati$pc1 <- df$Dim.1
dataset_codici_non_numerati$pc2 <- df$Dim.2


# Abbiamo calcolato le variazioni da un anno all'altro delle componenti principali.
#Abbiamo selezionato solo le prime 2 componenti principali perchè sono quelle 
#più esplicative. Le utilizzeremo per la creazione di grafici 
# Abbiamo reso il nostro databse orizzontale, nel senso che abbiamo creato una colonna 
#  per il 2022 e per il 2023 per ogni indice e componente principale
finale_wide <- dataset_codici_non_numerati %>%
  pivot_wider(
    id_cols = c(codice_fiscale),
    names_from = anno,
    values_from = c(ricavi_vendite, ROA, ROS, EBITDAMargin, CCN, PFN, utile_netto,
                    current_ratio, quick_ratio, immobiliz_su_tot_attivo, pc1, pc2)
  )

# Abbiamo calcolato le variazioni da un anno all'altro delle componenti principali.
#Abbiamo selezionato solo le prime 2 componenti principali perchè sono quelle 
#più esplicative. Le utilizzeremo per la creazione di grafici 
finale_wide <- finale_wide %>%
  mutate(Delta_pc1 = pc1_2023 - pc1_2022,
         Delta_pc2 = pc2_2023 - pc2_2022,
         )

# Abbiamo creato un database con il codice fiscale e le sole componenti principali
finale_pca <- finale_wide %>%
  select(codice_fiscale, pc1_2022, pc2_2022, pc1_2023, pc2_2023) %>%
  pivot_longer(
    cols = starts_with("pc"),
    names_to = c("Dim", "Anno"),
    names_sep = "_",
    values_to = "Valore"
  ) %>%
  pivot_wider(names_from = Dim, values_from = Valore)

#install.packages("ggplot2")
library(ggplot2)
library(grid)

# Abbiamo disegnato il grafico in cui mostra con una freccia l'andamento della 
#  componenete principale in quel specifico codice fiscale
print(
  ggplot(finale_pca, aes(x = pc1, y = pc2, group = codice_fiscale)) +
    geom_point(aes(color = Anno)) +
    geom_line(arrow = arrow(length = unit(0.2, "cm")), alpha = 0.6) +
    theme_minimal() +
    labs(title = "Spostamento delle aziende tra 2022 e 2023 nel piano PCA",
         x = "Componente principale 1",
         y = "Componente principale 2")
)

#Questo grafico visualizza la traiettoria di ogni singola azienda nel piano 
#dei primi due componenti principali tra il 2022 e il 2023.
#Ogni freccia rappresenta un'azienda specifica, con il punto iniziale 
#che indica la sua posizione nel 2022 (colore blu) e la punta della freccia 
#che indica la sua posizione nel 2023 (colore rosso).

# Selezioniamo solo le variabili effettivamente migliorate nel tempo
aziende_migliorate <- finale_wide %>%  
  filter(Delta_pc1 > 0, Delta_pc2 > 0)

# Media per ogni indici del Gruppo migliorato per il 2022
aziende_migliorate %>%
  summarise(across(ends_with("_2022"), mean, na.rm = TRUE))

# Media per ogni indici dell'Intero campione per il 2022
finale_wide %>%
  summarise(across(ends_with("_2022"), mean, na.rm = TRUE))

# Media per ogni indici del Gruppo migliorato per il 2023
aziende_migliorate %>%
  summarise(across(ends_with("_2023"), mean, na.rm = TRUE))

# Media per ogni indici dell'Intero campione per il 2023
finale_wide %>%
  summarise(across(ends_with("_2023"), mean, na.rm = TRUE))


#  Calcola la distanza nello spazio PCA
aziende_migliorate <- aziende_migliorate %>%
  mutate(Distanza = sqrt(Delta_pc1^2 + Delta_pc2^2)
  )
#aggiungo codici fiscali
aziende_migliorate_1 <- aziende_migliorate %>%
  left_join(finale_wide %>% select(codice_fiscale), by = "codice_fiscale")


#  Prepara il dataset in formato long
aziende_migliorate_long <- aziende_migliorate_1 %>%
  select(codice_fiscale, pc1_2022, pc2_2022, pc1_2023, pc2_2023) %>%
  pivot_longer(cols = starts_with("pc"),
               names_to = c("PC", "Anno"),
               names_sep = "_",
               values_to = "Valore") %>%
  pivot_wider(names_from = PC, values_from = Valore)

#  Ordina per codice fiscale per disegnare correttamente le linee
aziende_migliorate_long <- aziende_migliorate_long %>%
  arrange(codice_fiscale)

#  Grafico
ggplot(aziende_migliorate_long , aes(x = pc1, y = pc2, group = codice_fiscale)) +
  geom_point(aes(color = Anno)) +
  geom_line(arrow = arrow(length = unit(0.2, "cm")), alpha = 0.5, color = "grey40") +
  labs(
    title = "Evoluzione delle aziende migliorate tra 2022 e 2023 nel piano PCA",
    x = "Componente principale 1",
    y = "Componente principale 2"
  )

#Questo è il grafico delle sole aziende migliorate, ciò si denota dal fatto che le 
#frecce sono tutte rivolte verso destra (aumento di pc1) e verso l'alto (aumento di pc2)


#Ora proviamo a capire se cominciare a produrre subito ha una variazione degli indici


#Dividiamo per anni tutti gli indici anche nel dataset dove i codici non sono numerati per anno
finale_produzione_subito <- dataset_codici_non_numerati %>%
  pivot_wider(
    id_cols = c(codice_fiscale),
    names_from = anno,
    values_from = c(ricavi_vendite, ROA, ROS, EBITDAMargin, CCN, PFN, utile_netto,
                    current_ratio, quick_ratio, immobiliz_su_tot_attivo, pc1, pc2, prod_subito)
  )


#Creiamo due dataset, uno per le aziende just in time e l'altro per l'aziende 
#che hanno cominciato la produzione in ritardo
produzione_subito <- finale_produzione_subito %>% 
  filter(prod_subito_2022 == 1, prod_subito_2023 == 1)

produzione_dopo <- finale_produzione_subito %>% 
  filter(prod_subito_2022 == 0, prod_subito_2023 == 0)

# Media per ogni indici del gruppo di aziende just in time  per il 2022
produzione_subito %>%
  summarise(across(ends_with("_2022"), mean, na.rm = TRUE))

# Media per ogni indici del gruppo di aziende non just in time per il 2022
produzione_dopo %>%
  summarise(across(ends_with("_2022"), mean, na.rm = TRUE))

# Media per ogni indici del gruppo di aziende just in time per il 2023
produzione_subito %>%
  summarise(across(ends_with("_2023"), mean, na.rm = TRUE))

# Media per ogni indici ddel gruppo di aziende non just in time per il 2023
produzione_dopo %>%
  summarise(across(ends_with("_2023"), mean, na.rm = TRUE))

#Ora facciamo i boxplot per visualizzare negli anni se cambiano le distribuzioni delle
#aziende just in time e in ritardo
Distanza = sqrt(finale_wide$Delta_pc1^2 + finale_wide$Delta_pc2^2)


ggplot(finale_produzione_subito, aes(x = as.factor(prod_subito_2022), y = Distanza, fill = as.factor(prod_subito_2022))) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(
    title = "Distanza nel piano PCA tra 2022 e 2023 per inizio produzione",
    x = "inizio produzione",
    y = "Distanza (spostamento PCA)",
    fill = "inizio produzione"
  ) +
  theme(legend.position = "none")

#Dai grafici denotiamo che e il 50% e la mediana di sistribuzione sono più basse
#per le aziende just in time, ciò significa che la loro distanza (spostamento 
#negli anni nello spazio delle variabili) sarà minore.Potrebbero aver mantenuto 
#una posizione più coerente nello spazio multi-dimensionale dei tuoi indici.
#Al contrario le aziende 0 hanno mediana più alta, questo potrebbe indicare una maggiore evoluzione, 
#crescita, declino o una variazione più pronunciata nelle loro caratteristiche misurate dagli indici.

#Le aziende 1 evidenziano quindi maggiore stabilità nel tempo nello spazio delle prime
#2 componenti principali rispetto alle aziende 0