
#on installe les packages nécessaire s'ils ne sont pas déjà installés
list.of.packages <- c("rtweet", "dplyr", "sjmisc","ggplot2","ggthemes","plyr",
                      "tm","SnowballC","wordcloud","RColorBrewer","writexl")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Chargement des packages 

library(rtweet)
library(dplyr)
library(sjmisc)
library(ggplot2)
library(ggthemes)
library(plyr)
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library(writexl)


# liste des utilisateurs dont on veut récupérer les tweets
users <- c("EmmanuelMacron", "JLMelenchon", "n_arthaud",
           "ZemmourEric", "MLP_officiel", "PhilippePoutou", "jeanlassalle",
           "vpecresse", "dupontaignan", "Anne_Hidalgo",
           "yjadot", "Fabien_Roussel")


# listes des termes pour sélectionner les tweets pertinents
terms_search <- "(Ukraine OR Russie OR Ukrainewar OR Zelensky OR Poutine OR Donbass) (#kiev OR  #ukrainerussie  OR #laguerre OR  #urkainewar OR #invasion OR #worldwar3 OR #ukraine OR #russie)"


# on récupère les tweets à partir de la liste des termes
team_rstats <- search_tweets(q = terms_search, n = 18000, lang="fr")
team_rstats


# on récupère les tweets des candidats à la présidentielle
# on filtre sur la période qui nous intéresse 
presidential_tweets <-get_timelines(users,
                       n=3200,
                       language="fr")

presidential_tweets <- presidential_tweets %>%
  dplyr::filter(created_at > "2022-02-24" & created_at <="2022-04-25")

# on vérifie qu'on a bien l'ensemble des candidats
unique(presidential_tweets$screen_name)

#on ajoute une colonne "concerns_conflict" indiquant si le tweet des candidats est relatif au conflit russo-ukrainien
presidential_tweets$concerns_conflict <- ifelse(grepl('Ukraine|Russie|Ukrainewar|Zelensky|Poutine|Donbass|Kiev|Ukrainien', presidential_tweets$text) |   grepl('Kiev | UkraineRussiaWar | UkraineRussie  | UkraineInvasion | LaGuerre | UrkaineWar | Invasion | Worldwar3 | Ukraine | Russie | Poutine ', presidential_tweets$hashtags),"Oui","Non") 


# on remplace les noms d'utilisateurs des candidats par leur nom de famille

presidential_tweets$screen_name <- factor(presidential_tweets$screen_name,
                                       levels=c("n_arthaud","Fabien_Roussel","PhilippePoutou",
                                                "JLMelenchon","Anne_Hidalgo","yjadot","EmmanuelMacron",
                                                "jeanlassalle","vpecresse","dupontaignan","MLP_officiel","ZemmourEric"))

presidential_tweets$screen_name <- revalue(presidential_tweets$screen_name, c("Anne_Hidalgo"="Hidalgo", 
                                     "dupontaignan"="Dupont Aignan",
                                     "EmmanuelMacron"="Macron",
                                     "Fabien_Roussel"="Roussel",
                                     "jeanlassalle"="Lassalle",
                                     "n_arthaud" = "Arthaud",
                                     "PhilippePoutou" = "Poutou",
                                     "vpecresse" = "Pécresse",
                                     "yjadot" = "Jadot",
                                     "JLMelenchon" = "Mélenchon",
                                     "MLP_officiel" = "LePen",
                                     "ZemmourEric" = "Zemmour"
                                     ))



# on stocke dans ukrainerussie les tweets des candidats relevant de la crise russo-ukrainienne
ukrainerussie <- presidential_tweets %>% filter(grepl('Ukraine|Russie|Ukrainewar|Zelensky|Poutine|Donbass|Kiev|Ukrainien', text) | 
                                                  grepl('Kiev | UkraineRussiaWar | UkraineRussie  | UkraineInvasion | LaGuerre | UrkaineWar | Invasion | Worldwar3 | Ukraine | Russie | Poutine ', hashtags)   )



#################################### Dataset avec l'ensemble des tweets (évoquant le conflit ou non)

# Visualisation de la répartition des tweets par candidat
ggplot(presidential_tweets,
       aes(x=screen_name,fill=concerns_conflict))+geom_bar(position="fill")+theme_economist()+ labs(title = "Répartition des tweets par candidat", x = "", y = "Nombre de tweets",fill="Concerne le conflit") + 
  theme(axis.text.x=element_text(angle=30)) 

# Répartition du nombre de favoris selon les candidats
ggplot(presidential_tweets,
       aes(x=screen_name,fill=screen_name,y=favorite_count))+geom_boxplot()+theme_economist()+ labs(title = "Tweets en favoris par candidat", x = "", y = "Nombre de favoris",fill="") + 
  theme(axis.text.x=element_text(angle=30)) +
  ylim(0, 100000)

# Répartition du nombre de retweets selon les candidats
ggplot(presidential_tweets,
       aes(x=screen_name,fill=screen_name,y=retweet_count))+geom_boxplot()+theme_economist()+ labs(title = "Retweets par candidat", x = "", y = "Nombre de retweets",fill="") + 
  theme(axis.text.x=element_text(angle=30))+
  ylim(0, 20000)



#################################### Dataset des tweets des candidats qui concernent le conflit


#Répartition du nombre de tweets par candidat
ggplot(ukrainerussie,
       aes(x=screen_name,fill=screen_name))+geom_bar()+theme_economist()+ labs(title = "Répartition des tweets par candidat", x = "", y = "Nombre de tweets",fill="") + 
  theme(axis.text.x=element_text(angle=30)) 


# Répartition du nombre de favoris selon les candidats
ggplot(ukrainerussie,
       aes(x=screen_name,fill=screen_name,y=favorite_count))+geom_boxplot()+theme_economist()+ labs(title = "Tweets en favoris par candidat", x = "", y = "Nombre de favoris",fill="") + 
  theme(axis.text.x=element_text(angle=30)) 

# Répartition du nombre de retweets selon les candidats
ggplot(ukrainerussie,
       aes(x=screen_name,fill=screen_name,y=retweet_count))+geom_boxplot()+theme_economist()+ labs(title = "Retweets par candidat", x = "", y = "Nombre de retweets",fill="") + 
  theme(axis.text.x=element_text(angle=30))


# on crée un dataset pour chacun des candidats
arthaud <- ukrainerussie %>%
  filter(screen_name=="Arthaud")

roussel <- ukrainerussie %>%
  filter(screen_name=="Roussel")

poutou <- ukrainerussie %>%
  filter(screen_name=="Poutou")

melenchon <- ukrainerussie %>%
  filter(screen_name=="Mélenchon")

hidalgo <- ukrainerussie %>%
  filter(screen_name=="Hidalgo")

jadot <- ukrainerussie %>%
  filter(screen_name=="Jadot")

macron <- ukrainerussie %>%
  filter(screen_name=="Macron")

lassalle <- ukrainerussie %>%
  filter(screen_name=="Lassalle")

pecresse <- ukrainerussie %>%
  filter(screen_name=="Pécresse")

dptaignan <- ukrainerussie %>%
  filter(screen_name=="Dupont Aignan")

lepen <- ukrainerussie %>%
  filter(screen_name=="LePen")

zemmour <- ukrainerussie %>%
  filter(screen_name=="Zemmour")

########################## Enregistrement des données pour les analyses complémentaires 

# on enregistre dans un fichier excel le dataset des tweets des candidats évoquant le conflit
write_xlsx(ukrainerussie,"ukrainerussie.xlsx")

# on crée également un fichier excel des tweets évoquant le conflit par candidat
write_xlsx(zemmour,"zemmour.xlsx")
write_xlsx(lepen,"lepen.xlsx")
write_xlsx(dptaignan,"dptaignan.xlsx")
write_xlsx(pecresse,"pecresse.xlsx")
write_xlsx(lassalle,"lassalle.xlsx")
write_xlsx(macron,"macron.xlsx")
write_xlsx(jadot,"jadot.xlsx")
write_xlsx(hidalgo,"hidalgo.xlsx")
write_xlsx(melenchon,"melenchon.xlsx")
write_xlsx(poutou,"poutou.xlsx")
write_xlsx(roussel,"roussel.xlsx")
write_xlsx(arthaud,"arthaud.xlsx")

# on crée un fichier excel pour les tweets concernant le conflit - hors candidats à la présidentielle
write_xlsx(team_rstats,"team_rstats2.xlsx")