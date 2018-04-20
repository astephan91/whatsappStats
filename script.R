######################################################################
# Le fichier chat.csv doit être de la forme dd/mm/aaaa;hh:mm;Prénom  #
# Le fichivier mots.csv doit contenir un message par ligne           #
######################################################################

setwd("chemin_du_fichier_csv")
library(ggplot2)

#Ouverture des fichiers csv
df <- read.csv('chat.csv', sep=";", header = F)
mots <- read.csv('mots.csv', sep=";", header = F)

#On renomme les colonnes et on les met au bon format
colnames(df) <- c('Date', 'Heure', 'Expéditeur')
df$Expéditeur <- as.factor(df$Expéditeur)
df$Date <- as.Date(df$Date,"%d/%m/%Y") # Les analyses portent du 16 juin 2017 au 13 avril 2018
df$Heure <- gsub("(..):..","\\1",df$Heure)

#On ajoute ensuite les jours de la semaine et les mois
df$Jour <- weekdays(df$Date)
df$Jour <- as.factor(df$Jour)
df$Jour <- factor(df$Jour, levels = c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi","Dimanche"))

df$Mois <- months(df$Date)
df$Mois <- as.factor(df$Mois)
df$Mois <-factor(df$Mois, levels = c("juin","juillet","août","septembre","octobre","novembre","décembre","janvier","février","mars","avril"))

#On renomme la colonnes des mots
colnames(mots) <- "Message"

#Et on règle le problème des accents
mots$Message <- gsub("\x8e", "é", mots$Message)
mots$Message <- gsub("\x88", "à", mots$Message)
mots$Message <- gsub("\x8f", "è", mots$Message)
mots$Message <- gsub("\x8d", "ç", mots$Message)
mots$Message <- gsub("\x90", "ê", mots$Message)
mots$Message <- gsub("\x9d", "ù", mots$Message)
mots$Message <- gsub("\x9e", "û", mots$Message)
mots$Message <- gsub("\xec", "Ï", mots$Message)
mots$Message <- gsub("\xfc\xbe\x98\xa6\x8c\xbc", "À", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8c\xa3\xa0\xbc", "Ç", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8e\x93\xa4\xbc", "ô", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8d\x93\xa4\xbc", "ï", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8d\x83\xa4\xbc", "î", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8e\x93\xa0\xbc", "â", mots$Message)
mots$Message <- gsub("\xfc\xbe\x99\xa6\x8c\xbc", "oe", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8d\xa6\x94\xbc", "Ê", mots$Message)
mots$Message <- gsub("\xfc\xbe\x98\xa6\x90\xbc", "€", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8c\xb3\xa0\xbc", "É", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8c\x86\x90\xbc", "&", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8c\x93\xa4\xbc", "ë", mots$Message)
mots$Message <- gsub("\xfc\xbe\x99\x96\x88\xbc", "ae", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8d\x96\x90\xbc", "'", mots$Message) #Apostrophe
mots$Message <- gsub("\xfc\xbe\x8d\xb6\x8c\xbc", "", mots$Message) #Guillemet
mots$Message <- gsub("\xfc\xbe\x8e\x86\x8c\xbc", "", mots$Message) #Guillemet
mots$Message <- gsub("\xfc\xbe\x8e\x96\x8c\xbc", "", mots$Message) #???
mots$Message <- gsub("\xfc\xbe\x8c\xa3\xa4\xbc", "i", mots$Message)
mots$Message <- gsub("\xfc\xbe\x99\xa3\xa4\xbc", "", mots$Message)
mots$Message <- gsub("\xfc\xbe\x8d\x86\x90\xbc", "'", mots$Message)
mots$Message <- gsub("\xfc\xbe\x98\x93\xa0\xbc", "a", mots$Message)

#On supprime les messages "fichiers omis", les appels manqués et les messages vides
mots <- subset(mots, mots$Message != "   <Fichier omis>")
mots <- subset(mots, mots$Message != "   ")
mots <- subset(mots, mots$Message != "   Appel vocal manqué")

#On enlève les espaces présents au début de chaque message
mots$Message <- trimws(mots$Message, which = "b")
rownames(mots) <- NULL

#On met tout en minuscule
mots$Message <- tolower(mots$Message)

#On fait une base de données des messages les plus courants
phrases<- sort(table(mots$Message), decreasing=T)
phrases <- as.data.frame(phrases)
colnames(phrases) <- c("Phrases","Fréquence")

#Puis une base de données pour les mots les plus courants
mots$Message <- strsplit(mots$Message," ",fixed=T)

#Fréquence des mots les plus employés
freq <- sort(table(unlist(mots$Message)),decreasing = T)
freq <- as.data.frame(freq)
colnames(freq) <- c("Mot","Fréquence")
freq$Mot <- as.factor(freq$Mot)

#Pour chercher la fréquence d'un mot en particulier
freq[freq$Mot =="gael",]

#Fréquence des mots rigolos
freqRig <- freq[freq$Mot == "mot1" |
                freq$Mot == "mot2",]

#Fréquence de haha
freqHaha <- freq[freq$Mot == "haha" |
                 freq$Mot == "hahaha" |
                 freq$Mot == "hahahaha" |
                 freq$Mot == "hahahahaha" |
                 freq$Mot == "hahahahahaha" |
                 freq$Mot == "hahahahahahaha",]

#Nombre de messages envoyés à la suite
sequence <- rle(as.character(df$Expéditeur))
sequence <- data.frame(unclass(sequence))
colnames(sequence) <- c("Longueur","Expéditeur")

sequenceA <- subset(sequence, sequence$Expéditeur == "Arnaud")
sequenceJ <- subset(sequence, sequence$Expéditeur == "Jeanne")

summary(sequenceA$Longueur)
summary(sequenceJ$Longueur)

#Graphiques

###QUANTITATIFS

#Expéditeurs
dfExped <- data.frame(
  Expéditeur = c("Jeanne", "Arnaud"),
  NbMessages = c(10459,12450))

camembert <- ggplot(dfExped, aes(x="", y=NbMessages, fill=Expéditeur))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y",start=0)+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
#camembert

#Date
date <- ggplot(df, aes(Date, fill = Expéditeur)) +
  geom_bar(width = 1,position = position_dodge()) +
  labs(x="Date",y="Nombre de messages")
#date

#Mois
mois <- ggplot(df, aes(Mois, fill = Expéditeur)) +
  geom_bar(width = 0.5,position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x="Mois",y="Nombre de messages")
#mois

#Jour
jour <- ggplot(df, aes(Jour, fill = Expéditeur)) +
  geom_bar(width = .5,position = position_dodge()) +
  labs(x="Jour", y="Nombre de messages")
#jour

#Heure
heure <- ggplot(df, aes(Heure, fill = Expéditeur)) +
  geom_bar(width=0.5, position = position_dodge())+
  labs(x="Heure", y="Nombre de messages")
#heure


###QUALITATIFS
freqRig$Mot <- as.factor(freqRig$Mot)
freqHaha$Mot <- as.factor(freqHaha$Mot)
phrases$Phrases <- as.factor(phrases$Phrases)

#Phrases les plus courantes
phrasescourantes <- ggplot(head(phrases,25), aes(Phrases, Fréquence))+
  geom_bar(fill='#99b3ff',stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")
#phrasescourantes


#Mots Rigolos
motsrigolos <- ggplot(freqRig, aes(Mot, Fréquence, fill="red"))+
  geom_bar(fill='#ffad99',stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")
#motsrigolos

#Haha
haha <- ggplot(freqHaha, aes(Mot, Fréquence, fill="red"))+
  geom_bar(width=.5,fill='#99b3ff',stat="identity")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  theme(legend.position="none")+
  labs(x="Type de haha")
#haha

