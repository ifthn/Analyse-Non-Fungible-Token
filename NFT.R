#packages

library(tidyverse)
library(skimr)
library(ggthemes)



# Ventes marche primaire = transaction entre createur et acheteur/collectionneurs de NFT
# Ventes marche secondaire = transaction entre acheteur/collectionneurs de NFT

#Importation de donnees
NFT <- read.csv("NFT_Sales.csv")

#3 chiffres pour les resultats
options(digits = 3)



str(NFT)

#Conversion en date
NFT$Date <- as.Date(NFT$Date)

#Conversion des nombres  entier en nombre numerique
x <- c(3,5)


for (i in x) {
  NFT[,i] <- as.numeric(NFT[,i])
}

str(NFT)




skim(NFT)





colSums(is.na(NFT)) %>% tibble(Variable = colnames(NFT),NAs = .)

#Suppression NA
NFT_clean <- na.omit(NFT)



skim(NFT_clean)

NFT_clean %>% ggplot(aes(x = Date, y = Active_Market_Wallets)) +
  geom_line()

#Le graph montre des fluctuations entre les portefeuilles actifs et fermes
#Pic en fin 2017 correspond avec ETH
#Pic important en 2021 puis une baisse

#Zoom sur 2021
NFT_clean %>% filter(Date > "2021-01-01") %>%
  ggplot(aes(x = Date, y = Active_Market_Wallets)) +
  geom_line()

head(NFT_clean)
tail(NFT_clean)

options(repr.plot.width = 15, repr.plot.height = 10)

plot_1 <- ggplot(data = NFT_clean, aes(x = Date, y = AverageUSD_cum)) +
  theme_bw() +
  geom_line(size = 1) +
  labs(title = "Valeur moyenne NFT dans le temps") +
  xlab("Date") +
  ylab("Moyenne USD par NFT") +
  theme(plot.title = element_text(hjust = 0.5))

plot_2 <- ggplot(data = NFT_clean, aes(x = Date, y = Active_Market_Wallets_cumsum)) +
  theme_bw() +
  geom_line(size = 1) +
  labs(title = "Nombre de portefeuilles actifs dans le temps") +
  xlab("Date") +
  ylab("Nombre de portefeuilles actifs") +
  theme(plot.title = element_text(hjust = .5))

plot_1
plot_2



NFT_clean %>% 
  ggplot() +
  geom_point(aes(Date, Primary_Sales_cumsum)) +
  geom_point(aes(Date, Secondary_Sales_cumsum), col = "Blue", alpha = .3) +
  scale_y_continuous(trans='log2') +
  labs(title = "Ventes dans le temps", x = "Date", y = "Ventes - Marche Primaire = Noir. Marche secondaire = Bleu (log2)") +
  theme(plot.title = element_text(hjust =.5))


# Ampleur relative entre les ventes sur le marche primaire et secondaire en 2021
NFT_Sales_Summary <- NFT %>% filter(Date > "2021-01-01") %>% 
  summarize(primary_millions = sum(Primary_Sales_cumsum)*10^-6,
            secondary_millions = sum(Secondary_Sales_cumsum)*10^-6,
            relative_magnitude = primary_millions/secondary_millions,
            total_millions = primary_millions + secondary_millions,
            proportion_primary = primary_millions/total_millions,
            proportion_secondary = secondary_millions/total_millions)         

NFT_Sales_Summary %>% tibble() %>% transpose()



# Volume des echanges a partir de 2021
NFT_clean %>% filter(Date > "2021-01-01") %>%
  ggplot(aes(x = Date, y = Sales_USD)) +
  geom_line()


########################################################################################



library(tidyverse)
library(skimr)




df <- read_csv("NFT_Sales.csv")


skim(df)






#On supprime les valeurs cumul
df_v2 <- df %>%  
  select(-c(Sales_USD_cumsum
            ,Number_of_Sales_cumsum
            ,Active_Market_Wallets_cumsum
            ,Primary_Sales_cumsum
            ,Secondary_Sales_cumsum
            ,AverageUSD_cum))


skim(df_v2)



#Suppression NA
df_v2 <- drop_na(df_v2)


skim(df_v2)



#On enleve les valeurs negatives des portefeuilles actifs
df_v2 <- df_v2[!(df_v2$Active_Market_Wallets<0),]


skim(df_v2)



head(df_v2)



#Dates
df_v2$Date <- as.Date(df_v2$Date, "%m/%d/%y")


skim(df_v2)






colnames(df_v2)
#Changement noms de colonnes en minuscule
names(df_v2) <- tolower(names(df_v2))


colnames(df_v2)


#### Observations et graphiques



# Nombre de ventes 
ggplot(df_v2, aes(x=date,y=number_of_sales)) +
  geom_line() +
  labs(title = "Nombre de ventes",
       subtitle = "Nombre moyen de transactions ")




#Engouement pour les NFT en 2021, mais tendance semble a la baisse depuis le troisieme trimestre


# Nombre de ventes par rapport au nombre de portefeuilles actifs
ggplot(df_v2, aes(x=active_market_wallets,y=number_of_sales)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(title = "Nombre de ventes par rapport aux portefeuilles actifs",
       subtitle = "On veut voir combien de portefeuilles achetent activement dans la pool")


#### Observation

# Il semble que certains portefeuilles effectuent plusieurs achats/echanges, et que tout
# les portefeuilles actifs n'effectuent pas d'achats. Il y a surement beaucoup de detenteurs
# de NFT qui les gardent




#Nombre de ventes sur le marché primaire
ggplot(df_v2, aes(x=date,y=primary_sales)) +
  geom_line() +
  labs(title = "Nombre de ventes sur le marché primaire",
       subtitle = "On veut voir combien de portefeuille achètent activement dans la pool")


#### Observation

#Il semble que la plupart des ventes primaires suivent l'evolution des ventes totales
# mais la difference est qu'elles representent environ la moitie des ventes totales.
# Peut etre que la moitie des ventes sont du marché secondaire, ou les gens ont plusieurs portefeuilles actifs


# Nombre de ventes primaires par rapport au nombre de portefeuille actifs
ggplot(df_v2, aes(x=active_market_wallets,y=primary_sales)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(title = "Nombre de ventes primaires par rapport au nombre de portefeuille actifs",
       subtitle = "On veut voir combien de portefeuilles font des achats sur le marché primaire")


#### Observations

# Il semble que tout les portefeuilles n'achetent pas directement aupres des createurs
# et que l'autre moitie semble etre plus interesser par du trading et de la speculation


 
# Nombre de ventes en USD
ggplot(df_v2, aes(x=date,y=sales_usd)) +
  geom_line() +
  labs(title = "Nombre de ventes en USD",
       subtitle = "On veut voir la quantité d'argent qui circule dans l'ecosysteme des NFT")

#### Observations

#Meme si il y a eu des pics dans le nombre de ventes au cours de l'annee 2018, c'est a peine perceptible
# dans ce graphique, ce qui signifie que ces NFT qui ont ete vendus en 2018 etaient assez bon marché
# alors que les prix actuels des NFT ont exploses.



#Nombre de ventes en USD par rapport au nombre de portefeuille
ggplot(df_v2, aes(x=active_market_wallets,y=sales_usd)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(title = "Nombre de ventes en USD par rapport aux portefeuilles actifs",
       subtitle = "On veut voir la quantité de dollars par rapport aux portefeuilles actifs du marché")


#### Observations

# Encore une fois, le graphique montre que peu de portefeuilles ont un pouvoir d'achat,
# ce qui signifie que la quantite de richesse est concentree sur quelques portefeuilles
# Probablement des gros bags et des pionniers de la scene NFT


# Nombre de ventes en USD par rapport au nombre de ventes sur le marché primaire
ggplot(df_v2, aes(x=primary_sales,y=sales_usd)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(title = "Nombre de ventes en USD par rapport au nombre de ventes sur le marché primaire",
       subtitle = "On veut voir la quantite de dollars par rapport aux ventes sur le marche primaire")


#### Observations

# Les ventes sur le marché primaires et les mint ne sont normalement pas aussi cher, du moins pour certaines
# collections les plus recentes. Mais on a des donnees qui prouvent que les prix actuels sont en effes gonfles
# donc que meme les nouvelles collections de NFT se vendent a des prix autant eleves


# Nombre de ventes par rapport au nombre de ventes en USD
ggplot(df_v2, aes(x=number_of_sales,y=sales_usd)) +
  geom_point() +
  geom_smooth(method = loess) +
  labs(title = "Nombre de ventes par rapport au nombre de ventes en USD",
       subtitle = "On veut voir le montant de USD par rapport au nombre de ventes")


#### Observations

# Il semble qu'il n'y ai pas eu beaucoup de ventes à des prix eleves, ce qui montre une fois de plus que les
# prix sont gonflés et qu'ils commencent à baisser. Il est fort probable que les ventes de NFT à des prix eleves
# (environ 50 000) soient à l'origine de l'engouement pour les NFT, car l'argent et la speculation attire. 


#### Observation finale


# Il semble que seule une certaine tranche d'age ou demographique qui est interesse par les NFT, on assiste à une adaptation
# exponentielle, mais la plupart utilisent plusieurs portefeuilles, puisque le nombre de transactions n'est pas equivalent au
# nombre de portefeuilles actifs sur le marche. Peut etre que cela signifie que les portefeuilles actifs ont un stock de NFT 
# qu'ils gardent. Les donnees sont limites pour le moment et peuvent reveler plus d'infos a l'avenir.



# Cela renforce l'idee que bien que les NFT se repandent enormement et qu'il y a encore beaucoup de gens qui ne sont pas
# inities a ces concepts, ce qui veut dire qu'il y a encore beaucoup de places pour la croissance en terme d'adoption des NFT
# et donc en nombre d'utilisateurs.

