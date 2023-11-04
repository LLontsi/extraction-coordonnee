extraction= function(cr2)
{
    # Lecture du fichier avec read.table
cr1 <- read.table("nom_du_fichier.txt", header = TRUE)

# Filtrer les lignes selon les conditions spécifiées
cr$3 <- cr$A3 >= 8.35 & cr$A3 <= 16.18
condition_colonne4 <- cr$A4 >= 1.5833 & cr$A4 <= 13.1833
donnees_filtrees <- cr[condition_colonne3 & condition_colonne4, ]

# Créer une nouvelle colonne avec la valeur 1 et incrémenter les autres valeurs
donnees_filtrees$V1 <- 1
donnees_filtrees$V1 <- donnees_filtrees$V1 + (1:nrow(donnees_filtrees)) - 1

# Réinitialiser la première colonne à 1 après avoir atteint 10800 dans le fichier d'origine
valeur_max <- 10800
nb_lignes_originales <- nrow(cr1)
indices <- (1:nrow(donnees_filtrees)) - 1
donnees_filtrees$V1 <- (donnees_filtrees$V1 - 1) %% valeur_max + 1
donnees_filtrees$V1 <- donnees_filtrees$V1 + (floor(indices/valeur_max) * valeur_max) %% nb_lignes_originales

# Écrire les données filtrées dans un nouveau fichier CSV
write.csv(donnees_filtrees, "emag1.csv", row.names = TRUE)

}
# Filtrer les lignes selon les conditions spécifiées
condition_colonne3 <- cr$A3  >= 8.35 & cr$A3  <= 16.18
condition_colonne4 <- cr$A3  >= 1.5833 & cr$A3  <= 13.1833
donnees_filtrees <- cr[condition_colonne3 & condition_colonne4, ]

# Créer une nouvelle colonne avec la valeur 1 et incrémenter les autres valeurs
donnees_filtrees$V1 <- 1
donnees_filtrees$V1 <- donnees_filtrees$V1 + (1:nrow(donnees_filtrees)) - 1

# Réinitialiser la première colonne à 1 après avoir atteint 10800 dans le fichier d'origine
valeur_max <- 10800
nb_blocs <- ceiling(nrow(donnees_filtrees) / valeur_max)
donnees_filtrees$V1 <- (donnees_filtrees$V1 - 1) %% valeur_max + 1
donnees_filtrees$V1 <- donnees_filtrees$V1 + (rep(0:(nb_blocs-1), each = valeur_max, length.out = nrow(donnees_filtrees)) * valeur_max)

# Écrire les données filtrées dans un nouveau fichier CSV
write.csv(donnees_filtrees, "nouveau_fichier.csv", row.names = FALSE)


# Filtrer les lignes selon les conditions spécifiées
condition_colonne3 <- cr$A3  >= 8.9833 & cr$A3  <= 14.8166666
condition_colonne4 <- cr$A4 >= 2.95 & cr$A4 <= 6.14666666
donnees_filtrees <- cr[condition_colonne3 & condition_colonne4, ]
# Écrire les données filtrées dans un nouveau fichier CSV
write.csv(donnees_filtrees, "emag2.csv", row.names = FALSE)



# Filtrer les lignes selon les conditions spécifiées
condition_colonne3 <- cr$V3 >= 8.9833  & cr$V3 <= 14.8166666
condition_colonne4 <- cr$V4 >= 2.95 & cr$V4 <= 6.14666666
donnees_filtrees <- cr[condition_colonne3 & condition_colonne4, ]

# Créer une nouvelle colonne avec la valeur 1 et incrémenter les autres valeurs
donnees_filtrees$V1 <- 1
donnees_filtrees$V1 <- donnees_filtrees$V1 + (1:nrow(donnees_filtrees)) - 1

# Écrire les données filtrées dans un nouveau fichier CSV
write.csv(donnees_filtrees, "emag3.csv", row.names = FALSE)




# Réinitialiser la première colonne à 1 après avoir atteint 10800 dans le fichier d'origine
valeur_max <- 10800
nb_blocs <- ceiling(nrow(cr1) / valeur_max)
cr$V1 <- (cr$V1 - 1) %% valeur_max + 1
cr$V1 <- cr$V1 + (rep(0:(nb_blocs-1), each = valeur_max, length.out = nrow(cr1)) * valeur_max)

# Modifier la deuxième colonne en fonction des intervalles de la première colonne
cr$V2 <- 1
cr$V2[!is.na(cr$V1) & cr$V1 <= 10800] <- 1 + (cr$V1[!is.na(cr$V1) & cr$V1 <= 10800] - 1) %% valeur_max
cr$V2[!is.na(cr$V1) & cr$V1 > 10800] <- 1 + (cr$V1[!is.na(cr$V1) & cr$V1 > 10800] - 10801) %% valeur_max

# Écrire les données modifiées dans un nouveau fichier CSV
write.csv(cr1, "emag12.csv", row.names = FALSE)



# Réinitialiser la première colonne à 1 après avoir atteint 10800 dans le fichier d'origine
valeur_max <- 10800
nb_blocs <- ceiling(nrow(cr1) / valeur_max)
cr1$V1 <- (cr1$V1 - 1) %% valeur_max + 1

# Modifier la deuxième colonne en fonction des intervalles de la première colonne
cr1$V2 <- rep(1, nrow(cr1)) # Initialiser la deuxième colonne à 1
intervalle <- valeur_max
indice_debut <- 1

for (i in 1:nb_blocs) {
  indice_fin <- min(indice_debut + intervalle - 1, nrow(cr1))
  cr1$V2[indice_debut:indice_fin] <- 1 + ((indice_debut:indice_fin) - indice_debut) %% intervalle
  indice_debut <- indice_fin + 1
}

# Écrire les données modifiées dans un nouveau fichier CSV
write.csv(cr1, "emag12.csv", row.names = FALSE)
cr1$V1[2] <- 1
for (i in 2:(nrow(cr1) - 1)) {
  if (cr1$V1[i] > cr1$V1[i+1]) {
    cr1$V1[i+1] <- 1
  } else {
    cr1$V1[i+1] <- cr1$V1[i] + 1
  }
}

# Écrire les données modifiées dans un nouveau fichier CSV
write.csv(cr1, "emag1.csv", row.names = FALSE)

Dans ce code, nous utilisons une boucle for pour parcourir les valeurs de la première colonne (jusqu'à l'avant-dernière ligne). À chaque itération, nous vérifions si la valeur de la première colonne de la ligne i est supérieure à celle de la ligne i+1. Si c'est le cas, nous mettons la valeur de la deuxième colonne de la ligne i+1 à 1. Sinon, nous incrémentons la valeur de la deuxième colonne de la ligne i+1 en utilisant la valeur de la ligne précédente.

Assurez-vous de remplacer "nom_du_fichier.txt" par le nom de votre fichier d'origine.
cr$V3 <- cr$VA3  >= 8.35 & cr$VA3  <= 16.18
cr$V4 <- cr$VA3  >= 1.5833 & cr$VA3  <= 13.1833
donnees_filtrees <- cr[cr$V3 & cr$V4, ]


# Écrire les données filtrées dans un nouveau fichier CSV
write.csv(donnees_filtrees, "emag12.csv", row.names = FALSE)
# Filtrer les lignes selon les conditions spécifiées
condition_colonne3 <- cr$V3  >= 8.35 & cr$V3  <= 16.18
condition_colonne4 <- cr$V4  >= 1.5833 & cr$V4  <= 13.1833
donnees_filtrees <- cr[condition_colonne3 & condition_colonne4, ]
write.csv(donnees_filtrees, "emag12.csv", row.names = FALSE)


cr1$v1[2] <- 1

# Parcourir les lignes à partir de la deuxième jusqu'à l'avant-dernière
for (i in 3:(nrow(cr1) - 1)) {
  # Si la valeur de la ligne i est supérieure à celle de i+1
  if (cr1$v1[i] > cr1$v1[i+1]) {
    # Mettre la valeur de i+1 à 1
    cr1$v1[i+1] <- 1
  } else {
    # Incrémenter la valeur de i+1 en utilisant la valeur de i
    cr1$v1[i+1] <- cr1$v1[i] + 1
  }
}

# Écrire les données modifiées dans un nouveau fichier CSV
write.csv(cr1, "emag1.csv", row.names = FALSE)




# Initialiser la première valeur de la première colonne à 1
cr1$V1[1] <- 1

# Parcourir les lignes à partir de la deuxième jusqu'à la dernière
for (i in 2:nrow(cr1)) {
  # Si la valeur de la ligne i est supérieure à celle de i+1
  if (cr1$V1[i] > cr1$V1[i+1]) {
    # Réinitialiser la séquence d'incrémentation à 1
    seq <- 1
  } else {
    # Incrémenter la séquence d'incrémentation
    seq <- seq + 1
  }
  
  # Affecter la valeur de la séquence d'incrémentation à la V1
  cr1$V1[i+1] <- seq
}

# Écrire les données modifiées dans un nouveau fichier CSV
write.csv(cr1, "emag3r.csv", row.names = FALSE)


cr1$V1[1] <- 1
seq=0
# Parcourir les lignes à partir de la deuxième jusqu'à l'avant-dernière
for (i in 2:(nrow(cr1) - 1)) {
  # Vérifier si les valeurs ne sont pas manquantes
  if (!is.na(cr1$V1[i]) && !is.na(cr1$V1[i+1])) {
    # Si la valeur de la ligne i est supérieure à celle de i+1
    if  (cr1$V1[i-1] == 485){
      # Réinitialiser la séquence d'incrémentation à 1
      seq <- 1
    } else {
      # Incrémenter la séquence d'incrémentation
      seq <- seq + 1
    }
    
    # Affecter la valeur de la séquence d'incrémentation à la V1
    cr1$V1[i] <- seq
  }
}

# Écrire les données modifiées dans un nouveau fichier CSV
write.csv(cr1, "emag4.csv", row.names = FALSE)
library(writexl)

# Écrire le data frame dans un fichier Excel
write_xlsx(cr1, "emag.xlsx")


fonction sum (tab,debut,fin):
  si debut=fin alors
     retourne tab[debut]
  finsi
  m=debut+fin/2
  gauche=sum(tab,debut,m)
  droit=sum(tab,m+1,fin)
  retourne gauche+fin


  