
library(ggplot2)
getwd()
fichiers = dir()
corpus_fichiers = lapply(fichiers, readLines)


#transformation de tous les textes en liste

#fonction pour calculer la fréquence des mots de chaque texte et mise dans un dataframe
calcul_frequence_mot = function(mon_texte, number){
  corpus_fichiers_ve= unlist(mon_texte)
  #prend les files dans le wd  
  corpus_fichiers_liste = strsplit(corpus_fichiers_ve, " ")
  corpus_fichiers_vmots = unlist(corpus_fichiers_liste)
  #on enlève un maximum de bruit
  corpus_fichiers_vmots = corpus_fichiers_vmots[!((corpus_fichiers_vmots == " "))]
  corpus_fichiers_vmots = corpus_fichiers_vmots[!((corpus_fichiers_vmots == ","))]
  corpus_fichiers_vmots = corpus_fichiers_vmots[!((corpus_fichiers_vmots == "."))]
  corpus_fichiers_vmots = corpus_fichiers_vmots[!((corpus_fichiers_vmots == ";"))]
  corpus_fichiers_vmots = corpus_fichiers_vmots[!((corpus_fichiers_vmots == ";"))]
  corpus_fichiers_vmots = corpus_fichiers_vmots[!((corpus_fichiers_vmots == "!"))]
  corpus_fichiers_vmots = corpus_fichiers_vmots[!((corpus_fichiers_vmots == "?"))]
  corpus_fichiers_vmots = corpus_fichiers_vmots[!((corpus_fichiers_vmots == "..."))]
  corpus_fichiers_freq = summary(factor(corpus_fichiers_vmots))#fréquence des 
  return(corpus_fichiers_freq[number])
  
}




############################################################################################################
############################################################################################################


#analyse de la fréquence de 40 mots parmi les 200 mots les plus fréquents de chaque pièce 



#creation de la matrice contenant les 40 mots les plus fréquents de chaque texte
matrice_freq = matrix(0, nrow=37, ncol= 40, byrow = TRUE)
for (j in 1:40) {print (j)
  for (i in 1:37) {matrice_freq[i, j] = calcul_frequence_mot(corpus_fichiers[i], j)
  }
}

rownames(matrice_freq) = fichiers



#graphiques
#ACP

freq_mat_acp = prcomp(matrice_freq[,1:40], scale = FALSE)
biplot(freq_mat_acp)
dfrd = data.frame(noms = fichiers, pc1 = freq_mat_acp$x[,1],
                  pc2 = freq_mat_acp$x[,2])

#affichage en nuage de points
ggplot(data = dfrd, mapping = aes(x = pc1, y = pc2)) +
  geom_point(aes(color= noms), size=5) +
  scale_color_hue(h = c(0, 150))



m = as.matrix(dfrd[,1:3])
rownames(m) = fichiers
d=dist(m)
#classification automatique: affichage en cluster (groupés par auteurs)
kmeans(m,4)
dfrd$clust=factor(kmeans(m,4)$clust)#????



#création du dendogramme permettant de visualiser la proximité des textes selon la fréquence de leur 40 et de leur 20 premiers mots.
n = dist(matrice_freq)
hc = hclust(d)
plot(hc, xlab = " ", sub = " ", hang = -10)


###############################################################################################################
################################################################################################################

#analyse de la fréquence des 20 mots les plus fréquents de chaque pièce 

#creation de la matrice contenant les 20 mots les plus fréquents de chaque texte
matrice_freq = matrix(0, nrow=37, ncol= 20, byrow = TRUE)
for (j in 1:20) {print (j)
  for (i in 1:37) {matrice_freq[i, j] = calcul_frequence_mot(corpus_fichiers[i], j)
  }
}

rownames(matrice_freq) = fichiers



#graphiques
#ACP

freq_mat_acp = prcomp(matrice_freq[,1:20], scale = TRUE)
biplot(freq_mat_acp)
dfrd = data.frame(noms = fichiers, pc1 = freq_mat_acp$x[,1],
                  pc2 = freq_mat_acp$x[,2])


#affichage en nuage de points
ggplot(data = dfrd, mapping = aes(x = pc1, y = pc2)) +
  geom_point(aes(color= noms), size=5) +
  scale_color_hue(h = c(0, 150))


#classification automatique: affichage en cluster (groupés par auteurs)
m = as.matrix(dfrd[,2:3])
rownames(m) = fichiers
d=dist(m)
kmeans(m,4)
dfrd$clust=factor(kmeans(m,4)$clust)#????



#création du dendogramme permettant de visualiser la proximité des textes selon la fréquence de leur 40 et de leur 20 premiers mots.
hc = hclust(d)
plot(hc, xlab = " ", sub = " ", hang = -10)

#Tendances d'auteurs se dessinent: on remarque un regroupement  sur le graphique coloré
#en fonction des auteurs bien que très distinct. En effet nous sommes partis de l'hypothèse 
#que la signature d'un auteur se caractérise en partie par la même fréquence d'usage de mots. 
#Cependant: le dendogramme ne signifie absolument rien. Comme l'ont fait JB Camps et F Cafiero,
# il faudrait coupler cette analyse à une analyse stylométrique et morphosyntaxique pour 
#identifier  chaque auteur. La fréquence de mots ne suffit pas. 








