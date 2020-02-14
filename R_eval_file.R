getwd()
corpus_fichiers = lapply(dir(), readLines)
#transformation de tous les textes en liste

#fonction pour calculer la fréquence des mots de chaque texte et mise dans un dataframe
calcul_frequence_mot = function(mon_texte, indice){
  #prend les files dans le wd  
  corpus_fichiers_ve= unlist(corpus_fichiers[indice])
  corpus_fichiers_liste = strsplit(corpus_fichiers_ve, " ")
  corpus_fichiers_vmots = unlist(corpus_fichiers_liste)
  corpus_fichiers_freq = summary(factor(corpus_fichiers_vmots))#fréquence des mots
  return(corpus_fichiers_freq[1:20])
}




#alternative à la boucle for:
for (i in 1:37){print(calcul_frequence_mot(corpus_fichiers, i))}

#création des matrices de fréquence 20 pour chaque texte
#vecteurs
vn = resultat
#fréq des 20 mots les plus fréquents
matrice= matrix(v1, nrow = 37, ncol = 20)
matrice

#création du dendogramme permettant de visualiser la proximité des textes selon la fréquence de leur 40 et de leur 20 premiers mots. 
