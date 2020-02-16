getwd()
noms_fichiers = dir()#prend les files dans le wd
corpus_fichiers = lapply(noms_fichiers, readLines)#transformation de tous les textes en liste
corpus_fichiers_ve= unlist(corpus_fichiers)
corpus_fichiers_liste = strsplit(corpus_fichiers_ve, " ")
corpus_fichiers_vmots = unlist(corpus_fichiers_liste)
corpus_fichiers_freq = summary(factor(corpus_fichiers_vmots))#fréquence des mots
corpus_fichiers_freq[1:5]

