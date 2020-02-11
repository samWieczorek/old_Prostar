Les noms des fichiers de ce repertoire doivent avoir comme prefixe 'watchPeptide', 'watchProtein', etc... suivis du
nom du script R qui contient la definition du module


Les fonctions dans ces fichiers servent d'une part à exécuter un module et ensuite d'en attendre un retour. Lorsque ce retour est fait, certaines actions de nettoyage sont réalisées

Comme les modules sont chargés dynamiquement au lancement de prostar, il est oblogatoire de respecter les nomenclatures des fichiers