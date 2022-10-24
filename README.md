# TP1IFT2035

Questions à poser pour la disponibilité de demain:

# Si on se retrouve avec Scons de Llambda et Llamba , ça va être quoi le type de retour
    exemple: Scons (Scons (Ssym "fn") (Scons (Scons (Ssym "x") Snil) (Scons (Ssym "x") Snil))) (Scons (Snum 2) Snil)
    lexpOf de ça : Lcall (Llambda "fn" (Lcall (Llambda "x" Lnil) (Llambda "x" Lnil))) (Lcall (Lnum 2) Lnil)

# Autre exemple: si on a Scons (ssym "nil" ) Snil), ça nous retourne       Lcall || Llambda  Lnil Lnil


# ça serait quoi le Slip de l'exemple Llambda du pdf...

# nil 1 , c'est quoi le lexp de retour et eval


# comment différencier dans dexp les opérations (+,-,..) des autres lref? ex : + x y
# Dans eval, quand on veut évaluer un Dlambda vu que il prend un seul argument, comment gérer la recursion pour qu'il puisse appeler une fonction et ses arguments(Dcall)


#Rapport

1 Introduction
Comme indiqu ́e dans l’ énoncé du devoir. Le travail vise à am ́eliorer notre compréhension des langages fonctionnels
en utilisant le langage Haskell pour compl ́eter le code qui impl ́emente un interprète d’un langage de programmation
fonctionnel Slip.
Dans ce rapport, on va parler principalement de la méthodologie utilisée pour comprendre l’ ́enoncé et compléter
le code. Mais aussi on va parler des problèmes rencontrées et les différentes manières qu’on a utilis ́ees pour les
résoudre.

2 Méthode utilisée

D’une manière générale, on a travaillé en binôme pour faire tout le travail tel que conseillé, que ce soit pour la compréhension et
pour compléter le code, mais exceptionnelement quand on rencontrait un problème qui nous empêche d’avancer,
chacune allait réfléchir de son côtté et on mettait nos id ́ees ensemble pour résoudre le problème et continuer.
Après avoir lu et relu le pdf de l’énoncé, on a ciblé les points importants qui vont nous aider à faire le travail.
