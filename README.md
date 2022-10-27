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

Problèmes résolues: 
------------------

1)Match
-------

Match est une fonction qui branche uniquement sur les list (nil et add) telle que mentionné dans l’énoncé.

Le premier problème rencontré avec le match est au niveau de son Lexp, au départ nous pensions que le lexp correspondant à (match nil (nil 1) ((add x y) (+ x y))) était Lmatch Lnil "x" "y" (Lnum 1) (Lcall (Lcall (Lref "+") (Lref "x")) (Lref "y")) mais en  posant des questions aux démonstreurs qui nous ont aidé avec la comprehension et référé à l’énoncé.
En relisant l’énoncé nous avions pu trouver à quoi le lexp devrait ressembler.
Le  (match e (nil en) ((add x xs) ec))  est Lmatch e x xs ec en, ainsi nous avons pensé à deux solutions :

1)Lmatch Lnil "x" "y" (Lcall (Lcall (Lref "+") (Lref "x")) (Lref "y")) (Lnum 1) 

2)Lmatch Lnil "x" "y" (Lcall (Lcall (Lref "+") (Lref "x")) (Lref "y")) (Lcall Lnil (Lnum 1))

Nous avons fini par choisir la première  solution  car (nil 1) equivalent à “(nil en)” de (match e (nil en) ((add x xs) ec)) doit donner comme lexp (s2l en) donc (s2l 1) qui est Lnum 1 

Le seul problème rencontré dans la fonction l2d est comment les var de l’expression lexp serait placé dans environnement.
l2d env(Lmatch lexp1 var1 var2 lexp2 lexp3)= ?

1)Dmatch (l2d (var1:env) lexp1) (l2d (var2:env) lexp2) (l2d env lexp3)

2)Dmatch (l2d env lexp1) (l2d (var1:(var2:env)) lexp2) (l2d env lexp3)

3)Dmatch (l2d env lexp1) (l2d (var2:(var1:env)) lexp2) (l2d env lexp3)

La première solution donnait une erreur car les variables ne sont pas reliés à lexp1, la deuxième solution donne une réponse car ces variables sont reliées à lexp2 mais les indexes ne sont pas placés dans le bon ordre  car au lieu de mettre d’abord var1 dans l’environnement c’est plutôt le var 2 qui est mis en premier avant var1, la troisième expression est la bonne elle met les var dans le bon ordre.

Concernant l’évaluation nous n’avions eu aucun problème.

La surprise rencontré dans cette étape est l’appelle de fonction contenant nil mais avec l’expression du match décrite dans l’énoncé les doutes ont été balayer  .

2)Appel de fonction et définition de fonction
-----------------------------------------------

Nous n’avions pas rencontré de problème particulier avec l’appel de fonction(Lcall) qui s’est fait rapidement grâce à la compréhension des précédentes démo faites en classe.
Concernant la définition de fonctions (Lambda) le s2l et eval se sont aussi fait rapidement, le seul problème a été au niveau du l2d où les variables ne sont pas représentées par les chaînes de caractères mais plutôt par les indexes de  “de Bruijn” , afin de gérer cela nous avons dû créer une fonction indexOf qui nous permettrait de trouver l’index des ces variables dans l’environnement.
