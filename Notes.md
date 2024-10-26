
Utilisez `assert_raises` pour vérifier les exceptions 

Squelette projet :


Main (set up): 
taille de la map 
nb bot / joueur

do loop

afficher score



boucle principale :

initialisation game_state (fonction init)
initialisation out_come (1e joueur qui joue)

while outcome != EndGame of (joueur gagnant) ~ Error :
    if current_player == bot:
        outcome = act ... (bot gameView)
    else :
        let play = ...demander un coup au joueur terminal 
        outcome = act ... play  

return (joueur gagnant)



act :
    executer le play
    vérifier si partie finie
    if case remplie:
        modif score
    else:
        current player = next player
(gère les erreurs)




J'ai créé un type side = N | O | S | E pour mieux géré les murs
