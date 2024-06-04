let rec hanoi n source cible auxiliaire =
  if n = 1 then
    Printf.printf "Déplacer le disque 1 de %c vers %c\n" source cible
  else begin
    hanoi (n - 1) source auxiliaire cible;
    Printf.printf "Déplacer le disque %d de %c vers %c\n" n source cible;
    hanoi (n - 1) auxiliaire cible source
  end
let () =
  hanoi 5 'A' 'C' 'B' ;;

let rec hanoi_aux n source cible auxiliaire =
  if n = 1 then
    deplace_disque source cible
  else begin
    hanoi_aux (n - 1) source auxiliaire cible;
    deplace_disque source cible;
    hanoi_aux (n - 1) auxiliaire cible source
  end
let hanoi n =
  hanoi_aux n 'A' 'C' 'B'
let () =
  hanoi 5 ;;

let hanoi n =
  let start_time = Sys.time () in
  hanoi_aux 5 'A' 'C' 'B';
  let end_time = Sys.time () in
  Printf.printf "Temps d'exécution pour %d disques : %.6f secondes\n" n (end_time -. start_time)

let rec cherche_n () =
  let n = 20 in (* Commencez avec une valeur plus grande *)
  let start_time = Sys.time () in
  hanoi n;
  let end_time = Sys.time () in
  if end_time -. start_time > 10.0 then
    n
  else
    cherche_n ()

let n_limite = cherche_n ()
Printf.printf "La valeur de n pour laquelle le temps d'exécution dépasse 10 secondes : %d\n" n_limite



