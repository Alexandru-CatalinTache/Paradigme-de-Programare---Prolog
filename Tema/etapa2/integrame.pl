:- ensure_loaded('checker.pl').

test_mode(detailed).

% Considerăm următoarele reprezentări:
%
% O integramă este reprezentată prin structura (compusul)
% integ(H, W, Lista, Vocab), unde:
% H este înălțimea integramei
% W este lățimea integramei
% Lista este o listă de tupluri (Poz, Valoare), unde
%   Poz este un tuplu (R, C) conținând rândul și coloana (0-based)
%   Valoare este una dintre:
%     x - dacă celula este neagră (nu poate fi completată cu litere)
%     o literă, dacă celula este completată cu o literă
%     o listă de întrebări, reprezentate ca tupluri (Text, Dir, ID), cu
%       Text - un literal, textul întrebării
%       Dir - una dintre valorile j sau d, indicând direcția întrebării
%       ID - un identificator numeric al întrebării
% Vocab este o listă de literali reprezentând cuvinte disponibile
% pentru a rezolva întrebarea.
%
% În ieșirea predicatului intrebări, o întrebare este reprezentată ca
% ((R, C), Text, Dir, ID), unde
% R este rândul căsuței cu întrebarea (0-based)
% C este coloana căsuței cu întrebarea (0-based)
% Text este textul întrebării (un literal)
% Dir este j sau d, reprezentând direcția în care trebuie să fie plasat
% răspunsul (jos sau dreapta)
% ID este un identificator numeric al întrebării.

% Puteți vizualiza integramele cu:
% integrama(0, W), print_integrama(W).
% integrama(1, W), print_integrama(W).
% integrama(2, W), print_integrama(W).
% integrama(3, W), print_integrama(W).
%
% Testați cu
% vmtest.
% Testați teste individuale (vedeți predicatul tt din checker.pl) cu
% vmtest(Test).
% de exemplu cu vmtest(intrebari).


% intrebari/2
% intrebari(integ(+H, +W, +Lista, +Vocab), -Lista_intrebari)
% Este adevărat atunci când Lista_intrebari este o lista de tupluri
% ((R, C), Text, Dir, ID), fiecare tuplu corespunzând unei întrebări din
% integramă (rândul, coloana, textul întrebării, direcția (j/d),
% identificatorul).
%
% BONUS: intrebari are o singură soluție (o singură listă) pentru o
% anumită integramă.

% cazul cu lista de intrebari vida
intrebari(integ(H, W, [], Vocab), []) :- true.

% cazul lista vida de intrebari din lista integramei
intrebari(integ(H, W, [((R, C), [])|Lista], Vocab), Lista_intrebari) :- intrebari(integ(H, W, Lista, Vocab), Lista_intrebari),!.

% cazul in care am pentru o pereche (R, C) una sau mai multe (in orice caz maxim 2) directii: il iau pe primul de forma (Text, Dir, ID) si al doilea (Tail) il apelez apoi
intrebari(integ(H, W, [((R, C), [(Text, Dir, ID)|Tail])|Lista], Vocab), [((R, C), Text, Dir, ID)|Lista_intrebari]) :-  
 intrebari(integ(H, W, [((R, C), Tail)|Lista], Vocab), Lista_intrebari), !.

% cazul cand nu primesc o lista cu text, dir si id, ci orice altceva (x sau o litera)
intrebari(integ(H, W, [((R, C), _)|Lista], Vocab), Lista_intrebari) :- intrebari(integ(H, W, Lista, Vocab), Lista_intrebari).


% id_intrebare/2
% id_intrebare(+Integ, ?Intrebare, ?Q_ID)
% Este adevărat dacă în integrama reprezentată ca integ(...), Intrebare
% este un text iar Q_ID este un identificator care corespund aceleași
% întrebări.

% cazul cu lista vida
id_intrebare(integ(H, W, [], Vocab), Intrebare, Q_ID) :- false.

% pentru o lista in care avem la o pozitie 2 directii
 id_intrebare(integ(H, W, [((R, C), [(Text1, Dir1, ID1), (Text2, Dir2, ID2)])|Lista], Vocab), Text1, ID1) :- true.
 id_intrebare(integ(H, W, [((R, C), [(Text1, Dir1, ID1), (Text2, Dir2, ID2)])|Lista], Vocab), Text2, ID2) :- true.

id_intrebare(integ(H, W, [((R, C), [(Text, Dir, ID)])|Lista], Vocab), Text, ID) :- true.

% cazul cu altceva in loc de intrebare (si cazul la care am staat 5 ore ca am scris intreg in loc de integ)
id_intrebare(integ(H, W, [((R, C), _)|Lista], Vocab), Text, ID) :- id_intrebare(integ(H, W, Lista, Vocab), Text, ID).

% completare/3
% completare(+Integ, +Sol, -Integrama)
% Predicatul produce Integrama, o structură de forma integ(...),
% pornind de la Integ, în care au fost completate celule conform cu
% soluția Sol.
% Soluția este reprezentată ca o listă de perechi (Întrebare, Răspuns),
% unde Întrebarea este textul unei întrebări, iar Răspuns este un cuvânt
% de completat; ambele sunt atomi (literali).
% De exemplu, o soluție parțială pentru integrama 0 poate fi:
% [('Din care plouă', 'NOR'), ('Al doilea număr', 'DOI')]
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), solutie(0, Sol), completare(W, Sol, W2),
%   print_integrama(W2).
completare(_, _, _) :- false.

% lungime_spatiu/3
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), +Intrebare, -Lungime)
% pentru Bonus:
% lungime_spatiu(integ(+H, +W, +Lista, +Vocab), ?Intrebare, ?Lungime)
%
% Returnează lungimea spațiului asociat întrebării date.
% Întrebarea este indicată prin textul ei. De exemplu:
% lungime_spatiu pentru integrama 0 și întrebarea 'Al doilea număr'
% trebuie să lege Lungime la 3.
%
% BONUS: lungime_spatiu are o singură soluție pentru o anumită
% întrebare.
% Puteți testa manual predicatul cu o interogare de forma:
% integrama(0, W), id_intrebare(W, Text, 3), lungime_spatiu(W, Text, X).

% pt cand am primit casuta cu textul intrebarii si directia in jos

lungime_spatiu(integ(H, W, [((R, C), [(Text, j, ID)])|Lista], Vocab), Text, X) :- NewR is R + 1,
                                                                                                 verificare_jos(NewR, C, j, H, W, Lista, 0, X).


verificare_jos(H, _, _, H, _, _, Acc, Acc).
verificare_jos(R, C, _, _, _, Lista, Acc, Acc) :- member(((R, C), _), Lista).
verificare_jos(R, C, j, H, W, Lista, Acc, X) :- R < H, \+ member(((R, C), _), Lista),
                                         NewR is R + 1, NewAcc is Acc + 1, verificare_jos(NewR, C, j, H, W, Lista, NewAcc, X).

% pt dreapta

lungime_spatiu(integ(H, W, [((R, C), [(Text, d, ID)])|Lista], Vocab), Text, X) :- NewC is C + 1,
                                                                                                 verificare_dreapta(R, NewC, d, H, W, Lista, 0, X).

verificare_dreapta(_, W, _, _, W, _, Acc, Acc).
verificare_dreapta(R, C, _, _, _, Lista, Acc, Acc) :- member(((R, C), _), Lista).
verificare_dreapta(R, C, d, H, W, Lista, Acc, X) :- C < W,  \+ member(((R, C), _), Lista),
                                              NewC is C + 1, NewAcc is Acc + 1, verificare_dreapta(R, NewC, d, H, W, Lista, NewAcc, X).

% pt casuta cu 2 intrebari

lungime_spatiu(integ(H, W, [((R, C), [(Text1, j, ID1), (Text, d, ID)])|Lista], Vocab), Text, X) :- NewC is C + 1,
                                                                                                 verificare_dreapta(R, NewC, d, H, W, Lista, 0, X).

lungime_spatiu(integ(H, W, [((R, C), [(Text, d, ID), (Text1, j, ID1)])|Lista], Vocab), Text, X) :- NewC is C + 1,
                                                                                                 verificare_dreapta(R, NewC, d, H, W, Lista, 0, X).

lungime_spatiu(integ(H, W, [((R, C), [(Text, j, ID), (Text1, d, ID1)])|Lista], Vocab), Text, X) :- NewR is R + 1, 
                                                                                                verificare_jos(NewR, C, j, H, W, Lista, 0, X).

lungime_spatiu(integ(H, W, [((R, C), [(Text1, d, ID1), (Text, j, ID)])|Lista], Vocab), Text, X) :- NewR is R + 1, 
                                                                                                verificare_jos(NewR, C, j, H, W, Lista, 0, X).

lungime_spatiu(integ(H, W, [_ | Lista], Vocab), Text, X) :- lungime_spatiu(integ(H, W, Lista, Vocab), Text, X).

% intersectie/5
% intersectie(integ(+H, +W, +Lista, +Voc), +I1, -Poz1, +I2, -Poz2)
% pentru Bonus:
% intersectie(integ(+H, +W, +Lista, +Voc), ?I1, ?Poz1, ?I2, ?Poz2)
%
% Pentru o integramă și două întrebări date prin textul lor (I1 și I2),
% al căror răspunsuri se intersectează, întoarce în Poz1 indicele din
% răspunsul la I1 la care este intersecția, și în Poz2 indicele din
% răspunsul la I2 la care este intersecția. Indecșii incep de la 0.
%
% De exemplu, în integrama 0:
%  █       █       2↓      3↓      █
%  █       0↓,1→   -       -       █
%  4→      -       -       -       █
%  5→      -       -       -       █
%  █       █       █       █       █
%
%  Întrebările 'Primii 3 din artă' și 'Afirmativ' (3, respectiv 1) se
%  intersectează la pozițiile 0, respectiv 2 (va fi litera A, de la
%  ART, respectiv DA).
% intersectie(_, _, _, _, _) :- false.

intersectie(integ(H, W, Lista, Voc), I1, Poz1, I2, Poz2) :- lungime_spatiu(integ(H, W, Lista, Voc), I1, Lung1), lungime_spatiu(integ(H, W, Lista, Voc), I2, Lung2),
                                                            poz_de_inceput(I1, Lista, R1, C1, Dir1), poz_de_inceput(I2, Lista, R2, C2, Dir2),
                                                            verif(Dir1, Dir2, R1, C1, R2, C2),
                                                            minmin(Lung1, Lung2, Lung), minmin(R1, R2, R_min), minmin(C1, C2, C_min),
                                                            calculare_poz(PPoz1, PPoz2, R_min, C_min, Lung, R1, R2, C1, C2, Dir1, Dir2), Poz1 is PPoz1 - 1, Poz2 is PPoz2 - 1.

% verificare directie diferita si coloana si rand de start diferite
verif(Dir1, Dir2, R1, C1, R2, C2) :- \+ Dir1 == Dir2, \+ R1 == R2, \+ C1 == C2.
intersectie(_, _, _, _, _) :- false.

% calculare poz1, poz2, in functie de directie
calculare_poz(Poz1, Poz2, R, C, Lung, R1, R2, C1, C2, Dir1, Dir2) :-
    Dir1 == d, R_a is R1 - R, R_b is R2 - R, Poz2 is max(R_a, R_b), C_a is C1 - C, C_b is C2 - C, Poz1 is max(C_a, C_b),!;
    R_a is R1 - R, R_b is R2 - R, Poz1 is max(R_a, R_b), C_a is C1 - C, C_b is C2 - C, Poz2 is max(C_a, C_b).   

minmin(Lung1, Lung2, Lung) :- Lung is min(Lung1, Lung2).                                           

% cautare valori R, C pentru textul unei intrebari
poz_de_inceput(I, [Head | Lista], R, C, Dir) :- member(((R, C), [(I, Dir, _ )]), Lista); member(((R, C), [(I, Dir, _), _]), Lista);
                                           member(((R, C), [_, (I, Dir, _)]), Lista).


% solutii_posibile/2
% solutii_posibile(integ(+H, +W, +Lista, +Vocabular), -Solutii)
% Formează o listă Solutii, conținând perechi de forma
% (Întrebare, Cuvinte), unde
% Întrebare este textul unei întrebări din integramă, iar Cuvinte este o
% listă de cuvinte sunt din Vocabular și au lungimea corectă pentru a fi
% răspuns la întrebare. Solutii conține câte o pereche pentru fiecare
% întrebare din integramă.
% Cuvintele sunt reprezentate ca liste de atomi, fiecare atom
% având lungime 1 (o singură literă).
% De exemplu, pentru integrama 0, Solutii conține 6 perechi, două dintre
% ele fiind:
% ('Afirmativ', [['D', 'A'], ['N', 'U']])
% ('Din care plouă',
% [['N','O','R'],['A','R','T'],['U','I','T'],['D','O','I']])
solutii_posibile(_, _) :- false.

% rezolvare/2
% rezolvare(+Integ, -Solutie)
% Rezolvare produce în Solutie soluția integramei Integ. Soluția este
% reprezentată ca o listă de perechi de literali, fiecare pereche
% conținând textul unei întrebări și cuvântul (ca literal) care este
% răspunsul la întrebare.
%
% BONUS: rezolvare nu oferă soluții duplicate - numărul de soluții ale 
% predicatului este chiar numărul de completări posibile ale integramei.
rezolvare(_, _) :- false.
