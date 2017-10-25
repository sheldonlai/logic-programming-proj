
:-  dynamic(occupied/2), dynamic(army/2), dynamic(infantryCount/2).

% :- retractall(occupied(_, _)), retractall(army(_, _)).

% tried to balance the amount of countries (now 3 groups + more connectivity)

% Canada
country(nwt, canada).
country(alberta, canada).
country(ontario, canada).
country(eastCanada, canada).

% USA
country(alaska, us).
country(eastUs, us).
country(westUs, us).
country(centralAmerica, us).

% South America
country(venezuela, sa).
country(peru, sa).
country(brazil, sa).
country(argentina, sa).

% Relations
% easier to make relations

next_to(alaska, nwt).
next_to(alaska, alberta).

next_to(ontario, eastCanada).
next_to(ontario, westUs).
next_to(ontario, eastUs).
next_to(ontario, alberta).
next_to(ontario, nwt).

next_to(centralAmerica, westUs).
next_to(centralAmerica, eastUs).
next_to(centralAmerica, venezuela).

next_to(venezuela, brazil).
next_to(venezuela, peru).

next_to(peru, brazil).
next_to(peru, argentina).

next_to(argentina, brazil).

team(player).
team(comp).

is_next_to(X, Y) :-
  next_to(X, Y);
  next_to(Y, X).


occupy(X, C) :-
  country(C, _),
  occupied(X, C),
  write(X),
  write(' is already occupying the location!'),
  nl,
  false.

% case where you occupy someone elses territory
occupy(X, C) :-
  country(C, _),
  occupied(X2, C),
  dif(X2, X),
  retract(occupied(X2, C)),
  assert(occupied(X, C)),
  write(X),
  write(' have occupied the country: '),
  write(C),
  write(', from: '),
  write(X2),
  nl.

% case where the country is empty
occupy(X, C) :-
  country(C, _),
  \+ occupied(_, C),
  assert(occupied(X, C)),
  assert(army(C, 1)).
  %write(X),
  %write(' occupied empty spot: '),
  %write(C),
  %nl.

own_canada(Team) :-
  occupied(Team, nwt),
  occupied(Team, alberta),
  occupied(Team, ontario),
  occupied(Team, eastCanada).

own_us(Team) :-
  occupied(Team, alaska),
  occupied(Team, westUs),
  occupied(Team, eastUs),
  occupied(Team, centralAmerica).

own_south(Team) :-
  occupied(Team, venezuela),
  occupied(Team, brazil),
  occupied(Team, peru),
  occupied(Team, argentina).

bonus(Team, SouthBonus) :-
  team(Team),
  own_south(Team),
  SouthBonus is 4.

bonus(Team, CanadaBonus) :-
  team(Team),
  own_canada(Team),
  CanadaBonus is 4.

bonus(Team, USBonus) :-
  team(Team),
  own_us(Team),
  USBonus is 4.


continent(N) :-
  country(_, N).

countriesLeft(X, N, C) :-
  country(C, N),
  \+ occupied(X, C).

yourcountries(X, N, C) :-
  country(C, N),
  occupied(X, C).

nearbyAICountry(Country, AI) :-
	occupied(comp, AI),
	is_next_to(Country, AI).

% returns number and list of countries left for a continent
count_countriesLeft(Player, Continent, Count, L) :-
  findall(Country, countriesLeft(Player, Continent, Country), L),
  length(L, Count).

count_yourcountries(Player, Continent, Count, L) :-
  findall(Country, yourcountries(Player, Continent, Country), L),
  length(L, Count).


target(X, Y, Z, B) :-
  B1 is min(X, Y),
  B is min(Z, B1).

% X is the player who is AttackingArmy
% AttackOn is the territory the player is attacking
% AttackFrom is where the [player] is attaking From
% AttackingArmy is the amount of attacking troops
% DefendingArmy is the amount of defending troops


% Exceptiosn
attack(X, C, _, _, _, _, _) :-
  country(C,_),
  X = X2,
  occupied(X2, C),
  write('You cannot attack your own territory'),
  fail.

attack(_, _, _, A, B, C, D) :-
  (not(integer(A)); not(integer(B)); not(integer(C)); not(integer(D))),
  write("All armies and dice should be integers!!!"), nl,
  fail.

attack(_, _, _, AttackingArmyErr, _, AttackDiceErr, _) :-
  (X is AttackingArmyErr - AttackDiceErr, X =< 1);
  AttackDiceErr > 3,
  write("The amount of dice and attacker rolls should be 1 - 3 and smaller than "),
  write(AttackingArmyErr),
  fail.

attack(_, _, _, _, DefendingArmyErr, _, DefendDiceErr) :-
  DefendDiceErr > 2;
  (Y is DefendingArmyErr - DefendDiceErr, Y < 0),
  write("The amount of dice and attacker rolls should "),
  write("be 1 - 2 and smaller or equal to "),
  write(DefendingArmyErr), nl,
  fail.

% actual function
attack(X, AttackOn, AttackFrom, AttackingArmy, _, AttackDice, DefendDice) :-
  country(AttackOn,_),
  occupied(X2, AttackOn),
  dif(X, X2),
  occupied(X, AttackFrom),
  is_next_to(AttackFrom, AttackOn),
  army(AttackOn, N),
  army(AttackFrom, N2),
  attack_result(AttackDice, DefendDice, AttackerLoss, DefenderLoss), !,
  write(X),
  write(" lost "),
  write(AttackerLoss),
  write(" troops from the attack, and "),
  write(X2),
  write(" lost "),
  write(DefenderLoss),
  write(" troops from the attack."),
  nl,
  N3 is N - DefenderLoss,
  (N3 > 0 ->
    set_army(AttackOn, N3),
    NewAttackingArmy is N2 - AttackerLoss,
    set_army(AttackFrom, NewAttackingArmy),
    write(AttackOn), write(" now has "), write(N3),
    write(" troops."), nl,
    write(AttackFrom), write(" now has "), write(NewAttackingArmy),
    write(" troops."), nl, fail
  ;
    occupy(X, AttackOn),
    NewAttackOnArmy is AttackingArmy - AttackerLoss,
    set_army(AttackOn, NewAttackOnArmy),
    write(AttackOn), write(" now has "), write(NewAttackOnArmy),
    write(" troops."), nl,
    NewAttackingArmy is N2 - AttackingArmy,
    write(AttackFrom), write(" now has "), write(NewAttackingArmy),
    write(" troops."), nl,
    set_army(AttackFrom, NewAttackingArmy),
    nl

  ).

% attack helpers

/**
 * compare_rolls takes in two sorted descending lists and compare which each
 * element with the rolls until one of the list runs out of elements
 * AttackerLoss is the amount of times where List2[i] >= List1[i]
 * DefenderLoss is the amount of times where List2[i] < List1[i]
 */
attack_result(AttackDice, DefendingDice, AttackerLoss, DefenderLoss) :-
  randset(AttackDice, 6, List1),
  reverse_list(List1, AttackRolls),
  write("Attacker rolled: "),
  write(AttackRolls),
  nl,
  randset(DefendingDice, 6, List2),
  reverse_list(List2, DefendRolls),
  write("Defender rolled: "),
  write(DefendRolls),
  nl,
  compare_rolls(AttackRolls, DefendRolls, AttackerLoss, DefenderLoss).


/**
 * compare_rolls takes in two sorted descending lists and compare which each
 * element with the rolls until one of the list runs out of elements
 * AttackerLoss is the amount of times where List2[i] >= List1[i]
 * DefenderLoss is the amount of times where List2[i] < List1[i]
 */
compare_rolls([], _, 0, 0).
compare_rolls(_, [], 0, 0).
compare_rolls([H|T],[H2|T2], AttackerLoss, DefenderLoss) :-
  H > H2,
  compare_rolls(T,T2, AttackerLoss, Loss2),
  DefenderLoss is Loss2 + 1.

compare_rolls([_|T],[_|T2], AttackerLoss, DefenderLoss) :-
  compare_rolls(T,T2, Loss1, DefenderLoss),
  AttackerLoss is Loss1 + 1.

/**
 * A helper function to free the previous and set the new army count
 */
set_army(Place, Amount) :-
  retractall(army(Place, _)),
  assert(army(Place, Amount)).

set_Infantry(Team, Amount) :-
  retractall(infantryCount(Team, _)),
  assert(infantryCount(Team, Amount)).


/**
 * List utills to reverse the list as described in its name
 */
reverse_list(List, Result) :-
  reverse_helper(List, [], Result).

reverse_helper([H|T], Acc, Result) :-
  reverse_helper(T, [H|Acc], Result).

reverse_helper([], Acc, Acc).


/**
 * Show territories that you can attack the specified target from
 */
can_attack_from(Player, AttackOn, _):-
  occupied(Player, AttackOn),
  write('You cannot attack your own territory'), nl,
  fail.

can_attack_from(Player, AttackOn, Result) :-
  country(AttackOn,_),
  occupied(Player2, AttackOn),
  dif(Player, Player2),
  findall(From,
         (
            occupied(Player, From),
            is_next_to(AttackOn, From),
            army(From, ArmySize),
            ArmySize > 1
         ),
         Result).

/**
 * Show territories that you can attack on
 */
can_attack_on(Player, Result) :-
  findall(
    On,
    (
      occupied(Player, X),
      army(X, N),
      N > 1,
      is_next_to(On, X),
      not(occupied(Player, On))
    ),
    Result
  ).

% attack helper ends

can_move_from(Player, MoveOn, Result) :-
  country(MoveOn,_),
  occupied(Player, MoveOn),
  army(MoveOn, _),
  findall(From, (occupied(Player, From), is_next_to(MoveOn, From), army(From, Armies), Armies > 1), Result).

your_armies(Player, Result) :-
  findall([Country | Armies] ,(state(Player, Armies, Country), occupied(Player, Country)), Result).

moveControl(Team) :-
  repeat,
  write("Would you like to move your armies? "),
  read(X), nl,
    (
    X == yes ->
    your_armies(Team, Result),
    write("Which country would you like to move your armies to?"), nl,
    write(Result), nl,
    read(Country),
    country(Country,_),
    can_move_from(Team, Country, R),
    length(R, Count),
    Count > 0,
    format("Which country do you want to move from? ~w", [R]), nl,
    read(MoveFrom),
    write("Armies to move : "),
    army(MoveFrom, Armies),
    read(ArmiesMove),
    ArmiesMove =< Armies,
    ArmiesMove > 0,
    army(Country, Prev),
    AddedArmies is Prev + ArmiesMove,
    RemovedArmies is Armies - ArmiesMove,
    set_army(Country, AddedArmies),
    set_army(MoveFrom, RemovedArmies),
    write("Now you have: "), nl,
    your_armies(Team, New),
    write(New)
    ;
    write("terminating moveControl"), nl,
    !
    ).

% print helper
print_territories_armies([]).

print_territories_armies([H|T]) :-
  country(H, _),
  army(H,N),
  write("    "), write(H), write(" : "), write(N), nl,
  print_territories_armies(T).

get_defending_army(0, 1).
get_defending_army(1, 1).
get_defending_army(Max, R):-
  random(1, Max, R).

aiSetup(Country, Armies) :-
	nearbyAICountry(Country, Nearby),
	infantryCount(comp, X),
	army(Nearby, Prev),
	Y is X - Armies,
	set_Infantry(comp, Y),
	NA is Armies + Prev,
	set_army(Nearby, NA),
	your_armies(comp , Result),
    write("Computers Armies ==> [Country | Armies]"), nl,
    write(Result), nl, nl.


armySetUp(Team) :-             % Need to figure out how turns will work/ errors as well.
  format("~w is now distributing armies... ~n", [Team]),
  repeat,
  write("Number of armies left to distribute: "),
  infantryCount(Team, X),
  write(X), nl,
  count_yourcountries(Team,_,_,L),
  format("Pick a country: ~w", [L]), nl,
  read(C),
  country(C,_),
  occupied(Team, C),
  write("How many armies do you want to add: "), nl,
  read(A), nl,
  A =< X,
  A > 0,
  Y is X - A,
  set_Infantry(Team, Y),
  army(C, Armies),
  NewArmies is Armies + A,
  set_army(C, NewArmies),
  once(aiSetup(C, A)),
  your_armies(Team , Result),
  write("Your Armies ==> [Country | Armies]"), nl,
  write(Result), nl, nl,
  infantryCount(Team, 0).

infantryControl(Team) :-
  count_yourcountries(Team, _, C, _),
  findall(Bonus, bonus(Team, Bonus), Result),
  sum_list(Result, Sum),
  Total is Sum + C,
  set_Infantry(Team, Total),
  armySetUp(Team).


attackControl :-
  repeat,
  write("Would you like to attack? Type 'yes.' to attack, Type 'end.' to end turn."), nl,
  read(X),
  (X == yes ->
    attackControlHelper,
    fail
  ;
    write("terminating attack loop"), nl,
    !
  ).

attackControlHelper :-
  can_attack_on(player, AttackList),
  print_territories_armies(AttackList),
  read(AttackOn),
  country(AttackOn, _),
  can_attack_from(player, AttackOn, FromList),
  write("Choose a territory to attack from:"), nl,
  print_territories_armies(FromList),
  read(From),
  country(From, _),
  write("Choose how many troops you would like to deploy."), nl,
  army(From, N),
  read(Troops),
  % RNG for defending troops for computer
  army(AttackOn, MaxDefending),
  DefendDice is min(2,MaxDefending),
  AttackDice is min(3, N - 1),
  attack(player, AttackOn, From, Troops, MaxDefending, AttackDice, DefendDice),
  moveControl(player),
  nl.

attack_control_ai :-
  nl,nl,
  can_attack_on(comp, AttList),
  length(AttList, Len),
  ( Len == 0 ->
    write("comp will not attack."), nl,
    fail;
    true
  ),
  choose_most_vulnerable(AttList, AttackOn),
  can_attack_from(comp, AttackOn, FromList),
  choose_strongest(FromList, AttackFrom),
  write("comp is Attacking "), write(AttackOn),
  write(" from "), write(AttackFrom), write("..."), nl,
  army(AttackOn, MaxDefending),
  MaxDefendingDice is min(2, MaxDefending),
  repeat,
  write("How dice would you like to roll? Max:"), write(MaxDefendingDice), nl,
  read(DefnedingDice),
  army(AttackFrom, AttackFromArmy),
  MaxAttackArmy = AttackFromArmy - 1,

  AttackDice is min(3, MaxAttackArmy),
  attack(comp, AttackOn, AttackFrom, MaxAttackArmy,
    MaxDefending, AttackDice, DefnedingDice).



/*
 * This method chooses the largest army from a list of territories
 */

choose_strongest(List, Result):-
  choose_strongest_helper(List, none, Result).

choose_strongest_helper([H|T], none, Result):-
  choose_strongest_helper(T, H, Result).

choose_strongest_helper([], Acc, Acc).
choose_strongest_helper([H|T], Acc, Result):-
  army(H, N),
  army(Acc, AccN),
  N > AccN,
  choose_strongest_helper(T, H, Result).

choose_strongest_helper([_|T], Acc, Result):-
  choose_strongest_helper(T, Acc, Result).

/*
 * This method chooses the smallest army from a list of territories
 */
choose_most_vulnerable(List, Result) :-
  choose_most_vulnerable_helper(List, none ,Result).

choose_most_vulnerable_helper([H|T], none, Result):-
  choose_most_vulnerable_helper(T, H, Result).

choose_most_vulnerable_helper([], Acc, Acc).

% if the current element's army size is less than Acc
choose_most_vulnerable_helper([H|T], Acc, Result):-
  army(H, N),
  army(Acc, AccN),
  N < AccN,
  choose_most_vulnerable_helper(T, H, Result).

% else
choose_most_vulnerable_helper([_|T], Acc, Result):-
  choose_most_vulnerable_helper(T, Acc, Result).

% setUpTurn is to set initial Infantry Values.

setUpTurn :-
  set_Infantry(player, 25),
  set_Infantry(comp, 25),
  armySetUp(player),
  attackControl,
  attack_control_ai,
  turn.

turn :-
  repeat,
  infantryControl(player),
  attackControl,
  attack_control_ai,
  write('End').

countryList(L) :-
  findall(X, country(X,_), L).

randomCountries([], _).

randomCountries(L, player) :-
  random_member(X, L),
  select(X, L, L2),
  occupy(player, X),
  randomCountries(L2, comp).

randomCountries(L, comp) :-
  random_member(X, L),
  select(X, L, L2),
  occupy(comp, X),
  randomCountries(L2, player).

start :-
  write('Welcome to the game of RISK.'), nl, nl,
  write('Randomizing Countries....'), nl, nl,
  countryList(CL),
  randomCountries(CL, player),
  your_armies(player, P),
  write("Player's Countries ==> "), nl,
  write(P), nl, nl,
  your_armies(comp, C),
  write("Computer's Countries ==>"), nl,
  write(C), nl, nl,
  setUpTurn,
  count_yourcountries(player,_,0,_); count_yourcountries(comp,_,0,_).

score(X, Score) :-
  totalArmies(X, N),
  count_countriesLeft(X, _, Count, _),
  Score is Count/12*N.

state(X, Armies, C) :-
  occupied(X, C),
  army(C, Armies).

totalArmies(X, N) :-
  team(X),
  findall(A, state(X, A, _), L),
  sum_list(L, N).
