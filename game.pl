
:-  dynamic(occupied/2), dynamic(army/2), dynamic(infantryCount/2).

:- retractall(occupied(_, _)), retractall(army(_, _)).

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
  write('Youre already occupying the location!'),
  nl,
  false.

% case where you occupy someone elses territory
occupy(X, C) :-
  country(C, _),
  occupied(X2, C),
  dif(X2, X),
  retract(occupied(X2, C)),
  assert(occupied(X, C)),
  write('You have occupied the country: '),
  write(C),
  write(', from player: '),
  write(X2),
  nl.

% case where the country is empty
occupy(X, C) :-
  country(C, _),
  \+ occupied(_, C),
  assert(occupied(X, C)),
  assert(army(C, 1)), 
  write(X),
  write(' occupied empty spot: '),
  write(C),
  nl.

own_canada(X) :-
  occupied(X, nwt),
  occupied(X, alberta),
  occupied(X, ontario),
  occupied(X, eastCanada).

own_us(X) :-
  occupied(X, alaska),
  occupied(X, westUs),
  occupied(X, eastUs),
  occupied(X, centralAmerica).

own_south(X) :-
  occupied(X, venezuela),
  occupied(X, brazil),
  occupied(X, peru),
  occupied(X, argentina).

continent(C, N) :-
  country(C, N).

countriesLeft(X, N, C) :-
  country(C, N),
  \+ occupied(X, C).

yourcountries(X, N, C) :-
  country(C, N),
  occupied(X, C).

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

attack(X, C) :-
  country(C,_),
  dif(X, X2),
  occupied(X2, C),
  write('You cannot attack your own territory').

attack(X, C) :-
  country(C,_),
  X = X2,
  occupied(X2, C),
  occupied(X, C2),
  is_next_to(C2, C),
  %
  write('TODO not yet implemented'),
  nl. 


armySetUp(Team) :-             % Need to figure out how turns will work/ errors as well.
  format("~w is now distributing armies... ~n", [Team]),
  repeat,
  write("Number of armies left to distribute: "), nl,
  infantryCount(Team, X),
  write(X), nl,
  count_yourcountries(Team,_,_,L),
  format("Pick a country: ~w", [L]), nl,
  read(C),
  country(C,_),
  occupied(Team, C),
  write("How many armies do you want to add: "), nl,
  read(A),
  A =< X,
  A > 0,
  format("~w armies will be added to ~w.", [A, C]), nl, nl,
  Y is X - A,
  assert(infantryCount(Team, Y)),
  retract(infantryCount(Team, X)),
  retract(army(C, Armies)),
  AN is Armies + A,
  assert(army(C, AN)),
  infantryCount(Team, 0).

infantryControl :-
  assert(infantryCount(player, 20)), % Starting infantry 
  assert(infantryCount(comp, 20)),
  armySetUp(player),
  armySetUp(comp).

attackControl :- % needs to be implemented
  repeat,
  write("Would you like to attack? Type end. to end turn."), nl,
  read(X),
  write(X), nl,
  true.


turn :-
  repeat,
  infantryControl,
  attackControl.


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
  write('Welcome to the game of RISK.'),
  nl,
  countryList(CL),
  randomCountries(CL, player),
  nl,
  turn,
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

