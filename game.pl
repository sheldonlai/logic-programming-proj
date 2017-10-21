
:-  dynamic(occupied/2), dynamic(army/2), dynamic(infantryCount/2).

:- retractall(occupied(_, _)), retractall(army(_, _)).

% tried to balance the amount of countries (now 3 groups + more connectivity)

% Canada
country(nwt).
country(alberta).
country(ontario).
country(eastCanada).

% USA
country(alaska).
country(eastUs).
country(westUs).
country(centralAmerica).

% South America
country(venezuela).
country(peru).
country(brazil).
country(argentina).

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
  country(C),
  occupied(X, C),
  write('Youre already occupying the location!'),
  nl,
  false.

% case where you occupy someone elses territory
occupy(X, C) :-
  country(C),
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
  country(C),
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

% Getting the continent of the highest number of owned countries.
%targetSetUp(X) :-



attack(X, C) :-
  country(C),
  dif(X, X2),
  occupied(X2, C),
  write('You cannot attack your own territory').

attack(X, C) :-
  country(C),
  X = X2,
  occupied(X2, C),
  occupied(X, C2),
  is_next_to(C2, C),
  %
  write('TODO not yet implemented'),
  nl. 

%compSetUp :-



armySetUp :-             % Need to figure out how turns will work/ errors as well.
  repeat,
  write("Number of armies left to distribute: "), nl,
  infantryCount(player, X),
  write(X), nl,
  write("Pick a country: "), nl,
  read(C),
  country(C),
  occupied(player, C),
  write("How many armies do you want to add: "), nl,
  read(A),
  A =< X,
  A > 0,
  format("~w armies will be added to ~w.", [A, C]), nl, nl,
  Y is X - A,
  assert(infantryCount(player, Y)),
  retract(infantryCount(player, X)),
  army(C, Armies),
  AN is Armies + A,
  assert(army(C, AN)),
  infantryCount(player, 0).

turn :-
  repeat,
  write("Would you like to attack? Type end. to end turn."), nl. % Needs to be implemented

countryList(L) :-
  findall(X, country(X), L).

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
  assert(infantryCount(player, 20)), % Starting infantry 
  assert(infantryCount(comp, 20)),
  nl,
  armySetUp, % Lets each player put armies in occupied positions
  turn,
  nl.