
:-  dynamic(occupied/2), dynamic(army/2).

:- retractall(occupied(_, _)), retractall(army(_, _)).

country(canada).
country(us).
country(mexico).
country(brazil).
country(peru).
country(agentina).
country(chile).
country(paraguay).
country(uruguay).
country(columbia).
country(venezuela).
country(bolivia).
country(ecuador).
country(carribean). % for simplicity's sake


next_to(canada, us).
next_to(mexico, us).
next_to(mexico, carribean).


next_to(agentina, chile).
next_to(agentina, paraguay).
next_to(agentina, uruguay).
next_to(agentina, bolivia).


next_to(bolivia, brazil).
next_to(bolivia, chile).
next_to(bolivia, peru).
next_to(bolivia, agentina).
next_to(bolivia, paraguay).

next_to(brazil, venezuela).
next_to(brazil, columbia).
next_to(brazil, peru).
next_to(brazil, paraguay).
next_to(brazil, uruguay).

next_to(peru, chile).
next_to(peru, columbia).
next_to(peru, edcuador).

next_to(columbia, edcuador).
next_to(columbia, venezuela).




is_next_to(X, Y) :-
  next_to(X, Y);
  next_to(Y, X).


occupy(X, C) :-
  country(C),
  occupied(X, C),
  write('Youre already occupying the location!'),
  nl.
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
  assert(army(C, 4)),
  write('occupied empty spot.'),
  nl.

own_north_america(X) :-
  occupied(X, canada),
  occupied(X, us).

attack(X, C) :-
  country(C),
  dif(X, X2),
  occupied(X2, C),
  write('You cannot attack your own territory').

attack(X, C) :-
  country(C),
  occupied(X2, C),
  X = X2,
  occupied(X, C2),
  is_next_to(C2, C),
  %
  write('TODO not yet implemented'),
  nl.

set_army(C, A) :-
  country(C),
  retract(army(C, A)),
  assert(army(C, A)).
