
:-  dynamic(occupied/2).

:- retractall(occupied(_, _)), retractall(army(_, _)).

country(canada).
country(us).
country(mexico).
country(brazil).
country(peru).
country(agentina).
country(chile).


occupy(X, C) :-
  country(C),
  occupied(X, C),
  write('You''re already occupying the lcoation!'),
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
  write(',from player: '),
  write(X2),
  nl, !.

% case where
occupy(X, C) :-
  country(C),
  \+ occupied(_, C),
  assert(occupied(X, C)),
  write('occupied empty spot.'),
  nl, !.

own_north_america(X) :-
  occupied(X, canada),
  occupied(X, us).

attack(X, C) :-
  country(C),
  dif(X, X2),
  occupied(X2, C).
