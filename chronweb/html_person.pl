/*
 * src/html_person.pl
 *
 * Copyright (C) 2018 James Hogan <james@albanarts.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details
 * (in the file called COPYING).
 *
 *
 * Generate HTML output about people.
 *
 */

:- module('html_person', [
		write_html_people_files/2
	]).

:- use_module(chron(chron)).
:- use_module(library(clpfd)).

% Top level html file generation
write_html_people_files(Dir, Events) :-
	write_html_css_files(Dir),
	is_person(Person),
	is_birth_name(Person),
	string_concat(Dir, '/', S1),
	string_concat(S1, Person, S2),
	string_concat(S2, '.html', S3),
	write_html_person(S3, Person, Events),
	fail.
write_html_people_files(_Dir, _Events).

write_html_css_files(Dir) :-
	string_concat(Dir, '/styles.css', CssFile),
	open(CssFile, write, S),
	write(S,
'a.person.man { background-color: lightblue; }\n\
a.person.woman { background-color: lightpink; }\n\
a.person.unknown { background-color: lightgrey; }\n\
'),
	close(S).

write_html_person(Filename, Person, Events) :-
	open(Filename, write, S),
	person_description(Person, Desc),
	write(S, '<html>'), nl(S),
	write(S, '<head>'), nl(S),
	write(S, '<link rel="stylesheet" href="styles.css">'), nl(S),
	write(S, '<title>'), write_html_val(S, Desc), write(S, '</title>'), nl(S),
	write(S, '</head>'), nl(S),
	write(S, '<body>'), nl(S),
	write(S, '<body>'), nl(S),
	write(S, '<h1>'), write_html_val(S, Desc), write(S, '</h1>'), nl(S),
	write(S, '<h2>Relationships</h2>'), nl(S),
	write(S, '<object data="'), write(S, Person), write(S, '.svg" type="image/svg+xml">'), nl(S),
	write(S, '<img src="'), write(S, Person), write(S, '.png" />'), nl(S),
	write(S, '</object>'), nl(S),
	write_html_person_spouses(S, Person),
	write_html_person_parents(S, Person),
	write_html_person_adopted_parents(S, Person),
	write_html_person_children(S, Person),
	write_html_person_adopted_children(S, Person),
	write_html_person_events(S, Person, Events),
	write(S, '</body>'), nl(S),
	write(S, '</html>'), nl(S),
	close(S).

relation_source_relation(RelationSources, Relation) :-
	member(p(Relation, _), RelationSources).

% Marriages
person_spouse(Person, Spouse, Source) :-
	is_married(A, B, Source),
	( (person_birth_name(B, Person), person_birth_name(A, Spouse))
	; (person_birth_name(A, Person), person_birth_name(B, Spouse))).

write_html_person_spouses(S, Person) :-
	setof(p(P1, S1), person_spouse(Person, P1, S1), SpouseSources),
	setof(P2, relation_source_relation(SpouseSources, P2), Spouses),
	write(S, '<h3>Marriages</h3>'), nl(S),
	member(Spouse, Spouses),
	setof(S2, member(p(Spouse, S2), SpouseSources), Sources),
	write_html_person_link(S, Spouse, Sources),
	fail.
write_html_person_spouses(_S, _Person).

% Natural parents
person_parent(Person, Parent, Source) :-
	is_parent_child(A, B, Source),
	person_birth_name(B, Person),
	person_birth_name(A, Parent).

write_html_person_parents(S, Person) :-
	setof(p(P1, S1), person_parent(Person, P1, S1), ParentSources),
	setof(P2, relation_source_relation(ParentSources, P2), Parents),
	write(S, '<h3>Parents</h3>'), nl(S),
	member(Parent, Parents),
	setof(S2, member(p(Parent, S2), ParentSources), Sources),
	write_html_person_link(S, Parent, Sources),
	fail.
write_html_person_parents(_S, _Person).

% Adoptive parents
person_adopted_parent(Person, Parent, Source) :-
	is_parent_adopted_child(A, B, Source),
	person_birth_name(B, Person),
	person_birth_name(A, Parent).

write_html_person_adopted_parents(S, Person) :-
	setof(p(P1, S1), person_adopted_parent(Person, P1, S1), ParentSources),
	setof(P2, relation_source_relation(ParentSources, P2), Parents),
	write(S, '<h3>Adoptive Parents</h3>'), nl(S),
	member(Parent, Parents),
	setof(S2, member(p(Parent, S2), ParentSources), Sources),
	write_html_person_link(S, Parent, Sources),
	fail.
write_html_person_adopted_parents(_S, _Person).

% Natural children
person_child(Person, Child, Source) :-
	is_parent_child(A, B, Source),
	person_birth_name(A, Person),
	person_birth_name(B, Child).

write_html_person_children(S, Person) :-
	setof(p(C1, S1), person_child(Person, C1, S1), ChildSources),
	setof(C2, relation_source_relation(ChildSources, C2), Children),
	write(S, '<h3>Children</h3>'), nl(S),
	member(Child, Children),
	setof(S2, member(p(Child, S2), ChildSources), Sources),
	write_html_person_link(S, Child, Sources),
	fail.
write_html_person_children(_S, _Person).

% Adopted children
person_adopted_child(Person, Child, Source) :-
	is_parent_adopted_child(A, B, Source),
	person_birth_name(A, Person),
	person_birth_name(B, Child).

write_html_person_adopted_children(S, Person) :-
	setof(p(C1, S1), person_adopted_child(Person, C1, S1), ChildSources),
	setof(C2, relation_source_relation(ChildSources, C2), Children),
	write(S, '<h3>Adoptive Children</h3>'), nl(S),
	member(Child, Children),
	setof(S2, member(p(Child, S2), ChildSources), Sources),
	write_html_person_link(S, Child, Sources),
	fail.
write_html_person_adopted_children(_S, _Person).

% Events
write_html_person_events(S, Person, Events) :-
	write(S, '<h3>Events</h3>'), nl(S),
	write_html_event(S, begin(lifetime(Person)), 'Birth', Events),
	write_html_event(S, end(lifetime(Person)), 'Death', Events),
	fail.
write_html_person_events(_S, _Person, _Events).

write_html_event(S, Event, ShortName, Events) :-
	member(event(Event, time(T, raw)), Events),
	fd_dom(T, D),
	write(S, '<div class="event">'),
	write(S, ShortName), write(S, ' '), write_html_time(S, D),
	write(S, '</div>'), nl(S).

write_html_time(S, X..X) :-
	!,
	write_html_time_single(S, X).
write_html_time(S, X..Y) :-
	write_html_time_single(S, X),
	write(S, ' .. '),
	write_html_time_single(S, Y).
write_html_time(S, X\/Y) :-
	write_html_time(S, X),
	write(S, ' \\/ '),
	write_html_time(S, Y).

write_html_time_single(S, inf) :-
	write(S, inf),
	!.
write_html_time_single(S, sup) :-
	write(S, sup),
	!.
write_html_time_single(S, X) :-
	split_time_units(X, TU),
	write_html_time_units(S, TU).

write_html_time_units(S, [tu(N,U)|R]) :-
	write(S, N),
	write(S, ' '),
	write(S, U),
	!,
	write_html_time_units_noz(S, R).
write_html_time_units(S, [N|R]) :-
	write(S, N),
	write_html_time_units_noz(S, R).

write_html_time_units_noz(_S, []).
write_html_time_units_noz(S, [X|R]) :-
	write(S, ' '),
	write_html_time_units(S, [X|R]).

person_gender_class(Person, 'woman') :-
	is_woman(Person), !.
person_gender_class(Person, 'man') :-
	is_man(Person), !.
person_gender_class(_Person, 'unknown').

write_html_person_link(S, Person, Sources) :-
	person_gender_class(Person, Gender),
	person_description(Person, Desc),
	print(S, '<div>'), nl(S),
	print(S, '<a class="person '), print(S, Gender), print(S, '" href="'), print(S, Person), print(S, '.html">'),
	write_html_val(S, Desc),
	print(S, '</a>'), nl(S),
	maplist(source_description, Sources, SourceDescriptions),
	write_html_val(S, join(', ', SourceDescriptions)),
	print(S, '<div>'), nl(S).


% Write a single value
write_html_val(S, string(X1)) :-
	write(S, '"'),
	X1=X2,
	write_html_val(S, X2),
	write(S, '"'), !.
write_html_val(_, concat([])) :- !.
write_html_val(S, concat([A|R])) :-
	write_html_val(S, A),
	write_html_val(S, concat(R)), !.
write_html_val(_, join(_, [])) :- !.
write_html_val(S, join(_, [A])) :-
	write_html_val(S, A),
	!.
write_html_val(S, join(Sep, [A|R])) :-
	write_html_val(S, A),
	write_html_val(S, Sep),
	write_html_val(S, join(Sep, R)), !.
write_html_val(S, X) :-
	write(S, X).
