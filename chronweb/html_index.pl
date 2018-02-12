/*
 * src/html_index.pl
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
 * Generate index.html landing page.
 *
 */

:- module('html_index', [
		write_html_index/1
	]).

:- use_module(chron(chron)).

% Top level html file generation
write_html_index(Dir) :-
	string_concat(Dir, '/index.html', Filename),
	open(Filename, write, S),
	write(S, '<html>'), nl(S),
	write(S, '<head>'), nl(S),
	write(S, '<link rel="stylesheet" href="styles.css">'), nl(S),
	write(S, '<title>Chronweb</title>'), nl(S),
	write(S, '</head>'), nl(S),
	write(S, '<body>'), nl(S),
	write(S, '<body>'), nl(S),
	write(S, '<h1>Chronweb</h1>'), nl(S),
	write(S, '<h2>People</h2>'), nl(S),
	write_html_people(S),
	write(S, '</body>'), nl(S),
	write(S, '</html>'), nl(S),
	close(S).

write_html_people(S) :-
	is_person(Person),
	is_birth_name(Person),
	write_html_person_link(S, Person),
	fail.
write_html_people(S).

person_gender_class(Person, 'woman') :-
	is_woman(Person), !.
person_gender_class(Person, 'man') :-
	is_man(Person), !.
person_gender_class(_Person, 'unknown').

write_html_person_link(S, Person) :-
	person_gender_class(Person, Gender),
	person_description(Person, Desc),
	print(S, '<a class="person '), print(S, Gender), print(S, '" href="'), print(S, Person), print(S, '.html">'),
	write_html_val(S, Desc),
	print(S, '</a>'), nl(S).


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
