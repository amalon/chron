/*
 * src/gnuclad_timeline.pl
 *
 * Copyright (C) 2013 James Hogan <james@albanarts.com>
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
 * Generate gnuclad CSV data representing chron timeline.
 *
 */

% Top level gnuclad file generation
write_gnuclad_timeline(Epoch, Filename) :-
	process_db(Epoch, Events),
	open(Filename, write, S),
	write_gnuclad_people(S, Events),
	close(S).

% Person colour
gnuclad_person_colour(Person, gnuclad_colour(blue)) :-
	man(Person), !.
gnuclad_person_colour(Person, gnuclad_colour(pink)) :-
	woman(Person), !.
gnuclad_person_colour(_Person, gnuclad_colour(black)).

% Person's primary parent
gnuclad_person_parent1(Person, Parent1) :-
	once((	% Father
		parent_child(Parent1, Person, _),
		man(Parent1)
	;	% Other parent
		parent_child(Parent1, Person, _),
		\+man(Parent1)
	;	% Male ascendent
		raw_parent_descendent(Parent1, Person, _),
		man(Parent1)
	;	% Other ascendent
		raw_parent_descendent(Parent1, Person, _),
		\+woman(Parent1)
	;	% Female ascendent
		raw_parent_descendent(Parent1, Person, _)
	;	% Otherwise
		Parent1 = ''
	)).

% Person's secondary parent
gnuclad_person_parent2(Person, Parent1, Parent2) :-
	parent_child(Parent2, Person, _),
	\+ Parent1 = Parent2.

% Write people nodes
write_gnuclad_people(S, Events) :-
	nl(S),
	write_gnuclad_comment(S, '------'),
	write_gnuclad_comment(S, 'People'),
	write_gnuclad_comment(S, '------'),
	forall(is_person(Person),
		write_gnuclad_person(S, Events, Person)).

write_gnuclad_person(S, Events, Person) :-
	gnuclad_person_colour(Person, Colour),
	gnuclad_person_parent1(Person, Parent),
	member(event(begin(lifetime(Person)), BirthTime), Events),
	member(event(end(lifetime(Person)), DeathTime), Events),
	summary_time(BirthTime, BirthSummary1, [min]),
	summary_time(DeathTime, DeathSummary1, [max]),
	Scale is 365.25*100,
	Epoch is 0,
	BirthSummary is round((BirthSummary1-Epoch) * 100 / Scale) / 100,
	DeathSummary is round((DeathSummary1-Epoch) * 100 / Scale) / 100,

	BirthTime = time(RawBirthTime, raw),
	DeathTime = time(RawDeathTime, raw),
	fd_dom(RawBirthTime, RawBirthTimeDom),
	fd_dom(RawDeathTime, RawDeathTimeDom),
	write_gnuclad_comment(S, concat([Person, ':\t', RawBirthTimeDom, ' -- ', RawDeathTimeDom])),
	write_gnuclad_node(S, Person, Colour, Parent, BirthSummary, DeathSummary, '', '', []),

	% secondary parent line (only if another parent node known)
	(	gnuclad_person_parent2(Person, Parent, Parent2)
	->	gnuclad_person_colour(Parent2, Colour2),
		write_gnuclad_connector(S, BirthSummary, Parent2, BirthSummary, Person, 2, Colour2)
	;	true
	).
