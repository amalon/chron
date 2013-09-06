/*
 * Dwelling places library
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
 * Library for handling dwelling paces and generating constraints out of them.
 *
 */

:- module('dwelling', []).

:- include(chron(database)).

:- import_ns_predicates([
		dwelt/4,
		people_dwelt/4,
		moved/6,
		people_moved/6
	]).

% dwelt(Person, Place, Number, Source).
period(dwell(Person, Place, Number)) :-
	dwelt(Person, Place, Number, _).
period_during(dwell(Person, Place, Number), lifetime(Person), Source) :-
	dwelt(Person, Place, Number, Source).

% people_dwelt(Group, Place, Number, Source).
dwelt(Person, Place, Number, Source) :-
	people_dwelt(Group, Place, Number, Source),
	person_in_group(Person, Group).

% moved(Person, From, FromNum, To, ToNum, Source).
% implies they dwelt at To
event(move(Person, From, FromNum, To, ToNum)) :-
	moved(Person, From, FromNum, To, ToNum, _).
dwelt(Person, To, ToNum, Source) :-
	moved(Person, _, _, To, ToNum, Source).
events_coincide([end(dwell(Person, From, FromNum)),
		move(Person, From, FromNum, To, ToNum),
		begin(dwell(Person, To, ToNum))], Source) :-
	moved(Person, From, FromNum, To, ToNum, Source).

% people_moved(Group, From, FromNum, To, ToNum, Source).
event(people_move(Group, From, FromNum, To, ToNum)) :-
	people_moved(Group, From, FromNum, To, ToNum, _).
moved(Person, From, FromNum, To, ToNum, Source) :-
	people_moved(Group, From, FromNum, To, ToNum, Source),
	person_in_group(Person, Group).
events_coincide([move(Person, From, FromNum, To, ToNum),
		people_move(Group, From, FromNum, To, ToNum)], Source) :-
	people_moved(Group, From, FromNum, To, ToNum, Source),
	person_in_group(Person, Group).
