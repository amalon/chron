/*
 * src/gnuclad.pl
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
 * Generate gnuclad CSV output.
 *
 */

:- module('gnuclad', [
		write_gnuclad_connector/7,
		write_gnuclad_comment/2,
		write_gnuclad_node/9
	]).

:- use_module(chron(csv)).

% CSV cell write proxy for gnuclad dates
:- define_csv_cell_proxy(S, gnuclad_date(Years),
	(	number(Years)
	->	write_csv_cell_gnuclad_date(S, years(Years))
	;	write_csv_cell_gnuclad_date(S, Years)
	)).
write_csv_cell_gnuclad_date(S, years(Years)) :-
	Year is floor(Years),
	Months is (Years - Year) * 10,
	Month is 1 + truncate(Months),
	Days is (Months - (Month - 1)) * 10,
	Day is 1 + truncate(Days),
	write_csv_cell(S, concat([Year, '.', Month, '.', Day])).

% gnuclad Colours
:- define_csv_cell_proxy(S, gnuclad_colour(Colour),
	(	gnuclad_colour(Colour, Hex),
		write_csv_cell(S, Hex)
	)).
gnuclad_colour(black, '#000000').
gnuclad_colour(blue,  '#0000ff').
gnuclad_colour(pink,  '#ff7f7f').

% Comments
write_gnuclad_comment(S, Comment) :-
	write_csv_line(S, ['#', string(Comment)]).

% Nodes
write_gnuclad_node(S, Name, Colour, Parent, Start, Stop, Icon, Description, NameChanges) :-
	gnuclad_flatten_name_changes(NameChanges, FlattenedNameChanges),
	write_csv_line(S, [
		string('N'),
		string(Name),
		string(Colour),
		string(Parent),
		string(gnuclad_date(Start)),
		string(gnuclad_date(Stop)),
		string(Icon),
		string(Description)|
		FlattenedNameChanges]).

gnuclad_flatten_name_changes([], []).
gnuclad_flatten_name_changes([name_change(To, When, Description)|Rest],
			[string(To), string(When), string(Description) |
			 FlattenedRest]) :-
	gnuclad_flatten_name_changes(Rest, FlattenedRest).

% Connectors
write_gnuclad_connector(S, FromWhen, From, ToWhen, To, Thickness, Colour) :-
	write_csv_line(S, [
		string('C'),
		string(gnuclad_date(FromWhen)),
		string(From),
		string(gnuclad_date(ToWhen)),
		string(To),
		string(Thickness),
		string(Colour)]).

% Domains
write_gnuclad_domain(S, Node, Colour, Intensity) :-
	write_csv_line(S, [
		string('D'),
		string(Node),
		string(Colour),
		Intensity]).

% Images
write_gnuclad_image(S, Type, Path, X, Y) :-
	write_csv_line(S, [
		string(Type),
		string(Path),
		gnuclad_date(X),
		Y]).
