/*
 * src/dot_chron.pl
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
 * Generate dot output representing chronological data.
 *
 */

write_dot(Filename) :-
	open(Filename, write, S),
	write_dot_head(S, digraph, 'chronology'),
	write_dot_events(S, Map),
	nl(S),
	write_dot_constraints(S, Map),
	write_dot_tail(S),
	close(S).

event_dot(E) :-
	is_event(E),
	% don't show conceptions
	\+E=conception(_).

% Writing events
write_dot_events(S, Map) :-
	write_dot_comment(S, 'Events'),
	findall(E, event_dot(E), Events),
	write_dot_events_raw(S, Events, Map, 0).

write_dot_events_raw(_, [], [], _).
write_dot_events_raw(S, [E|Rest], [[E|Name]|Map], N) :-
	event_dot_name(N, Name),
	write_dot_event(S, E, Name),
	NN is N + 1,
	write_dot_events_raw(S, Rest, Map, NN).

event_dot_name(Num, concat([ev,Num])).

write_dot_event(S, E, Name) :-
	simplify_event(E, Simple),
	write_dot_node(S, Name, [attr(label, string(Simple))]).

lookup_dot_event_name(Event, Map, d(Name)) :-
	expand_event(Event, RawEvent),
	member([RawEvent|Name], Map), !.
/*
lookup_dot_event_name(Event, Map, _) :-
	print('Cannot find '), write(S, Event), write(S, ' in event map'), nl,
	member(X, Map),
	write(S, X), nl,
	fail.
*/

% makes them directional
lookup_dot_event_names([], _, []).
lookup_dot_event_names([Event|Events], Map, [Name|Names]) :-
	expand_event(Event, RawEvent),
	lookup_dot_event_name(RawEvent, Map, Name),
	lookup_dot_event_names(Events, Map, Names).


% Writing constraints
write_dot_constraints(S, Map) :-
	write_dot_comment(S, 'Event separation constraints'),
	internal_constraint(event_separation, [E1,E2|_], _, _),
	lookup_dot_event_name(E1, Map, E1N),
	lookup_dot_event_name(E2, Map, E2N),
	write_dot_edges(S, [E1N, E2N], []),
	fail.
write_dot_constraints(S, Map) :-
	write_dot_comment(S, 'Events ordered constraints'),
	internal_constraint(events_ordered, Events, _, _),
	lookup_dot_event_names(Events, Map, Names),
	write_dot_edges(S, Names, [attr(color,grey)]),
	fail.
write_dot_constraints(_, _).
