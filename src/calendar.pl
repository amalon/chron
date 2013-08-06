/*
 * Calendar handling library
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
 * Library for handling calendar facts and generating constraints out of them.
 *
 */

calendar_interval(_, _, _) :- fail.
event_date(_, _, _) :- fail.

derived_interval(calendar_interval(D1, D2), Interval) :-
	calendar_interval(D1, D2, Interval).

% avoid redundancy in constraints, so only generate constraints between dates
% without other events in between.
calendar_event_between(D1, D2) :-
	event_date(_, D, _),
	calendar_ordered(D1, D),
	calendar_ordered(D, D2).

event_interval(E1, E2, calendar_interval(D1, D2), [S1, S2]) :-
	event_date(E1, D1, S1),
	event_date(E2, D2, S2),
	calendar_ordered(D1, D2),
	\+E1=E2,
	% no event in between
	\+calendar_event_between(D1, D2).

events_coincide([E1, E2], [S1, S2]) :-
	event_date(E1, D, S1),
	event_date(E2, D, S2),
	\+E1=E2.
