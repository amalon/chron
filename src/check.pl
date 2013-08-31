/*
 * src/check.pl
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
 * Perform a self-test on various functions.
 *
 */

:- include(wrapper).
:- include(test).

% ======= Tests =======

% the event "epoch" should exist for the tests to work
test(is_event(epoch),
	[msg(	'epoch found in database',
		'no epoch in database')]).

% print_db must be fully deterministic
test(is_deterministic(print_db(epoch)),
	[single]).

% process_db must generate a list of events, at least one of which has a finite
% time relative to the epoch
test(process_db(epoch, Events),
	[	single,
		% All items are events with a finite domain
		cond(	(	forall(member(X, Events),
				(	X = event(E, time(T, raw)),
					is_event(E),
					fd_dom(T, _)
				))
			),
			'events mapping looks okay',
			'events mapping invalid'),
		% At least one (non-epoch) item is finite
		cond(	(	member(event(E, time(T, raw)), Events),
				E \= epoch,
				fd_dom(T, D),
				D \= inf..sup
			),
			'finite dated event found',
			'no finite dated event found')]).

% check applying constraints twice does not make a difference
% if reapplying the constraints makes a difference then it suggests they
% weren't properly applied the first time around
test((	process_db(epoch, Events1),
	process_db(epoch, Events2),
	process_db(epoch, Events2)),
	[	single,
		cond(forall(is_event(Event),
				diff_event_time(Events1, Events2, Event)),
			'constraints fully applied',
			'constraints not fully applied')]).
diff_event_time(Events1, Events2, Event) :-
	(	% find events in event-time mappings
		(	get_event_time(Events1, Event, time(T1, raw)),
			get_event_time(Events2, Event, time(T2, raw))
		->	true
		;	ansi_format([fg(red)],
				'~w not found in event output\n', [Event]),
			fail
		),
		% extract time domains
		fd_dom(T1, Dom1),
		fd_dom(T2, Dom2),
		% check they match exactly
		(	Dom1 = Dom2
		->	true
		;	ansi_format([fg(red)],
				'domain mismatch: ~20w: (~20w) = (~20w)\n',
				[Event, Dom1, Dom2]),
			fail
		)
	).

% constraints dot output must match reference file
test(write_dot('constraints.dot'),
	[	single,
		file_match('constraints.dot', 'constraints.dot.ref')
	]).

% ancestry dot output must match reference file
test(write_dot_ancestry('ancestry.dot'),
	[	single,
		file_match('ancestry.dot', 'ancestry.dot.ref')
	]).

% timeline gnuclad output must match reference file
test(write_gnuclad_timeline(epoch, 'timeline.csv'),
	[	single,
		file_match('timeline.csv', 'timeline.csv.ref')
	]).


% ======= Other useful predicates =======

is_deterministic(X) :-
	call_cleanup(X, Det = yes),
	\+ var(Det).


% ======= Autorun and halt =======

% Run all tests automatically
:- run_tests.

% Halt on completion
:- halt.
