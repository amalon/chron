/*
 * src/precompute.pl
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
 * Handy procedure to precompute values according to mappings defined by
 * precompute_mapping/2.
 *
 */

% precompute_mapping(Computed, Original).
% Defines a mapping from originals facts to computed facts that should be
% automatically precomputed by precompute/0.
precompute_mapping(_, _) :- fail.

% Precomputes terms defined by precompute_mapping/2
precompute :-
	% For each precompute_mapping/2 rule, precompute
	forall(precompute_mapping(Computed, Original),
		precompute(Computed, Original)).

% Specific precomputation
precompute(Computed, Original) :-
	% First retract any old precomputed terms
	retractall(Computed),
	% Gather a (new) set of computed terms
	(	setof(Computed, Original, Terms)
	;	Terms = []
	),
	% And assert each one
	forall(member(Term, Terms),
		assertz(Term)).
