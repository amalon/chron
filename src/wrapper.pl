/*
 * src/wrapper.pl
 *
 * Copyright (C) 2012-2013 James Hogan <james@albanarts.com>
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
 * Test program.
 *
 */

% Preliminary setup
:- include(prechron).

% include your data file here
%:- include('../../bible/bible').

% All the chron clauses
:- include(calendar).
:- include(chron).
:- include(precompute).

% Auxilliary clauses (not part of core)
:- include(dwelling).

% Dot output
:- include(dot).
:- include(dot_ancestry).
:- include(dot_chron).

% Gnuclad output
:- include(csv).
:- include(gnuclad).
:- include(timeline).
:- include(gnuclad_timeline).

% Precompute facts before doing any processing
:- precompute.
