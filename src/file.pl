/*
 * src/file.pl
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
 * Library for loading Chron database files.
 *
 */

:- module('file', [
		load_chron_db/1,
		load_chron_db/2
	]).

:-	use_module(chron(chron)).

% Load a chron database file and fail if no chron modules found
load_chron_db(File) :-
	load_chron_db(File, Modules),
	(	Modules \= []
	->	true
	;	format('No chron modules found in ~w\n', File),
		fail).

% Load a chron database file and return list of chron modules loaded in Modules
load_chron_db(File, Modules) :-
	% Clear list of modules loaded
	retractall(last_loaded_db_module(_)),
	% Load the database
	use_module(File),
	% Check some modules were returned
	findall(Module, last_loaded_db_module(Module), Modules),
	% Precompute facts before doing any processing
	chron:precompute.
