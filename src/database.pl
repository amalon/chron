/*
 * src/database.pl
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
 * Import necessary modules for use of chron for defining facts in the current
 * namespace.
 *
 */

% The core Chron modules
:-	use_module(chron(chron)).
:-	use_module(chron(calendar)).
:-	use_module(chron(dwelling)).

% Constraints are sometimes required in facts
:-	use_module(library(clpfd)).

% The definition of Chron facts are usually discontinuous
:-	style_check(-discontiguous).

% Register this module as containing database facts
chron_database_temp.
:-	strip_module(chron_database_temp, Module, _),
	chron:database(Module),
	assert(file:last_loaded_db_module(Module)).
