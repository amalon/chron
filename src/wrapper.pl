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

opts_spec(
	[	[	opt(file),
			meta('FILE'),
			type(atom),
			shortflags([f]),
			longflags(['file']),
			help('read chron events from file')]
	]
).

:-	use_module(chron(chron)).
:-	use_module(chron(file)).
:-	use_module(library(clpfd)).

% Read data file from arguments
:-
	% Get the file argument
	opts_spec(OptsSpec),
	opt_arguments(OptsSpec, Opts, _PositionalArgs),
	memberchk(file(File), Opts),
	load_chron_db(File).
