/*
 * chronweb/chronweb.pl
 *
 * Copyright (C) 2013-2018 James Hogan <james@albanarts.com>
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
 * Generate an ancestry dot file from a Chron dataset.
 *
 */

opts_spec(
	[
		[
			opt(file),
			meta('FILE'),
			type(atom),
			shortflags([f]),
			longflags(['file']),
			help('read chron events from file')
		],
		[
			opt(output),
			meta('DIR'),
			type(atom),
			default('www'),
			shortflags([o]),
			longflags(['output']),
			help('chronweb output directory')
		],
		[
			opt(jobs),
			type(integer),
			default(4),
			shortflags([j]),
			longflags(['jobs']),
			help('Makefile jobs')
		],
		[
			opt(keep),
			type(boolean),
			default(false),
			shortflags([k]),
			longflags(['keep']),
			help('Keep intermediate files (e.g. *.dot, Makefile)')
		]
	]
).

:-	use_module(chron(chron)).
:-	use_module(chron(file)).
:-	use_module(library(clpfd)).
:-	use_module(chron(dot_ancestry)).
:-	use_module(html_index).
:-	use_module(html_person).

main :-
	% Load dataset
	opts_spec(OptsSpec),
	opt_arguments(OptsSpec, Opts, _PositionalArgs),
	memberchk(file(File), Opts),
	load_chron_db(File),

	% Read options
	memberchk(output(Output), Opts),
	memberchk(jobs(Jobs), Opts),
	memberchk(keep(Keep), Opts),

	% Process events
	process_db(epoch, Events),

	% Create directory
	print('Output directory ('), print(Output), print(')'), nl,
	(\+ exists_directory(Output) -> make_directory(Output) ; true),

	% The Makefile for generating SVG files
	print('Build files'), nl,
	print('  Makefile'), nl,
	write_makefile(Output),

	% The HTML files
	print('HTML files'), nl,
	print('  index.html'), nl,
	write_html_index(Output),
	print('  people'), nl,
	write_html_people_files(Output, Events),

	% The ancestry dot files
	print('DOT files'), nl,
	write_dot_ancestry_files(Output),

	% Run the Makefile
	print('SVG files'), nl,
	string_concat('make -s -j', Jobs, Cmd1),
	string_concat(Cmd1, ' -C "', Cmd2),
	string_concat(Cmd2, Output, Cmd3),
	string_concat(Cmd3, '"', Cmd),
	shell(Cmd),

	% Clean source files
	( Keep = true ->
		print('Keeping intermediates'), nl
	; Keep = false ->
		print('Cleaning intermediates'), nl,
		string_concat(Cmd, ' clean_sources', CleanCmd),
		shell(CleanCmd)).

write_makefile(Output) :-
	string_concat(Output, '/Makefile', MakefilePath),
	open(MakefilePath, write, S),
	write(S, 'DOT   = dot\n'),
	write(S, 'DOTS := $(wildcard *.dot)\n'),
	write(S, 'SVGS := $(patsubst %.dot,%.svg,$(DOTS))\n'),
	write(S, 'PNGS := $(patsubst %.dot,%.png,$(DOTS))\n'),
	write(S, 'all: $(SVGS)\n'),
	write(S, '%.svg: %.dot\n'),
	write(S, '	$(DOT) -Tsvg -o"$@" "$^"\n'),
	write(S, '%.png: %.dot\n'),
	write(S, '	$(DOT) -Tpng -o"$@" "$^"\n'),
	write(S, 'clean_sources:\n'),
	write(S, '	rm -f $(DOTS) Makefile\n'),
	close(S).

:-	main,
	halt.
