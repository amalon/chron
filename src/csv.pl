/*
 * src/csv.pl
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
 * Generate CSV output.
 *
 */

% Single cell
write_csv_cell_proxy(S, string(Cell)) :-
	write(S, '"'),
	write_csv_cell(S, Cell),
	write(S, '"').
write_csv_cell_proxy(S, concat(ConcatList)) :-
	write_csv_cell_concat(S, ConcatList).
write_csv_cell_concat(_, []).
write_csv_cell_concat(S, [C|Cs]) :-
	write_csv_cell(S, C),
	write_csv_cell_concat(S, Cs).

write_csv_cell(S, X) :-
	% fallback to plain write
	(	write_csv_cell_proxy(S, X) -> true
	;	write(S, X)
	).

% Line of cell
write_csv_line(S, []) :-
	nl(S).
write_csv_line(S, [Cell|Tail]) :-
	write_csv_cell(S, Cell),
	write_csv_line_tail(S, Tail).

% Line of cells starting with comma
write_csv_line_tail(S, []) :-
	nl(S).
write_csv_line_tail(S, [H|T]) :-
	write(S, ','),
	write_csv_line(S, [H|T]).
