/*
 * src/dot.pl
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
 * Generate dot output.
 *
 */

% general dot output
write_dot_head(S, Type, Name) :-
	write(S, Type), write(S, ' '), write(S, Name), write(S, ' {'), nl(S).
write_dot_tail(S) :-
	write(S, '}'), nl(S).

% Write a node
write_dot_node(S, Name, Attrs) :-
	write(S, '\t'),
	write_dot_val(S, Name),
	write_dot_attrs(S, Attrs),
	write(S, ';'), nl(S).

% Write an edge
write_dot_edges(S, Names, Attrs) :-
	write(S, '\t'),
	write_dot_edges_raw(S, Names),
	write_dot_attrs(S, Attrs),
	write(S, ';'), nl(S).

% Write a raw set of edges
write_dot_edges_raw(S, [First|Rest]) :-
	write_dot_val(S, First),
	write_dot_edges_raw_p(S, Rest).
write_dot_edges_raw_p(S, [Next|Rest]) :-
	write_dot_edge_raw(S, Next),
	write_dot_edges_raw_p(S, Rest).
write_dot_edges_raw_p(_, []).

% Write a single edge and node
write_dot_edge_raw(S, d(Target)) :-
	write(S, ' -> '),
	write_dot_val(S, Target), !.
write_dot_edge_raw(S, Target) :-
	write(S, ' -- '),
	write_dot_val(S, Target).

% Write a set of attriutes enclosed in square brackets
write_dot_attrs(_, []) :-
	!.
write_dot_attrs(S, Attrs) :-
	write(S, ' ['),
	write_dot_attrs_raw(S, Attrs),
	write(S, ']').

% Write a set of attributes
write_dot_attrs_raw(_, []).
write_dot_attrs_raw(S, [Attr]) :-
	write_dot_attr(S, Attr), !.
write_dot_attrs_raw(S, [Attr|Tail]) :-
	write_dot_attr(S, Attr),
	write(S, ' '),
	write_dot_attrs_raw(S, Tail).

% Write a single attribute
write_dot_attr(S, attr(Attr, Val)) :-
	write(S, Attr), write(S, '='), write_dot_val(S, Val).

% Write a single value
write_dot_val(S, d(X)) :-
	write_dot_val(S, X), !.
write_dot_val(S, string(X)) :-
	write(S, '"'), write_dot_val(S, X), write(S, '"'), !.
write_dot_val(_, concat([])) :- !.
write_dot_val(S, concat([A|R])) :-
	write_dot_val(S, A),
	write_dot_val(S, concat(R)), !.
write_dot_val(S, X) :-
	write(S, X).

% Write a comment
write_dot_comment(S, X) :-
	write(S, '\t// '), write(S, X), nl(S).
