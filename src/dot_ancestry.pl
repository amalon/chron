/*
 * src/dot_ancestry.pl
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
 * Generate dot output representing ancestry data.
 *
 */

% Top level dot file generation
write_dot_ancestry(Filename) :-
	open(Filename, write, S),
	write_dot_head(S, digraph, 'ancestry'),
	write_dot_people(S, Map),
	write_dot_relationships(S, Map),
	write_dot_tail(S),
	close(S).

% Attributes for a person node
dot_person_attr(Person, [attr(color, blue)]) :-
	man(Person), !.
dot_person_attr(Person, [attr(color, pink)]) :-
	woman(Person), !.
dot_person_attr(_Person, []).

% Write people nodes
write_dot_people(S, _) :-
	nl(S),
	write_dot_comment(S, 'People'),
	person(Person),
	person_description(Person, Desc),
	dot_person_attr(Person, Attr),
	write_dot_node(S, Person, [attr(label, string(Desc))|Attr]),
	fail.
write_dot_people(_, _).

% Write edges for relationships between people
% Parent -> child
write_dot_relationships(S, _) :-
	nl(S),
	write_dot_comment(S, 'Relationships'),
	parent_child(Parent, Child, Src),
	source_description(Src, Desc),
	write_dot_edges(S, [Parent, d(Child)], [attr(label, string(Desc))]),
	fail.
% Parent -> descendent
write_dot_relationships(S, _) :-
	nl(S),
	write_dot_comment(S, 'Ancestry'),
	parent_descendent(Parent, Child, Src),
	source_description(Src, Desc),
	write_dot_edges(S, [Parent, d(Child)],
			[attr(label, string(Desc)), attr(style, dashed)]),
	fail.
% Husband -- Wife
write_dot_relationships(S, _) :-
	nl(S),
	write_dot_comment(S, 'Marriages'),
	married(Man, Woman, _),
	write_dot_edges(S, [Man, d(Woman)],
			[attr(dir,none),
			 attr(constraint,false),
			 attr(color,red)]),
	fail.
write_dot_relationships(_, _).
