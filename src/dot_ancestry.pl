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

:- module('dot_ancestry', [
		write_dot_ancestry/1
	]).

:- use_module(chron(chron)).
:- use_module(chron(dot)).

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
	is_man(Person), !.
dot_person_attr(Person, [attr(color, pink)]) :-
	is_woman(Person), !.
dot_person_attr(_Person, []).

% Write people nodes
write_dot_people(S, _) :-
	nl(S),
	write_dot_comment(S, 'People'),
	is_person(Person),
	is_birth_name(Person),
	person_description(Person, Desc),
	dot_person_attr(Person, Attr),
	write_dot_node(S, Person, [attr(label, string(Desc))|Attr]),
	fail.
write_dot_people(_, _).

% Write edges for relationships between people
% Parent -> child
natural_relationship([ParentName, d(ChildName)]) :-
	is_parent_child(Parent, Child, _),
	person_birth_name(Parent, ParentName),
	person_birth_name(Child, ChildName).
natural_relationship_desc([ParentName, d(ChildName)], Desc) :-
	is_parent_child(Parent, Child, Src),
	person_birth_name(Parent, ParentName),
	person_birth_name(Child, ChildName),
	source_description(Src, Desc).
% Parent -> adopted child
adoptive_relationships([ParentName, d(ChildName)],
			[attr(label, string(Desc)), attr(style, dashed)]) :-
	is_parent_adopted_child(Parent, Child, Src),
	person_birth_name(Parent, ParentName),
	person_birth_name(Child, ChildName),
	source_description(Src, Desc).
% Parent -> descendent
descendent_relationships([ParentName, d(ChildName)],
			[attr(label, string(Desc)), attr(style, dotted)]) :-
	is_parent_descendent(Parent, Child, Src),
	person_birth_name(Parent, ParentName),
	person_birth_name(Child, ChildName),
	source_description(Src, Desc).
% Husband -- Wife
marriage_relationships([ManName, d(WomanName)],
			[attr(dir,none), attr(color,red)]) :-
	is_married(Man, Woman, _),
	person_birth_name(Man, ManName),
	person_birth_name(Woman, WomanName).

write_dot_relationships(S, _) :-
	nl(S),
	write_dot_comment(S, 'Natural Relationships'),
	setof(Names1, natural_relationship(Names1), Edges),
	member(Names2, Edges),
	setof(Label, natural_relationship_desc(Names2, Label), Labels),
	write_dot_edges(S, Names2, [attr(label, string(join('\n', Labels)))]),
	fail.
write_dot_relationships(S, _) :-
	nl(S),
	write_dot_comment(S, 'Adopted Relationships'),
	adoptive_relationships(Names, Attrs),
	write_dot_edges(S, Names, Attrs),
	fail.
write_dot_relationships(S, _) :-
	nl(S),
	write_dot_comment(S, 'Ancestry'),
	descendent_relationships(Names, Attrs),
	write_dot_edges(S, Names, Attrs),
	fail.
write_dot_relationships(S, _) :-
	nl(S),
	write_dot_comment(S, 'Marriages'),
	marriage_relationships(Names, Attrs),
	write_dot_head(S, subgraph, ''),
	write_dot_attrs_graph(S, [attr(rank, same)]),
	write_dot_edges(S, Names, Attrs),
	write_dot_tail(S),
	fail.
write_dot_relationships(_, _).
