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
		write_dot_ancestry/1,
		write_dot_ancestry/2
	]).

:- use_module(chron(chron)).
:- use_module(chron(dot)).

% Top level dot file generation
write_dot_ancestry(Filename) :-
	write_dot_ancestry(Filename, any).

write_dot_ancestry(Filename, Filter) :-
	expand_filter(Filter, Filter2),
	print(Filename),nl,
	open(Filename, write, S),
	write_dot_head(S, digraph, 'ancestry'),
	write_dot_people(S, Map, Filter2),
	write_dot_relationships(S, Map, Filter2),
	write_dot_tail(S),
	close(S).

expand_filter(any, any).
expand_filter(directly_related(Person), one_of(People)) :-
	setof(Other, filter_person(Other, directly_related(Person)), People).

filter_match(A, B, X, Y) :-
	(	person_birth_name(A, X),
		person_birth_name(B, Y)
	;	person_birth_name(B, X),
		person_birth_name(A, Y)).

filter_person(Person, any) :-
	is_person(Person),
	is_birth_name(Person).
filter_person(Person, directly_related(Person)).
filter_person(Person, directly_related(Other)) :-
	(	is_parent_child(A, B, _)
	;	is_parent_adopted_child(A, B, _)
	;	is_parent_descendent(A, B, _)
	;	is_married(A, B, _)),
	filter_match(A, B, Person, Other).
filter_person(Person, one_of(People)) :-
	member(Person, People).

% Attributes for a person node
dot_person_attr(Person, [attr(color, blue)]) :-
	is_man(Person), !.
dot_person_attr(Person, [attr(color, pink)]) :-
	is_woman(Person), !.
dot_person_attr(_Person, []).

% Write people nodes
write_dot_people(S, _, Filter) :-
	nl(S),
	write_dot_comment(S, 'People'),
	filter_person(Person, Filter),
	person_description(Person, Desc),
	dot_person_attr(Person, Attr),
	write_dot_node(S, Person, [attr(label, string(Desc))|Attr]),
	fail.
write_dot_people(_, _, _).

% Write edges for relationships between people
% Parent -> child
natural_relationship([ParentName, d(ChildName)], Filter) :-
	is_parent_child(Parent, Child, _),
	person_birth_name(Parent, ParentName),
	filter_person(ParentName, Filter),
	person_birth_name(Child, ChildName),
	filter_person(ChildName, Filter).
natural_relationship_desc([ParentName, d(ChildName)], Desc) :-
	is_parent_child(Parent, Child, Src),
	person_birth_name(Parent, ParentName),
	person_birth_name(Child, ChildName),
	source_description(Src, Desc).
% Parent -> adopted child
adoptive_relationships([ParentName, d(ChildName)],
			[attr(label, string(Desc)), attr(style, dashed)], Filter) :-
	is_parent_adopted_child(Parent, Child, Src),
	person_birth_name(Parent, ParentName),
	filter_person(ParentName, Filter),
	person_birth_name(Child, ChildName),
	filter_person(ChildName, Filter),
	source_description(Src, Desc).
% Parent -> descendent
descendent_relationships([ParentName, d(ChildName)],
			[attr(label, string(Desc)), attr(style, dotted)], Filter) :-
	is_parent_descendent(Parent, Child, Src),
	person_birth_name(Parent, ParentName),
	filter_person(ParentName, Filter),
	person_birth_name(Child, ChildName),
	filter_person(ChildName, Filter),
	source_description(Src, Desc).
% Husband -- Wife
marriage_relationships([ManName, d(WomanName)],
			[attr(dir,none), attr(color,red)], Filter) :-
	is_married(Man, Woman, _),
	person_birth_name(Man, ManName),
	filter_person(ManName, Filter),
	person_birth_name(Woman, WomanName),
	filter_person(WomanName, Filter).

write_dot_relationships(S, _, Filter) :-
	nl(S),
	write_dot_comment(S, 'Natural Relationships'),
	setof(Names1, natural_relationship(Names1, Filter), Edges),
	member(Names2, Edges),
	setof(Label, natural_relationship_desc(Names2, Label), Labels),
	write_dot_edges(S, Names2, [attr(label, string(join('\n', Labels)))]),
	fail.
write_dot_relationships(S, _, Filter) :-
	nl(S),
	write_dot_comment(S, 'Adopted Relationships'),
	adoptive_relationships(Names, Attrs, Filter),
	write_dot_edges(S, Names, Attrs),
	fail.
write_dot_relationships(S, _, Filter) :-
	nl(S),
	write_dot_comment(S, 'Ancestry'),
	descendent_relationships(Names, Attrs, Filter),
	write_dot_edges(S, Names, Attrs),
	fail.
write_dot_relationships(S, _, Filter) :-
	nl(S),
	write_dot_comment(S, 'Marriages'),
	marriage_relationships(Names, Attrs, Filter),
	write_dot_head(S, subgraph, ''),
	write_dot_attrs_graph(S, [attr(rank, same)]),
	write_dot_edges(S, Names, Attrs),
	write_dot_tail(S),
	fail.
write_dot_relationships(_, _, _).
