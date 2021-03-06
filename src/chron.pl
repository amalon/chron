/*
 * src/chron.pl
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
 * A library to define events, periods, geneologies, and chronological
 * constraints, allowing the time of an event relative to some other event to be
 * queried as a domain of possible values.
 *
 */

:- module('chron', [
		/* Standard Prolog interface */
		born_person/1,
		expand_event/2,
		get_event_time/3,
		internal_constraint/4,
		import_ns_predicates/1,
		is_birth_name/1,
		is_event/1,
		is_person/1,
		is_married/3,
		is_man/1,
		is_parent_adopted_child/3,
		is_parent_child/3,
		is_parent_descendent/3,
		is_person_same/3,
		is_raw_parent_descendent/3,
		person_birth_name/2,
		person_description/2,
		person_in_group/2,
		print_db/1,
		process_db/2,
		simplify_event/2,
		split_time_units/2,
		source_description/2,
		is_woman/1,
		pair_sublist/2,

		/* C++ interface */
		domain_to_ranges/3,
		get_time_domain/2,
		lookup_db_event/4,
		lookup_db_period/4,
		lookup_event/2,
		range_to_minmax/3,
		range_to_minmax/3,
		time_to_domain/2
	]).

% Call from database file to define a database module namepsace
database(Module) :-
	assertz(dbns(Module)).

% dbns(X) for all database module namespaces
:- dynamic dbns/1.
dbns(_) :- fail.

% define cases of a predicate to import that predicate from db namespaces
:- meta_predicate import_ns_predicates(0).
import_ns_predicates(Preds) :-
	strip_module(Preds, Module, Plain),
	forall(member(Pred, Plain),
		import_ns_predicate(Module, Pred)).
import_ns_predicate(Module, Pred/Arity) :-
	functor(Functor, Pred, Arity),
	(dynamic Module:Pred/Arity),
	Module:asserta(Functor :-
			(	chron:dbns(Ns),
				Ns \= Module,
				current_predicate(Ns:Pred/Arity),
				Ns:Functor
			)).

% wrap imported predicates so they can be exposed for normal use
% creates new predicates prefixed is_
wrap_ns_predicates(Preds) :-
	forall(member(Pred, Preds),
		wrap_ns_predicate(Pred)).
wrap_ns_predicate(Pred/Arity) :-
	atom_concat(is_, Pred, IsPred),
	functor(Functor, Pred, Arity),
	Functor =.. [_|FunctorList],
	IsFunctor =.. [IsPred|FunctorList],
	asserta(IsFunctor :- Functor).


:- import_ns_predicates([
		age_ordering/2,
		derived_interval/2,
		derived_unit/3,
		describe_source/2,
		event/1,
		event_during/3,
		event_interval/4,
		events_coincide/2,
		events_ordered/2,
		man/1,
		married/3,
		mature/3,
		murdered/3,
		newunits_between/5,
		newunits_phase/5,
		newyears_phase/4,
		parent_adopted_son/3,
		parent_child/3,
		parent_daughter/3,
		parent_daughters/3,
		parent_descendent/3,
		parent_descendents/3,
		parent_son/3,
		parent_sons/3,
		parents_child/3,
		parents_daughter/3,
		parents_son/3,
		parents_sons/3,
		people_group/2,
		period/1,
		period_during/3,
		period_len/3,
		period_subdivides/3,
		periods_len/3,
		person/1,
		person_lifetime/3,
		person_name/2,
		person_rename/4,
		person_same/3,
		population_bottleneck/3,
		raw_parent_descendent/3,
		twins/3,
		unborn/1,
		woman/1
	]).

% create is_ versions
:- wrap_ns_predicates([
		man/1,
		married/3,
		parent_adopted_child/3,
		parent_child/3,
		parent_descendent/3,
		person_same/3,
		raw_parent_descendent/3,
		woman/1
	]).

% We have lots of discontiguous clauses so silence all the warnings
:- style_check(-discontiguous).

% constraint library
:- use_module(library(clpfd)).
:- use_module(precompute).

% Ensure full propagation
:- set_prolog_flag(clpfd_propagation, full).

/*
 * Precomputing
 */

% Precompute facts before doing any processing
precompute :- precompute:precompute(chron).

/*
 * Debugging
 */

% true to enable printing of constraints as they're applied
debug_printing :- fail.
% true to make errors fatal
errors_fatal :- true.

dbg_print(X) :-
	(debug_printing -> print(X) ; true).
dbg_nl :-
	(debug_printing -> nl ; true).

error :-
	(errors_fatal -> (!, fail) ; true).

/*
 * General utilities
 */

% 2 elements in a list
pair_sublist([A, B], [A, B|_]).
pair_sublist([A, B], [_|T]) :-
	pair_sublist([A, B], T).

/*
 * Time
 */

% Derived time units
%derived_unit(second, 1, raw).
%derived_unit(minute, 60, second).
%derived_unit(hour, 60, minute).
%derived_unit(day, 24, hour).
raw_time_unit(day).
derived_unit(week, 7, day).
derived_unit(year, 365, day). % it's unlikely to matter that this isn't precise
derived_unit(century, 100, year).
derived_unit(millenia, 1000, year).

% Preferred units to use for printing
%preferred_unit(second).
%preferred_unit(minute).
preferred_unit(hour).
preferred_unit(day).
preferred_unit(year).

% Simplify a derived time into a raw time
derived_unit(Unit, 1, raw) :-
	raw_time_unit(Unit).

simplify_time(X, X) :-
	X = time(_, raw).
simplify_time(time(Derived, X), Raw) :-
	derived_unit(X, Multiple, Y),
	Base #= Derived * Multiple,
	simplify_time(time(Base, Y), Raw).

% Pretty printing

% describe a source
source_description(Source, Desc) :-
	% Split out flags
	preprocess_source(Source, PPSource, Flags),
	% Describe the source (excluding flags)
	once(	describe_source(PPSource, PartDesc)
	;	PartDesc = Source
	),
	% Append any flags in brackets (if applicable)
	(	Flags = []
	->	Desc = PartDesc
	;	Desc = concat([PartDesc, ' (', concat(FlagsDesc), ')']),
		describe_flags(Flags, FlagsDesc)
	).

% Preprocess a source
% preprocess_source(Source, PreProcessedSource, Flags).
% If Source is a list, separate out flags
preprocess_source([], [], []).
% Non-list sources map unaltered
preprocess_source(X, X, []) :-
	\+ X = [_|_].
preprocess_source([H|T], Out, Flags) :-
	once((	preprocess_source_aux2(H, [H|T], Out, Flags)
	;	Out = [H|Rest],
		preprocess_source(T, Rest, Flags)
	)).

preprocess_source_aux2(flags(Flags), [_|T0], T, Flags) :-
	preprocess_source(T0, T, _).

% Describe normal (non-flag) sources
describe_source([], '').
describe_source([H|T], Description) :-
	source_description(H, HDesc),
	(	T = []
	->	Description = HDesc
	;	Description = concat([HDesc, ', ', Rest]),
		describe_source(T, Rest)
	).

% Describe common generic source types
describe_source(url(Url), Url).
describe_source(comment(Comment), concat(['"', Comment, '"'])).

% Describe flags without leading comma
% Output is a list which should be wrapped in concat/1
describe_flags([H|T], Desc) :-
	(	T = []
	->	Desc = [H]
	;	Desc = [H|Rest],
		describe_flags_comma(T, Rest)
	).
% Describe flags with a leading comma
% Output is a list which should be wrapped in concat/1
describe_flags_comma([H|T], Desc) :-
	(	T = []
	->	Desc = [', ', H]
	;	Desc = [', ', H|Rest],
		describe_flags(T, Rest)
	).

/*
 * Periods
 */

% The beginning and end of a period are events
event(begin(Period)) :-
	period(Period).
event(end(Period)) :-
	period(Period).

% Periods start before they end
events_ordered([begin(Period), end(Period)], period_order) :-
	period(Period).

% period_during(Inner, Outer, Source).
event_during(begin(Inner), Outer, Source) :-
	period_during(Inner, Outer, Source).
event_during(end(Inner), Outer, Source) :-
	period_during(Inner, Outer, Source).

/*
 * People
 */

:- dynamic is_person/1.
is_person(_) :- fail.
:- dynamic is_event/1.
is_event(_) :- fail.
precompute_mapping(is_person(Person), person(Person)).
precompute_mapping(is_event(Event), event(Event)).

% Person names, based on:
%	person_name(Person, Name).
%	person_same(Person1, Person2, Source).
%	person_rename(Person, NewPerson, Event, Source).
raw_person_description(Person, Desc) :-
	person_name(Person, Desc), !.
raw_person_description(Person, PersonCap) :-
	capitalise_atom(Person, PersonCap).

person_same(_, _) :- fail.
person_alias_backward(Person, Alias) :-
	(	person_same(Person, Alias, _)
	;	person_same(Mid, Alias, _),
		person_alias_backward(Person, Mid)).
person_alias_forward(Person, Alias) :-
	(	person_same(Person, Alias, _)
	;	person_same(Person, Mid, _),
		person_alias_forward(Mid, Alias)).
person_aliases(Person, Aliases) :-
	setof(P, ( person_alias_backward(P, Person)
		 ; person_alias_forward(Person, P)), Aliases).

aliases_akas([], [L], _, _, L).
aliases_akas([H|T], [F, HDesc|TDesc], F, M, L) :-
	raw_person_description(H, HDesc),
	aliases_akas(T, TDesc, M, M, L).

person_birth_name(Person, BirthName) :-
	person_rename(Mid, Person, _, _),
	!,
	person_birth_name(Mid, BirthName).
person_birth_name(Person, Person).

is_birth_name(Person) :-
	\+person_rename(_, Person, _, _).

person_description(Person, Desc) :-
	person_rename(Person, NewName, _, _),
	!,
	person_description(NewName, NewDesc),
	raw_person_description(Person, RawDesc),
	Desc = concat([NewDesc, ' (was ', RawDesc, ')']).
person_description(Person, Desc) :-
	person_aliases(Person, Aliases),
	!,
	raw_person_description(Person, RawDesc),
	aliases_akas(Aliases, Akas, ' (aka ', ', ', ')'),
	Desc = concat([RawDesc|Akas]).
person_description(Person, Desc) :-
	raw_person_description(Person, Desc).

capitalise_atom(A, B) :-
	atom_codes(A, AC),
	capitalise(AC, BC),
	atom_codes(B, BC).
capitalise([], []).
capitalise([H1|T], [H2|T]) :-
	code_type(H2, to_upper(H1)).

% Convenience people groups
% people_gruop(Label, People).
people_in_group(group(Group), People) :-
	people_group(Group, People).
people_in_group([P|Ps], [P|Ps]).
person_in_group(Person, Group) :-
	people_in_group(Group, People),
	member(Person, People).

% Any person who isn't unborn was once born
born_person(Person) :-
	is_person(Person),
	\+unborn(Person).

event(conception(Person)) :-
	born_person(Person).

% Both men and women are people
person(Man) :-
	man(Man).
person(Woman) :-
	woman(Woman).

% A person's lifetime is a period
period(lifetime(Person)) :-
	is_person(Person).

event_simplifier(begin(lifetime(Person)), birth(Person)) :-
	is_person(Person).
event_simplifier(end(lifetime(Person)), death(Person)) :-
	is_person(Person).

% Human gestation is approximately 40 weeks
event_separation(conception(Person), birth(Person), time(Weeks, week), pregnancy) :-
	born_person(Person),
	Weeks in 37..42.

% Relationships

% mother needs to be present at birth
event_during(birth(Child), lifetime(Mother), Source) :-
	parent_child(Mother, Child, Source),
	woman(Mother).
% both parents need to be present at conception
event_during(conception(Child), lifetime(Parent), Source) :-
	parent_child(Parent, Child, Source).
% parent's must be sexually mature to have children
event(maturity(Person)) :-
	mature(Person, _, _).
event_interval(birth(Parent), maturity(Parent), Interval, Source) :-
	mature(Parent, Interval, Source).
events_ordered([maturity(Parent), conception(Child)], Source) :-
	mature(Parent, _, _),
	parent_child(Parent, Child, Source).

% adopted children born during lifetime of adopted parent
event_during(birth(Child), lifetime(Parent), Source) :-
	parent_adopted_child(Parent, Child, Source).
% parent_adopted_son(Parent, Son, Source).
man(Son) :-
	parent_adopted_son(_, Son, _).
parent_adopted_child(Parent, Son, Source) :-
	parent_adopted_son(Parent, Son, Source).

% parent_son(Parent, Son, Source).
man(Son) :-
	parent_son(_, Son, _).
parent_child(Parent, Son, Source) :-
	parent_son(Parent, Son, Source).
% parent_daughter(Parent, Daughter, Source).
woman(Daughter) :-
	parent_daughter(_, Daughter, _).
parent_child(Parent, Daughter, Source) :-
	parent_daughter(Parent, Daughter, Source).
% parent_sons(Parent, Sons, Source).
parent_son(Parent, Son, Source) :-
	parent_sons(Parent, Sons, Source),
	member(Son, Sons).
% parent_daughters(Parent, Daughters, Source).
parent_daughter(Parent, Daughter, Source) :-
	parent_daughters(Parent, Daughters, Source),
	member(Daughter, Daughters).
% parent_descendent(Parent, Descendent, Source).
person(Descendent) :-
	parent_descendent(_, Descendent, _),
	% don't duplicate personage
	\+man(Descendent),
	\+woman(Descendent).
% use when descendent already a person (to avoid recursion)
raw_parent_descendent(Parent, Descendent, Source) :-
	parent_descendent(Parent, Descendent, Source).
events_ordered([birth(Parent), conception(Descendent)], Source) :-
	raw_parent_descendent(Parent, Descendent, Source).
% parent_descendents(Parent, Descendents, Source).
parent_descendent(Parent, Descendent, Source) :-
	parent_descendents(Parent, Descendents, Source),
	member(Descendent, Descendents).
% parents_child([Father, Mother], Child, Source).
parent_child(Parent, Child, Source) :-
	parents_child([Parent, _], Child, Source).
parent_child(Parent, Child, Source) :-
	parents_child([_, Parent], Child, Source).
% parents_son([Father, Mother], Child, Source).
parents_child([A, B], Child, Source) :-
	parents_son([A, B], Child, Source).
man(Child) :-
	parents_son([_, _], Child, _).
% parents_sons([Father, Mother], Sons, Source).
parents_son(Parents, Son, Source) :-
	parents_sons(Parents, Sons, Source),
	member(Son, Sons).
% parents_daughter([Father, Mother], Child, Source).
parents_child([A, B], Child, Source) :-
	parents_daughter([A, B], Child, Source).
woman(Child) :-
	parents_daughter([_, _], Child, _).

% Marriage

% marriage is a period
period(marriage(Man, Woman)) :-
	married(Man, Woman, _),
	man(Man),
	woman(Woman).

% marriage during both people's lifetimes
period_during(marriage(Man, Woman), lifetime(Man), Source) :-
	married(Man, Woman, Source).
period_during(marriage(Man, Woman), lifetime(Woman), Source) :-
	married(Man, Woman, Source).

% Simplify a derived interval parameter
simplify_interval(Derived, Root) :-
	derived_interval(Derived, SubRoot),
	simplify_interval(SubRoot, Root),
	!.
simplify_interval(X, X).

simplified_event_interval(Event1, Event2, Interval, Source) :-
	event_interval(Event1, Event2, DerivedInterval, Source),
	simplify_interval(DerivedInterval, Interval).


% Year/Age constraints based on interval methods

% Simple precise interval type
% interval: time(Num, Unit)
% meaning:  interval = Num units
event_separation(Event1, Event2, Interval, Source) :-
	simplified_event_interval(Event1, Event2, Interval, Source),
	Interval = time(_, _).

% Rounded down age (i.e. I'm 25 until my 26th birthday)
% interval: floor(Num, Unit)
% meaning:  interval >= Num Units
%           interval < (Num + 1) Units
derived_interval(floor(Num, Unit), time(Raw, raw)) :-
	NextNum #= 1 + Num,
	simplify_time(time(Num, Unit), time(RawNum, raw)),
	simplify_time(time(NextNum, Unit), time(RawNextNum, raw)),
	Raw #>= RawNum,
	Raw #< RawNextNum.

% new units between events, e.g. there's 1 new year in the fortnight after Christmas
% interval: new_units(Num, Unit).
% meaning:  interval >= Num Units,
%           interval < (Num + 1) Units.
newunits_between(Event1, Event2, NewUnit, time(Num, Unit), Source) :-
	simplified_event_interval(Event1, Event2, new_units(Num, Unit, NewUnit), Source),

% new years between events, e.g. there's 1 new year in the fortnight after Christmas
% drived interval: new_years(Num, NewYear)
% root interval:   new_units(Num, year, NewYear)
derived_interval(new_years(Num, NewYear), new_units(Num, year, NewYear)).

newunits_phase(Event, NewYear, year, Phase, Source) :-
	newyears_phase(Event, NewYear, Phase, Source).

% Generic constraints using intervals
% A person's lifetime as an interval
event_interval(birth(Person), death(Person), Interval, Source) :-
	person_lifetime(Person, Interval, Source).


% age_ordering(Children, Source).
% births always ordered
events_ordered([birth(Older), birth(Younger)], Source) :-
	age_ordering(Children, Source),
	pair_sublist([Older, Younger], Children).
% if same mother and not twins, birth-conception also ordered
events_ordered([birth(Older), conception(Younger)], Source) :-
	age_ordering(Children, Source),
	pair_sublist([Older, Younger], Children),
	% both have the same mother
	parent_child(Mother, Older, _),
	woman(Mother),
	parent_child(Mother, Younger, _),
	% not twins
	\+ twins(Older, Younger, _).

% murdered(Murderer, Victim, Source).
event_during(death(Victim), lifetime(Murderer), Source) :-
	murdered(Murderer, Victim, Source).

/*
 * C++: Global database lookup
 */

lookup_event(SimpleEvent, Event) :-
	event_expansion(Event, SimpleEvent).

get_time_domain(time(X, raw), Domain) :-
	fd_dom(X, Domain).

/*
 * C++: Domain interface
 */
is_range(X) :-
	integer(X).
is_range(_.._).

domain_to_ranges_p(X, [X], Count, Count) :-
	is_range(X).
domain_to_ranges_p(Rest\/Range, [Range|Ranges], Count, SoFar) :-
	Increment is SoFar + 1,
	domain_to_ranges_p(Rest, Ranges, Count, Increment).
domain_to_ranges(Domain, Ranges, Count) :-
	fd_dom(Domain, Dom),
	domain_to_ranges_p(Dom, Ranges, Count, 1).

range_to_minmax(X, X, X) :-
	integer(X).
range_to_minmax(X..Y, X, Y).

/*
 * C++: Time interface
 */

time_to_domain(Time, Domain) :-
	simplify_time(Time, time(Domain, raw)).

/*
 * C++: Concrete database lookup
 */

lookup_db_event(Events, SimpleEvent, Event, Time) :-
	member(event(Event, Time), Events),
	simplify_event(Event, SimpleEvent).

lookup_db_period(Events, Period, Begin, End) :-
	period(Period),
	member(event(begin(Period), Begin), Events),
	member(event(end(Period), End), Events).


/*
 * Printing of a database
 */

% Print an entire database
print_db(Epoch) :-
	% Get the mapping of events to times
	process_db(Epoch, Events),
	% Print the times of events
	print_db_events(Events),
	% And then the times of periods
	print_db_periods(Events).

% Print the times of the events in an event-time mapping
print_db_events(Events) :-
	member(event(Event, Time), Events),
	simplify_event(Event, SimpleEvent),
	print('event\t'),
	print(SimpleEvent),
	print('\t'),
	print_db_time(Time),
	nl,
	false.
print_db_events(_).

% Print the times of beginning and ends of periods using an event-time mapping
print_db_periods(Events) :-
	period(Period),
	member(event(begin(Period), Begin), Events),
	member(event(end(Period), End), Events),
	print('period\t'),
	print(Period),
	print('\t'),
	print_db_time(Begin),
	print('\t'),
	print_db_time(End),
	nl,
	false.
print_db_periods(_).

% Print a time
print_db_time(time(X, raw)) :-
	% Extract the domain
	fd_dom(X, Dom),
	% Print the domain
	print_db_time_domain(Dom).

% Print a domain of time
print_db_time_domain(X..X) :-
	!,
	print_db_time_single(X).
print_db_time_domain(X..Y) :-
	print_db_time_single(X),
	print(' .. '),
	print_db_time_single(Y).
print_db_time_domain(X\/Y) :-
	print_db_time_domain(X),
	print(' \\/ '),
	print_db_time_domain(Y).

print_db_time_single(inf) :-
	print(inf),
	!.
print_db_time_single(sup) :-
	print(sup),
	!.
print_db_time_single(X) :-
	split_time_units(X, TU),
	print_db_time_units(TU).

print_db_time_units([tu(N,U)|R]) :-
	print(N),
	print(' '),
	print(U),
	!,
	print_db_time_units_noz(R).
print_db_time_units([N|R]) :-
	print(N),
	print_db_time_units_noz(R).

/* Print a time domain only if it's non-zero */
print_db_time_units_noz([]).
print_db_time_units_noz([X|R]) :-
	print(' '),
	print_db_time_units([X|R]).

% More general, split to array of tu(N, Unit) or raw value.
split_time_units(X..X, TU) :-
	split_time_units(X, TU),
	!.
split_time_units(X, [tu(N, U)|R]) :-
	% U is a preferred unit
	preferred_unit(U),
	% No unit derived from U that would be a better choice (<= X)
	\+bigger_unit(X, U, _),
	% Divide by the chosen unit
	simplify_time(time(1, U), time(BT, raw)),
	N #= X / BT,
	N #\= 0,
	% Find remainder
	Rounded #= BT * N,
	X #= Rounded + Remainder,
	% Print it, or not if it's zero
	Remainder #= P,
	fd_dom(P, Dom),
	split_time_units_noz(Dom, R),
	!.
split_time_units(X, [X]).

/* U derives from Than, and 1 U <= X */
bigger_unit(X, Than, U) :-
	preferred_unit(U),
	derived_unit(U, _, Than),
	simplify_time(time(1, U), time(Time, raw)),
	Time =< abs(X).

/* Append a time domain only if it's non-zero */
split_time_units_noz(0, []) :-
	!.
split_time_units_noz(0..0, []) :-
	!.
split_time_units_noz(X, Split) :-
	split_time_units(X, Split).


/*
 * Event simplification and expansion
 */

% recursively expand using event_simplifier, finishing when we hit an event
% matches both derived and underived versions
expand_event(Event, Expanded) :-
	(	is_event(Event) -> Expanded = Event
	;	event_simplifier(Expand, Event),
		expand_event(Expand, Expanded)
	).

% simplify an event so it's as human friendly as possible
% matches only derived and underivable versions
simplify_event(Event, Simple) :-
	(	event_simplifier(Event, Simpler) -> simplify_event(Simpler, Simple)
	;	Simple = Event
	).

% event expansion - works both ways
event_expansion(Expanded, Simple) :-
	(	\+ var(Simple)
	->	expand_event(Simple, Expanded)
	;	\+ var(Expanded)
	->	simplify_event(Expanded, Simple)
	;	is_event(Expanded),
		simplify_event(Expanded, Simple)
	).

/*
 * Satisfying a database
 */

% Produce a mapping of all events to their times relative to Epoch
process_db(SimpleEpoch, Events) :-
	% Events is list of event(Event, Time) mapping all events to times
	events_time_mapping(Events),
	% Epoch must be an event
	expand_event(SimpleEpoch, Epoch),
	% Epoch is at time 0
	get_event_time(Events, Epoch, time(0, raw)),
	% Apply the constraints to the events
	apply_constraints(Events).

% Events maps all events to raw times
event_to_time_map(Event, event(Event, time(_, raw))).
events_time_mapping(Events) :-
	% be careful that events only have a single time
	% produce a set list of event times
	setof(event(Event, time(_, raw)), is_event(Event), Events).

% Lookup the time of an event in an event-time mapping
%get_event_time([event(Event, Time), ...], Event, Time).
get_event_time(Events, SimpleEvent, time(Time, raw)) :-
	expand_event(SimpleEvent, Event),
	memberchk(event(Event, time(EventTime, raw)), Events),
	EventTime #= Time.

% Apply all constraints to the events
apply_constraints(Events) :-
	% Get a list of all raw constraints
	findall(constraint(Type, Data),
		internal_constraint(raw(Type), Data, _, _),
		Constraints),
	% And apply them all
	maplist(apply_constraint_wrapper(Events), Constraints).

% Apply a list of constraints to the times of events
apply_constraint_wrapper(Events, Constraint) :-
	dbg_print('Applying '), dbg_print(Constraint), dbg_print(' ... '),
	(	apply_constraint(Events, Constraint) -> dbg_print('done'), dbg_nl
	;	dbg_print('FAIL'), dbg_nl,
		print('ERROR: failed to apply constraint: '),
		print(Constraint), nl,
		error
	).

% Pre-process arguments
apply_preargs(Events, Args, ProcessedArgs) :-
	maplist(apply_prearg(Events), Args, ProcessedArgs).

% Convert events to times
apply_prearg(Events, event_time(Event), Time) :-
	(	get_event_time(Events, Event, time(Time, raw)) -> true
	;	dbg_nl,
		print('ERROR: invalid event: '), print(Event), nl,
		fail
	).
% Direct pass through arguments.
apply_prearg(_, pass(Data), Data).

% Apply a generic constraint
apply_constraint(Events, constraint(generic, [Apply, Args])) :-
	% Pre-process arguments
	apply_preargs(Events, Args, ProcessedArgs),
	% Call the Apply routine
	apply(Apply, ProcessedArgs).

/*
 * Derived constraints
 */

internal_constraint(X, Y, Z, user) :-
	constraint(X, Y, Z).

/* constraint(generic, [Apply, Args], Source). */
generic_constraint(_, _, _) :- fail.
constraint(raw(generic), [Apply, Args], Source) :-
	generic_constraint(Apply, Args, Source).

/* constraint(event_separation, [Event1, Event2, time(N, U)], Source). */
constraint(event_separation, [Event1, Event2, Time], Source) :-
	event_separation(Event1, Event2, Time, Source).
apply_event_separation(Time1, Time2, Time) :-
	dbg_nl,dbg_print(Time2 #= Time1 + Time),dbg_nl,
	Time2 #= Time1 + Time.
internal_constraint(raw(generic), [apply_event_separation,
			[event_time(Event1),
			 event_time(Event2),
			 pass(Raw)]], Source,
			constraint(event_separation, Data, Higher)) :-
	Data = [Event1, Event2, Derived],
	internal_constraint(event_separation, Data, Source, Higher),
	simplify_time(Derived, time(Raw, raw)).

/* constraint(newunits_phase, [Event, NewYear, Unit, Phase], Source). */
constraint(newunits_phase, [Event, NewYear, Unit, Phase], Source) :-
	newunits_phase(Event, NewYear, Unit, Phase, Source).
apply_newunits_phase(Time, TimeBase, Unit, Phase) :-
	simplify_time(time(1, Unit), time(TimeUnit, raw)),
	simplify_time(Phase, time(TimePhase, raw)),
	% Match phase with input
	(Time - TimeBase) mod TimeUnit #= TimePhase.
internal_constraint(raw(generic), [apply_newunits_phase,
			[event_time(Event),
			 event_time(NewYear),
			 pass(Unit),
			 pass(Phase)]], Source,
			constraint(newunits_phase, Data, Higher)) :-
	Data = [Event, NewYear, Unit, Phase],
	internal_constraint(newunits_phase, Data, Source, Higher).

/* constraint(newunits_between, [Event1, Event2, NewYear, Count], Source). */
constraint(newunits_between, [Event1, Event2, NewYear, time(Count, Unit)], Source) :-
	newunits_between(Event1, Event2, NewYear, time(Count, Unit), Source).
% there isn't a floored divide in clp(fd), so here's a makeshift one using mod
floor_div(Num, Den, Res) :-
	Num - (Num mod Den) #= Res * Den.
apply_newunits_between(Time1, Time2, TimeBase, Unit, Count) :-
	simplify_time(time(1, Unit), time(TimeUnit, raw)),
	% each end relative to near year divided into unit
	dbg_nl,
	dbg_print(A #= (Time1 - TimeBase) / TimeUnit),dbg_nl,
	dbg_print(B #= (Time2 - TimeBase) / TimeUnit),dbg_nl,
	% constrain the time base
	TimeUnitMinus1 is TimeUnit - 1,
	TimeBase in 0..TimeUnitMinus1,
	% number of time bases in between
	floor_div(Time1 - TimeBase, TimeUnit, A),
	floor_div(Time2 - TimeBase, TimeUnit, B),
	% the count is simply the difference
	dbg_print(Count #= B - A),dbg_nl,
	Count #= B - A.
internal_constraint(raw(generic), [apply_newunits_between,
			[event_time(Event1),
			 event_time(Event2),
			 event_time(NewYear),
			 pass(Unit),
			 pass(Count)]], Source,
			constraint(newunits_between, Data, Higher)) :-
	Data = [Event1, Event2, NewYear, time(Count, Unit)],
	internal_constraint(newunits_between, Data, Source, Higher).


/* constraint(event_during, [Event, Period], Source). */
constraint(event_during, [Event, Period], Source) :-
	event_during(Event, Period, Source).
internal_constraint(events_ordered, [begin(Period), Event, end(Period)], Source,
		    constraint(event_during, Data, Higher)) :-
	Data = [Event, Period],
	internal_constraint(event_during, Data, Source, Higher),
	period(Period).

/* constraint(events_coincide, Events, Source). */
constraint(events_coincide, Events, Source) :-
	events_coincide(Events, Source).
internal_constraint(event_separation, [Event1, Event2, time(0, raw)], Source,
		    constraint(events_coincide, Data, Higher)) :-
	Data = [Event1|Tail],
	internal_constraint(events_coincide, Data, Source, Higher),
	member(Event2, Tail).

/* constraint(events_ordered, Events, Source). */
constraint(events_ordered, Events, Source) :-
	events_ordered(Events, Source).
apply_event_relation(Time1, Op, Time2) :-
	apply(Op, [Time1, Time2]).
internal_constraint(raw(generic), [apply_event_relation,
			[event_time(Before),
			 pass(#=<),
			 event_time(After)]], Source,
			constraint(events_ordered, Events, Higher)) :-
	internal_constraint(events_ordered, Events, Source, Higher),
	pair_sublist([Before, After], Events).

/* constraint(period_len, [Period, Time], Source). */
simplified_period_len(Period, Time, Source) :-
	period_len(Period, DerivedTime, Source),
	simplify_interval(DerivedTime, Time).
constraint(period_len, [Period, Time], Source) :-
	simplified_period_len(Period, Time, Source).
internal_constraint(event_separation, [begin(Period), end(Period), Time], Source,
		    constraint(period_len, Data, Higher)) :-
	Data = [Period, Time],
	internal_constraint(period_len, Data, Source, Higher),
	period(Period).

/* constraint(periods_len, [Periods, Time], Source). */
simplified_periods_len(Periods, Time, Source) :-
	periods_len(Periods, DerivedTime, Source),
	simplify_interval(DerivedTime, Time).
constraint(periods_len, [Periods, Time], Source) :-
	simplified_periods_len(Periods, Time, Source).
internal_constraint(period_len, [Period, T], Source,
		    constraint(periods_len, Data, Higher)) :-
	Data = [PeriodList, T],
	internal_constraint(periods_len, Data, Source, Higher),
	member(Period, PeriodList),
	period(Period).

/* constraint(period_subdivides, [Period, Periods], Source). */
constraint(period_subdivides, [Period, Periods], Source) :-
	period_subdivides(Period, Periods, Source).
internal_constraint(events_coincide, [begin(Period), begin(First)], Source,
		    constraint(period_subdivides, Data, Higher)) :-
	Data = [Period, [First|_]],
	internal_constraint(period_subdivides, Data, Source, Higher),
	period(Period),
	period(First).
internal_constraint(events_coincide, [end(Period), end(Last)], Source,
		    constraint(period_subdivides, Data, Higher)) :-
	Data = [Period, Periods],
	internal_constraint(period_subdivides, Data, Source, Higher),
	last(Periods, Last),
	period(Period),
	period(Last).
internal_constraint(events_coincide, [end(A), begin(B)], Source,
		    constraint(period_subdivides, Data, Higher)) :-
	Data = [_, Periods],
	internal_constraint(period_subdivides, Data, Source, Higher),
	pair_sublist([A, B], Periods),
	period(A),
	period(B).

% constraint(population_bottleneck, [Bottleneck, Survivors], Source).
constraint(population_bottleneck, [Bottleneck, Survivors], Source) :-
	population_bottleneck(Bottleneck, Group, Source),
	people_in_group(Group, Survivors).
apply_population_bottleneck(Birth, Death, Bottleneck) :-
	% Anybody born before bottleneck didn't live beyond bottleneck
	(Birth #< Bottleneck) #==> (Death #=< Bottleneck).
internal_constraint(raw(generic), [apply_population_bottleneck,
			[event_time(birth(Person)),
			 event_time(death(Person)),
			 event_time(Bottleneck)]], Source,
			constraint(population_bottleneck, Data, Higher)) :-
	Data = [Bottleneck, Survivors],
	internal_constraint(population_bottleneck, Data, Source, Higher),
	% applies to all people who aren't survivors
	is_person(Person),
	\+member(Person, Survivors).

/*
 * Constraint searching.
 */

% Make the results unique
constraint_uses(C, I) :-
	setof(Constraint, constraint_uses_inner(Constraint, I), Cs),
	member(C, Cs).

% Consider subparts of higher level types
constraint_uses_inner(X, Period) :-
	period(Period),
	constraint_uses_inner(X, begin(Period)).
constraint_uses_inner(X, Period) :-
	period(Period),
	constraint_uses_inner(X, end(Period)).
constraint_uses_inner(X, Person) :-
	is_person(Person),
	constraint_uses_inner(X, lifetime(Person)).
constraint_uses_inner(X, Person) :-
	is_person(Person),
	constraint_uses_inner(X, conception(Person)).

% An internal constraint uses Item.
constraint_uses_inner(constraint(TType, TData, Source), Item) :-
	internal_constraint(Type, Data, Source, Next),
	contains_reference_to(Data, Item),
	% Match against the top level constraint.
	constraint_highest(constraint(Type, Data, Next), constraint(TType, TData)).

% Get highest level internal constraint
constraint_highest(constraint(T, D, user), constraint(T, D)) :-
	internal_constraint(T, D, _, user). % not strictly necessary for foward operation
constraint_highest(constraint(T, D, Next), Highest) :-
	internal_constraint(T, D, _, Next), % not strictly necessary for foward operation
	constraint_highest(Next, Highest).

% Search a compound for an item
contains_reference_to(Item, Event) :-
	expand_event(Item, Common),
	expand_event(Event, Common).
contains_reference_to(event_time(Item), Event) :-
	contains_reference_to(Item, Event).
contains_reference_to(Item, Event) :-
	period(Item),
	Item = Event.
contains_reference_to([Compound|_], Item) :-
	contains_reference_to(Compound, Item).
contains_reference_to([_|Tail], Item) :-
	contains_reference_to(Tail, Item).
