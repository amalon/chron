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

/*
 * Debugging
 */

% Uncomment to enable printing of constraints as the're applied
%debug_printing.

debug_printing :- fail.
dbg_print(X) :-
	debug_printing,
	print(X),
	!.
dbg_print(_).
dbg_nl :-
	debug_printing,
	nl,
	!.
dbg_nl.

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
	describe_source(Source, Desc), !.
source_description(Source, Source).

/*
 * Events
 */

event(_) :- fail.

/*
 * Periods
 */

period(_) :- fail.

% The beginning and end of a period are events
event(begin(Period)) :-
	period(Period).
event(end(Period)) :-
	period(Period).

% Periods start before they end
events_ordered([begin(Period), end(Period)], period_order) :-
	period(Period).

% period_during(Inner, Outer, Source).
period_during(_, _, _) :- fail.
event_during(begin(Inner), Outer, Source) :-
	period_during(Inner, Outer, Source).
event_during(end(Inner), Outer, Source) :-
	period_during(Inner, Outer, Source).

/*
 * People
 */

unborn(_) :- fail.
man(_) :- fail.
woman(_) :- fail.
twins(_, _, _) :- fail.

% Person names
person_name(_, _) :- fail.
person_description(Person, Desc) :-
	person_name(Person, Desc), !.
person_description(Person, Person).

% Convenience people groups
% people_gruop(Label, People).
people_group(_, _) :- fail.
people_in_group(group(Group), People) :-
	people_group(Group, People).
people_in_group([P|Ps], [P|Ps]).
person_in_group(Person, Group) :-
	people_in_group(Group, People),
	member(Person, People).

% Any person who isn't unborn was once born
born_person(Person) :-
	person(Person),
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
	person(Person).

event_simplifier(begin(lifetime(Person)), birth(Person)) :-
	person(Person).
event_simplifier(end(lifetime(Person)), death(Person)) :-
	person(Person).

% Human gestation is approximately 40 weeks
event_separation(conception(Person), birth(Person), time(Weeks, week), pregnancy) :-
	born_person(Person),
	Weeks in 37..42.

% Relationships
parent_child(_, _, _) :- fail.

% mother needs to be present at birth
event_during(birth(Child), lifetime(Mother), Source) :-
	parent_child(Mother, Child, Source),
	woman(Mother).
% both parents need to be present at conception
event_during(conception(Child), lifetime(Parent), Source) :-
	parent_child(Parent, Child, Source).

% parent_son(Parent, Son, Source).
parent_son(_, _, _) :- fail.
man(Son) :-
	parent_son(_, Son, _).
parent_child(Parent, Son, Source) :-
	parent_son(Parent, Son, Source).
% parent_sons(Parent, Sons, Source).
parent_sons(_, _, _) :- fail.
parent_son(Parent, Son, Source) :-
	parent_sons(Parent, Sons, Source),
	member(Son, Sons).
% parent_descendent(Parent, Descendent, Source).
parent_descendent(_, _, _) :- fail.
person(Descendent) :-
	parent_descendent(_, Descendent, _).
events_ordered([birth(Parent), conception(Descendent)], Source) :-
	parent_descendent(Parent, Descendent, Source).
% parent_descendents(Parent, Descendents, Source).
parent_descendents(_, _, _) :- fail.
parent_descendent(Parent, Descendent, Source) :-
	parent_descendents(Parent, Descendents, Source),
	member(Descendent, Descendents).
% parents_child([Father, Mother], Child, Source).
parents_child(_, _, _) :- fail.
parent_child(Parent, Child, Source) :-
	parents_child([Parent, _], Child, Source).
parent_child(Parent, Child, Source) :-
	parents_child([_, Parent], Child, Source).
% parents_son([Father, Mother], Child, Source).
parents_son(_, _, _) :- fail.
parents_child([A, B], Child, Source) :-
	parents_son([A, B], Child, Source).
man(Child) :-
	parents_son([_, _], Child, _).
% parents_daughter([Father, Mother], Child, Source).
parents_daughter(_, _, _) :- fail.
parents_child([A, B], Child, Source) :-
	parents_daughter([A, B], Child, Source).
woman(Child) :-
	parents_daughter([_, _], Child, _).

% Marriage
married(_, _, _) :- fail.

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

% Generalised interval methods
% event_interval(Event1, Event2, Param, Source)
event_interval(_, _, _, _) :- fail.
% Parameter can be derived
% derived_interval(Param, Root)
derived_interval(_, _) :- fail.

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

newyears_phase(_, _, _, _) :- fail.
newunits_phase(Event, NewYear, year, Phase, Source) :-
	newyears_phase(Event, NewYear, Phase, Source).

% Generic constraints using intervals
% A person's lifetime as an interval
person_lifetime(_, _, _) :- fail.
event_interval(birth(Person), death(Person), Interval, Source) :-
	person_lifetime(Person, Interval, Source).


% age_ordering(Children, Source).
age_ordering(_, _) :- fail.
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
murdered(_, _, _) :- fail.
event_during(death(Victim), lifetime(Murderer), Source) :-
	murdered(Murderer, Victim, Source).

/*
 * C++: Global database lookup
 */

lookup_event(SimpleEvent, Event) :-
	simplify_event(Event, SimpleEvent).

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
	% U is a preferred unit
	preferred_unit(U),
	% No unit derived from U that would be a better choice (<= X)
	\+bigger_unit(X, U, _),
	% Divide by the chosen unit
	simplify_time(time(1, U), time(BT, raw)),
	N #= X / BT,
	N #\= 0,
	% Print this part of the unit
	print(N),
	print(' '),
	print(U),
	% Find remainder
	Rounded #= BT * N,
	X #= Rounded + Remainder,
	% Print it, or not if it's zero
	Remainder #= P,
	fd_dom(P, Dom),
	print_db_time_domain_noz(Dom),
	!.
print_db_time_single(X) :-
	print(X).

/* U derives from Than, and 1 U <= X */
bigger_unit(X, Than, U) :-
	preferred_unit(U),
	derived_unit(U, _, Than),
	simplify_time(time(1, U), time(Time, raw)),
	Time =< abs(X).

/* Print a time domain only if it's non-zero */
print_db_time_domain_noz(0) :-
	!.
print_db_time_domain_noz(0..0) :-
	!.
print_db_time_domain_noz(X) :-
	print(' '),
	print_db_time_domain(X).


/*
 * Event simplification and expansion
 */

% recursively expand using event_simplifier, finishing when we hit an event
% matches both derived and underived versions
expand_event(Simple, Simple) :-
	event(Simple).
expand_event(Simple, Raw) :-
	event_simplifier(Middle, Simple),
	expand_event(Middle, Raw).

% simplify an event so it's as human friendly as possible
% matches only derived and underivable versions
simplify_event(Raw, Simple) :-
	event_simplifier(Raw, Middle),
	simplify_event(Middle, Simple),
	!.
simplify_event(Raw, Simple) :-
	event_simplifier(Middle, Simple),
	simplify_event(Raw, Middle),
	!.
simplify_event(Raw, Raw) :-
	event(Raw).

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
events_time_mapping(EventTimes) :-
	% be careful that events only have a single time
	% produce a set list of just events
	setof(Event, event(Event), Events),
	% and then make it into a time map
	maplist(event_to_time_map, Events, EventTimes).

% Lookup the time of an event in an event-time mapping
%get_event_time([event(Event, Time)|_], Event, Time).
get_event_time([event(Event, time(InTime, raw))|_], SimpleEvent, time(OutTime, raw)) :-
	expand_event(SimpleEvent, Event),
	InTime #= OutTime.
get_event_time([_|T], Event, Time) :-
	get_event_time(T, Event, Time).

% Apply all constraints to the events
apply_constraints(Events) :-
	% Get a list of all raw constraints
	findall(constraint(Type, Data),
		internal_constraint(raw(Type), Data, _, _),
		Constraints),
	% And apply them all
	apply_constraints_inner(Events, Constraints).

% Apply a list of constraints to the times of events
apply_constraints_inner(_, []).
apply_constraints_inner(Events, [Constraint|Tail]) :-
	dbg_print('Applying '), dbg_print(Constraint), dbg_print(' ... '),
	apply_constraint(Events, Constraint),
	dbg_print('done'), dbg_nl,
	apply_constraints_inner(Events, Tail).

% Pre-process arguments
apply_preargs(_, [], []).
% Convert events to times
apply_preargs(Events, [event_time(Event)|Tail], [Time|RTail]) :-
	get_event_time(Events, Event, time(Time, raw)),
	apply_preargs(Events, Tail, RTail).
% Direct pass through arguments.
apply_preargs(Events, [pass(Data)|Tail], [Data|RTail]) :-
	apply_preargs(Events, Tail, RTail).

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
event_separation(_, _, _, _) :- fail.
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
newunits_phase(_, _, _, _, _) :- fail.
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
newunits_between(_, _, _, _, _) :- fail.
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
event_during(_, _, _) :- fail.
constraint(event_during, [Event, Period], Source) :-
	event_during(Event, Period, Source).
internal_constraint(events_ordered, [begin(Period), Event, end(Period)], Source,
		    constraint(event_during, Data, Higher)) :-
	Data = [Event, Period],
	internal_constraint(event_during, Data, Source, Higher),
	period(Period).

/* constraint(events_coincide, Events, Source). */
events_coincide(_, _) :- fail.
constraint(events_coincide, Events, Source) :-
	events_coincide(Events, Source).
internal_constraint(event_separation, [Event1, Event2, time(0, raw)], Source,
		    constraint(events_coincide, Data, Higher)) :-
	Data = [Event1|Tail],
	internal_constraint(events_coincide, Data, Source, Higher),
	member(Event2, Tail).

/* constraint(events_ordered, Events, Source). */
events_ordered(_, _) :- fail.
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
period_len(_, _, _) :- fail.
constraint(period_len, [Period, Time], Source) :-
	period_len(Period, Time, Source).
internal_constraint(event_separation, [begin(Period), end(Period), Time], Source,
		    constraint(period_len, Data, Higher)) :-
	Data = [Period, Time],
	internal_constraint(period_len, Data, Source, Higher),
	period(Period).

/* constraint(periods_len, [Periods, Time], Source). */
periods_len(_, _, _) :- fail.
constraint(periods_len, [Periods, Time], Source) :-
	periods_len(Periods, Time, Source).
internal_constraint(period_len, [Period, T], Source,
		    constraint(periods_len, Data, Higher)) :-
	Data = [PeriodList, T],
	internal_constraint(periods_len, Data, Source, Higher),
	member(Period, PeriodList),
	period(Period).

/* constraint(period_subdivides, [Period, Periods], Source). */
period_subdivides(_, _, _) :- fail.
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
population_bottleneck(_, _, _) :- fail.
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
	person(Person),
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
	person(Person),
	constraint_uses_inner(X, lifetime(Person)).
constraint_uses_inner(X, Person) :-
	person(Person),
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
