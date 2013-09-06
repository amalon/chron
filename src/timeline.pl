/*
 * src/timeline.pl
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
 * Timeline helpers.
 *
 */

:- module('timeline', [
		summary_time/3
	]).

% Turn a fd domain into a concrete number, which approximates the range very
% simplistically.
summary_time(time(Time, raw), SumTime, Flags) :-
	fd_dom(Time, RawTime),
	summary_time_p(RawTime, SumTime, Flags).
summary_time_p(X, X, _) :-
	integer(X).
summary_time_p(X..sup, X, _) :-
	integer(X).
summary_time_p(inf..X, X, _) :-
	integer(X).
summary_time_p(inf..sup, Default, Flags) :-
	member(default(Default), Flags).
summary_time_p(inf..sup, 0, Flags) :-
	\+member(default(_), Flags).
summary_time_p(X..Y, Min, Flags) :-
	member(min, Flags),
	integer(X),
	integer(Y),
	Min is min(X, Y).
summary_time_p(X..Y, Max, Flags) :-
	member(max, Flags),
	integer(X),
	integer(Y),
	Max is max(X, Y).
summary_time_p(X..Y, Mean, Flags) :-
	member(mean, Flags),
	integer(X),
	integer(Y),
	Mean is (X + Y) / 2.
summary_time_p(X\/_, Sum, Flags) :-
	summary_time_p(X, Sum, Flags).
