/*
 * src/test.pl
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
 * Test harness.
 *
 */

:- use_module(library(ansi_term)).

% printing pass and fail messages

result_colour(pass, green).
result_colour(fail, red).

result_print(pass) :-
	result_colour(pass, Colour),
	ansi_format([bold, fg(Colour)], 'PASS', []).
result_print(fail) :-
	result_colour(fail, Colour),
	ansi_format([bold, fg(Colour)], 'FAIL', []).
result_print(result(Result, Msg)) :-
	result_print(Result),
	result_colour(Result, Colour),
	ansi_format([fg(Colour)], ': ~w', [Msg]).
result_print(result(Result, Msg, Data)) :-
	result_print(Result),
	result_colour(Result, Colour),
	ansi_format([fg(Colour)], ': ~w: ', [Msg]),
	result_print_fail(Data).

% results(N)
% expect a certain number of solutions to be found
result_check(results(ExpectedResults), Results, Result) :-
	length(Results, NResults),
	(	NResults = ExpectedResults
	->	Result = result(pass, 'Right number of results', results(NResults, ExpectedResults))
	;	Result = result(fail, 'Wrong number of results', results(NResults, ExpectedResults))
	).
result_print_fail(results(N, Exp)) :-
	print('Got '), print(N), print(' result'),
	( N = 1 -> true ; print('s') ),
	print(', expected '), print(Exp).

% single
% expect a single solution to be found
result_check(single, Results, Result) :-
	result_check(results(1), Results, Result).

% msg(Pass, Fail)
% print a message for pass or fail
result_check(msg(Pass, Fail), Results, Result) :-
	length(Results, NResults),
	(	NResults = 1
	->	Result = result(pass, Pass)
	;	Result = result(fail, Fail)
	).

% cond(Condition, PassMsg, FailMsg)
% pass or fail based on a given condition and print one of two messages
result_check(cond(Cond, Pass, Fail), Results, Result) :-
	% For each successful run of the test, unify Cond with the one in the run's flags
	member(Flags, Results),
	memberchk(cond(Cond, Pass, Fail), Flags),
	% Now that it's unified, call the condition
	(	\+ call(Cond)
	->	Result = result(fail, Fail)
	;	Result = result(pass, Pass)
	).

% shell(Command, PassMsg, FailMsg)
% runs a shell command and passes if it exits with status 0
result_check(shell(Cmd, Pass, Fail), Results, Result) :-
	Results \= [],
	shell(Cmd, Status),
	(	Status = 0
	->	Result = Pass
	;	Result = Fail
	).

% file_match(File1, File2)
% passes if file contents match (according to diff -q)
result_check(file_match(File1, File2), Results, Result) :-
	string_concat('diff -q "', File1, Cmd1),
	string_concat(Cmd1, '" "', Cmd2),
	string_concat(Cmd2, File2, Cmd3),
	string_concat(Cmd3, '"', Cmd),
	result_check(shell(Cmd,
			result(pass, 'files match', file_match(File1, File2)),
			result(fail, 'files do not match', file_match(File1, File2))
		), Results, Result).
result_print_fail(file_match(File1, File2)) :-
	format('~w, ~w', [File1, File2]).


% run a single test
run_test(Cmd, Flags, ResultList) :-
	% run the test and collect the value of the flags for each solution
	ansi_format([bold], '~w', [Cmd]), print(' ...'), nl,
	findall(Flags, call(Cmd), Results),
	% analyse each flag and generate results in a big array
	print('\tAnalyzing results ...'), nl,
	findall(Result,
		(	member(Flag, Flags),
			format('\tchecking ~w\n', [Flag]),
			(	result_check(Flag, Results, Result)
			->	true
			;	Result = result(fail, Flag)
			)
		), ResultList).

% run all tests defined with test/2
run_tests :-
	% first run all the tests
	nl, ansi_format([bold,underline], 'Running tests:', []), nl, nl,
	findall(result(Cmd, Flags, ResultList),
		(	test(Cmd, Flags),
			run_test(Cmd, Flags, ResultList)
		),
		Tests),
	% then emit the results all in one go
	nl, ansi_format([bold,underline], 'Results:', []), nl, nl,
	forall(member(result(Cmd, Flags, ResultList), Tests),
		print_test_result(Cmd, Flags, ResultList)).

% print the results for a single test
print_test_result(Cmd, _, ResultList) :-
	ansi_format([bold], '~w', [Cmd]), nl,
	forall(member(Result, ResultList),
		(	print('\t'),
			result_print(Result),
			nl
		)).
