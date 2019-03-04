%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVENT INCOMPLETENESS PROBLEM
% entry point for the event incompleteness problem
%
%
%
:- module('AlpBPM_trace_event_incompleteness',
		[
			trace_incompleteness/7
			, test_trace_incompleteness/0
			, test_trace_incompleteness/1
			, event_incompleteness/7
			, test_event_incompleteness/0
			, test_event_incompleteness/1
			, test_event_trace_incompleteness/0
			, test_event_trace_incompleteness/1
		]
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRACE INCOMPLETENESS
trace_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Trace
				, ExtendedTraces
				, Options) :-
	create_project(Model, Observability, DurationConstraints, InterTaskConstraints, Trace, Options)
	, project('temp')
	, write('Trace Incompleteness Reasoning Task'), nl
	, write('Trying to complete the following trace:'), nl
	, print_nice_traces([Trace]), nl, nl
	, statistics(runtime,_)
	, incompleteness(ExtendedTraces, Options)
	, statistics(runtime,[_,Time])
	, write('Computed extensions: '), length(ExtendedTraces, X), write(X), nl
	, write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl
	, nl, nl, write('Extended Traces:'), nl
	, print_nice_traces(ExtendedTraces)
	, nl, nl, nl, write('Computed extensions: '), write(X), nl
	, write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl, nl.
	
	
test_trace_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_trace_incompleteness(ExampleName).'), nl, nl,
	write('Available examples: ')
	, findall(ExampleName,
				find_example(trace_incompleteness,
					ExampleName
					, _Model
					, _Observability
					, _DurationConstraints
					, _InterTasksConstraints
					, _Trace
					, _Options),
				ExampleList)
	, write('Available examples for the trace incompleteness task: '), write(ExampleList), nl, nl.


test_trace_incompleteness(ExampleName) :-
	find_example(trace_incompleteness,
					ExampleName, Model, Observability, DurationConstraints, InterTaskConstraints, Trace, Options)
	, trace_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Trace
				, _ExtendedTrace
				, Options
	)
	, nl, nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVENT INCOMPLETENESS
event_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Trace
				, ExtendedTraces
				, Options) :-
	create_project(Model, Observability, DurationConstraints, InterTaskConstraints, Trace, Options)
	, project('temp')
	, write('Event Incompleteness Reasoning Task'), nl
	, write('Trying to complete the following trace:'), nl
	, print_nice_traces([Trace]), nl, nl
	, statistics(runtime,_)
	, incompleteness(ExtendedTraces, Options)
	, statistics(runtime,[_,Time])
	, write('Computed extensions: '), length(ExtendedTraces, X), write(X), nl
	, write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl
	, nl, nl, write('Extended Traces:'), nl
	, print_nice_traces(ExtendedTraces)
	, nl, nl, nl, write('Computed extensions: '), write(X), nl
	, write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl, nl.
	
	
test_event_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_event_incompleteness(ExampleName).'), nl, nl
	, findall(ExampleName,
				find_example(event_incompleteness,
					ExampleName
					, _Model
					, _Observability
					, _DurationConstraints
					, _InterTasksConstraints
					, _Trace
					, _Options),
				ExampleList)
	, write('Available examples for the event incompleteness task: '), write(ExampleList), nl, nl.


test_event_incompleteness(ExampleName) :-
	find_example(event_incompleteness,
					ExampleName, Model, Observability, DurationConstraints, InterTaskConstraints, Trace, Options)
	, event_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Trace
				, _ExtendedTrace
				, Options
	)
	, nl, nl.
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRACE - EVENT INCOMPLETENESS
event_trace_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Trace
				, ExtendedTraces
				, Options) :-
	create_project(Model, Observability, DurationConstraints, InterTaskConstraints, Trace, Options)
	, project('temp')
	, write('Event-Trace Incompleteness Reasoning Task'), nl
	, write('Trying to complete the following trace:'), nl
	, print_nice_traces([Trace]), nl, nl
	, statistics(runtime,_)
	, incompleteness(ExtendedTraces, Options)
	, statistics(runtime,[_,Time])
	, write('Computed extensions: '), length(ExtendedTraces, X), write(X), nl
	, write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl
	, nl, nl, write('Extended Traces:'), nl
	, print_nice_traces(ExtendedTraces)
	, nl, nl, nl, write('Computed extensions: '), write(X), nl
	, write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl, nl.
	
	
test_event_trace_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_trace_event_incompleteness(ExampleName).'), nl, nl
	, findall(ExampleName,
				find_example(event_trace_incompleteness,
					ExampleName
					, _Model
					, _Observability
					, _DurationConstraints
					, _InterTasksConstraints
					, _Trace
					, _Options),
				ExampleList)
	, write('Available examples for the event incompleteness task: '), write(ExampleList), nl, nl.


test_event_trace_incompleteness(ExampleName) :-
	find_example(event_trace_incompleteness,
					ExampleName, Model, Observability, DurationConstraints, InterTaskConstraints, Trace, Options)
	, event_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Trace
				, _ExtendedTrace
				, Options
	)
	, nl, nl.
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
incompleteness(ExtendedTraces, Options) :-
	(member(first_solution, Options) ->
		write('Option first_solution has been specified. The procedure will stop at the first solution found.'), nl,
		compute_first_solution(ExtendedTraces)
	;
		(member(all_solutions, Options) ->
			write('Option all_solutions has been specified. The procedure will compute all solutions.'), nl,
			retractall(extended_traces(_)),
			assert(extended_traces([])),
			(compute_all_extensions -> fail ; true ),
			extended_traces(ExtendedTraces)
		;
			write('No option between {first_solution | all_solutions} has been specified; assuming first_solution. The procedure will stop at the first solution found.'), nl,
			compute_first_solution(ExtendedTraces)
		)
	).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(extended_traces/1).


compute_first_solution([LLL]) :-
	run,
	findall_constraints_nsquare(abd([_,_],X,T),L),
	findall_constraints_nsquare(h([_,_],X,T),LL),
	append(L, LL, LLL).


compute_all_extensions :-
	run,
	write('Found a Solution!'), nl, nl,
	findall_constraints_nsquare(abd([_,_],X,T),L),
	findall_constraints_nsquare(h([_,_],X,T),LL),
	append(L, LL, LLL),
	retract(extended_traces(Tail)),
	assert(extended_traces([LLL|Tail])),
	fail.

