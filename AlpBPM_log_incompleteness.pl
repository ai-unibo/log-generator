%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOG INCOMPLETENESS PROBLEM
% entry point for the log incompleteness problem
% log_incompleteness(+Log, -MissingTraces)
%
% +Log is a list of traces, each trace a list of terms hap(event(Desc,Pos), Timestamp)
% I would suggest to invoke it only like hap(event(Desc,_), _)
%
% -MissingTraces is a list of all the traces that can be generated, each trace a list of abducibles
%
%
:- module('AlpBPM_log_incompleteness',
		[
			log_incompleteness/7
			, test_log_incompleteness/0
			, test_log_incompleteness/1
		]
	).




log_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Log
				, MissingTraces
				, Options) :-
	push_all_to_never(Observability, ObsNever)
	, create_project(Model, ObsNever, DurationConstraints, InterTaskConstraints, [], Options)
	, project('temp')
	, statistics(runtime,_)
	, log_incompleteness(Log, MissingTraces, Options)
	, statistics(runtime,[_,Time])
	, write('Computed Missing Traces: '), length(MissingTraces, X), write(X), nl
	, write('Required time to compute all the missing traces: '), write(Time), write('ms.'), nl
	, nl, nl, write('Existing Traces:'), nl
	, print_nice_traces(Log)
	, nl, nl, write('Missing Traces:'), nl
	, print_nice_traces(MissingTraces)
	, nl, nl, write('Computed Missing Traces: '), length(MissingTraces, X), write(X), nl
	, write('Required time to compute all the missing traces: '), write(Time), write('ms.'), nl
	, nl, nl.
	
test_log_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_log_incompleteness(ExampleName).'), nl, nl
	, findall(ExampleName,
				find_example(log_incompleteness,
					ExampleName
					, _Model
					, _Observability
					, _DurationConstraints
					, _InterTasksConstraints
					, _Log
					, _Options),
				ExampleList)
	, write('Available examples for the log incompleteness task: '), write(ExampleList), nl, nl.

test_log_incompleteness(ExampleName) :-
	find_example(log_incompleteness,
					ExampleName, Model, Observability, DurationConstraints, InterTaskConstraints, Log, Options)
	, log_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Log
				, _MissingTraces
				, Options
	)
	, nl, nl.
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

log_incompleteness(Log, MissingTraces, _Options) :-
	findall_traces(All),
	extract_missing(Log, All, MissingTraces).







% test_log_incompleteness(Options) :-
	% example_model(Flow, Obs),
	% push_all_to_never(Obs, ObsNever),
	% create_project(Flow, ObsNever, [], Options),
	% project('sciff/temp'),
	% example_trace(Ex),
	% statistics(runtime,_),
	% log_incompleteness(Ex, MissingTraces, Options),
	% statistics(runtime,[_,Time]),
	% write('Computed Missing Traces: '), length(MissingTraces, X), write(X), nl,
	% write('Required time to compute all the missing traces: '), write(Time), write('ms.'), nl,
	% nl, nl, write('Existing Traces:'), nl,
	% print_nice_traces(Ex),
	% nl, nl, write('Missing Traces:'), nl,
	% print_nice_traces(MissingTraces),
	% nl, nl, write('Computed Missing Traces: '), length(MissingTraces, X), write(X), nl,
	% write('Required time to compute all the missing traces: '), write(Time), write('ms.'), nl,
	% nl, nl.
	
push_all_to_never([], []).
push_all_to_never([obs(X, _)|Tail], [obs(X, never)|TailResult]) :-
	push_all_to_never(Tail, TailResult).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(all_traces/1).

findall_traces(Result) :-
	retractall(all_traces(_)),
	assert(all_traces([])),
	(compute_all_solutions -> fail ; true ),
	all_traces(Result).

compute_all_solutions :-
	run,
	findall_constraints_nsquare(abd([_,_],_,_),L),
	retract(all_traces(Tail)),
	assert(all_traces([L|Tail])),
	fail.

extract_missing(_, [], []).
extract_missing(Log, [HAll|TAll], MissingTraces) :-
	trace_member(HAll, Log),
	!,
	extract_missing(Log, TAll, MissingTraces).
extract_missing(Log, [HAll|TAll], [HAll|RTail]) :-
	extract_missing(Log, TAll, RTail).

trace_member(HAll, [HLog|_]) :-
	trace_unif(HAll, HLog),
	!.
trace_member(HAll, [_|TLog]) :-
	trace_member(HAll, TLog).
	
trace_unif(HAll, HLog) :-
	length(HAll, Length),
	length(HLog, Length),
	check_each_element(HAll, HLog).

% funziona solo se gli eventi sono in ordine di accadimento
check_each_element([], []).
check_each_element([AH|AT], [BH|BT]) :-
	event_unif(AH, BH),
	check_each_element(AT, BT).

% OCCHIO: versione preliminare che unifica solo H con ABD
event_unif(abd(_, event(Desc, _), T), hap(event(Desc, _), T)).