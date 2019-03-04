:- module('AlpBPM_opt_incompleteness', [opt_incompleteness/7,
									test_opt_incompleteness/0,
									test_opt_incompleteness/1,
									best_time/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OPTIMAL INCOMPLETENESS
opt_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Trace
				, ExtendedTrace
				, Options) :-
	create_project(Model, Observability, DurationConstraints, InterTaskConstraints, Trace, Options)
	, project('temp')
	, write('Trace/Event Optimal Incompleteness Reasoning Task'), nl
	, write('Trying to complete the following trace:'), nl
	, print_nice_traces([Trace]), nl, nl
	, statistics(runtime,_)
	, opt_incompleteness(ExtendedTrace, Options)
	, statistics(runtime,[_,Time])
	, write('Required time to find the optimal extensions: '), write(Time), write('ms.'), nl
	, nl, nl, write('Extended Trace:'), nl
	, print_nice_traces([ExtendedTrace])
	, nl, nl, nl
	, write('Required time to compute the optimal extension: '), write(Time), write('ms.'), nl, nl.
	
	
test_opt_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_opt_incompleteness(ExampleName).'), nl, nl,
	write('Available examples: ')
	, findall(ExampleName,
				find_example(opt_incompleteness,
					ExampleName
					, _Model
					, _Observability
					, _DurationConstraints
					, _InterTasksConstraints
					, _Trace
					, _Options),
				ExampleList)
	, write('Available examples for the trace incompleteness task: '), write(ExampleList), nl, nl.


test_opt_incompleteness(ExampleName) :-
	find_example(opt_incompleteness,
					ExampleName, Model, Observability, DurationConstraints, InterTaskConstraints, Trace, Options)
	, opt_incompleteness(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Trace
				, _ExtendedTrace
				, Options
	)
	, nl, nl.








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%
%
%

opt_incompleteness(ExtendedTrace, _Options) :-
	retractall(best_time(_))
	, assert(best_time(1000))
	, retractall(opt_trace(_))
	, assert(opt_trace([]))
	, ( compute_opt_extension -> fail ; true )
	, opt_trace(ExtendedTrace)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic(opt_trace/1).
:- dynamic(best_time/1).




compute_opt_extension :-
	run
	, write('Found a Solution!'), nl, nl
	, findall_constraints_nsquare(abd([_,_],X,T),L)
	, findall_constraints_nsquare(h([_,_],X,T),LL)
	, append(L, LL, LLL)
	, member(abd([_,_],event(stop, _),Ts), LLL)
	, once( labeling([min(Ts)], [Ts]) )
	, write('Best time: '), write(Ts), nl
	, retractall(best_time(_))
	, assert(best_time(Ts))
	, retractall(opt_trace(_))
	, assert(opt_trace(LLL))
	, fail.

