:- module('AlpBPM_examples', [find_example/8, test_install/0, help_options/0] ).




:- use_module('examples/AlpBPM_log_incompleteness_examples', [find_example/8 as fe_log] ).
:- use_module('examples/AlpBPM_trace_incompleteness_examples', [find_example/8 as fe_t] ).
:- use_module('examples/AlpBPM_event_incompleteness_examples', [find_example/8 as fe_e] ).
:- use_module('examples/AlpBPM_event_trace_inc_examples', [find_example/8 as fe_te] ).

:- use_module('examples/AlpBPM_fracture_treatment', [find_example/8 as fe_frac] ).
%:- use_module('examples/AlpBPM_daniela', [find_example/8 as fe_dan] ).


% find_example(ReasoningTask, ExampleName, Model, Observability,
			% DurationConstraints,InterTasksConstraints, Trace).
find_example(P1,P2,P3,P4,P5,P6,P7,P8) :-
	fe_log(P1,P2,P3,P4,P5,P6,P7,P8).
find_example(P1,P2,P3,P4,P5,P6,P7,P8) :-
	fe_t(P1,P2,P3,P4,P5,P6,P7,P8).
find_example(P1,P2,P3,P4,P5,P6,P7,P8) :-
	fe_e(P1,P2,P3,P4,P5,P6,P7,P8).
find_example(P1,P2,P3,P4,P5,P6,P7,P8) :-
	fe_te(P1,P2,P3,P4,P5,P6,P7,P8).

% The Fracture Treatment example
find_example(P1,P2,P3,P4,P5,P6,P7,P8) :-
	fe_frac(P1,P2,P3,P4,P5,P6,P7,P8).

% The Daniela example
find_example(P1,P2,P3,P4,P5,P6,P7,P8) :-
	fe_dan(P1,P2,P3,P4,P5,P6,P7,P8).

test_install :-
	test_log_incompleteness(log_incompleteness_ex1)
	, test_trace_incompleteness(trace_incompleteness_ex1)
	, test_event_incompleteness(event_incompleteness_ex1)
	, test_event_trace_incompleteness(event_trace_incompleteness_ex1)
	, test_opt_incompleteness(fracture_treatment)
	.
	



help_options :-
	write('Options must be a list of terms. Allowed terms:'), nl,
	write(' -- trace_max_length(Max)  sets the maximum length of a trace. Max should be a ground, positive integer.'), nl,
	write(' -- first_solution computes the first solution and stops. If not specified otherwise, this is the default option'), nl,
	write(' -- all_solutions computes all solutions.'), nl,
	write(' -- opt_solution compute the best solution with respect to the time variable, i.e., it computes the shortest solution feasible with the given constraints.'), nl.

	
	
	


	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compute all the times for the paper CAISE 2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compute_all_times :-
	
	% write table header
	FormatHeader = '%52c|%42c\n',
	Format = '%32l%20l|%16r%16r%10r\n',
	writef('%r\n', ['-', 96]),
	writef(FormatHeader, ['Input', 'Output']),
	writef(Format,['Example','Path max length', 'Parsing Time', 'Computing Time', '#Traces']),
	writef('%r\n', ['-', 96]),	
	
	% log_incompleteness
	Max1 = 16,
	example_model(Flow, Obs),
	push_all_to_never(Obs, ObsNever),
	statistics(runtime,_),
	create_project(Flow, ObsNever, [], [path_max_length(Max1)]),
	statistics(runtime,[_,Time1]),
	project('sciff/temp'),
	example_trace(Ex1),
	statistics(runtime,_),
	log_incompleteness(Ex1, MissingTraces),
	statistics(runtime,[_,Time2]),
	length(MissingTraces, Length1),
	
	writef(Format, ['Log incompleteness',Max1,Time1,Time2,Length1]),
	
	
	% trace incompleteness
	Max2 = 16,
	findall([A,Max2,B,C,D], times_trace_incompleteness(A, Max2, B, C, D), L),
	print_times(Format, L),
		
	% event incompleteness
	findall([A,MaxLength,B,C,D], times_event_incompleteness(A, MaxLength, B, C, D), L2),
	print_times(Format, L2),
	
	% event and trace incompleteness
	findall([A,Max2,B,C,D], times_event_trace_incompleteness(A, Max2, B, C, D), L3),
	print_times(Format, L3),
	
	% municipality examples
	Max3 = 40,
	findall([A,Max3,B,C,D], times_municipality(A, Max3, B, C, D), L4),
	print_times(Format, L4),
	
	nl, nl, nl, nl,
	writef('%r\n', ['-', 96]),
	writef(FormatHeader, ['Input', 'Output']),
	writef(Format,['Example','Path max length', 'Parsing Time', 'Computing Time', '#Traces']),
	writef('%r\n', ['-', 96]),
	writef(Format, ['Log incompleteness',Max1,Time1,Time2,Length1]),
	print_times(Format, L),
	print_times(Format, L2),
	print_times(Format, L3),
	print_times(Format, L4).



times_trace_incompleteness(Example_name, Max_length, Parsing_time, Computing_time, Num_traces) :-
	example_model(Flow, Obs),
	trace_incomplete(Example, Trace),
	atom_concat('Trace_incomplete_', Example, Example_name),
	write(Example_name), nl,
	statistics(runtime,_),
	create_project(Flow, Obs, Trace, [path_max_length(Max_length)]),
	statistics(runtime,[_,Parsing_time]),
	project('sciff/temp'),
	statistics(runtime,_),
	trace_incompleteness(ExtendedTraces),
	statistics(runtime,[_,Computing_time]),
	length(ExtendedTraces, Num_traces).

times_event_incompleteness(Example_name, Max_length, Parsing_time, Computing_time, Num_traces) :-
	example_model(Flow, Obs),
	event_incomplete(Example, Trace),
	length(Trace, LL),
	Max_length is LL+2,
	atom_concat('Event_incomplete_', Example, Example_name),
	write(Example_name), nl,
	statistics(runtime,_),
	create_project(Flow, Obs, Trace, [path_max_length(Max_length)]),
	statistics(runtime,[_,Parsing_time]),
	project('sciff/temp'),
	statistics(runtime,_),
	trace_incompleteness(ExtendedTraces),
	statistics(runtime,[_,Computing_time]),
	length(ExtendedTraces, Num_traces).

times_event_trace_incompleteness(Example_name, Max_length, Parsing_time, Computing_time, Num_traces) :-
	example_model(Flow, Obs),
	event_trace_incomplete(Example, Trace),
	atom_concat('Event_trace_incomplete_', Example, Example_name),
	write(Example_name), nl,
	statistics(runtime,_),
	create_project(Flow, Obs, Trace, [path_max_length(Max_length)]),
	statistics(runtime,[_,Parsing_time]),
	project('sciff/temp'),
	statistics(runtime,_),
	trace_incompleteness(ExtendedTraces),
	statistics(runtime,[_,Computing_time]),
	length(ExtendedTraces, Num_traces).	

times_municipality(Example_name, Max_length, Parsing_time, Computing_time, Num_traces) :-
	mun_model(Flow, Obs),
	mun_incomplete(Example, Trace),
	atom_concat('Mun_incomplete_', Example, Example_name),
	nl, nl, write('Start: '), write(Example_name), nl,
	statistics(runtime,_),
	create_project(Flow, Obs, Trace, [path_max_length(Max_length)]),
	statistics(runtime,[_,Parsing_time]),
	project('sciff/temp'),
	statistics(runtime,_),
	trace_incompleteness(ExtendedTraces),
	statistics(runtime,[_,Computing_time]),
	nl, nl, write('End: '), write(Example_name), nl,
	length(ExtendedTraces, Num_traces).	
	
print_times(_, []) :- !.
print_times(Format, [H|T]) :-
	writef(Format, H),
	print_times(Format, T).