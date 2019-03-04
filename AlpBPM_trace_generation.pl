:- module('AlpBPM_trace_generation', [
									trace_generation/5,
									trace_generation/6,
									generate_trace_limited/3,
									%%  generate_trace_and_print_single_trace/2,
									%%  generate_trace_and_print_single_events/2,
									test_trace_generation/0,
									test_trace_generation/1
									%%  , test/0
									]).

:- use_module(library(memfile)).
:- use_module(ics_parser).
:- use_module(sokb_parser).
:- use_module(history_parser).
:- use_module(library(http/http_open)).


%%  test :-
%%  	trace_generation([seq(start,a), seq(a,b), seq(b,stop)], [obs(start, never), obs(a, partially), obs(b, partially), obs(stop, never)], [], [], 'daniela.txt', [ trace_max_length(33), time_limit(1000), instances_for_each_path(200) ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRACE GENERATION
%%  
%%  trace_generation(Model, Observability, DurationConstraints, InterTaskConstraints, FileNameForSavingTraces, Options)

trace_generation(Model
				, DurationConstraints
				, InterTaskConstraints
				, FileNameForSavingTraces
				, Options) :-
	get_activity_list(Model, LA)
	, build_observability_list(LA, Observability)
	, trace_generation(Model, Observability, DurationConstraints, InterTaskConstraints, FileNameForSavingTraces, Options)
	, !
	, atom_concat('http://localhost:8080/myapp/receiveRequest/', FileNameForSavingTraces, URL)
	, http_open(URL, In, [])
	%%  , copy_stream_data(In, user_output)
	, close(In)
	.

trace_generation(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, FileNameForSavingTraces
				, Options) :-
	new_memory_file(ICS_Handle)
	, new_memory_file(History_Handle)
	, new_memory_file(SOKB_Handle)
	%%  create_project(Flow, Obs, DurationConstraints, InterTaskConstraints, History, Options, ICS_Handle, History_Handle, SOKB_Handle)
	, create_project(Model, Observability, DurationConstraints, InterTaskConstraints, [], Options, ICS_Handle, History_Handle, SOKB_Handle)
	%%  , project('temp') %%% REMOVED because it works now only memory to memory
	, translate_mem_to_mem(ICS_Handle, History_Handle, SOKB_Handle, [ [allow_events_not_expected,no], [coloring,yes] ])
	, memory_file_to_string(ICS_Handle, TheString)
	, write(TheString)
	, write('Trace Generation Reasoning Task'), nl
	, statistics(runtime,_)
	, open(FileNameForSavingTraces, write, Stream)
	, ( generate_trace_and_print_single_events(Stream, Options) -> fail ; true )
	, close(Stream)
	, statistics(runtime,[_,Time])
	, write('Required time to generate the traces: '), write(Time), write('ms.'), nl
	.


build_observability_list([], []).
build_observability_list([H|T], [obs(H, never)|TResult]) :-
	build_observability_list(T, TResult).



%%  required_option(allow_events_not_expected,no).
%%  %required_option(fdet,yes).
%%  required_option(coloring,yes).

%%  %%%%%%%%%%%%%%%%%%%%%%% Constant Part %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  build_prj(Path):-
%%      findall(F,ics_file(F),ICS_files), append_path(Path,ICS_files,IcsPathFiles),
%%      translate_ics_files(IcsPathFiles,'./ics.pl'),
%%      findall(F,history_file(F),Hist_files),  append_path(Path,Hist_files,HistPathFiles),
%%      translate_histories(HistPathFiles,'./history.pl'),
%%      findall(F,sokb_file(F),Sokb_files),     append_path(Path,Sokb_files,SokbPathFiles),
%%      convert_sokb(SokbPathFiles,'./sokb.pl'),
%%      compile(sokb), compile(history), compile(ics),
%%      findall([O,V],required_option(O,V),LOptions),
%%      set_options(LOptions).

%%  % Default:
%%  run(_):- run.
%%  run_open(_):- run_no_close.
%%  run_closed(_):- run.


%%  %%%%
%%  % fdet option
%%  % :- dynamic fdet/1.
%%  % fdet(e(event(_,_),_)).





	
test_trace_generation :-
	nl, write('Usage:'),nl,writef('\ttest_trace_generation(ExampleName).'), nl, nl
	, write('Available examples: ')
	, findall(ExampleName,
				find_example(trace_generation,
					ExampleName
					, _Model
					, _Observability
					, _DurationConstraints
					, _InterTasksConstraints
					, _Trace
					, _Options),
				ExampleList)
	, write('Available examples for the trace generation task: '), write(ExampleList), nl, nl.


test_trace_generation(ExampleName) :-
	find_example(trace_generation,
					ExampleName, Model, Observability, DurationConstraints, InterTaskConstraints, _Trace, Options)
	,write('Options: '), write(Options), nl
	,write('Model: '), write(Model), nl
	%%  trace_generation(Model, Observability, DurationConstraints, InterTaskConstraints, FileNameForSavingTraces, Options)
	, trace_generation(Model
				  , Observability
				, DurationConstraints
				, InterTaskConstraints
			  	, '/Users/daniela/Desktop/daniela.txt' %
				%, 'daniela.txt'
				, Options
	)
	, nl, nl.






:- dynamic(counter/1).
:- dynamic(pathCounter/1).
%%  time_limit(1000), instances_for_each_path(20)

generate_trace_and_print_single_trace(Stream, Options) :-
	(
		(member(time_limit(TimeLimit), Options) ) ->
			true
		;
			TimeLimit = 0
	)
	, (
		(member(instances_for_each_path(PathLimit), Options) ) ->
			true
		;
			PathLimit = 0
	)
	, generate_trace_limited(Trace, PathLimit, TimeLimit)
	, counter(Counter)
	, retractall(counter(_))
	, NewCounter is Counter + 1
	, asserta(counter(NewCounter))
	, print_trace_to_file(Stream, Trace, NewCounter)
	, fail
	.

generate_trace_and_print_single_events(Stream, Options) :-
	(
		(member(time_limit(TimeLimit), Options) ) ->
			true
		;
			TimeLimit = 0
	)
	, (
		(member(instances_for_each_path(PathLimit), Options) ) ->
			true
		;
			PathLimit = 0
	)
	, generate_trace_limited(Trace, PathLimit, TimeLimit)
	, counter(Counter)
	, retractall(counter(_))
	, NewCounter is Counter + 1
	, asserta(counter(NewCounter))
	, print_events_to_file(Stream, Trace, NewCounter)
	, fail
	.

%%  generate_trace(Trace) :-
%%  	retractall(counter(_))
%%  	, assert(counter(0))
%%  	, run
%%  	, findall_constraints_nsquare(abd([_,_],X,T),L)
%%  	, findall_constraints_nsquare(h([_,_],X,T),LL)
%%  	, append(L, LL, LLL)
%%  	, member(abd([_,_],event(stop, _),Ts), LLL)
%%  	, Ts #< 230 % da modificare e trasformare in opzione
%%  	, extract_variables(LLL, List_of_vars)
%%  	, labeling([], List_of_vars)
%%  	, delete(LLL, abd([_,_],event(start,_),_), FF)
%%  	, delete(FF, abd([_,_],event(stop,_),_), FFF)
%%  	, delete(FFF, abd([_,_],and_split_support(_,_),_), FFFF)
%%  	, delete(FFFF, abd([_,_],and_join_support(_,_),_), Trace)
%%  	.

% generate traces in a limited number for each possible pathway defined in the Model
% the aim is to provide at least Limit traces for each possible path, so that all the Model
% is covered...
generate_trace_limited(Trace, PathLimit, TimeLimit) :-
	retractall(counter(_))
	, assert(counter(0))
	, run
	, findall_constraints_nsquare(abd([_,_],X,T),L)
	, findall_constraints_nsquare(h([_,_],X,T),LL)
	, append(L, LL, LLL)
	, member(abd([_,_],event(stop, _),Ts), LLL)
	, (
		(TimeLimit > 0) ->
			Ts #< TimeLimit 
		;
			true
	)
	, (
		(PathLimit > 0) ->
			perform_limited_labeling(LLL, PathLimit)
		;
			extract_variables(LLL, List_of_vars), reverse(List_of_vars, Reversed), labeling([], Reversed)
	)
	, delete(LLL, abd([_,_],event(start,_),_), FF)
	, delete(FF, abd([_,_],event(stop,_),_), FFF)
	, delete(FFF, abd([_,_],and_split_support(_,_),_), FFFF)
	, delete(FFFF, abd([_,_],xor_join_support(_,_),_), FFFFF)
	, delete(FFFFF, abd([_,_],and_join_support(_,_),_), Trace)
	.



perform_limited_labeling(Trace, Limit) :-
	extract_variables(Trace, List_of_vars)
	%%  , member(abd([_,_],event(stop, _),Ts), Trace)
	, reverse(List_of_vars, Reversed)
	, Reversed = [Ts|_T]
	%%  , write(List_of_vars)
	, retractall(pathCounter(_))
	, asserta(pathCounter(0))
	, labeling([ff, bisect, min(Ts)], Reversed)
	, pathCounter(PathCounter)
	, retractall(pathCounter(_))
	, NewPathCounter is PathCounter + 1
	, asserta(pathCounter(NewPathCounter))
	, (
		(NewPathCounter = Limit) ->
			!
		;
			true
	)
	.



extract_variables([], []).
extract_variables([abd([_,_],event(start, _),_)|T], TResult) :-
	!
	, extract_variables(T, TResult)
	.
extract_variables([abd([_,_],event(stop, _),_)|T], TResult) :-
	!
	, extract_variables(T, TResult)
	.
extract_variables([abd([_,_],and_split_support(_, _),_)|T], TResult) :-
	!
	, extract_variables(T, TResult)
	.
extract_variables([abd([_,_],and_join_support(_, _),_)|T], TResult) :-
	!
	, extract_variables(T, TResult)
	.
extract_variables([abd([_,_],xor_join_support(_, _),_)|T], TResult) :-
	!
	, extract_variables(T, TResult)
	.
extract_variables([abd([_,_],event(_, _),H)|T], [H|TResult]) :-
	var(H)
	, !
	, extract_variables(T, TResult)
	.
extract_variables([_|T], TResult) :-
	extract_variables(T, TResult)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prints a whole trace to the row in a file
%
print_trace_to_file(Stream, Trace, NewCounter) :-
	determine_arity(Trace, N)
	, write(Stream, 't('), write(Stream, NewCounter), write(Stream, ',[')
	, (sort( [N,2],@=<, Trace, TraceSorted) ->
		print_single_trace(Stream, TraceSorted)
		;
		print_single_trace(Stream, Trace)
	)
	, write(Stream, ']).'), nl(Stream)
	.

print_single_trace(_Stream, []) :-
	!.
print_single_trace(Stream, [H]) :-
	!
	, write_daniela(Stream, H)
	.
print_single_trace(Stream, [H|T]) :-
	write_daniela(Stream, H)
	, write(Stream, ',')
	, print_single_trace(Stream,T).

determine_arity([H|_], N) :- functor(H,_,M), N is M-1.

write_daniela(Stream, abd(_,Event,Timestamp)) :-
	write(Stream, h(Event,Timestamp)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print single events to file
print_events_to_file(Stream, Trace, TraceId) :-
	determine_arity(Trace, N)
	%%, write(Stream, 't('), write(Stream, NewCounter), write(Stream, ',[')
	, (sort( [N,2],@=<, Trace, TraceSorted) ->
		print_single_events(Stream, TraceSorted, TraceId)
		;
		print_single_events(Stream, Trace, TraceId)
	)
	 , write(Stream, t(TraceId,end_of_the_world) ) , write(Stream, '.') %%MOD BY DANIELA 10/03/2017
    %%, write(Stream, ']).')
	, nl(Stream)
	.

print_single_events(_Stream, [], _TraceId) :-
	!.
print_single_events(Stream, [abd(_,Event,Timestamp) |T], TraceId) :-
	write(Stream, t(TraceId,h(Event,Timestamp)) ) %write(Stream, h(Event,Timestamp) ) 
	, write(Stream, '.') %, nl(Stream)
	, print_single_events(Stream, T, TraceId).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cosa fa? Forse si puo' rimuovere?
%%  print_trace_to_file3(Stream, Trace, NewCounter) :-
%%  	determine_arity(Trace, N)
%%  	, (sort( [N,2],@=<, Trace, TraceSorted) ->
%%  		print_single_trace3(Stream, NewCounter, TraceSorted)
%%  		;
%%  		print_single_trace3(Stream, NewCounter, Trace)
%%  	)
%%  	, nl(Stream)
%%  	.

%%  print_single_trace3(_Stream, _Tr, []) :-
%%  	!.
%%  print_single_trace3(Stream, TrNumber, [abd(_,Event,Timestamp)|T]) :-
%%  	!
%%  	, write(Stream, h(TrNumber, Event, Timestamp)), write('.')
%%  	, print_single_trace3(Stream, TrNumber, [ T ]  )
%%  	.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%