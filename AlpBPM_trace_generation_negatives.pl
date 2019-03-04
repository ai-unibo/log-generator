:- module('AlpBPM_trace_generation_negatives', [
									trace_generation_negatives/4
									%, test_negatives/0
									, procedural_example/0
									, declarative_example/0
									]).

:- use_module(library(memfile)).
:- use_module(ics_parser).
:- use_module(sokb_parser).
:- use_module(history_parser).
:- use_module(library(http/http_open)).

:- dynamic(counter/1).
:- dynamic(pathCounter/1).



%% Meaning of the Options = [...]:
%%
%% procedural(start(x)): the process is procedural, and the starting activity is x (x ground)
%% declarative: the process is declarative
%% NOTE: procedural and declarative should not be specified at the same time
%%
%% positives: ask to generate positive traces only
%% negatives: ask to generate negative traces only
%% NOTE: positive and negative can be specified at the same time, so to generate in a single file both the trace types.


procedural_example :-
	Model = [start(a), xor_split(a,[b,c]), seq(c,d), seq(b,e), seq(d,e) ]
	, ActivityList = [a,b,c,d,e]
	, Options = [trace_max_length(5), time_limit(5), instances_for_each_path(1), procedural, positives, negatives, output(xes) ]
	, FileNameForSavingTraces = 'OUTPUT/procedural_traces.xml'
	, trace_generation_negatives(Model
				, ActivityList
				, FileNameForSavingTraces
				, Options
	)
	, nl, nl.

declarative_example :-
	Model = [ coexistence(have_a_coffe,pay_a_coffe)  ]
	, ActivityList = [have_a_coffe, pay_a_coffe, chat_with_the_barman]
	, Options = [trace_max_length(6), time_limit(6), instances_for_each_path(1), declarative, positives, negatives, output(xes) ]
	, FileNameForSavingTraces = 'OUTPUT/declarative_traces.xml'
	, trace_generation_negatives(Model
				, ActivityList
				, FileNameForSavingTraces
				, Options
	)
	, nl, nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRACE GENERATION
%%  
trace_generation_negatives(Model
				, ActivityList
				, FileNameForSavingTraces
				, Options) :-
	%% Opening header
		write('\n\n')
		, write('Model: '), write(Model), nl
		, write('ActivityList: '), write(ActivityList), nl
		, write('Options: '), write(Options), nl
		, write('Saving to file: '), write(FileNameForSavingTraces), nl
		, write('\n\n')
		, write('Trace Generation Reasoning Task\n\n'), nl
		, statistics(runtime,_)		
	%% Opening the output file and other initialization stuff
		, open(FileNameForSavingTraces, write, Stream)
		, assert(counter(0))
		, ( member(trace_max_length(TML), Options) -> trace_max_length(TML) ; true )
		, current_trace_max_length(0)
		, ( member(output(xes), Options) ->
				atomics_to_string([
					'<?xml version="1.0" ?>\n'
					, '<log xes.version="1.0" xmlns="http://www.xes-standard.org/">\n'
  					, '\t<extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"></extension>\n'
  					, '\t<extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"></extension>\n'
				], Header)
				, write(Stream, Header)
			;
				true
		)
	%% let us explore all the alternatives and generate all the traces
		, (
			%% determine the model features
				determine_model_features(Options, ProcDecl, PosNeg)
			%% create the ICs
				, generate_ics( ActivityList, Model, Options, ProcDecl, PosNeg, ICS, Meta)
				, write(ICS) %% debug
				, write_debug(Stream, Options, Meta, ICS)
				% , write(Stream,'\n%% '), write(Stream, Meta), write(Stream,'\n') %% debug
				% , write(Stream, ICS), write(Stream, '\n') %% HEAVY debug
				, new_memory_file(ICS_Handle)
				, open_memory_file(ICS_Handle, write, StreamICS)
				, write(StreamICS, ICS)
				, close(StreamICS)
			%% create the SOKB
				, generate_sokb(Model, Options, ProcDecl, PosNeg, SOKB)
				, write(SOKB) %% debug
				, new_memory_file(SOKB_Handle)
				, open_memory_file(SOKB_Handle, write, StreamSOKB)
				, write(StreamSOKB, SOKB)
				, close(StreamSOKB)
			%% prepare the memory files to write the sciff version of the model
				, new_memory_file(History_Handle)
			%% compile the sciff project
				, translate_mem_to_mem(ICS_Handle, History_Handle, SOKB_Handle, [ [allow_events_not_expected,no], [coloring,yes] ])
			%% let us generate the trace
				% , write('Press a key'), get_char(_)
				, generate_trace_and_print_single_events(Stream, PosNeg, Options)
				-> fail ; true
		)
		, ( member(output(xes), Options) ->
				atomics_to_string(['</log>'], Footer), write(Stream, Footer)
			;
				true
		)
		, close(Stream)
		, statistics(runtime,[_,Time])
		, write('Required time to generate the traces: '), write(Time), write('ms.'), nl
	.


write_debug(Stream, Options, Meta, ICS) :-
	( member(output(xes), Options) ->
			write_debug_xes(Stream, Options, Meta, ICS)
		;
			write_debug_txt(Stream, Options, Meta, ICS)
	)
	.
	
write_debug_txt(Stream, _Options, Meta, ICS) :-
	write(Stream,'\n%% '), write(Stream, Meta), write(Stream,'\n') %% debug
	, write(Stream, ICS), write(Stream, '\n') %% HEAVY debug
	.
write_debug_xes(Stream, _Options, Meta, ICS) :-
	write(Stream, '<!--\n')
	, write(Stream,'%% '), write(Stream, Meta), write(Stream,'\n') %% debug
	%% filtering out the ---> because of XML restrictions...
		, atomic_list_concat(Words, '--->', ICS), atomic_list_concat(Words, '->', FilteredICS)
	, write(Stream, FilteredICS) %, write(Stream, '\n') %% HEAVY debug
	, write(Stream, '-->\n')
	.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DETERMINE THE MODEL FEATURES
%%
% determine_model_features(Options, ProcDecl, PosNeg)
determine_model_features(Options, procedural, positives) :-
	member(procedural, Options)
	, member(positives, Options)
	, write('%%%%%%% PROCEDURAL'), write(', POSITIVE TRACES %%%%%%%%%%%%%%%%%%%%%%%%%%%%\n')
	, write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n')
	.
determine_model_features(Options, procedural, negatives) :-
	member(procedural, Options)
	, member(negatives, Options)
	, write('%%%%%%% PROCEDURAL'), write(', NEGATIVE TRACES %%%%%%%%%%%%%%%%%%%%%%%%%%\n')
	, write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n')
	.
determine_model_features(Options, declarative, positives) :-
	member(declarative, Options)
	, member(positives, Options)
	, write('%%%%%%% DECLARATIVE, POSITIVE TRACES %%%%%%%%%%%%%%%%%%%%%%%%%%%%\n')
	, write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n')
	.
determine_model_features(Options, declarative, negatives) :-
	member(declarative, Options)
	, member(negatives, Options)
	, write('%%%%%%% DECLARATIVE, NEGATIVE TRACES %%%%%%%%%%%%%%%%%%%%%%%%%%%%\n')
	, write('%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n')
	.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GENERATION OF ICs
%%
generate_ics( ActivityList, Model, Options, ProcDecl, PosNeg, ICS, Meta) :-
	translate_negatives_activity_list(ActivityList, Options, ProcDecl, PosNeg, R1)
	, translate_negatives_model(Model, Options, ProcDecl, PosNeg, R2, Meta)
	, atomics_to_string([R1,R2], ICS)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ACTIVITY LIST -related ICs
%%
translate_negatives_activity_list( _ActivityList, _Options, procedural, positives, '') :-
	!
	.
translate_negatives_activity_list( [], _Options, _ProcDecl, _PosNeg, '').
translate_negatives_activity_list( [H | T], Options, procedural, negatives, Result) :-
	%% first, get the time limit (if it is specified)
		( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	%% write the ICs about the generation
		, atomics_to_string([
			'true ---> true \\/ ABD(event(', H, '),T) /\\ T>0 /\\ T<', TimeLimit, ' /\\ ABD(nic(0),0)', '.\n'
			, 'ABD(event(', H,'), T) ---> true \\/ ABD(event(', H,'), T1) /\\ T1>T /\\ T1<', TimeLimit, ' /\\ ABD(nic(0),0)', '.\n'
			], R1)
	%% recursive call
		, translate_negatives_activity_list( T, Options, procedural, negatives, R2)
		, atomics_to_string([R1, R2], Result).
translate_negatives_activity_list( [H | T], Options, declarative, PosNeg, Result) :-
	%% first, get the time limit (if it is specified)
		( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	%% write the ICs about the generation
		, atomics_to_string([
			'true ---> true \\/ ABD(event(', H, '),T) /\\ T>0 /\\ T<', TimeLimit, '.\n'
			, 'ABD(event(', H,'), T) ---> true \\/ ABD(event(', H,'), T1) /\\ T1>T /\\ T1<', TimeLimit, '.\n'
			], R1)
	%% recursive call
		, translate_negatives_activity_list( T, Options, declarative, PosNeg, R2)
		, atomics_to_string([R1, R2], Result).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODEL TRANSLATION - ICS
%%  
% translate_negatives_model(Model, Options, ProcDecl, PosNeg, Result, Meta)
translate_negatives_model([], _Options, _ProcDecl, positives, '', []).
translate_negatives_model([H|T], Options, ProcDecl, positives, Result, [Meta1|Meta2]) :-
	translate_single_constraint(H, Options, ProcDecl, positives, R1, Meta1)
	, translate_negatives_model(T, Options, ProcDecl, positives, R2, Meta2)
	, atomics_to_string([R1, R2], Result)
	.

translate_negatives_model([], _Options, _ProcDecl, negatives, '', []).
translate_negatives_model([H|T], Options, ProcDecl, negatives, Result, [Meta1|Meta2]) :-
	translate_single_constraint(H, Options, ProcDecl, _, R1, Meta1)
	, translate_negatives_model(T, Options, ProcDecl, negatives, R2, Meta2)
	, atomics_to_string([R1, R2], Result)
	.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PROCEDURAL(START(S))
%%  
translate_single_constraint(start(S), Options, procedural, positives, Result, Meta) :-
	( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	, atomics_to_string([
		'%%% positive: start(', S, ')\n'
		, 'true ---> ABD(event(', S, '),T) /\\ T>0 /\\ T<', TimeLimit, '.\n'
		], Result)
	, atomics_to_string(['positive:start(', S, ')'], Meta)
	.
translate_single_constraint(start(S), _Options, procedural, negatives, Result, Meta) :-
	atomics_to_string([
		'%%% negative: start(', S, ')\n'
		, 'true ---> ABD(nic(0),0).\n'
		, 'ABD(event(', S, '),T) ---> fail.\n'
		], Result)
	, atomics_to_string(['negative:start(', S, ')'], Meta)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SEQ(A,B)
translate_single_constraint(seq(A,B), Options, procedural, positives, Result, Meta) :-
	( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	, atomics_to_string([
			'%%% positive: seq(', A, ',', B, ')\n'
			, 'ABD(event(', A, '), T1)'
			, ' ---> '
			, 'ABD(event(', B, '),T2)'
			, ' /\\ T2>T1 /\\ T2<', TimeLimit, '.\n'
		], Result)
	, atomics_to_string(['positive:seq(', A, ',', B, ')'], Meta)
	.
translate_single_constraint(seq(A,B), _Options, procedural, negatives, Result, Meta) :-
	atomics_to_string([
		'%%% negative_1: seq(', A, ',', B, ')\n'
		, 'ABD(event(', A, '), T1)'
		, ' ---> '
		, 'ABD(nic(0),0).\n'
		, 'ABD(event(', A, '), T1)'
		, ' /\\ ABD(event(', B, '), T2)'
		, ' /\\ T2>T1 '
		, ' ---> fail.\n'
		], Result)
	, atomics_to_string(['negative_1:seq(', A, ',', B, ')'], Meta)
	.
translate_single_constraint(seq(A,B), Options, procedural, negatives, Result, Meta) :-
	( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	, atomics_to_string([
		'%%% negative_2: seq(', A, ',', B, ')\n'
		, 'ABD(event(', A,'), T1)'
		, ' ---> '
		, 'ABD(event(', B,'), T2)'
		, ' /\\ T2<T1 /\\ T2<', TimeLimit, ' /\\ T2>0'
		, ' /\\ ABD(nic(0),0)'
		, '.\n'
		, 'ABD(event(', A, '), T1)'
		, ' /\\ ABD(event(', B, '), T2)'
		, ' /\\ T2>T1 '
		, ' ---> fail.\n'
		], Result)
	, atomics_to_string(['negative_2:seq(', A, ',', B, ')'], Meta)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XOR(A,[B,C])
translate_single_constraint(xor_split(A,[B,C]), Options, procedural, positives, Result, Meta) :-
	( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	, atomics_to_string([
			'%%% positive: xor(', A, ',', '[', B, ',', C, ']', ')\n'
			, 'ABD(event(', A, '), T1)'
			, ' ---> '
			, 'ABD(event(', B, '),T2)'
			, ' /\\ T2>T1 /\\ T2<', TimeLimit, '\n'
			, '\t\\/ ABD(event(', C, '),T2)'
			, ' /\\ T2>T1 /\\ T2<', TimeLimit, '.\n'
			, 'ABD(event(', B, '), T1)'
			, ' /\\ ABD(event(', C, '),T2)'
			, ' ---> fail.\n'
		], Result)
	, atomics_to_string(['positive:xor(', A, ',', '[', B, ',', C, ']', ')'], Meta)
	.
translate_single_constraint(xor_split(A,[B,C]), Options, procedural, negatives, Result, Meta) :-
	( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	, atomics_to_string([
			'%%% negative_1: xor(', A, ',', '[', B, ',', C, ']', ')\n'
			, 'ABD(event(', A, '), T1)'
			, ' ---> '
			, 'ABD(event(', B, '),T2)'
			, ' /\\ T2>T1 /\\ T2<', TimeLimit, '\n'
			, '\t/\\ ABD(event(', C, '),T3)'
			, ' /\\ T3>T1 /\\ T3<', TimeLimit
			, ' /\\ ABD(nic(0),0)'
			, '.\n'
		], Result)
	, atomics_to_string(['negative_1:xor(', A, ',', '[', B, ',', C, ']', ')'], Meta)
	.
translate_single_constraint(xor_split(A,[B,C]), _Options, procedural, negatives, Result, Meta) :-
	atomics_to_string([
			'%%% negative_2: xor(', A, ',', '[', B, ',', C, ']', ')\n'
			, 'ABD(event(', A, '), T1)'
			, ' ---> '
			, 'ABD(nic(0),0).\n'
			, 'ABD(event(', A, '), T1)'
			, ' /\\ ABD(event(', B, '),T2)'
			, ' /\\ ABD(event(', C, '),T3)'
			, ' /\\ T2>T1 /\\ T3>T1 ---> fail.\n'
		], Result)
	, atomics_to_string(['negative_2:xor(', A, ',', '[', B, ',', C, ']', ')'], Meta)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COEXISTENCE(A,B)
translate_single_constraint(coexistence(A,B), Options, declarative, positives, Result, Meta) :-
	( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	, atomics_to_string([
			'%%% positive: coexistence(', A, ',', B, ')\n'
			, 'ABD(event(', A, '), T1)'
			, ' ---> '
			, 'E(event(', B, '),T2) /\\ T2>0 /\\ T2<', TimeLimit
			, '\t\\/ ABD(event(', B, '),T2) /\\ T2>0 /\\ T2<', TimeLimit, '.\n'
			, 'ABD(event(', B, '), T1)'
			, ' ---> '
			, 'E(event(', A, '),T2) /\\ T2>0 /\\ T2<', TimeLimit
			, '\t\\/ ABD(event(', A, '),T2) /\\ T2>0 /\\ T2<', TimeLimit, '.\n'
		], Result)
	, atomics_to_string(['positive:coexistence(', A, ',', B, ')'], Meta)
	.
translate_single_constraint(coexistence(A,B), Options, declarative, negatives, Result, Meta) :-
	( (member(time_limit(TimeLimit), Options)) -> true ; TimeLimit = 100)
	, atomics_to_string([
			'%%% negative: coexistence(', A, ',', B, ')\n'
			, 'ABD(event(', A, '), T1) ---> ABD(nic(0),0).\n'
			, 'ABD(event(', B, '), T1) ---> ABD(nic(0),0).\n'
			, 'ABD(event(', A, '), T1)', '/\\ ABD(event(', B, '),T2)'
			, ' ---> fail.\n'
		], Result)
	, atomics_to_string(['negative:coexistence(', A, ',', B, ')'], Meta)
	.







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOKB
generate_sokb(_DeclareModel, _Options, _ProcDecl, _PosNeg, Result) :-
	atomics_to_string([
			':- dynamic(fdet/1).\n'
			, ':- dynamic(society_goal/0).\n\n'
			, 'fdet(_).\n'
			, 'society_goal :- true.\n'
		], Result)
	.

write_max_trace_length_option_in_SOKB(Options, Result) :-
	( member(trace_max_length(TML), Options) ->
		atomics_to_string([
			'trace_max_length('
			, TML
			, ')\n'
			, ', current_trace_max_length(0)\n'
		], Result)
	;
		amotics_to_string(['true'], Result)
	).

write_start_event(_Options, _ProcDecl, _PosNeg, Result) :-
	atomics_to_string([
		'true'
	], Result)
	.
	




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



generate_trace_and_print_single_events(Stream, PosNeg, Options) :-
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
	%% generate a trace
		, generate_trace_limited(Trace, PathLimit, TimeLimit)
	%% a negative trace is valid only if it contains a nic abducible
		, (	(PosNeg = negatives) -> member(abd([_,_],nic(0),0), Trace) ; true )
	%% remove the nic abducible
		, delete(Trace, abd([_,_],nic(0),0), TraceCleaned)
	%% check if the trace is not empty
		, TraceCleaned = [_|_]
	%% if all the checks are passed, let us proceed to print it
	%% establish a new trace id
		, counter(Counter)
		, retractall(counter(_))
		, NewCounter is Counter + 1
		, asserta(counter(NewCounter))
	%% print the trace
		, ( member(output(xes), Options) ->
				atomics_to_string([
					'\t<trace>\n'
    				, '\t\t<string key="concept:name" value="', NewCounter, '"></string>\n'
				], TraceHeader)
				, write(Stream, TraceHeader)
			;
				true
		)
		, ( member(output(xes), Options) ->
				print_events_to_file(Stream, xes, TraceCleaned, NewCounter)
			;
				print_events_to_file(Stream, txt, TraceCleaned, NewCounter)
		)
		, ( member(output(xes), Options) ->
				atomics_to_string(['\t</trace>\n'], TraceFooter)
				, write(Stream, TraceFooter)
			;
				true
		)
		, flush_output(Stream)
	%% backtracking
	, fail
	.


% generate traces in a limited number for each possible pathway defined in the Model
% the aim is to provide at least Limit traces for each possible path, so that all the Model
% is covered...
generate_trace_limited(Trace, PathLimit, _TimeLimit) :-
	run
	, findall_constraints_nsquare(abd([_,_],X,T),L)
	, findall_constraints_nsquare(h([_,_],X,T),LL)
	, append(L, LL, LLL)
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
	, reverse(List_of_vars, Reversed)
	, retractall(pathCounter(_))
	, asserta(pathCounter(0))
	, all_distinct(Reversed)
	, labeling([], Reversed)
	, pathCounter(PathCounter)
	, retractall(pathCounter(_))
	, NewPathCounter is PathCounter + 1
	, asserta(pathCounter(NewPathCounter))
	, (
		(NewPathCounter >= Limit) ->
			!
		;
			true
	)
	.

stampa_domains([]).
stampa_domains([H|T]) :-
	fd_dom(H, Dom)
	, write(H), write(' :' ), write(Dom), nl
	, stampa_domains(T)
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
extract_variables([abd([_,_],event(Event),Timestamp)|T], Result) :-
	!
	, term_variables(Event, Result1)
	, Result2 = [Timestamp|Result1]
	, extract_variables(T, TResult)
	, append(Result2, TResult, Result)
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
	% , (sort( [N,2],@=<, Trace, TraceSorted) ->
	, (sort( [N,1],@=<, Trace, TraceSorted) ->
		print_single_trace(Stream, TraceSorted)
		;
		print_single_trace(Stream, Trace)
	)
	, write(Stream, ']).'), nl(Stream)
	, flush_output(Stream)
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
	write(Stream, h(Event,Timestamp))
	, flush_output(Stream)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print single events to file
print_events_to_file(Stream, txt, Trace, TraceId) :-
	TraceSet=Trace
	, determine_arity(TraceSet, N)
	, (sort( [N,1],@=<, TraceSet, TraceSorted) ->
		print_single_events(Stream, TraceSorted, TraceId)
		;
		print_single_events(Stream, TraceSet, TraceId)
	)
	%  , write(Stream, t(TraceId,end_of_the_world) ) , write(Stream, '.') %%MOD BY DANIELA 10/03/2017
    %%, write(Stream, ']).')
	, nl(Stream)
	, flush_output(Stream)
	.

print_single_events(_Stream, [], _TraceId) :-
	!.
print_single_events(Stream, [abd(_,Event,Timestamp) |T], TraceId) :-
	write(Stream, t(TraceId,h(Event,Timestamp)) ) %write(Stream, h(Event,Timestamp) ) 
	, write(Stream, '.') %, nl(Stream)
	, flush_output(Stream)
	, print_single_events(Stream, T, TraceId).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% print single events to XES format
print_events_to_file(Stream, xes, Trace, TraceId) :-
	TraceSet=Trace
	, determine_arity(TraceSet, N)
	, (sort( 3, @=<, TraceSet, TraceSorted) ->
		print_single_events_xes(Stream, TraceSorted, TraceId)
		;
		print_single_events_xes(Stream, TraceSet, TraceId)
	)
	%  , write(Stream, t(TraceId,end_of_the_world) ) , write(Stream, '.') %%MOD BY DANIELA 10/03/2017
    %%, write(Stream, ']).')
	, flush_output(Stream)
	.

print_single_events_xes(_Stream, [], _TraceId) :-
	!.
print_single_events_xes(Stream, [abd(_,event(Event),Timestamp) |T], TraceId) :-
	get_time(CurrentTime)
	, CurrentTimestamp is CurrentTime + Timestamp 
	, format_time(string(StampString), '%FT%T.000%:z', CurrentTimestamp)
	, atomics_to_string([
		'\t\t<event>\n'
      	, '\t\t\t<string key="concept:name" value="', Event, '"></string>\n'
      	, '\t\t\t<date key="time:timestamp" value="', StampString, '"></date>\n'
    	, '\t\t</event>\n'
	], XMLEvent)
	, write(Stream, XMLEvent)
	, flush_output(Stream)
	, print_single_events_xes(Stream, T, TraceId)
	.



