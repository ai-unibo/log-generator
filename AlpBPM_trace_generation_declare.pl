:- module('AlpBPM_trace_generation_declare', [
									trace_generation_declare/4
									, test_declare/0
									, test_vasyl_loan/0
									%% , trace_generation/6,
									%% generate_trace_limited/3,
									%%  generate_trace_and_print_single_trace/2,
									%%  generate_trace_and_print_single_events/2,
									% test_trace_generation/0,
									% test_trace_generation/1
									%%  , test/0
									]).

:- use_module(library(memfile)).
:- use_module(ics_parser).
:- use_module(sokb_parser).
:- use_module(history_parser).
:- use_module(library(http/http_open)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRACE GENERATION
%%  
%%  trace_generation(DeclareModel, ActivityList, FileNameForSavingTraces, Options, 
trace_generation_declare(DeclareModel
				, ActivityList
				, FileNameForSavingTraces
				, Options) :-
	%% prepare the meory files to write the sciff version of the model
		new_memory_file(ICS_Handle)
		, new_memory_file(History_Handle)
		, new_memory_file(SOKB_Handle)
	%% compile the model and create a sciff version in memory files
		, create_declare_project(DeclareModel, ActivityList, Options, ICS_Handle, History_Handle, SOKB_Handle)
	%% compile the sciff project
		, translate_mem_to_mem(ICS_Handle, History_Handle, SOKB_Handle, [ [allow_events_not_expected,no], [coloring,yes] ])
	%% for debug purposes
		, memory_file_to_string(ICS_Handle, TheString)
		, write(TheString)
	%% let us generate the trace
		, write('Trace Generation Reasoning Task'), nl
		, statistics(runtime,_)
		, open(FileNameForSavingTraces, write, Stream)
		, ( generate_trace_and_print_single_events(Stream, Options) -> fail ; true )
		, close(Stream)
		, statistics(runtime,[_,Time])
		, write('Required time to generate the traces: '), write(Time), write('ms.'), nl
	.




%%  The following creates project only in memory, without writing anything to disk
create_declare_project(DeclareModel, ActivityList, Options, ICS_Handle, History_Handle, SOKB_Handle) :-
	% write ICS
		open_memory_file(ICS_Handle, write, StreamICS),
		translate_declare_model(DeclareModel, ActivityList, StreamICS, Options),
		close(StreamICS),
	% write history
		open_memory_file(History_Handle, write, StreamHistory),
		% write_hap_to_file([], StreamHistory), %%% perche' c'e' una lista vuota..? da indagare...
		close(StreamHistory),
	% write sokb
		open_memory_file(SOKB_Handle, write, StreamSOKB),
		write_declare_sokb(StreamSOKB, DeclareModel, Options),
		close(StreamSOKB).



translate_declare_model(DeclareModel, ActivityList, StreamICS, _Options) :-
	%% translate the activity list
		translate_declare_activity_list(ActivityList, StreamICS)
	%% translate the Declare model
		, translate_declare_model(DeclareModel, StreamICS)
		.




% tranlate_declare_activity_list(ActivityList, StreamICS)
translate_declare_activity_list( [], _StreamICS).
translate_declare_activity_list( [H | T], StreamICS) :-
	write(StreamICS, 'true ---> ')
	, write(StreamICS, 'true \\/ ')
	, write(StreamICS, 'ABD'(event(H),'T'))
	, write(StreamICS, ' /\\ T>0 /\\ T<11')
	, write(StreamICS, '.')
	, nl(StreamICS)
	, write(StreamICS, 'ABD'(event(H),'T'))
	, write(StreamICS, ' ---> true \\/ ')
	, rename_term_variables(H, H1)
	, write(StreamICS, 'ABD'(event(H1),'T1'))
	, write(StreamICS, ' /\\ T1>T /\\ T1<11.')
	, nl(StreamICS)
	, nl(StreamICS)
	, translate_declare_activity_list( T, StreamICS).

rename_term_variables(H, H1) :-
	H =.. [Term|Params]
	, rename_variables(Params, Params1)
	, H1 =.. [Term|Params1]
	.
rename_variables([], []).
rename_variables([H|T], [H1|T1]) :-
	atom_concat(H, '_1', H1)
	, rename_variables(T,T1)
	.



translate_declare_model([], _StreamICS).
translate_declare_model([H|T], StreamICS) :-
	translate_single_constraint(H, StreamICS)
	, translate_declare_model(T, StreamICS)
	.

translate_single_constraint(response(A,B), StreamICS) :-
	write(StreamICS, '%%% response('), write(StreamICS, A), write(StreamICS, ','), write(StreamICS, B), write(StreamICS, ')'), nl(StreamICS)
	, write(StreamICS, 'ABD'(event(A),'T1'))
	, write(StreamICS, ' ---> ')
	, write(StreamICS, 'ABD'(event(B),'T2'))
	, write(StreamICS, ' /\\ T2>T1 /\\ T2<11')
	, write(StreamICS, '.')
	.
translate_single_constraint(response(A,B,LCA,LCB), StreamICS) :-
	write(StreamICS, '%%% response(')
	, write(StreamICS, A), write(StreamICS, ',')
	, write(StreamICS, B), write(StreamICS, ',')
	, write(StreamICS, LCA), write(StreamICS, ',')
	, write(StreamICS, LCB), write(StreamICS, ')'), nl(StreamICS)
	, write(StreamICS, 'ABD'(event(A),'T1'))
	, write_constraint_list(StreamICS, LCA)
	, write(StreamICS, ' ---> ')
	, write(StreamICS, 'ABD'(event(B),'T2'))
	, write_constraint_list(StreamICS, LCB)
	, write(StreamICS, ' /\\ T2>T1 /\\ T2<11')
	, write(StreamICS, '.')
	, nl(StreamICS), nl(StreamICS)
	.
% translate_single_constraint(absence(Event, 2), StreamICS) :-
% 	write(StreamICS, '%%% absence(')
% 	, write(StreamICS, Event), write(StreamICS, ',')
% 	, write(StreamICS, '2')
% 	, write(StreamICS, ')'), nl(StreamICS)
% 	, write(StreamICS, 'ABD'(event(Event),'T1'))
% 	, write(StreamICS, ' /\\ ')
% 	, rename_term_variables(Event, Event2)
% 	, write(StreamICS, 'ABD'(event(Event2),'T2'))
% 	, write(StreamICS, ' /\\ ')
% 	, rename_term_variables(Event2, Event3)
% 	, write(StreamICS, 'ABD'(event(Event3),'T3'))
% 	, write(StreamICS, ' /\\ T1<>T2  /\\ T2<>T3  /\\ T1<>T3 ')
% 	, write(StreamICS, ' ---> ')
% 	, write(StreamICS, 'fail')
% 	, write(StreamICS, '.')
% 	, nl(StreamICS), nl(StreamICS)
% 	.
translate_single_constraint(absence(Event, 2), StreamICS) :-
	write(StreamICS, '%%% absence(')
	, write(StreamICS, Event), write(StreamICS, ',')
	, write(StreamICS, '2')
	, write(StreamICS, ')'), nl(StreamICS)
	, write(StreamICS, 'ABD'(event(Event),'T1'))
	, write(StreamICS, ' /\\ ')
	, rename_term_variables(Event, Event2)
	, write(StreamICS, 'ABD'(event(Event2),'T2'))
	, write(StreamICS, ' /\\ T1<>T2 ')
	, write(StreamICS, ' ---> ')
	, rename_term_variables(Event2, Event3)
	, write(StreamICS, 'EN'(event(Event3),'T3'))
	, write(StreamICS, ' /\\ T3>T2 /\\ T3>T1 ')
	, write(StreamICS, '.')
	, nl(StreamICS), nl(StreamICS)
	.



write_constraint_list(_StreamICS, []).
write_constraint_list(StreamICS, [H|T]) :-
	write(StreamICS, ' /\\ ')
	, write(StreamICS, H)
	, write_constraint_list(StreamICS, T)
	.






test_declare :-
	DeclareModel = [response(a,b), response(b,c)]
	, ActivityList = [a,b,c]
	, Options = [trace_max_length(5), time_limit(5), instances_for_each_path(0) ]
	, FileNameForSavingTraces = 'daniela.txt'
	, write('DeclareModel: '), write(DeclareModel), nl
	, write('ActivityList: '), write(ActivityList), nl
	, write('Options: '), write(Options), nl
	, write('Saving to file: '), write(FileNameForSavingTraces), nl
	, trace_generation_declare(DeclareModel
				, ActivityList
				, FileNameForSavingTraces
				, Options
	)
	, nl, nl.


test_vasyl_loan :-
	DeclareModel = [
						response(
							submitLoanApplication('Salary','Amount'), assessApplication('AssessmentType','AssessmentCost')
							, ['Salary<=24', 'Amount>5'], ['AssessmentType==1', 'AssessmentCost>1']
						)
						, absence(submitLoanApplication('Salary','Amount'), 2)
						, absence(assessApplication('AssessmentType','AssessmentCost'), 2)
					]
	, ActivityList = [	
						submitLoanApplication('Salary','Amount')
						, assessApplication('AssessmentType','AssessmentCost')
						% , checkCareer, checkMedicalHistory, notifyOutcome
					]
	, Options = [trace_max_length(5), time_limit(5), instances_for_each_path(1) ]
	, FileNameForSavingTraces = 'daniela.txt'
	, write('DeclareModel: '), write(DeclareModel), nl
	, write('ActivityList: '), write(ActivityList), nl
	, write('Options: '), write(Options), nl
	, write('Saving to file: '), write(FileNameForSavingTraces), nl
	, trace_generation_declare(DeclareModel
				, ActivityList
				, FileNameForSavingTraces
				, Options
	)
	, nl, nl.

write_declare_sokb(Stream, _DeclareModel, Options) :-
	write(Stream, ':- dynamic(fdet/1).'), nl(Stream)
	, write(Stream, ':- dynamic(society_goal/0).'), nl(Stream), nl(Stream)
	, write(Stream, 'fdet(_).'), nl(Stream)
	, write(Stream, 'society_goal :-'), nl(Stream)
	, write_max_trace_length_option_in_SOKB(Stream, Options)
	%, write_opt_solution_option_in_SOKB(Stream, Options),
	% write(Stream, ', abd(event(start,0),0)'), nl(Stream),
	% write_history_in_sokb(Stream, HTrace, Model, Obs, Options),
	, write(Stream,'.')
	, nl(Stream), nl(Stream), nl(Stream).

	
write_max_trace_length_option_in_SOKB(Stream, Options) :-
	( member(trace_max_length(TML), Options) ->
		% write(Stream, ', '),
		write(Stream,trace_max_length(TML)), nl(Stream),
		write(Stream, ', '), write(Stream,current_trace_max_length(0)), nl(Stream)
	;
		write(Stream, 'true')
	).

% write_opt_solution_option_in_SOKB(Stream, Options) :-
% 	( (member(opt_solution, Options)) ->
% 		write(Stream, ', '), write(Stream, opt_solution),  nl(Stream)
% 	;
% 		true
% 	).
	







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
	, flush_output(Stream)
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
	, flush_output(Stream)
	, fail
	.


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
	% rimosso perchè in declare non c'e'il concetto di ultimo evento...
	% questo vuol dire che il time_limit è da imporre su tutte le variabili...
	% , member(abd([_,_],event(stop, _),Ts), LLL)
	% , (
	% 	(TimeLimit > 0) ->
	% 		Ts #< TimeLimit 
	% 	;
	% 		true
	% )
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
	, Reversed ins 0..25
	% , labeling([ff, bisect, min(Ts)], Reversed)
	, stampa_domains(Reversed)
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
	, constraints_closure(H,C), write(C), nl, nl
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
% debugging:
% extract_variables([abd([_,_],event(assessApplication(Salary,Amount)),Timestamp)|T], [Timestamp,Salary,Amount|TResult]) :-
% 	var(Timestamp)
% 	, var(Salary)
% 	, var(Amount)
% 	, !
% 	, extract_variables(T, TResult)
% 	.
% extract_variables([abd([_,_],event(submitLoanApplication(Salary,Amount)),Timestamp)|T], [Timestamp,Salary,Amount|TResult]) :-
% 	var(Timestamp)
% 	, var(Salary)
% 	, var(Amount)
% 	, !
% 	, extract_variables(T, TResult)
% 	.
% extract_variables([abd([_,_],event(_Event),Timestamp)|T], [Timestamp|TResult]) :-
% 	var(Timestamp)
% 	, !
% 	, extract_variables(T, TResult)
% 	.
extract_variables([abd([_,_],event(Event),Timestamp)|T], Result) :-
	!
	, term_variables(Event, Result1)
	, ( (var(Timestamp)) ->
			Result2 = [Timestamp|Result1]
			;
			Result2 = Result1
	)
	% , var(Timestamp)
	% , !
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
print_events_to_file(Stream, Trace, TraceId) :-
	list_to_set(Trace, TraceSet)
	, determine_arity(TraceSet, N)
	% , (sort( [N,2],@=<, Trace, TraceSorted) ->
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




