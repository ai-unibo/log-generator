%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This parser translate an intermediate internal notation of YAWL into
% a sciff program.
%
% This parser supports also the concept of observable, partially observable,
% never observable activities.
%
% The parser supports both atomic activites as well as activities with duration
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module('AlpBPM_parser', [create_project/6, create_project/7, create_project/9] ).


:- use_module(library(lists)).
:- use_module(library(memfile)).

:- ['AlpBPM_mapping_seq'].
:- ['AlpBPM_mapping_xor_split'].
:- ['AlpBPM_mapping_xor_join'].
:- ['AlpBPM_mapping_and_split'].
:- ['AlpBPM_mapping_and_join'].

:- ['AlpBPM_mapping_duration'].
:- ['AlpBPM_mapping_interTask_constraint'].






	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create_project(Flow, Obs, History, Options) :-
	% create_project(Flow, Obs, [], History, Options, './temp/').
create_project(Flow, Obs, DurationConstraints, InterTaskConstraints, History, Options) :-
	create_project(Flow, Obs, DurationConstraints, InterTaskConstraints, History, Options, './temp/').
create_project(Flow, Obs, DurationConstraints, InterTaskConstraints, History, Options, Path) :-
	% write ICS
		atom_concat(Path, 'ics.txt', FileICS),
		open(FileICS, write, StreamICS),
		translate(Flow, Obs, DurationConstraints, StreamICS, Options),
		% get_activity_list(Flow, List_of_activities),
		get_activity_list(Flow, DurationConstraints, InterTaskConstraints, List_of_activities),
		translate_duration(List_of_activities, DurationConstraints, Options, StreamICS),
		translate_interTask_constraint(InterTaskConstraints, Options, StreamICS),
		close(StreamICS),
	% write history
		atom_concat(Path, 'history.txt', FileHistory),
		open(FileHistory, write, StreamHistory),
		write_hap_to_file([], StreamHistory),
		close(StreamHistory),
	% write sokb
		atom_concat(Path, 'sokb.pl', FileSOKB),
		open(FileSOKB, write, StreamSOKB),
		write_sokb(StreamSOKB, History, Flow, Obs, Options),
		close(StreamSOKB).
%%  The following creates project only in memory, without writing anything to disk
create_project(Flow, Obs, DurationConstraints, InterTaskConstraints, History, Options, ICS_Handle, History_Handle, SOKB_Handle) :-
	% write ICS
		open_memory_file(ICS_Handle, write, StreamICS),
		translate(Flow, Obs, DurationConstraints, StreamICS, Options),
		% get_activity_list(Flow, List_of_activities),
		get_activity_list(Flow, DurationConstraints, InterTaskConstraints, List_of_activities),
		translate_duration(List_of_activities, DurationConstraints, Options, StreamICS),
		translate_interTask_constraint(InterTaskConstraints, Options, StreamICS),
		close(StreamICS),
	% write history
		open_memory_file(History_Handle, write, StreamHistory),
		write_hap_to_file([], StreamHistory), %%% perche' c'e' una lista vuota..? da indagare...
		close(StreamHistory),
	% write sokb
		open_memory_file(SOKB_Handle, write, StreamSOKB),
		write_sokb(StreamSOKB, History, Flow, Obs, Options),
		close(StreamSOKB).





verify_options([]).
verify_options([H|T]) :-
	verify_single_option(H),
	verify_options(T).
verify_single_option(path_max_length(Max)) :-
	!,
	(integer(Max) ->
		true
		;
		write('ERROR: Max in path_max_length(Max) is not an integer...'), nl, fail
	).
verify_single_option(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% translate/3 receives as input a list of activities, network relations, and observability properties
% produces a sciff representation, by writing it directly to a stream
% translate(Network_list, Observability_list, Stream)

translate(Network, ObservabilityList, DurationConstraints, Stream) :-
	translate(Network, ObservabilityList, DurationConstraints, Stream, []).

translate(Network, ObservabilityList, DurationConstraints, Stream, Options):-
	% check_flow(Network, ObservabilityList),
	write_common_ics(Stream, Options)
	, filter_network(Network, FilteredNetwork)
	, translate_flow(FilteredNetwork, ObservabilityList, DurationConstraints, Options, Stream).

%%  translate_flow(FilteredNetwork, ObservabilityList, DurationConstraints, Options, Stream)
translate_flow([], _, _, _, _).
translate_flow([Head|Tail], Obs, DurationConstraints, Options, Stream) :-
	translate_single(Head, Obs, DurationConstraints, Options, Stream),
	nl(Stream),
	translate_flow(Tail, Obs, DurationConstraints, Options, Stream).



translate_single(seq(A,B), Obs, DurationConstraints, Options, Stream) :-
	!,
	write(Stream, '% Translation of: '), write(Stream, seq(A,B)), nl(Stream),
	translate_seq(A,B, Obs, DurationConstraints, Options, Stream).
translate_single(xor_split(A,BList), Obs, Durations, Options, Stream) :-
	!,
	write(Stream, '% Translation of: '), write(Stream, xor_split(A,BList)), nl(Stream),
	translate_xor_split(A,BList, Obs, Durations, Options, Stream).
translate_single(xor_join(AList,B), Obs, DurationConstraints, Options, Stream) :-
	!,
	write(Stream, '% Translation of: '), write(Stream, xor_join(AList,B)), nl(Stream),
	translate_xor_join(AList, B, Obs, DurationConstraints, Options, Stream), nl(Stream).
translate_single(and_split(A,BList), Obs, DurationConstraints, Options, Stream) :-
	!,
	write(Stream, '% Translation of: '), write(Stream, and_split(A,BList)), nl(Stream),
	translate_and_split(A,BList, Obs, DurationConstraints, Options, Stream), nl(Stream), nl(Stream).
translate_single(and_join(AList,B), Obs, DurationConstraints, Options, Stream) :-
	!,
	write(Stream, '% Translation of: '), write(Stream, and_join(AList,B)), nl(Stream),
	translate_and_join(AList, B, Obs, DurationConstraints, Options, Stream), nl(Stream), nl(Stream).
translate_single(X, _Obs, _DurationConstraints,  _Options, Stream) :-
	write('Warning: element '), write(X), write(' is not supported!'), nl,
	write(Stream, '% Warning: element '), write(Stream, X), write(Stream, ' is not supported!'), nl(Stream).




% translate_single(xor_join(AList,B), _Obs, Stream) :-
	% !,
	% write(Stream, '% Translation of: '), write(Stream, xor_join(AList,B)), nl(Stream),
	% write(Stream, '% No Op.'), nl(Stream), nl(Stream).
% translate_single(and_split(A,BList), Obs, Stream) :-
	% !,
	% write(Stream, '% Translation of: '), write(Stream, and_split(A,BList)), nl(Stream),
	% translate_and_split(A,BList, Obs, Stream), nl(Stream), nl(Stream).
% translate_single(and_join(AList,B), Obs, Stream) :-
	% !,
	% write(Stream, '% Translation of: '), write(Stream, and_join(AList,B)), nl(Stream),
	% translate_and_join(AList, B, Obs, Stream), nl(Stream), nl(Stream).


	
write_common_ics(_Stream, []) :-
	% write(Stream, '% Explicit negation for abducibles:'), nl(Stream),
	% write(Stream, 'ABD(event(X, LPos), _) /\\ ABD(nevent(X, LPos), _) ---> fail.'), nl(Stream), nl(Stream),
	% write(Stream, '% Forcing oredering on the positions:'), nl(Stream),
	% write(Stream, 'H(event(_X1, Pos1), T1) /\\ H(event(_X2, [Pos2|LPos]), T2) /\\ ground(T1) /\\ ground(T2) /\\ T2>T1 ---> Pos2>Pos1.'), nl(Stream), nl(Stream),
	% write(Stream, '% Forcing deterministic fulfilling:'), nl(Stream),
	% write(Stream, 'H(event(X, Pos), T) ---> EN(event(X, Pos), T2) /\\ T2<>T.'), nl(Stream), nl(Stream),
	%%  nl(Stream), nl(Stream), nl(Stream).
	!
	, true.
write_common_ics(Stream, [H|T]) :-
	write_common_ics_option(Stream, H),
	write_common_ics(Stream, T).
write_common_ics_option(Stream, path_max_length(M)) :-
	!,
	atomic_list_concat( [ '% ', 'path_max_length(', M, ')', ' option specified. Encoding:' ], Message),
	write(Stream, Message), nl(Stream),
	write(Stream, 'ABD(event(_,Pos), T) ---> Pos<'), write(Stream, M), write(Stream, ' /\\ Pos>=0'), write(Stream, ' /\\ T>=0'), write(Stream, '.'),
	nl(Stream),
	write(Stream, 'H(event(_,Pos), T) ---> Pos<'), write(Stream, M), write(Stream, ' /\\ Pos>=0'), write(Stream, ' /\\ T>=0'), write(Stream, '.'),
	nl(Stream),
	write(Stream, 'E(event(_,Pos), T) ---> Pos<'), write(Stream, M), write(Stream, ' /\\ Pos>=0'), write(Stream, ' /\\ T>=0'), write(Stream, '.'),
	nl(Stream),
	nl(Stream).
write_common_ics_option(_, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Checks about the network and the observability list

% check_flow(Network, ObservabilityList),

filter_network(Network, FilteredNetwork) :-
	filter_network(Network, Network, FilteredNetwork).
filter_network([], _, []).
filter_network([seq(A,B)|TailNetwork], Original, FilteredNetwork) :-
	member(and_join(List, B), Original),
	member(A, List),
	!,
	filter_network(TailNetwork, Original, FilteredNetwork).
filter_network([H|TailNetwork], Original, [H|FilteredNetwork]) :-
	filter_network(TailNetwork, Original, FilteredNetwork).



	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% History utilities
write_hap_to_file([], _).
write_hap_to_file([H|T], Stream) :-
	write(Stream, H), write(Stream, '.'), nl(Stream),
	write_hap_to_file(T, Stream).


	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOKB utilities
	
write_sokb(Stream, Trace, Model, Obs, Options) :-
	%%  write(Stream, 'fdet(_):-fail.'), nl(Stream),
	write(Stream, ':- dynamic(fdet/1).'), nl(Stream),
	write(Stream, ':- dynamic(society_goal/0).'), nl(Stream), nl(Stream),
	write(Stream, 'fdet(_).'), nl(Stream),
	write(Stream, 'society_goal :-'), nl(Stream),
	write_max_trace_length_option_in_SOKB(Stream, Options),
	write_opt_solution_option_in_SOKB(Stream, Options),
	write_observed_events_are_ordered(Stream, Trace, Options),
	write(Stream, ', abd(event(start,0),0)'), nl(Stream),
	from_hap_to_h(Trace,HTrace),
	write_history_in_sokb(Stream, HTrace, Model, Obs, Options),
	write(Stream,'.'),
	nl(Stream), nl(Stream), nl(Stream).

	
write_max_trace_length_option_in_SOKB(Stream, Options) :-	
	( member(trace_max_length(TML), Options) ->
		% write(Stream, ', '),
		write(Stream,trace_max_length(TML)), nl(Stream),
		write(Stream, ', '), write(Stream,current_trace_max_length(0)), nl(Stream)
	;
		write(Stream, 'true')
	).

write_opt_solution_option_in_SOKB(Stream, Options) :-
	( (member(opt_solution, Options)) ->
		write(Stream, ', '), write(Stream, opt_solution),  nl(Stream)
	;
		true
	).
	
write_observed_events_are_ordered(Stream, Trace, Options) :-
	(member(observed_events_are_ordered, Options) ->
		write('Eureka'),
		write(Trace),
		extract_time_variables(Trace, ListOfTimes),
		write(ListOfTimes),
		impose_time_ordering(Stream, ListOfTimes)
	;
		true
	).

% write_history_in_sokb(Stream,Trace, Model, Obs, Options)
write_history_in_sokb(_Stream,[], _Model, _Obs, _Options) :- !.
write_history_in_sokb(Stream,Trace, Model, Obs, Options) :-
	write(Stream, ', set_term_quantification('), write(Stream,Trace), write(Stream, ', existsf)'), nl(Stream),
	extract_time_variables(Trace, TimeList),
	write(Stream, ', '), write(Stream, 'my_all_distinct('), write(Stream, TimeList), write(Stream, ')'), nl(Stream),
	
	% path_max_length option
	( member(path_max_length(M), Options) ->
		Max is M-2,
		extract_pos_variables(Trace, PosList),
		write(Stream, ', '), write(Stream, PosList), write(Stream, ' ins 1..'), write(Stream, Max), nl(Stream)
	;
		true
	),
	% filter_out_never_observable(Obs, ObsFiltered),
	candidates(Model, Obs, Trace, Candidates),
	write(Stream, ', '), write(Stream, 'call_smart_list('), write(Stream, Trace), write(Stream, ', '), write(Stream, Candidates), write(Stream,')').

	
	
candidates(Model, Observability, History, Candidates) :-
	get_activity_list(Model, LA)
	, find_activities_in_loops(Model, InLoop, NotInLoop)
	, write(LA), nl
	, write(InLoop), nl
	, write(NotInLoop), nl
	, filter_history(History, HistoryFiltered)
	, write(HistoryFiltered), nl
	, candidates(LA, Model, Observability, HistoryFiltered, InLoop, NotInLoop, Candidates).
candidates([], _Model, _Observability, _History, _InLoop, _NotInLoop, []).
candidates([HA|TA], Model, Observability, History, InLoop, NotInLoop, [HA|Tail]) :-
	(
		member(obs(HA,partially), Observability)
		;
		member(obs(HA,always), Observability)
	)
	, (
		member(HA, InLoop)
		;
		member(HA, NotInLoop), \+member(HA, History)
	)
	, !
	, candidates(TA, Model, Observability, History, InLoop, NotInLoop, Tail).
candidates([_HA|TA], Model, Observability, History, InLoop, NotInLoop, Result) :-
	candidates(TA, Model, Observability, History, InLoop, NotInLoop, Result).

filter_history([], []).	
filter_history([h(event(Activity,_Pos),_T) | Tail], RTail) :-
	var(Activity),
	!,
	filter_history(Tail, RTail).
filter_history([h(event(Activity,_Pos),_T)|Tail], [Activity|RTail]) :-
	filter_history(Tail, RTail).

filter_out_never_observable([], []).
filter_out_never_observable([obs(_A,never)|T], Tr) :-
	!,
	filter_out_never_observable(T, Tr).
filter_out_never_observable([H|T], [H|Tr]) :-
	filter_out_never_observable(T, Tr).
	
from_hap_to_h([],[]).
from_hap_to_h([hap(event(Ev,LPos), Time)|Tail],[h(event(Ev,LPos), Time)|TailResult]) :-
	!,
	from_hap_to_h(Tail, TailResult).
from_hap_to_h([H|Tail],[H|TailResult]) :-
	from_hap_to_h(Tail, TailResult).
	
extract_pos_variables([], []).
extract_pos_variables([h(event(_,Pos),_)|T], [Pos|Tr]):-
	extract_pos_variables(T,Tr).

	
extract_time_variables([], []).
extract_time_variables([h(event(_,_),Time)|Tail], [Time|Tr]):-
	extract_time_variables(Tail,Tr).
extract_time_variables([hap(event(_,_),Time)|Tail], [Time|Tr]):-
	extract_time_variables(Tail,Tr).
	
impose_time_ordering(_Stream, [_X]).
impose_time_ordering(Stream, [X1,X2|Tail]) :-
	write(Stream, ','), write(Stream, X1), write(Stream, '#<'), write(Stream, X2),
	impose_time_ordering(Stream, [X2|Tail]).