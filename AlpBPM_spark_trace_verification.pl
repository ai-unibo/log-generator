:- module('AlpBPM_spark_trace_verification', [
									test_spark/0
									, test_spark/1
									, create_spark_project/5
									, spark_trace_verification/1
									%%  , spark_trace_verification_from_stdin/0
									, spark_trace_verification/5
									, fdet/1
									, spark_trace_verification_standard_sciff/2
									, chesio/0
									]).


:- dynamic(buffered_history/2).
:- dynamic(acknowledge_printed/1).
:- dynamic(generate_current_time/1).


chesio :-
	spark_trace_verification_standard_sciff(
'H(event(TraceId, a_partlysubmitted,complete,_R1), T1) ---> EN(event(TraceId, a_partlysubmitted,complete,_R2), T2) /\\ T2<>T1.
H(event(TraceId, a_submitted,complete,_R1), T1) ---> EN(event(TraceId, a_submitted,complete,_R2), T2) /\\ T2<>T1.
H(event(TraceId, a_submitted,complete,_R1), T1) ---> E(event(TraceId, a_partlysubmitted,complete,_R2), T2) /\\ T2>T1
                                                            /\\ EN(event(TraceId, _Act, _Lifecyle, _R3), Tin) /\\ Tin>T1 /\\ Tin<T2.
H(event(TraceId, a_partlysubmitted,complete,_R2), T2) ---> E(event(TraceId, a_submitted,complete,_R1), T1) /\\ T2>T1
                                                            /\\ EN(event(TraceId, _Act, _Lifecyle, _R3), Tin) /\\ Tin>T1 /\\ Tin<T2.
true --->   E(event(TraceId, a_submitted,complete,_R1), T1)
            /\\ E(event(TraceId, a_partlysubmitted,complete,_R2), T2) /\\ T2>T1            
            /\\ EN(event(TraceId, _ActA, _LifecyleA, _Rbefore), Tbefore) /\\ Tbefore<T1
            /\\ EN(event(TraceId, _ActB, _LifecyleB, _Rin), Tin) /\\ Tin>T1 /\\ Tin<T2.
H(event(TraceId, w_afhandelen_leads, schedule, _R1), T1) ---> E(event(TraceId, w_afhandelen_leads, start,_R2), T2) /\\ T2>T1
                                                            /\\ EN(event(TraceId, w_afhandelen_leads, schedule, _Rin1), Tin1) /\\ Tin1>T1 /\\Tin1<T2
                                                            /\\ EN(event(TraceId, w_afhandelen_leads, start, _Rin2), Tin2) /\\ Tin2>T1 /\\Tin2<T2
                                                            /\\ EN(event(TraceId, w_afhandelen_leads, complete, _Rin3), Tin3) /\\ Tin3>T1 /\\Tin3<T2.
H(event(TraceId, w_afhandelen_leads, start, _R1), T1) ---> E(event(TraceId, w_afhandelen_leads, complete,_R2), T2) /\\ T2>T1
                                                            /\\ EN(event(TraceId, w_afhandelen_leads, schedule, _Rin1), Tin1) /\\ Tin1>T1 /\\Tin1<T2
                                                            /\\ EN(event(TraceId, w_afhandelen_leads, start, _Rin2), Tin2) /\\ Tin2>T1 /\\Tin2<T2
                                                            /\\ EN(event(TraceId, w_afhandelen_leads, complete, _Rin3), Tin3) /\\ Tin3>T1 /\\Tin3<T2.
H(event(TraceId, w_completeren_aanvraag, schedule, _R1), T1) ---> E(event(TraceId, w_completeren_aanvraag, complete, _R2), T2) /\\ T2>T1.
H(event(TraceId, w_completeren_aanvraag, complete, _R2), T2) ---> E(event(TraceId, w_completeren_aanvraag, schedule, _R1), T1) /\\ T2>T1.
H(event(TraceId, w_beoordelen_fraude, schedule, _R1), T1) ---> E(event(TraceId, w_beoordelen_fraude, start,_R2), T2) /\\ T2>T1
                                                            /\\ EN(event(TraceId, w_beoordelen_fraude, schedule, _Rin1), Tin1) /\\ Tin1>T1 /\\Tin1<T2
                                                            /\\ EN(event(TraceId, w_beoordelen_fraude, start, _Rin2), Tin2) /\\ Tin2>T1 /\\Tin2<T2
                                                            /\\ EN(event(TraceId, w_beoordelen_fraude, complete, _Rin3), Tin3) /\\ Tin3>T1 /\\Tin3<T2.
H(event(TraceId, w_beoordelen_fraude, start, _R1), T1) ---> E(event(TraceId, w_beoordelen_fraude, complete,_R2), T2) /\\ T2>T1
                                                            /\\ EN(event(TraceId, w_beoordelen_fraude, schedule, _Rin1), Tin1) /\\ Tin1>T1 /\\Tin1<T2
                                                            /\\ EN(event(TraceId, w_beoordelen_fraude, start, _Rin2), Tin2) /\\ Tin2>T1 /\\Tin2<T2
                                                            /\\ EN(event(TraceId, w_beoordelen_fraude, complete, _Rin3), Tin3) /\\ Tin3>T1 /\\Tin3<T2.
H(event(TraceId, o_selected, complete, _R1), T1) ---> E(event(TraceId, o_created, complete, _R2), T2) /\\ T2>T1.
H(event(TraceId, o_created, complete, _R1), T1) ---> E(event(TraceId, o_sent, complete, _R2), T2) /\\ T2>T1.
H(event(TraceId, o_sent, complete, _R2), T2) ---> E(event(TraceId, o_created, complete, _R1), T1) /\\ T2>T1.
H(event(TraceId, o_created, complete, _R2), T2) ---> E(event(TraceId, o_selected, complete, _R1), T1) /\\ T2>T1.
H(event(TraceId, a_cancelled, complete, _R1), _T1) ---> EN(event(TraceId, a_activated, complete, _R), _T).
H(event(TraceId, a_cancelled, complete, _R1), _T1) ---> EN(event(TraceId, a_registered, complete, _R), _T).
H(event(TraceId, a_cancelled, complete, _R1), _T1) ---> EN(event(TraceId, a_approved, complete, _R), _T).
H(event(TraceId, a_cancelled, complete, _R1), _T1) ---> EN(event(TraceId, a_declined, complete, _R), _T).
H(event(TraceId, w_beoordelen_fraude, schedule, _R1), _T1) ---> EN(event(TraceId, w_wijzigen_contractgegevens, schedule, _R), _T).
%H(event(TraceId, a_accepted, _LifeCycleA, _R1), _T1) ---> EN(event(TraceId, a_declined, _LifeCycleB, _R), _T).
H(event(TraceId, a_submitted, complete, _R1), T1) ---> E(event(TraceId, a_partlysubmitted, complete, _R2), T2) /\\ T2>T1 /\\ T2<=T1+22000.
H(event(TraceId, a_partlysubmitted, complete, _R2), T2) ---> E(event(TraceId, a_submitted, complete, _R1), T1) /\\ T2>T1 /\\ T2<=T1+22000.
H(event(TraceId, w_completeren_aanvraag, schedule, _R1), T1) ---> E(event(TraceId, w_completeren_aanvraag, complete, _R2), T2) /\\ T2>T1+22000 /\\ T2<=T1+239368000.
%H(event(TraceId, w_completeren_aanvraag, complete, _R2), T2) ---> E(event(TraceId, w_completeren_aanvraag, schedule, _R1), T1) /\\ T2>T1+22000 /\\ T2<=T1+239368000.
H(event(TraceId, a_submitted, complete, R), _T1) ---> E(event(TraceId, a_partlysubmitted, complete, R), _T2).
H(event(TraceId, a_partlysubmitted, complete, R), _T1) ---> E(event(TraceId, a_submitted, complete, R), _T2).
H(event(TraceId, a_accepted, _LifeCycleA, R), _T1) ---> E(event(TraceId, a_finalized, _LifeCycleB, R), _T2).
H(event(TraceId, a_finalized, _LifeCycleA, R), _T1) ---> E(event(TraceId, a_accepted, _LifeCycleB, R), _T2).
H(event(TraceId, a_submitted, _LifeCycleA, R), _T1) ---> EN(event(TraceId, a_finalized, _LifeCycleB, R), _T2).
H(event(TraceId, a_finalized, _LifeCycleA, R), _T1) ---> EN(event(TraceId, a_submitted, _LifeCycleB, R), _T2).
',
		[ trace_max_length(43), generate_current_time(no), activities_have_start_and_end, double_chained_translation, events_contain_traceId(yes) ] % , Options
	).
	% create_spark_project(
	% 	[ seq(a,b), and_join( [ a9, a10 ], a11 ) ], % Model
	% 	[obs(a,always), obs(b, always), obs(a9,always),obs(a10,always),obs(a11,always)], % , Observability
	% 	[],	% , DurationConstraints
	% 	[],  % , InterTaskConstraints
	% 	[ trace_max_length(43), generate_current_time(no), activities_have_start_and_end, double_chained_translation, events_contain_traceId(yes) ] % , Options
	% ).
	% spark_trace_verification(
	% 	[ and_join( [ a9, a10 ], a11 ) ],
	% 	[obs(a,always), obs(b, always), obs(a9,always),obs(a10,always),obs(a11,always)],
	% 	[],
	% 	[],
	% 	[ trace_max_length(43), generate_current_time(no), activities_have_start_and_end, double_chained_translation, events_contain_traceId(yes) ]
	% ).


spark_trace_verification(Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Options) :-
	new_memory_file(ICS_Handle)
	, new_memory_file(History_Handle)
	, new_memory_file(SOKB_Handle)
	, retractall(generate_current_time(_))
	, ( ( member(generate_current_time(yes), Options) ) ->
			asserta(generate_current_time(yes))
		;
			retractall(generate_current_time(_))
		)
	, create_project(Model, Observability, DurationConstraints, InterTaskConstraints, [], Options, ICS_Handle, History_Handle, SOKB_Handle)
	, translate_mem_to_mem(ICS_Handle, History_Handle, SOKB_Handle, [ [coloring,yes] ])
	, findall(ic(Head,Body), ics(Head,Body), ICSs)
    , call_list(ICSs)
    , current_time(0)
    , society_goal
	, retractall(buffered_history(_,_))
	, retractall(acknowledge_printed(_))
	, call_spark_history_from_stdin(0)
    , phase(deterministic)
    , close_history
	, !
	, writeln('Yes')
	, halt
	.
spark_trace_verification(_Model
				, _Observability
				, _DurationConstraints
				, _InterTaskConstraints
				, _Options) :-
	writeln('No')
	, halt(1)
	.



spark_trace_verification_standard_sciff(ICSList, Options) :-
	new_memory_file(ICS_Handle)
	, new_memory_file(History_Handle)
	, new_memory_file(SOKB_Handle)
	, retractall(generate_current_time(_))
	, ( ( member(generate_current_time(yes), Options) ) ->
			asserta(generate_current_time(yes))
		;
			retractall(generate_current_time(_))
		)
	, open_memory_file(ICS_Handle, write, StreamICS)
	, write(StreamICS, ICSList)
	%, write(ICSList)
	, close(StreamICS)
	, open_memory_file(SOKB_Handle, write, StreamSOKB)
		, write(StreamSOKB, ':- dynamic(fdet/1).'), nl(StreamSOKB)
		, write(StreamSOKB, ':- dynamic(society_goal/0).'), nl(StreamSOKB), nl(StreamSOKB)
		% , write(StreamSOKB, 'fdet(_).'), nl(StreamSOKB),
		, write(StreamSOKB, 'society_goal.'), nl(StreamSOKB)
	, close(StreamSOKB)
	, translate_mem_to_mem(ICS_Handle, History_Handle, SOKB_Handle, [ [coloring,yes] ])
	, findall(ic(Head,Body), ics(Head,Body), ICSs)
    , call_list(ICSs)
    , current_time(0)
    , society_goal
	, retractall(buffered_history(_,_))
	, retractall(acknowledge_printed(_))
	, call_spark_history_from_stdin(0)
    , phase(deterministic)
    , close_history
	, !
	, writeln('Yes')
	, halt
	.
spark_trace_verification_standard_sciff(_ICSList, _Options) :-
	writeln('No')
	, halt(1)
	.







:- dynamic(fdet/1).
:- multifile(fdet/1).

fdet(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% init part, due to the spark setting and needs derived from multiple
%%  sciff instances at the same time.
:- dynamic required_option/2.
%%  required_option(allow_events_not_expected,no).
required_option(coloring,yes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  create_spark_project/5
create_spark_project(
				Model
				, Observability
				, DurationConstraints
				, InterTaskConstraints
				, Options) :-
	create_project(Model, Observability, DurationConstraints, InterTaskConstraints, [], Options)
	, !
	%%  , project('temp')
	, default_dir(Dir)
	, atom_concat(Dir,'temp',Path)
	, atom_concat(Path,'/',PathSlash)
	, atom_concat(PathSlash,'project.pl',PrjFile)
	, compile(PrjFile)
	%%  , build_prj(PathSlash)
	, findall(F,ics_file(F),ICS_files), append_path(PathSlash,ICS_files,IcsPathFiles)
    , translate_ics_files(IcsPathFiles,'./ics.pl')
    , findall(F,history_file(F),Hist_files), append_path(PathSlash,Hist_files,HistPathFiles)
    , translate_histories(HistPathFiles,'./history.pl')
    , findall(F,sokb_file(F),Sokb_files), append_path(PathSlash,Sokb_files,SokbPathFiles)
    , convert_sokb(SokbPathFiles,'./sokb.pl')
    %%  , make
	%%  , spark_init
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  spark_trace_verification/1
spark_trace_verification(Trace) :-
    compile(sokb)
	, compile(history)
	, compile(ics)
	, findall([O,V],required_option(O,V),LOptions)
	, set_options(LOptions)
	%%  , load_ics
	, findall(ic(Head,Body), ics(Head,Body), ICSs)
    , call_list(ICSs)
    , current_time(0)
    , society_goal
	, set_term_quantification(Trace, existsf)
	, call_spark_history(Trace)
    , phase(deterministic)
    , close_history
	.

call_spark_history([]).
call_spark_history([H|T]) :-
	call(H)
	, call_spark_history(T).

%%  spark_trace_verification_from_stdin :-
%%      compile(sokb)
%%  	, compile(history)
%%  	, compile(ics)
%%  	, findall([O,V],required_option(O,V),LOptions)
%%  	, set_options(LOptions)
%%  	%%  , load_ics
%%  	, findall(ic(Head,Body), ics(Head,Body), ICSs)
%%      , call_list(ICSs)
%%      , current_time(0)
%%      , society_goal
%%  	%%  , set_term_quantification(Trace, existsf)
%%  	, call_spark_history_from_stdin
%%      , phase(deterministic)
%%      , close_history
%%  	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
call_spark_history_from_stdin(Pos) :-
	get_event_at_pos(Pos, H, _Source)
	, filter_event_from_stdin(Pos, H)
	, write_answer_back_to_spark(Pos, H)
	, NewPos is Pos+1
	%%  , call_spark_history_from_stdin(NewPos)
	, recursive_call_spark_history_from_stdin(NewPos, H)
	.
%%  call_spark_history_from_stdin(Pos) :-
%%  	read_term(H, [])
%%  	%%  , write('Letto: '), write(H), nl
%%  	, asserta(buffered_history(Pos,H))
%%  	, filter_event_from_stdin(Pos,H) 
%%  	.


get_event_at_pos(Pos, H, fromCache) :-
	buffered_history(Pos, H)
	, !
	.
get_event_at_pos(Pos, H, fromStdin) :-
	read_term(H, [])
	, asserta(buffered_history(Pos,H))
	.


write_answer_back_to_spark(_Pos, end_of_the_world) :-
	!.
write_answer_back_to_spark(Pos, _H) :-
	acknowledge_printed(Pos)
	, !
	.
write_answer_back_to_spark(Pos, _H) :-
	asserta(acknowledge_printed(Pos))
	, write('Ni'), nl
	.

recursive_call_spark_history_from_stdin(_NewPos, abort).
recursive_call_spark_history_from_stdin(_NewPos, end_of_the_world).
recursive_call_spark_history_from_stdin(NewPos, h(current_time, _CT)) :- !, call_spark_history_from_stdin(NewPos).
recursive_call_spark_history_from_stdin(NewPos, h(_Event, _Timestamp)) :- call_spark_history_from_stdin(NewPos).

filter_event_from_stdin(_Pos, abort) :-
	halt(-2).
filter_event_from_stdin(_Pos, end_of_the_world) :-
	!.
filter_event_from_stdin(_Pos, h(current_time, CT)) :-
	! 
	, set_term_quantification([h(current_time, CT)], existsf)
	, call(h(current_time, CT))
	.
filter_event_from_stdin(_Pos,h(Event, Timestamp)) :-
	!
	, set_term_quantification([h(Event,Timestamp)], existsf)
	, call(h(Event,Timestamp))
	%%  , set_term_quantification([h(current_time, Timestamp)], existsf)
	, ( (generate_current_time(yes)) ->
			!, call(h(current_time, Timestamp))
		;
			true
	)
	.
filter_event_from_stdin(_Pos, X) :-
	write(' Wrong format read in input: ')
	, write(X)
	, nl
	, fail
	.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Test utilities
test_spark :-
	example_model(Model)
	, observability(Observability)
	, durationConstraints(DurationConstraints)
	, inter_task_constraints(InterTaskConstraints)
	, options(Options)
	%%  , trace(Trace)
	%%  , create_spark_project( Model, Observability, DurationConstraints, InterTaskConstraints, Options)
	%%  , spark_trace_verification(Trace)
	, spark_trace_verification( Model, Observability, DurationConstraints, InterTaskConstraints, Options)
	.

test_spark(X) :-
	sub_model(X, Model, DurationConstraints, InterTaskConstraints)
	, observability(Observability)
	, options(Options)
	%%  , trace(X, Trace)
	%%  , create_spark_project( Model, Observability, DurationConstraints, InterTaskConstraints, Options)
	%%  , spark_trace_verification(Trace)
	, spark_trace_verification( Model, Observability, DurationConstraints, InterTaskConstraints, Options)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLES

example_model([
	%%  seq(start, a1)
	%%  ,
	seq(a1, a2)
	% xor-split X1
		%%  , xor_split(a2,[ a3, a5 ])
	%%  , seq(a3, a4)
	%%  , seq(a5, a6)
	%%  % xor-split X2
	%%  	, xor_split(a6,[ a7, a8 ])			
	%%  % and-split P1
	%%  	, and_split(a8, [ a9, a10 ] )
	%%  % and-join P2
	%%  	, and_join( [ a9, a10 ], a11 )
	%%  , seq(a11, a12)
	%%  % xor_join X4:
		%%  , xor_join([a4, a7, a12], a13),
	%%  , seq(a13, a14)
	%%  , seq(a14, stop)
]).


observability([
	obs(start, never)
	, obs(a1, always)
	, obs(a2, always)
	, obs(a3, always)
	, obs(a4, always)
	, obs(a5, always)
	, obs(a6, always)
	, obs(a7, always)
	, obs(a8, always)
	, obs(a9, always)
	, obs(a10, always)
	, obs(a11, always)
	, obs(a12, always)
	, obs(a13, always)
	, obs(a14, always)
	, obs(stop, never)
]).


durationConstraints([
	  (a1, (5,10))
	, (a2, (5,10))
	, (a3, (30,40))
	, (a4, (150,200))
	, (a5, (20,40))
	, (a6, (5,10))
	, (a7, (200,225))
	, (a8, (10,20))
	, (a9, (30,40))
	, (a10, (80,80))
	, (a11, (100,120))
	, (a12, (200,400))
	, (a13, (15,20))
	, (a14, (5,10))
]).


% itc are terms like: (A,AEdge, B,BEdge, Dist, [Low,Up])
inter_task_constraints([
	(a1, '_start', a2, '_end', 1, [0,30] )
	%%  , (a8, '_start', a11, '_start', 2, [100,140] )
	%%  , (a10, '_end', a11, '_start', 1, [20,10000] )
	%%  , (a10, '_start', a11, '_end', 1, [0,250] )
]).


%trace([h(event(a5_start, Pos1), _T3_start), h(event(a5_end, Pos1), _T3_end)]).
%trace([h(event(a2_start,6), _T2_start)]).
%%  trace([h(event(start,0),0),h(event(a1_start,1),1),h(event(a1_end,1),6),h(event(a2_start,2),7),h(event(a2_end,2),12),h(event(a3_start,3),13),h(event(a3_end,3),43),h(event(a4_start,4),44),h(event(a4_end,4),194),h(event(a13_start,5),195),h(event(a13_end,5),210),h(event(a14_start,6),211),h(event(a14_end,6),216),h(event(stop,7),219)]).
%%  trace([h(event(a1_start,1),1),h(event(a1_end,1),6),h(event(a2_start,2),7),h(event(a2_end,2),12),h(event(a3_start,3),13),h(event(a3_end,3),43),h(event(a4_start,4),44),h(event(a4_end,4),194),h(event(a13_start,5),195),h(event(a13_end,5),210),h(event(a14_start,6),211),h(event(a14_end,6),216)]).
%trace([h(event(a1_start,1),1),h(event(a1_end,1),6),h(event(a2_start,2),7),h(event(a2_end,2),12),h(event(a3_start,3),13),h(event(a3_end,3),43),h(event(a4_start,4),44),h(event(a4_end,4),194),h(event(a13_start,5),195),h(event(a13_end,5),210),h(event(a14_start,6),211),h(event(a14_end,6),216)]).
%trace([h(event(a5_start, Pos1), _T3_start), h(event(a5_end, Pos1), _T3_end)]).
%%  trace([h(event(a1_start,1),1), h(event(a1_end,1),7), h(event(a2_start,2),8), h(event(a2_end,2),14), h(event(a5_start,3),15), h(event(a5_end,3),35)]).
trace([h(event(a1_start,1),1), h(event(a1_end,1),7) , h(event(a2_start,2),8), h(event(a2_end,2),14)]).



options(
	%%  [ trace_max_length(43) ]
	[ trace_max_length(43), generate_current_time(yes), activities_have_start_and_end, double_chained_translation ]
).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUB-MODELS EXAMPLES

%%  sub_model( X, Flow, Durations, InterTasks)
sub_model(1
	, [ seq(a1, a2), xor_split(a2,[ a3, a5 ]), seq(a3, a4) ]
	, [ (a1, (5,10)), (a2, (5,10)), (a3, (30,40)) ]
	, [ (a1, '_start', a2, '_end', 1, [0,30] ) ]
).

sub_model(2
	, [ seq(a5, a6), xor_split(a6,[ a7, a8 ]) ]
	, [ (a5, (20,40)), (a6, (5,10)), (a8, (10,20))]
	, []
).

sub_model(3
	, [ and_split(a8, [ a9, a10 ] ), and_join( [ a9, a10 ], a11 ) , seq(a11, a12) ]
	, [ (a9, (30,40)), (a10, (80,80)) , (a11, (100,120)) ]
	, [ (a8, '_start', a11, '_start', 2, [100,140] ), (a10, '_end', a11, '_start', 1, [20,10000] ), (a10, '_start', a11, '_end', 1, [0,250] )]
).

sub_model(4
	, [ xor_join([a4, a7, a12], a13), seq(a13, a14) ]
	, [	(a4, (150,200)), (a7, (200,225)), (a12, (200,400)), (a13, (15,20)), (a14, (5,10)) ]
	, []
).




%%  trace( 1, [ h(event(a1_start,1),1), h(event(a1_end,1),7) , h(event(a2_start,2),8), h(event(a2_end,2),14), h(event(a3_start,3),20), h(event(a3_end,3),55), h(event(a4_start,4),60), h(event(a4_end,4),220)]).
trace( 1, [ h(event(a1_start,1),1), h(event(a1_end,1),7) , h(event(a2_start,2),8), h(event(a2_end,2),14), h(event(a5_start,3), 20), h(event(a5_end,3),45) ]).
%%  trace( 1, [ h(event(a1_start,1),1), h(event(a1_end,1),17) ]).

%%  trace( 2, [ h(event(a5_start,3), 20), h(event(a5_end,3),45), h(event(a6_start,4), 50), h(event(a6_end,4), 60), h(event(a7_start,5), 80), h(event(a7_end,5), 305) ]).
trace( 2, [ h(event(a5_start,3), 20), h(event(a5_end,3),45), h(event(a6_start,4), 50), h(event(a6_end,4), 60), h(event(a8_start,5), 80), h(event(a8_end,5), 95) ]).

trace( 3, [ h(event(a8_start,5), 80), h(event(a8_end,5), 90)
			, h(event(a10_start,6), 91), h(event(a9_start,6), 100), h(event(a9_end,6), 135), h(event(a10_end,6), 171)
			, h(event(a11_start,7), 191), h(event(a11_end,7), 292)
			, h(event(a12_start,8), 300), h(event(a12_end,8), 450)]).

%%  trace( 4, [ h(event(a4_start,4), 60), h(event(a4_end,4),260), h(event(a13_start,5), 270), h(event(a13_end,5), 285), h(event(a14_start,6), 286), h(event(a14_end,6), 296) ]).
%%  trace( 4, [ h(event(a7_start,5), 80), h(event(a7_end,5),305), h(event(a13_start,6), 310), h(event(a13_end,6), 330), h(event(a14_start,7), 340), h(event(a14_end,7), 345) ]).
trace( 4, [ h(event(a12_start,8), 300), h(event(a12_end,8),450), h(event(a13_start,9), 460), h(event(a13_end,9), 480), h(event(a14_start,10), 490), h(event(a14_end,10), 500) ]).


