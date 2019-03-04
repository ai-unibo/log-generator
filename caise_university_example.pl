test_log_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_log_incompleteness(Options).'), nl, nl,
	help_options, nl, nl.

test_log_incompleteness(Options) :-
	university_model(Flow, Obs),
	push_all_to_never(Obs, ObsNever),
	create_project(Flow, ObsNever, [], Options),
	project('sciff/temp'),
	trace_LInc2(LInc2),
	statistics(runtime,_),
	log_incompleteness(LInc2, MissingTraces),
	statistics(runtime,[_,Time]),
	write('Computed Missing Traces: '), length(MissingTraces, X), write(X), nl,
	write('Required time to compute all the missing traces: '), write(Time), write('ms.'), nl,
	nl, nl, write('Existing Traces:'), nl,
	print_nice_traces(LInc2),
	nl, nl, write('Missing Traces:'), nl,
	print_nice_traces(MissingTraces),
	nl, nl, write('Computed Missing Traces: '), length(MissingTraces, X), write(X), nl,
	write('Required time to compute all the missing traces: '), write(Time), write('ms.'), nl,
	nl, nl.


push_all_to_never([], []).
push_all_to_never([obs(X, _)|Tail], [obs(X, never)|TailResult]) :-
	push_all_to_never(Tail, TailResult).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TRACE INCOMPLETENESS

test_trace_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_trace_incompleteness(Example, Options).'), nl, nl,
	write('Available examples: '),
	findall(X, trace_incomplete(X, _), L),
	write(L), nl,
	help_options, nl, nl.
	
test_trace_incompleteness(Example, Max) :-
	trace_incomplete(Example, Trace),
	test_incompleteness(Trace, Max).
	
test_trace_incompleteness_university(Log, Index,  Max) :-
	nth0(Index, Log,Trace),
	test_incompleteness(Trace, Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Examples of traces incompleteness
  
trace_incomplete(t1, [
      hap(event(lcm,_Pos1),100)
      , hap(event(paca,_Pos2),436510)
]).

trace_incomplete(t2,  [
      hap(event(gag,_Pos3),100)
]).

trace_incomplete(t3,     [
      hap(event(scm,_Pos4),100)
    ]).
    
trace_incomplete(t4,     [
	hap(event(lcm,_Pos2),80),
      hap(event(slpa,_Pos4),100),
      hap(event(paca,_Pos5),110)
    ]).    
    
    
trace_incomplete(tr_1,     [
      hap(event(scm,_Pos4),100)
    ]).

trace_incomplete(tr_2, [
      hap(event(scm,_Pos1),100)
      , hap(event(paca,_Pos2),436510)
]).   

trace_incomplete(tr_3,     [
	hap(event(scm,_Pos2),80),
      hap(event(slpa,_Pos4),100),
      hap(event(paca,_Pos5),110)
    ]).  
    
trace_incomplete(tr_5,[
           hap(event(dr,_Pos12),_T2)
          , hap(event(scm,_Pos15),_T16)
          , hap(event(slpa,_Pos21),_T22)
          , hap(event(sbc,_Pos26),_T27)
          , hap(event(paca,_Pos29),_T30)
          ]).
        
trace_incomplete(tr_15,[    
           hap(event(ao,_Pos11),_T1)
          , hap(event(sdp,_Pos12),_T2)
          , hap(event(dr,_Pos14),_T4)        
          , hap(event(tas,_Pos9),_T10)
          , hap(event(txs,_Pos13),_T14)
          , hap(event(scm,_Pos15),_T16)
          , hap(event(txj,_Pos17),_T18)
          , hap(event(e,_Pos19),_T20)
          , hap(event(slpa,_Pos21),_T22)
          , hap(event(ap,_Pos23),_T24)
          , hap(event(wbc,_Pos24),_T25)
          , hap(event(sbc,_Pos26),_T27)
          , hap(event(cc,_Pos28),_T29)
          , hap(event(paca,_Pos29),_T30)
          , hap(event(ex,_Pos31),_T32)
          ]).    
    
trace_incomplete(tr_30,[    
           hap(event(ao,_Pos1_1),_T1_1)
          , hap(event(sdp,_Pos1_2),_T1_2)
          , hap(event(dr,_Pos1_3),_T1_3)
          , hap(event(cd,_Pos1_4),_T1_4)
          , hap(event(ao,_Pos2_1),_T2_1)
          , hap(event(sdp,_Pos2_2),_T2_2)
          , hap(event(dr,_Pos2_3),_T2_3)
          , hap(event(cd,_Pos2_4),_T2_4)
          , hap(event(ao,_Pos3_1),_T3_1)          
          , hap(event(sdp,_Pos3_2),_T3_2)
          , hap(event(dr,_Pos3_4),_T3_4)
          , hap(event(ao,_Pos4_1),_T4_1)
          , hap(event(sdp,_Pos4_2),_T4_2)
          , hap(event(dr,_Pos4_3),_T4_3)
          , hap(event(cd,_Pos4_4),_T4_4)          
          , hap(event(tas,_Pos9),_T10)
          , hap(event(tat,_Pos12),_T13)      
          , hap(event(txs,_Pos13),_T14)
          , hap(event(scm,_Pos15),_T16)
          , hap(event(txj,_Pos17),_T18)
          , hap(event(e,_Pos19),_T20)
          , hap(event(slpa,_Pos21),_T22)
          , hap(event(ap,_Pos23),_T24)
          , hap(event(wbc,_Pos24),_T25)
          , hap(event(sbc,_Pos26),_T27)
          , hap(event(cc,_Pos27),_T28)
          , hap(event(bag,_Pos28),_T29)
          , hap(event(paca,_Pos29),_T30)
          , hap(event(ccj,_Pos30),_T31)
          , hap(event(ex,_Pos31),_T32)
          ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVENT INCOMPLETENESS 

test_event_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_event_incompleteness(Example, Options).'), nl, nl,
	write('Available examples: '),
	findall(X, event_incomplete(X, _), L),
	write(L), nl,
	help_options, nl, nl.

test_event_incompleteness(Example, Max) :-
	event_incomplete(Example, Trace),
	test_incompleteness(Trace, Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Examples of traces for event incompleteness  

event_incomplete(ei1, 
    [
        hap(event(ao,_Pos1),123)
      , hap(event(sdp,_Pos3),125)
      , hap(event(dr,_Pos5),128)
      , hap(event(cd,_Pos7),130)
      , hap(event(tas,_Pos9),135)
      , hap(event(_Ev1,_Pos11),137)
      , hap(event(tat,_Pos12),150)      
      , hap(event(txs,_Pos13),160)
      , hap(event(scm,_Pos15),171)
      , hap(event(txj,_Pos17),182)
      , hap(event(e,_Pos19),193)
      , hap(event(sla,_Pos21),213)
      , hap(event(a,_Pos23),244)
      , hap(event(ex,_Pos25),262)
    ]
).

event_incomplete(ei2, 
    [
        hap(event(ao,_Pos1),_T1)
      , hap(event(sdp,_Pos3),_T2)
      , hap(event(dr,_Pos5),_T3)
      , hap(event(cd,_Pos7),_T4)
      , hap(event(tas,_Pos9),_T5)
      , hap(event(_Ev1,_Pos11),_T6)
      , hap(event(tat,_Pos12),_T7)      
      , hap(event(txs,_Pos13),_T8)
      , hap(event(scm,_Pos15),_T9)
      , hap(event(txj,_Pos17),_T10)
      , hap(event(e,_Pos19),_T11)
      , hap(event(sla,_Pos21),_T12)
      , hap(event(a,_Pos23),_T13)
      , hap(event(ex,_Pos25),_T14)
    ]
).
  

event_incomplete(ei3, 
    [
        hap(event(ao,_Pos1),123)
      , hap(event(sdp,_Pos3),125)
      , hap(event(dr,_Pos5),128)
      , hap(event(cd,_Pos7),130)
      , hap(event(tas,_Pos9),135)
      , hap(event(_Ev1,_Pos11),137)
      , hap(event(tat,_Pos12),150)      
      , hap(event(txs,_Pos13),160)
      , hap(event(scm,_Pos15),171)
      , hap(event(txj,_Pos17),182)
      , hap(event(_Ev2,_Pos19),_T20)
      , hap(event(sla,_Pos21),213)
      , hap(event(a,_Pos23),244)
      , hap(event(ex,_Pos25),262)
    ]
).


event_incomplete(ei4, 
    [
        hap(event(ao,_Pos1),_T2)
      , hap(event(sdp,_Pos3),_T4)
      , hap(event(dr,_Pos5),_T6)
      , hap(event(cd,_Pos7),_T8)
      , hap(event(tas,_Pos9),_T10)
      , hap(event(_Ev1,_Pos11),_T12)
      , hap(event(tat,_Pos12),_T13)      
      , hap(event(txs,_Pos13),_T14)
      , hap(event(scm,_Pos15),_T16)
      , hap(event(txj,_Pos17),_T18)
      , hap(event(_Ev2,_Pos19),_T20)
      , hap(event(sla,_Pos21),_T22)
      , hap(event(a,_Pos23),_T24)
      , hap(event(ex,_Pos25),_T26)
    ]
).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EVENT AND TRACE INCOMPLETENESS

test_event_trace_incompleteness :-
	nl, write('Usage:'),nl,writef('\ttest_event_trace_incompleteness(Example, Options).'), nl, nl,
	write('Available examples: '),
	findall(X, event_trace_incomplete(X, _), L),
	write(L), nl,
	help_options, nl, nl.

test_event_trace_incompleteness(Example, Max) :-
	event_trace_incomplete(Example, Trace),
	test_incompleteness(Trace, Max).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Example of traces for the event and trace incompleteness

event_trace_incomplete(tr_ev_1,     [
      hap(event(_Ev1,_Pos4),100)
]).

    
event_trace_incomplete(tr_ev_5,[    
           hap(event(dr,_Pos12),_T2)
          , hap(event(scm,_Pos15),_T16)
          , hap(event(slpa,_Pos21),_T22)
          , hap(event(_Ev1,_Pos26),_T27)
          , hap(event(paca,_Pos29),_T30)
          ]).    
        
event_trace_incomplete(tr_ev_15,[
           hap(event(ao,_Pos11),_T1)
          , hap(event(sdp,_Pos12),_T2)
          , hap(event(dr,_Pos14),_T4)        
          , hap(event(tas,_Pos9),_T10)
          , hap(event(txs,_Pos13),_T14)
          , hap(event(scm,_Pos15),_T16)
          , hap(event(txj,_Pos17),_T18)
          , hap(event(e,_Pos19),_T20)
          , hap(event(slpa,_Pos21),_T22)
          , hap(event(ap,_Pos23),_T24)
          , hap(event(wbc,_Pos24),_T25)
          , hap(event(_Ev1,_Pos26),_T27)
          , hap(event(cc,_Pos28),_T29)
          , hap(event(paca,_Pos29),_T30)
          , hap(event(ex,_Pos31),_T32)
          ]).    
    
event_trace_incomplete(tr_ev_30,[
           hap(event(ao,_Pos1_1),_T1_1)
          , hap(event(sdp,_Pos1_2),_T1_2)
          , hap(event(dr,_Pos1_3),_T1_3)
          , hap(event(cd,_Pos1_4),_T1_4)
          , hap(event(ao,_Pos2_1),_T2_1)
          , hap(event(sdp,_Pos2_2),_T2_2)
          , hap(event(dr,_Pos2_3),_T2_3)
          , hap(event(cd,_Pos2_4),_T2_4)
          , hap(event(ao,_Pos3_1),_T3_1)          
          , hap(event(sdp,_Pos3_2),_T3_2)
          , hap(event(dr,_Pos3_4),_T3_4)
          , hap(event(ao,_Pos4_1),_T4_1)
          , hap(event(sdp,_Pos4_2),_T4_2)
          , hap(event(dr,_Pos4_3),_T4_3)
          , hap(event(cd,_Pos4_4),_T4_4)          
          , hap(event(tas,_Pos9),_T10)
          , hap(event(tat,_Pos12),_T13)      
          , hap(event(txs,_Pos13),_T14)
          , hap(event(scm,_Pos15),_T16)
          , hap(event(txj,_Pos17),_T18)
          , hap(event(e,_Pos19),_T20)
          , hap(event(slpa,_Pos21),_T22)
          , hap(event(ap,_Pos23),_T24)
          , hap(event(wbc,_Pos24),_T25)
          , hap(event(_Ev1,_Pos26),_T27)
          , hap(event(cc,_Pos27),_T28)
          , hap(event(bag,_Pos28),_T29)
          , hap(event(paca,_Pos29),_T30)
          , hap(event(ccj,_Pos30),_T31)
          , hap(event(ex,_Pos31),_T32)
          ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compute all the times for the university admission of the paper CAISE 2016
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
compute_all_times_university :-
	
	% write table header
	tell(times0b),
	FormatHeader = '%52c|%42c\n',
	Format = '%32l%20l|%16r%16r%10r\n',
	writef('%r\n', ['-', 96]),
	writef(FormatHeader, ['Input', 'Output']),
	writef(Format,['Example','Path max length', 'Parsing Time', 'Computing Time', '#Traces']),
	writef('%r\n', ['-', 96]),	
	told,
	
	append(times0b),
	 
	Max1 = 14,
	university_model(Flow1, Obs1),
	%university_model_observable_5(Flow1, Obs1),
	%university_model_observable_15(Flow1, Obs1),
	push_all_to_never(Obs1, ObsNever1),
	statistics(runtime,_),
	create_project(Flow1, ObsNever1, [], [path_max_length(Max1)]),
	statistics(runtime,[_,Time1]),
	project('sciff/temp'),
	trace_LInc1(LInc1),
	statistics(runtime,_),
	log_incompleteness(LInc1, MissingTraces1),
	statistics(runtime,[_,Time2]),
	length(MissingTraces1, Length1),
	
	writef(Format, ['Log incompleteness',Max1,Time1,Time2,Length1]),
	told,
			
	append(times0b),

	university_model(Flow3, Obs3),
	%university_model_observable_5(Flow3, Obs3),
	push_all_to_never(Obs3, ObsNever3),
	statistics(runtime,_),
	create_project(Flow3, ObsNever3, [], [path_max_length(Max1)]),
	statistics(runtime,[_,Time5]),
	project('sciff/temp'),	
	trace_LInc3(LInc3a),
	statistics(runtime,_),
	log_incompleteness(LInc3a, MissingTraces3),
	statistics(runtime,[_,Time6]),
	length(MissingTraces3, Length3),
	
	writef(Format, ['Log incompleteness',Max1,Time5,Time6,Length3]),
	told,
			
	append(times0b),	

	university_model(Flow4, Obs4),
	%university_model_observable_5(Flow4, Obs4),
	push_all_to_never(Obs4, ObsNever4),
	statistics(runtime,_),
	create_project(Flow4, ObsNever4, [], [path_max_length(Max1)]),
	statistics(runtime,[_,Time7]),
	project('sciff/temp'),	
	trace_LInc4(LInc4a),
	statistics(runtime,_),
	log_incompleteness(LInc4a, MissingTraces4),
	statistics(runtime,[_,Time8]),
	length(MissingTraces4, Length4),
	
	writef(Format, ['Log incompleteness',Max1,Time7,Time8,Length4]),	
	

	Max1c = 30,
	university_model(Flow5, Obs5),
	%university_model_observable_15(Flow5, Obs5),
	%university_model_observable_5(Flow5, Obs5),
	push_all_to_never(Obs5, ObsNever5),	
	statistics(runtime,_),
	create_project(Flow5, ObsNever5, [], [path_max_length(Max1c)]),
	statistics(runtime,[_,Time9]),
	project('sciff/temp'),
	trace_LInc1(LInc1c),
	statistics(runtime,_),
	log_incompleteness(LInc1c, MissingTraces5),
	statistics(runtime,[_,Time10]),
	length(MissingTraces5, Length5),	
	writef(Format, ['Log incompleteness',Max1c,Time9,Time10,Length5]),
	

	university_model(Flow7, Obs7),
	%university_model_observable_15(Flow7, Obs7),
	%university_model_observable_5(Flow7, Obs7),
	push_all_to_never(Obs7, ObsNever7),	
	statistics(runtime,_),
	create_project(Flow7, ObsNever7, [], [path_max_length(Max1c)]),
	statistics(runtime,[_,Time13]),
	project('sciff/temp'),
	trace_LInc3(LInc3c),
	statistics(runtime,_),
	log_incompleteness(LInc3c, MissingTraces7),
	statistics(runtime,[_,Time14]),
	length(MissingTraces7, Length7),	
	writef(Format, ['Log incompleteness',Max1c,Time13,Time14,Length7]),
	

	university_model(Flow8, Obs8),
	%university_model_observable_15(Flow8, Obs8),
	%university_model_observable_5(Flow8, Obs8),
	push_all_to_never(Obs8, ObsNever8),
	statistics(runtime,_),
	create_project(Flow8, ObsNever8, [], [path_max_length(Max1c)]),
	statistics(runtime,[_,Time15]),
	project('sciff/temp'),	
	trace_LInc4(LInc4c),
	statistics(runtime,_),
	log_incompleteness(LInc4c, MissingTraces8),
	statistics(runtime,[_,Time16]),
	length(MissingTraces8, Length8),	

	writef(Format, ['Log incompleteness',Max1c,Time15,Time16,Length8]),	
	told, 
	

	append(times0b),
	% trace incompleteness
	Max2a = 14,
	findall([A,Max2a,B,C,D], times_trace_incompleteness(A,Max2a, B, C, D), La),
	print_times(Format, La),
	told, 
	
	append(times0b),
	% trace incompleteness
	Max2c = 30,
	findall([A,Max2c,B,C,D], times_trace_incompleteness(A,Max2c, B, C, D), Lc),
	print_times(Format, Lc),
	told, 			
	
	append(times0b),
	% event incompleteness
	Max3 = 14,
	findall([A,Max3,B,C,D], times_event_incompleteness(A, Max3, B, C, D), L2),
	print_times(Format, L2),
	told, 		
	
	append(times0b),
	% event and trace incompleteness
	Max4 = 14,
	findall([A,Max4,B,C,D], times_event_trace_incompleteness(A, Max4, B, C, D), L3),
	print_times(Format, L3),
	told, 		
		
	append(times0b),
	
	nl, nl, nl, nl,
	writef('%r\n', ['-', 96]),
	writef(FormatHeader, ['Input', 'Output']),
	writef(Format,['Example','Path max length', 'Parsing Time', 'Computing Time', '#Traces']),
	writef('%r\n', ['-', 96]),
	writef(Format, ['Log incompleteness',Max1,Time1,Time2,Length1]),
	writef(Format, ['Log incompleteness',Max1,Time5,Time6,Length3]),
	writef(Format, ['Log incompleteness',Max1,Time7,Time8,Length4]),
	writef(Format, ['Log incompleteness',Max1c,Time9,Time10,Length5]),	
	writef(Format, ['Log incompleteness',Max1,Time13,Time14,Length7]),
	writef(Format, ['Log incompleteness',Max1,Time15,Time16,Length8]),	
	
	
	print_times(Format, La),
	print_times(Format, Lc),
	print_times(Format, L2),
	print_times(Format, L3),
	told.


times_trace_incompleteness(Example_name, Max_length, Parsing_time, Computing_time, Num_traces) :-
	university_model(Flow, Obs),
	%university_model_observable_15(Flow, Obs),	
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
	university_model(Flow, Obs),
	%university_model_observable_15(Flow, Obs),
	event_incomplete(Example, Trace),
	%length(Trace, LL),
	
	%ML is LL+2,
	%write(ML), nl,
	atom_concat('Event_incomplete_', Example, Example_name),

	append(times3_ei_14),
	write(Example_name), nl,
	statistics(runtime,_),
	create_project(Flow, Obs, Trace, [path_max_length(Max_length)]),
	statistics(runtime,[_,Parsing_time]),
	project('sciff/temp'),
	statistics(runtime,_),
	trace_incompleteness(ExtendedTraces),
	statistics(runtime,[_,Computing_time]),
	length(ExtendedTraces, Num_traces),
	told.

times_event_trace_incompleteness(Example_name, Max_length, Parsing_time, Computing_time, Num_traces) :-
	university_model(Flow, Obs),	
	%university_model_observable_15(Flow, Obs),
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
	
print_times(_, []) :- !.
print_times(Format, [H|T]) :-
	writef(Format, H),
	print_times(Format, T).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MODIFICARE QUI IL MODELLO, A SECONDA DELL OSSERVABILITA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_incompleteness(Trace, Options) :-
        university_model(Flow, Obs),
        %university_model_5(Flow, Obs),	
        %university_model_observable_15(Flow, Obs),
	create_project(Flow, Obs, Trace, Options),
	project('sciff/temp'),
	write('Trying to complete the following trace:'), nl,
	print_nice_traces([Trace]), nl, nl,
	statistics(runtime,_),
	trace_incompleteness(ExtendedTraces, Options),
	statistics(runtime,[_,Time]),
	write('Computed extensions: '), length(ExtendedTraces, X), write(X), nl,
	write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl,
	nl, nl, write('Extended Traces:'), nl,
	print_nice_traces(ExtendedTraces),
	nl, nl, nl, write('Computed extensions: '), write(X), nl,
	write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl.


	
help_options :-
	write('Options must be a list of terms. Allowed terms:'), nl,
	write(' -- path_max_length(Max)   sets the maximum length of a path. Max should be a ground, positive integer.'), nl,
	write(' -- trace_max_length(Max)  sets the maximum length of a trace. Max should be a ground, positive integer.'), nl.

  
