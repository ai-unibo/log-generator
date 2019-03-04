:- module('AlpBPM_mapping_duration', [translate_duration/4]).

%%  translate_duration(List_of_activities, Durations, Options, Stream)
translate_duration(List_of_activities, Durations, Options, Stream) :-
	member(double_chained_translation, Options)
	, !
	, write(Stream, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl(Stream)
	, write(Stream, '% Non-atomic activities constraints:'), nl(Stream), nl(Stream)
	, translate_duration_double_chain(List_of_activities, Durations, Options, Stream)
	.
translate_duration(_List_of_activities, [], Options, Stream) :-
	\+ member(activities_have_start_and_end, Options)
	, !
	, write(Stream, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl(Stream)
	, write(Stream, '% No Duration Constraints were specified!'), nl(Stream)
	, write(Stream, '% Activities are atomic!'), nl(Stream), nl(Stream)
	, write(Stream, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl(Stream).

translate_duration(List_of_activities, Durations, Options, Stream) :-
	write(Stream, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl(Stream)
	, write(Stream, '% Non-atomic activities constraints:'), nl(Stream), nl(Stream)
	, translate_list(List_of_activities, Durations, Options, Stream)
	.



translate_list([], _Durations, _Options, _Stream).
translate_list([Head | Tail], Durations, Options, Stream) :-
	Head \= start
	, Head \= stop
	, !
	, ( (member((Head, (Amin,Amax)), Durations))
	->
		translate_duration_one((Head, (Amin,Amax)), Options, Stream)
		, nl(Stream)
	;
		translate_duration_one(Head, Options, Stream)
		, nl(Stream)
	)
	, translate_list( Tail, Durations, Options, Stream)
	.
translate_list([_Head | Tail], Durations, Options, Stream) :-
	translate_list( Tail, Durations, Options, Stream).


%%  translate_duration(List_of_activities, [Head | Tail], Options, Stream) :-
%%  	write(Stream, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl(Stream)
%%  	, write(Stream, '% Duration constraints:'), nl(Stream), nl(Stream)
%%  	, translate_duration_more([Head | Tail], Stream).

%%  translate_duration_more([], _Stream) :- !.
%%  translate_duration_more([Head | Tail], Stream) :-
%%  	translate_duration_one(Head, Stream)
%%  	, nl(Stream)
%%  	, translate_duration_more(Tail, Stream).
	
	
	


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Writing the rules
% translate_duration_one(obs(Activity, always), Stream) :-
	% Activity =.. [ADesc, duration(Amin,Amax)]
	% , ADesc \= start, ADesc \= stop
translate_duration_one((ADesc, (Amin,Amax)), Options, Stream) :-
	!
	, atom_concat(ADesc, '_start', Astart)
	, atom_concat(ADesc, '_end', Aend)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',Astart,'Pos'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event('TraceId',Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',Aend,'Pos'), 'T2'))
		;
			write(Stream, 'E'(event(Astart,'Pos'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event(Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(Aend,'Pos'), 'T2'))
	)
	, write(Stream, ' /\\ '), write(Stream, 'T2'), write(Stream, '>='), write(Stream, 'T1'), write(Stream,'+'), write(Stream,Amin)
	, write(Stream, ' /\\ '), write(Stream, 'T2'), write(Stream, '<='), write(Stream, 'T1'), write(Stream,'+'), write(Stream,Amax)
	, write(Stream, '.')
	, nl(Stream)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'ABD'(event('TraceId',Aend,'Pos'), 'T2'))
		;
			write(Stream, 'ABD'(event(Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'ABD'(event(Aend,'Pos'), 'T2'))
	)
	, write(Stream, ' /\\ '), write(Stream, 'T2'), write(Stream, '>='), write(Stream, 'T1'), write(Stream,'+'), write(Stream,Amin)
	, write(Stream, ' /\\ '), write(Stream, 'T2'), write(Stream, '<='), write(Stream, 'T1'), write(Stream,'+'), write(Stream,Amax)
	, write(Stream, '.')
	, nl(Stream)
	.
%%  Rimosso su richiesta di Daniela. In generale, per verifica che gira su singolo nodo
%%  di computazione, questo predicato e' necessario...
%%  translate_duration_one(_ADesc, _Stream).
translate_duration_one(ADesc, Stream) :-
	atom_concat(ADesc, '_start', Astart)
	, atom_concat(ADesc, '_end', Aend)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',Astart,'Pos'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event('TraceId',Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',Aend,'Pos'), 'T2'))
		;
			write(Stream, 'E'(event(Astart,'Pos'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event(Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(Aend,'Pos'), 'T2'))
	)
	, write(Stream, ' /\\ '), write(Stream, 'T2'), write(Stream, '>='), write(Stream, 'T1')
	, write(Stream, '.')
	, nl(Stream)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'ABD'(event('TraceId',Aend,'Pos'), 'T2'))
		;
			write(Stream, 'ABD'(event(Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'ABD'(event(Aend,'Pos'), 'T2'))
	)
	, write(Stream, ' /\\ '), write(Stream, 'T2'), write(Stream, '>='), write(Stream, 'T1')
	, write(Stream, '.')
	, nl(Stream)
	.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Double chained translation

translate_duration_double_chain([], _Durations, _Options, _Stream).
translate_duration_double_chain([start | Tail], Durations, Options, Stream) :-
	!, translate_duration_double_chain( Tail, Durations, Options, Stream).
translate_duration_double_chain([stop | Tail], Durations, Options, Stream) :-
	!, translate_duration_double_chain( Tail, Durations, Options, Stream).
translate_duration_double_chain([Head | Tail], Durations, Options, Stream) :-
	Head \= start
	, Head \= stop
	, !
	, ( (member((Head, (Amin,Amax)), Durations))
	->
		translate_duration_one_double_chain((Head, (Amin,Amax)), Options, Stream)
		, nl(Stream)
	;
		translate_duration_one_double_chain(Head, Options, Stream)
		, nl(Stream)
	)
	, translate_duration_double_chain( Tail, Durations, Options, Stream)
	.
translate_duration_one_double_chain((ADesc, (Amin,Amax)), Options, Stream) :-
	!
	, atom_concat(ADesc, '_start', Astart)
	, atom_concat(ADesc, '_end', Aend)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',Aend,'Pos'), 'T2'))
		;
			write(Stream, 'H'(event(Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(Aend,'Pos'), 'T2'))
	)
	, write(Stream, ' /\\ T2>=T1+'), write(Stream,Amin)
	, write(Stream, ' /\\ T2<=T1+'), write(Stream,Amax)
	, write(Stream, '.')
	, nl(Stream)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',Aend,'Pos'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',Astart,'Pos'), 'T1'))
		;
			write(Stream, 'H'(event(Aend,'Pos'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(Astart,'Pos'), 'T1'))
	)
	, write(Stream, ' /\\ T2>=T1+'), write(Stream,Amin)
	, write(Stream, ' /\\ T2<=T1+'), write(Stream,Amax)
	, write(Stream, '.')
	, nl(Stream)
	.
translate_duration_one_double_chain(ADesc, Options, Stream) :-
	atom_concat(ADesc, '_start', Astart)
	, atom_concat(ADesc, '_end', Aend)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',Aend,'Pos'), 'T2'))
		;
			write(Stream, 'H'(event(Astart,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(Aend,'Pos'), 'T2'))
	)
	, write(Stream, ' /\\ T2>=T1')
	, write(Stream, '.')
	, nl(Stream)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',Aend,'Pos'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',Astart,'Pos'), 'T1'))
		;
			write(Stream, 'H'(event(Aend,'Pos'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(Astart,'Pos'), 'T1'))
	)
	, write(Stream, ' /\\ T2>=T1')
	, write(Stream, '.')
	, nl(Stream)
	.
	



% seq(start, execute_birth)
% ABD(event(start,Pos),T1)
% ---> ABD(event(execute_birth,Next), T2) /\ T2>=T1 /\ Next==Pos+1.


