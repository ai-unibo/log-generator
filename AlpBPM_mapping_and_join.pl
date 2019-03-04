:- module('AlpBPM_mapping_and_join', [translate_and_join/6]).

translate_and_join(AList, B, _Obs, Durations, Options, Stream) :-
	member(double_chained_translation, Options)
	, !
	, translate_and_join_double_chain(AList, B, Durations, Options, Stream)
	.
translate_and_join(AList,B, Obs, Durations, Options, Stream) :-
	rename_activities_with_durations(AList, Durations, Options, AListRenamed)
	, ( (activity_has_start_and_end( B, Durations, Options)) ->
		atom_concat(B, '_start', BAtom)
		;
		BAtom = B
	)
	, rename_obs_with_durations(Obs, Durations, Options, ObsRenamed)
	, translate_triggering_rules(AListRenamed, ObsRenamed, Options, Stream)
	, nl(Stream)
	, translate_and_join_support_rule(AListRenamed, BAtom, ObsRenamed, Options, Stream)
	, nl(Stream)
	, nl(Stream).


rename_activities_with_durations([], _Durations, _Options, []).
rename_activities_with_durations([Head|Tail], Durations, Options, [Atom|TailResult]) :-
	( (activity_has_start_and_end( Head, Durations, Options)) ->
		atom_concat(Head, '_end', Atom)
		;
		Atom = Head
	)
	, rename_activities_with_durations(Tail, Durations, Options, TailResult)
	.

rename_obs_with_durations([], _Durations, _Options, []).
rename_obs_with_durations([obs(Head, Something)|Tail], Durations, Options, [obs(Head_start,Something), obs(Head_end,Something) | TailResult]) :-
	activity_has_start_and_end( Head, Durations, Options)
	, !
	, atom_concat(Head, '_start', Head_start)
	, atom_concat(Head, '_end', Head_end)
	, rename_obs_with_durations(Tail, Durations, Options, TailResult).
rename_obs_with_durations([obs(Head, Something)|Tail], Durations, Options, [obs(Head,Something) | TailResult]) :-
	rename_obs_with_durations(Tail, Durations, Options, TailResult).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Triggering rules	
translate_triggering_rules([], _Obs, _Options, _Stream).
translate_triggering_rules([AHead|ATail], Obs, Options, Stream) :-
	member(obs(AHead,AObs), Obs)
	, translate_one_triggering_rule(AHead, AObs, Options, Stream)
	, nl(Stream)
	, nl(Stream)
	, translate_triggering_rules(ATail, Obs, Options, Stream).
	
translate_one_triggering_rule(A, always, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',A,'Pos'), 'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event('TraceId',A,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'ABD'(and_join_support('TraceId',A,'Pos'), 'T1'))
		;
			write(Stream, 'E'(event(A,'Pos'), 'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event(A,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'ABD'(and_join_support(A,'Pos'), 'T1'))
	)
	, write(Stream, '.').
translate_one_triggering_rule(A, never, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',A,'Pos'), 'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'ABD'(and_join_support('TraceId',A,'Pos'), 'T1'))
		;
			write(Stream, 'ABD'(event(A,'Pos'), 'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'ABD'(and_join_support(A,'Pos'), 'T1'))
	)
	, write(Stream, '.').
translate_one_triggering_rule(A, partially, Options, Stream) :-
	translate_one_triggering_rule(A, always, Options, Stream)
	, nl(Stream)
	, translate_one_triggering_rule(A, never, Options, Stream).
	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Support rule

translate_and_join_support_rule(AList, B, Obs, Options, Stream) :-
	write_support_abducibles(AList, Options, Stream, ListOfPositions, ListOfTimes)
	, write(Stream, ' /\\ ')
	, write(Stream, max_list(ListOfPositions, 'Max'))
	, write(Stream, ' ---> ')
	, member(obs(B,BObs), Obs)
	, translate_and_join_support_rule_consequent(B, BObs, ListOfPositions, ListOfTimes, Options, Stream)
	, write(Stream, '.').

	
% write_support_abducibles(List_of_activities, Stream, PositionX, List_of_Positions)
write_support_abducibles(ActivityList, Options, Stream, ListOfPositions, ListOfTimes) :-
	write_support_abducibles(ActivityList, Options, Stream, 1, ListOfPositions, ListOfTimes).
	
write_support_abducibles([], _, _, _, '[]', []).
write_support_abducibles([BHead|BTail], Options, Stream, CurrentPosition, ListOfPositions, [TimeAtom|OtherTimes]) :-
	atom_concat('Pos', CurrentPosition, PositionAtom)
	, atom_concat('T', CurrentPosition, TimeAtom)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(and_join_support('TraceId',BHead,PositionAtom), TimeAtom))
		;
			write(Stream, 'ABD'(and_join_support(BHead,PositionAtom), TimeAtom))
	)
	, NextPosition is CurrentPosition+1
	, write_support_abducibles_more(BTail, Options, Stream, NextPosition, OtherPositions, OtherTimes)
	, atom_concat('[', PositionAtom, Temp10)
	, atom_concat(Temp10, OtherPositions, ListOfPositions)
	.
	
write_support_abducibles_more([], _Options, _Stream, _CurrentPosition, ']',[]).
write_support_abducibles_more([BHead|BTail], Options, Stream, CurrentPosition, ListOfPositions, [TimeAtom|OtherTimes]) :-
	nl(Stream)
	, write(Stream, ' /\\ ')
	, atom_concat('Pos', CurrentPosition, PositionAtom)
	, atom_concat('T', CurrentPosition, TimeAtom)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(and_join_support('TraceId',BHead,PositionAtom), TimeAtom))
		;
			write(Stream, 'ABD'(and_join_support(BHead,PositionAtom), TimeAtom))
	)
	, NextPosition is CurrentPosition+1
	, write_support_abducibles_more(BTail, Options, Stream, NextPosition, OtherPositions, OtherTimes)
	, atom_concat(',', PositionAtom, Temp10)
	, atom_concat(Temp10, OtherPositions, ListOfPositions)
	.

	
	
	
	
translate_and_join_support_rule_consequent(B, always, _ListOfPositions, ListOfTimes, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',B,'Next'), 'TMax'))
		;
			write(Stream, 'E'(event(B,'Next'), 'TMax'))
	)
	, write_times_ordering(Stream, ListOfTimes)
	, write(Stream, ' /\\ '), write(Stream, 'Next==Max+1').
translate_and_join_support_rule_consequent(B, never, _ListOfPositions, ListOfTimes, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',B,'Next'), 'TMax'))
		;
			write(Stream, 'ABD'(event(B,'Next'), 'TMax'))
	)
	, write_times_ordering(Stream, ListOfTimes)
	, write(Stream, ' /\\ '), write(Stream, 'Next==Max+1').
translate_and_join_support_rule_consequent(B, partially, ListOfPositions, ListOfTimes, Options, Stream) :-
	translate_and_join_support_rule_consequent(B, always, ListOfPositions, ListOfTimes, Options, Stream)
	, nl(Stream)
	, write(Stream, ' \\/ ')
	, translate_and_join_support_rule_consequent(B, never, ListOfPositions, ListOfTimes, Options, Stream).
	
	

write_times_ordering(_Stream, []).
write_times_ordering(Stream, [H|T]) :-
	write(Stream, ' /\\ '),
	write(Stream, 'TMax > '),
	write(Stream,H),
	write_times_ordering(Stream, T).
	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Double chained translation

translate_and_join_double_chain(AList, B, Durations, Options, Stream) :-
	write_and_join_body(AList, Stream, ListOfPositions, ListOfTimes, Durations, Options)
	, nl(Stream)
	, write(Stream, '/\\ ')
	, write(Stream, max_list(ListOfPositions, 'Max'))
	, nl(Stream), write(Stream, '   ')
	, write(Stream, ' ---> ')
	, ( (activity_has_start_and_end(B, Durations, Options)) ->
			atom_concat(B, '_start', BAtom)
		;
			BAtom = B
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',BAtom,'Next'), 'TMax'))
		;
			write(Stream, 'E'(event(BAtom,'Next'), 'TMax'))
	)
	, nl(Stream), write(Stream, '   /\\ '), write(Stream, 'Next==Max+1')
	, write_times_ordering(Stream, ListOfTimes)
	, write(Stream, '.')
	, nl(Stream)
	, write_and_join_backward_chain(AList, B, Durations, Options, Stream) 
	.

write_and_join_body([A | ATail], Stream, [PosAtom | TListOfPositions], [TimeAtom | TListOfTimes], Durations, Options) :-
	( (activity_has_start_and_end(A, Durations, Options)) ->
			atom_concat(A, '_end', AAtom)
		;
			AAtom = A
	)
	, atom_concat('Pos', AAtom, PosAtom)
	, atom_concat('T', AAtom, TimeAtom)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',AAtom,PosAtom), TimeAtom))
		;
			write(Stream, 'H'(event(AAtom,PosAtom), TimeAtom))
	)
	, write_and_join_body_more( ATail, Stream, TListOfPositions, TListOfTimes, Durations, Options)
	.
write_and_join_body_more([], _Stream, [], [], _Durations, _Options).
write_and_join_body_more([A | ATail], Stream, [PosAtom | TListOfPositions], [TimeAtom | TListOfTimes], Durations, Options) :-
	( (activity_has_start_and_end(A, Durations, Options)) ->
			atom_concat(A, '_end', AAtom)
		;
			AAtom = A
	)
	, atom_concat('Pos', AAtom, PosAtom)
	, atom_concat('T', AAtom, TimeAtom)
	, nl(Stream), write(Stream, '/\\ ')
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',AAtom,PosAtom), TimeAtom))
		;
			write(Stream, 'H'(event(AAtom,PosAtom), TimeAtom))
	)
	, write_and_join_body_more( ATail, Stream, TListOfPositions, TListOfTimes, Durations, Options)
	.

write_and_join_backward_chain([], _B, _Durations, _Options, _Stream).
write_and_join_backward_chain([A | ATail], B, Durations, Options, Stream) :-
	( (activity_has_start_and_end(A, Durations, Options)) ->
			atom_concat(A, '_end', AAtom)
		;
			AAtom = A
	)
	, ( (activity_has_start_and_end(B, Durations, Options)) ->
		atom_concat(B, '_start', BAtom)
		;
		BAtom = B
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',BAtom,'_Next'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',AAtom,'_Pos'), 'T1'))
		;
			write(Stream, 'H'(event(BAtom,'_Next'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(AAtom,'_Pos'), 'T1'))
	)
	, write(Stream, ' /\\ '), write(Stream, 'T2>T1')
	, write(Stream, '.')
	, nl(Stream)
	, write_and_join_backward_chain(ATail, B, Durations, Options, Stream)
	.