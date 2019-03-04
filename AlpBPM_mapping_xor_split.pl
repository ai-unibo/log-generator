:- module('AlpBPM_mapping_xor_split', [translate_xor_split/6] ).

translate_xor_split( A, BList, _Obs, Durations, Options, Stream) :-
	member(double_chained_translation, Options)
	, !
	, translate_xor_split_double_chain(A, BList, Durations, Options, Stream).
translate_xor_split(A,BList, Obs, Durations, Options, Stream) :-
	member(obs(A,AObs), Obs),
	translate_xor_split_single_chain(A, AObs, BList, Obs, Durations, Options, Stream).
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ANTECEDENT
%
% case #1: the antecedent is always observable
translate_xor_split_single_chain(A,always, BList, Obs, Durations, Options, Stream) :-
	( (activity_has_start_and_end( A, Durations, Options)) ->
		atom_concat(A, '_end', AAtom)
		;
		AAtom = A
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',AAtom,'Pos'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event('TraceId',AAtom,'Pos'),'T1'))
		;
			write(Stream, 'E'(event(AAtom,'Pos'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event(AAtom,'Pos'),'T1'))
	)
	, write(Stream, ' ---> ')
	, translate_xor_split_disjuncts(BList, Obs, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream).
%
% case #2: the antecedent is never observable
translate_xor_split_single_chain(A,never, BList, Obs, Durations, Options, Stream) :-
	( (activity_has_start_and_end( A, Durations, Options)) ->
		atom_concat(A, '_end', AAtom)
		;
		AAtom = A
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',AAtom,'Pos'),'T1'))
		;
			write(Stream, 'ABD'(event(AAtom,'Pos'),'T1'))
	)
	, write(Stream, ' ---> ')
	, translate_xor_split_disjuncts(BList, Obs, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream).
%	
% case #3: the antecedent is partially observable
translate_xor_split_single_chain(A,partially, BList, Obs, Durations, Options, Stream) :-
	translate_xor_split_single_chain(A,always, BList, Obs, Durations, Options, Stream),
	nl(Stream),
	translate_xor_split_single_chain(A,never, BList, Obs, Durations, Options, Stream).
	

	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSEQUENT

translate_xor_split_disjuncts(BList, Obs, Durations, Options, Stream) :-
	gen_lists_deleted(BList, BListDeleted)
	, generate_single_xor_disjuncts(BList, BListDeleted, Obs, Durations, Options, Stream).

generate_single_xor_disjuncts([], _, _, _, _, _).
generate_single_xor_disjuncts([HB|BTail], [HBListDeleted|TailBListDeleted], Obs, Durations, Options, Stream) :-
	member(obs(HB,always), Obs)
	, disjunct_always([HB|BTail], [HBListDeleted|TailBListDeleted], Obs, Durations, Options, Stream).
generate_single_xor_disjuncts([HB|BTail], [HBListDeleted|TailBListDeleted], Obs, Durations, Options, Stream) :-
	member(obs(HB,never), Obs)
	, disjunct_never([HB|BTail], [HBListDeleted|TailBListDeleted], Obs, Durations, Options, Stream).
generate_single_xor_disjuncts([HB|BTail], [HBListDeleted|TailBListDeleted], Obs, Durations, Options, Stream) :-
	member(obs(HB,partially), Obs)
	, disjunct_partially([HB|BTail], [HBListDeleted|TailBListDeleted], Obs, Durations, Options, Stream).

generate_single_xor_disjuncts_more([], _, _, _, _, _) :- !.
generate_single_xor_disjuncts_more(B, BListDeleted, Obs, Durations, Options, Stream) :-
	nl(Stream)
	, write(Stream, ' \\/ ')
	, generate_single_xor_disjuncts(B, BListDeleted, Obs, Durations, Options, Stream).


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
disjunct_always([HB|BTail], [_HBListDeleted|TailBListDeleted], Obs, Durations, Options, Stream) :-
	( (activity_has_start_and_end( HB, Durations, Options)) ->
		atom_concat(HB, '_start', BAtom)
		;
		BAtom = HB
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',BAtom,'Next'), 'T2'))
		;
			write(Stream, 'E'(event(BAtom,'Next'), 'T2'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next')
	, generate_single_xor_disjuncts_more(BTail, TailBListDeleted, Obs, Durations, Options, Stream).
%	
disjunct_never([HB|BTail], [_HBListDeleted|TailBListDeleted], Obs, Durations, Stream) :-
	( (activity_has_start_and_end( HB, Durations, Options)) ->
		atom_concat(HB, '_start', BAtom)
		;
		BAtom = HB
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',BAtom,'Next'), 'T2'))
		;
			write(Stream, 'ABD'(event(BAtom,'Next'), 'T2'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next')
	, generate_single_xor_disjuncts_more(BTail, TailBListDeleted, Obs, Durations, Options, Stream).
%	
disjunct_partially([HB|BTail], [_HBListDeleted|TailBListDeleted], Obs, Durations, Options, Stream) :-
	( (activity_has_start_and_end( HB, Durations, Options)) ->
		atom_concat(HB, '_start', BAtom)
		;
		BAtom = HB
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',BAtom,'Next'), 'T2'))
		;
			write(Stream, 'E'(event(BAtom,'Next'), 'T2'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next')
	, nl(Stream)
	, write(Stream, ' \\/ ')
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',BAtom,'Next'), 'T2'))
		;
			write(Stream, 'ABD'(event(BAtom,'Next'), 'T2'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next')
	, generate_single_xor_disjuncts_more(BTail, TailBListDeleted, Obs, Durations, Options, Stream).



	

	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% It generates a new list LL where, for each element of the list LStart
% in LL there is the list of the remaining elements
% gen_lists_deleted(LStart, LL)

gen_lists_deleted(LStart, Result):-
	gen_lists_deleted(LStart, Result, LStart).
gen_lists_deleted([], [], _).
gen_lists_deleted([H|Tail], [HDelete|TailDelete], Temp) :-
	delete(Temp, H, HDelete),
	gen_lists_deleted(Tail, TailDelete, Temp).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% double chained translation for verification
%
translate_xor_split_double_chain(A, [HBList | TBList], Durations, Options, Stream) :-
	( (activity_has_start_and_end( A, Durations, Options)) ->
		atom_concat(A, '_end', AAtom)
		;
		AAtom = A
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',AAtom,'Pos'),'T1'))
		;
			write(Stream, 'H'(event(AAtom,'Pos'),'T1'))
	)
	, write(Stream, ' ---> ')
	, translate_xor_split_double_chain_disjunct(HBList, Durations, Options, Stream)
	, translate_xor_split_double_chain_disjunct_more( TBList, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream)
	, translate_xor_split_backward_chain([HBList | TBList], A, Durations, Options, Stream)
	.
translate_xor_split_double_chain_disjunct(HB, Durations, Options, Stream) :-
	( (activity_has_start_and_end( HB, Durations, Options)) ->
		atom_concat(HB, '_start', BAtom)
		;
		BAtom = HB
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',BAtom,'Next'), 'T2'))
		;
			write(Stream, 'E'(event(BAtom,'Next'), 'T2'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next').
translate_xor_split_double_chain_disjunct_more([], _Durations, _Options, _Stream) .
translate_xor_split_double_chain_disjunct_more([HBList | TBList], Durations, Options, Stream) :-
	nl(Stream)
	, write(Stream, '   \\/ ')
	, translate_xor_split_double_chain_disjunct(HBList, Durations, Options, Stream)
	, translate_xor_split_double_chain_disjunct_more(TBList, Durations, Options, Stream).
translate_xor_split_backward_chain([], _A, _Durations, _Options, _Stream).
translate_xor_split_backward_chain([B | Tail], A, Durations, Options, Stream) :-
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
			write(Stream, 'H'(event('TraceId',BAtom,'Next'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',AAtom,'Pos'), 'T1'))
		;
			write(Stream, 'H'(event(BAtom,'Next'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(AAtom,'Pos'), 'T1'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next')
	, write(Stream, '.')
	, nl(Stream)
	, translate_xor_split_backward_chain(Tail, A, Durations, Options, Stream)
	.	















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities

write_time_constraint(Stream, T1, T2) :-
	write(Stream, ' /\\ '), write(Stream, T2), write(Stream, '>'), write(Stream, T1).
write_pos_constraint(Stream, Pos, Next) :-
	write(Stream, ' /\\ '), write(Stream, Next), write(Stream, '=='), write(Stream, Pos), write(Stream, '+'), write(Stream, 1).

