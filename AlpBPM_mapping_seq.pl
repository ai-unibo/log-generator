:- module('AlpBPM_mapping_seq', [translate_seq/6] ).


translate_seq(A,B, _Obs, Durations, Options, Stream) :-
	member(double_chained_translation, Options)
	, !
	, translate_seq_double_chain(A, B, Durations, Options, Stream).

translate_seq(A,B, Obs, Durations, Options, Stream) :-
	% write(Obs), nl, nl, 
	member(obs(A,AObs), Obs),
	translate_seq_single_chain(A, AObs, B, Obs, Durations, Options, Stream).
	
	



	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Antecedent - translation
%
translate_seq_single_chain(A,always, B, Obs, Durations, Options, Stream) :-
	( (activity_has_start_and_end(A, Durations, Options)) ->
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
	, member(obs(B, Val), Obs)
	, translate_seq_con_single_chain(B,Val, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream).	
translate_seq_single_chain(A,never, B, Obs, Durations, Options, Stream) :-
	( (activity_has_start_and_end(A, Durations, Options)) ->
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
	, member(obs(B, Val), Obs)
	, translate_seq_con_single_chain(B,Val, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream).
translate_seq_single_chain(A,partially, B, Obs, Durations, Options, Stream) :-
	translate_seq_single_chain(A,always, B, Obs, Durations, Options, Stream),
	nl(Stream),
	translate_seq_single_chain(A,never, B, Obs, Durations, Options, Stream).

% Modified by Federico: this version does not support the trace identifier
%  translate_seq_single_chain(A,always, B, Obs, Durations, Options, Stream) :-
% 	( (activity_has_start_and_end(A, Durations, Options)) ->
% 			atom_concat(A, '_end', AAtom)
% 		;
% 			AAtom = A
% 	)
% 	, write(Stream, 'E'(event(AAtom,'Pos'),'T1'))
% 	, write(Stream, ' /\\ ')
% 	, write(Stream, 'H'(event(AAtom,'Pos'),'T1'))
% 	, write(Stream, ' ---> ')
% 	, member(obs(B, Val), Obs)
% 	, translate_seq_con_single_chain(B,Val, Durations, Options, Stream)
% 	, write(Stream, '.')
% 	, nl(Stream).	
% translate_seq_single_chain(A,never, B, Obs, Durations, Options, Stream) :-
% 	( (activity_has_start_and_end(A, Durations, Options)) ->
% 		atom_concat(A, '_end', AAtom)
% 		;
% 		AAtom = A
% 	)
% 	, write(Stream, 'ABD'(event(AAtom,'Pos'),'T1'))
% 	, write(Stream, ' ---> ')
% 	, member(obs(B, Val), Obs)
% 	, translate_seq_con_single_chain(B,Val, Durations, Options, Stream)
% 	, write(Stream, '.')
% 	, nl(Stream).
% translate_seq_single_chain(A,partially, B, Obs, Durations, Options, Stream) :-
% 	translate_seq_single_chain(A,always, B, Obs, Durations, Options, Stream),
% 	nl(Stream),
% 	translate_seq_single_chain(A,never, B, Obs, Durations, Options, Stream). 






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Consequent - translation

translate_seq_con_single_chain(B,always, Durations, Options, Stream) :-
	( (activity_has_start_and_end(B, Durations, Options)) ->
		atom_concat(B, '_start', BAtom)
		;
		BAtom = B
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',BAtom,'Next'), 'T2'))
		;
			write(Stream, 'E'(event(BAtom,'Next'), 'T2'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next').
translate_seq_con_single_chain(B,never, Durations, Options, Stream) :-
	( (activity_has_start_and_end(B, Durations, Options)) ->
		atom_concat(B, '_start', BAtom)
		;
		BAtom = B
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',BAtom,'Next'), 'T2'))
		;
			write(Stream, 'ABD'(event(BAtom,'Next'), 'T2'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next').
translate_seq_con_single_chain(B,partially, Durations, Options, Stream) :-
	translate_seq_con_single_chain(B,never, Durations, Options, Stream)
	, nl(Stream)
	, write(Stream, ' \\/ ')
	, translate_seq_con_single_chain(B,always, Durations, Options, Stream).
	

	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Double-chained translation
%
translate_seq_double_chain(A, B, Durations, Options, Stream) :-
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
			write(Stream, 'H'(event('TraceId',AAtom,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',BAtom,'Next'), 'T2'))
		;
			write(Stream, 'H'(event(AAtom,'Pos'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(BAtom,'Next'), 'T2'))
	)
	, write_time_constraint(Stream, 'T1', 'T2')
	, write_pos_constraint(Stream, 'Pos', 'Next')
	, write(Stream, '.')
	, nl(Stream)
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
	.	









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Utilities

write_time_constraint(Stream, T1, T2) :-
	write(Stream, ' /\\ '), write(Stream, T2), write(Stream, '>'), write(Stream, T1).
write_pos_constraint(Stream, Pos, Next) :-
	write(Stream, ' /\\ '), write(Stream, Next), write(Stream, '=='), write(Stream, Pos), write(Stream, '+'), write(Stream, 1).
	
write_en_constraint(Stream, Start, Middle, _End) :-
	write(Stream, ' /\\ '), write(Stream, Middle), write(Stream, '>='), write(Stream, Start)
	% , write(Stream, ' /\\ '), write(Stream, Middle), write(Stream, '<='), write(Stream, _End)
	.