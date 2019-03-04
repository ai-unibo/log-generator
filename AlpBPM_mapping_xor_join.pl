:- module('AlpBPM_mapping_xor_join', [translate_xor_join/6]).

translate_xor_join(AList, B, _Obs, Durations, Options, Stream) :-
	member(double_chained_translation, Options)
	, !
	, translate_xor_join_double_chain(AList, B, Durations, Options, Stream)
	.
translate_xor_join(AList,B, Obs, Durations, Options, Stream) :-	
	translate_triggering_rules_single_chain(AList, B, Obs, Durations, Options, Stream)
	, nl(Stream)
	.



	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Triggering rules	
translate_triggering_rules_single_chain([], _B, _Obs, _Durations, _Options, _Stream).
translate_triggering_rules_single_chain([AHead|ATail], B, Obs, Durations, Options, Stream) :-
	( (activity_has_start_and_end(AHead, Durations, Options)) ->
			atom_concat(AHead, '_end', AAtom)
		;
			AAtom = AHead
	)
	, member(obs(AHead,AObs), Obs)
	, member(obs(B,BObs), Obs)
	, translate_single_rule(AAtom, AObs, B, BObs, Durations, Options, Stream)
	, nl(Stream)
	, translate_triggering_rules_single_chain(ATail, B, Obs, Durations, Options, Stream)
	.

%%  translate_single_rule(A, AObs, B, Obs, Durations, Options, Stream)
translate_single_rule(A, always, B, BObs, Durations, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',A,'Pos'), 'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event('TraceId',A,'Pos'),'T1'))
		;
			write(Stream, 'E'(event(A,'Pos'), 'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event(A,'Pos'),'T1'))
	)
	, write(Stream, ' ---> ')
	, write_consequent(B, BObs, Durations, Options, Stream)
	, write(Stream, '.')
	.
translate_single_rule(A, never, B, BObs, Durations, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',A,'Pos'), 'T1'))
		;
			write(Stream, 'ABD'(event(A,'Pos'), 'T1'))
	)
	, write(Stream, ' ---> ')
	, write_consequent(B, BObs, Durations, Options, Stream)
	, write(Stream, '.')
	.
translate_single_rule(AAtom, partially, B, BObs, Durations, Options, Stream) :-
	translate_single_rule(AAtom, always, B, BObs, Durations, Options, Stream)
	, nl(Stream)
	, translate_single_rule(AAtom, never, B, BObs, Durations, Options, Stream)
	.

write_consequent(B, always, Durations, Options, Stream) :-
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
	, write(Stream, ' /\\ T2>T1')
	, write(Stream, ' /\\ Next==Pos+1')
	.
write_consequent(B, never, Durations, Options, Stream) :-
	( (activity_has_start_and_end(B, Durations, Options)) ->
			atom_concat(B, '_start', BAtom)
		;
			BAtom = B
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId', BAtom,'Next'), 'T2'))
		;
			write(Stream, 'ABD'(event(BAtom,'Next'), 'T2'))
	)
	, write(Stream, ' /\\ T2>T1')
	, write(Stream, ' /\\ Next==Pos+1')
	.
write_consequent(B, partially, Durations, Options, Stream) :-
	write_consequent(B, always, Durations, Options, Stream)
	, nl(Stream)
	, write(Stream, '   \\/ ')
	, write_consequent(B, never, Durations, Options, Stream)
	.


	
	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Double chained translation

translate_xor_join_double_chain(AList, B, Durations, Options, Stream) :-
	translate_double_chain_forward(AList, B, Durations, Options, Stream)
	, translate_double_chain_backward(AList, B, Durations, Options, Stream)
	.

%%  translate_double_chain_forward(AList, B, Durations, Options, Stream)
translate_double_chain_forward([], _B, _Durations, _Options, _Stream).
translate_double_chain_forward( [A | ATail], B, Durations, Options, Stream) :-
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
	, write(Stream, ' /\\ T2>T1')
	, write(Stream, ' /\\ Next==Pos+1')
	, write(Stream, '.')
	, nl(Stream)
	, translate_double_chain_forward( ATail, B, Durations, Options, Stream)
	.

%%  translate_double_chain_backward(AList, B, Durations, Options, Stream)
translate_double_chain_backward(AList, B, Durations, Options, Stream) :-
	( (activity_has_start_and_end(B, Durations, Options)) ->
		atom_concat(B, '_start', BAtom)
		;
		BAtom = B
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',BAtom,'Next'),'T2'))
		;
			write(Stream, 'H'(event(BAtom,'Next'),'T2'))
	)
	, write(Stream, ' ---> ')
	, write_double_chain_disjunct(AList, B, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream)
	.
write_double_chain_disjunct([A | ATail], B, Durations, Options, Stream) :-
	( (activity_has_start_and_end(A, Durations, Options)) ->
			atom_concat(A, '_end', AAtom)
		;
			AAtom = A
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',AAtom,'Pos'), 'T1'))
		;
			write(Stream, 'E'(event(AAtom,'Pos'), 'T1'))
	)
	, write(Stream, ' /\\ T2>T1')
	, write(Stream, ' /\\ Next==Pos+1')
	, write_double_chain_disjunct_more(ATail, B, Durations, Options, Stream)
	.
write_double_chain_disjunct_more([], _B, _Durations, _Options, _Stream).
write_double_chain_disjunct_more([A | ATail], B, Durations, Options, Stream) :-
	( (activity_has_start_and_end(A, Durations, Options)) ->
			atom_concat(A, '_end', AAtom)
		;
			AAtom = A
	)
	, nl(Stream)
	, write(Stream, '   \\/ ')
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',AAtom,'Pos'), 'T1'))
		;
			write(Stream, 'E'(event('TraceId',AAtom,'Pos'), 'T1'))
	)
	, write(Stream, ' /\\ T2>T1')
	, write(Stream, ' /\\ Next==Pos+1')
	, write_double_chain_disjunct_more(ATail, B, Durations, Options, Stream)
	.