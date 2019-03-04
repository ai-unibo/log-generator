:- module('AlpBPM_mapping_and_split', [translate_and_split/6]).

translate_and_split( A, B, _Obs, Durations, Options, Stream) :-
	member(double_chained_translation, Options)
	, !
	, translate_and_split_double_chain(A, B, Durations, Options, Stream).
translate_and_split( A, B, Obs, Durations, Options, Stream) :-
	member(obs(A,AObs), Obs)
	, ( (activity_has_start_and_end( A, Durations, Options)) ->
		atom_concat(A, '_end', AAtom)
		;
		AAtom = A
	)
	, write_support_rule(AAtom, AObs, B, Obs, Durations, Options, Stream)
	, nl(Stream)
	, translate_triggering_rules(B, Obs, Durations, Options, Stream)
	, nl(Stream)
	, nl(Stream).
	



	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Support rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_support_rule(A,always, B, _Obs, Durations, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',A,'Pos'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event('TraceId',A,'Pos'),'T1'))
		;
			write(Stream, 'E'(event(A,'Pos'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event(A,'Pos'),'T1'))
	)
	, write(Stream, ' ---> ')
	, write_support_abducibles(B, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream).
write_support_rule(A,never, B, _Obs, Durations, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',A,'Pos'),'T1'))
		;
			write(Stream, 'ABD'(event(A,'Pos'),'T1'))
	)
	, write(Stream, ' ---> ')
	, write_support_abducibles(B, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream).
write_support_rule(A,partially, B, _Obs, Durations, Options,  Stream) :-
	write_support_rule(A,always, B, Obs, Durations, Options, Stream),
	nl(Stream),
	write_support_rule(A,never, B, Obs, Durations, Options, Stream).
%
%
write_support_abducibles([], _, _, _).
write_support_abducibles([BHead|BTail], Durations, Options, Stream) :-
	% support_descriptor(BHead, BDesc)
	( (activity_has_start_and_end( BHead, Durations, Options)) ->
		atom_concat(BHead, '_start', BAtom)
		;
		BAtom = BHead
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(and_split_support('TraceId',BAtom,'Pos'), 'T1'))
		;
			write(Stream, 'ABD'(and_split_support('TraceId',BAtom,'Pos'), 'T1'))
	)
	, write_support_abducibles_more(BTail, Durations, Options, Stream).
%	
write_support_abducibles_more([], _, _, _).
write_support_abducibles_more([BHead|BTail], Durations, Options, Stream) :-
	% support_descriptor(BHead, BDesc)
	( (activity_has_start_and_end( BHead, Durations, Options)) ->
		atom_concat(BHead, '_start', BAtom)
		;
		BAtom = BHead
	)
	, nl(Stream)
	, write(Stream, ' /\\ ')
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(and_split_support('TraceId',BAtom,'Pos'), 'T1'))
		;
			write(Stream, 'ABD'(and_split_support(BAtom,'Pos'), 'T1'))
	)
	, write_support_abducibles_more(BTail, Durations, Options, Stream).


	
	
	
% event_descriptor(A, event(A,'Pos')) :-
	% caise_option(position_explicit, yes), !.
% event_descriptor(A, A) :-
	% caise_option(position_explicit, no), !.
% event_descriptor(A, event(A,'Pos')).

% support_descriptor(BHead, and_split_support(BHead,'Pos')) :-
	% caise_option(position_explicit, yes), !.
% support_descriptor(BHead, and_split_support(BHead)) :-
	% caise_option(position_explicit, no), !.
% support_descriptor(BHead, and_split_support(BHead,'Pos')).

% exp_descriptor(A, event(A,'Next')) :-
	% caise_option(position_explicit, yes), !.
% exp_descriptor(A, A) :-
	% caise_option(position_explicit, no), !.
% exp_descriptor(A, event(A,'Next')).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Triggering rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

translate_triggering_rules([], _, _, _, _).
translate_triggering_rules([BHead|BTail], Obs, Durations, Options, Stream) :-
	translate_one_triggering_rule(BHead, Obs, Durations, Options, Stream)
	, nl(Stream)
	, nl(Stream)
	, translate_triggering_rules(BTail, Obs, Durations, Options, Stream).
%
%	
translate_one_triggering_rule(BHead, Obs, Durations, Options, Stream) :-
	% support_descriptor(BHead, BDesc)
	member(obs(BHead,BObs), Obs)
	, ( (activity_has_start_and_end( BHead, Durations, Options)) ->
		atom_concat(BHead, '_start', BAtom)
		;
		BAtom = BHead
	)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(and_split_support('TraceId',BAtom,'Pos'), 'T1'))
		;
			write(Stream, 'ABD'(and_split_support(BAtom,'Pos'), 'T1'))
	)
	, write(Stream, ' ---> ')
	, write_trigger_consequent(BAtom, BObs, Options, Stream)
	, write(Stream, '.').


	

write_trigger_consequent(BHead, always, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',BHead,'Next'), 'T2'))
		;
			write(Stream, 'E'(event(BHead,'Next'), 'T2'))
	)
	, write(Stream, ' /\\ '), write(Stream, 'T2>T1')
	, write(Stream, ' /\\ '), write(Stream, 'Next==Pos+1').
write_trigger_consequent(BHead, never, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',BHead,'Next'), 'T2'))
		;
			write(Stream, 'ABD'(event(BHead,'Next'), 'T2'))
	)
	, write(Stream, ' /\\ '), write(Stream, 'T2>T1')
	, write(Stream, ' /\\ '), write(Stream, 'Next==Pos+1').
write_trigger_consequent(BHead, partially, Options, Stream) :-
	write_trigger_consequent(BHead, always, Options, Stream)
	, nl(Stream)
	, write(Stream, ' \\/ ')
	, write_trigger_consequent(BHead, never, Options, Stream).

	
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Double Chained translation

translate_and_split_double_chain(A, [BHead | BTail], Durations, Options, Stream) :-
	( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',A,'Pos'),'T1'))
		;
			write(Stream, 'H'(event(A,'Pos'),'T1'))
	)
	, write(Stream, ' ---> ')
	, write_single_and_split(BHead, Durations, Options, Stream)
	, write_more_and_split(BTail, Durations, Options, Stream)
	, write(Stream, '.')
	, nl(Stream)
	, translate_and_split_backward_chain([BHead | BTail], A, Durations, Options, Stream)
	.
write_single_and_split(BHead, Durations, Options, Stream) :-
	( (activity_has_start_and_end( BHead, Durations, Options)) ->
		atom_concat(BHead, '_start', BAtom)
		;
		BAtom = BHead
	)
	, atom_concat('T', BHead, TimeAtom)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',BAtom,'Next'), TimeAtom))
		;
			write(Stream, 'E'(event(BAtom,'Next'), TimeAtom))
	)
	, write(Stream, ' /\\ '), write(Stream, TimeAtom), write(Stream, '>T1')
	, write(Stream, ' /\\ '), write(Stream, 'Next==Pos+1')
	.
write_more_and_split([], _Durations, _Options, _Stream).
write_more_and_split([BHead | BTail], Durations, Options, Stream) :-
	nl(Stream)
	, write(Stream, '   /\\ ')
	, write_single_and_split(BHead, Durations, Options, Stream)
	, write_more_and_split(BTail, Durations, Options, Stream)
	.
translate_and_split_backward_chain([], _A, _Durations, _Options, _Stream).
translate_and_split_backward_chain([B | Tail], A, Durations, Options, Stream) :-
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
	, write(Stream, ' /\\ '), write(Stream, 'T2>T1')
	, write(Stream, ' /\\ '), write(Stream, 'Next==Pos+1')
	, write(Stream, '.')
	, nl(Stream)
	, translate_and_split_backward_chain(Tail, A, Durations, Options, Stream)
	.
	



