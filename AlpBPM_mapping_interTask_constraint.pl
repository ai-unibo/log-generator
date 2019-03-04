:- module('AlpBPM_mapping_interTask_constraint', [translate_interTask_constraint/3]).

translate_interTask_constraint([], _Options, _Stream) :- !.
translate_interTask_constraint([Head | Tail], Options, Stream) :-
	write(Stream, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'), nl(Stream)
	, write(Stream, '% Inter Tasks constraints:'), nl(Stream), nl(Stream)
	, (member(double_chained_translation, Options) ->
			translate_interTask_constraint_more_double_chain([Head | Tail], Options, Stream)
		;
			translate_interTask_constraint_more([Head | Tail], Options, Stream)
	)
	.

translate_interTask_constraint_more([], _Options, _Stream) :- !.
translate_interTask_constraint_more([Head | Tail], Options, Stream) :-
	translate_interTask_constraint_one_h(Head, Options, Stream)
	, translate_interTask_constraint_one_abd(Head, Options, Stream)
	, nl(Stream)
	, translate_interTask_constraint_more(Tail, Options, Stream).
	
	
	


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Writing the rules
translate_interTask_constraint_one_h((A,AEdge, B,BEdge, Dist, [Low,Up]), Options, Stream) :-
	% A =.. [ADesc, duration(_,_)]
	% , ADesc \= start, ADesc \= stop
	% , B =.. [BDesc, duration(_,_)]
	% , BDesc \= start, BDesc \= stop
	% , !
	atom_concat(A, AEdge, AAtom)
	, atom_concat(B, BEdge, BAtom)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'E'(event('TraceId',AAtom,'PosA'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event('TraceId',AAtom,'PosA'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'E'(event('TraceId',BAtom,'PosB'),'T2'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event('TraceId',BAtom,'PosB'),'T2'))
		;
			write(Stream, 'E'(event(AAtom,'PosA'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event(AAtom,'PosA'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'E'(event(BAtom,'PosB'),'T2'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'H'(event(BAtom,'PosB'),'T2'))
	)
	, write(Stream, ' /\\ ')
	, write(Stream, 'PosB'), write(Stream, '=='), write(Stream, 'PosA'), write(Stream,'+'), write(Stream,Dist)
	, write(Stream, ' ---> ')
	, write(Stream, 'T2'), write(Stream, '>='), write(Stream, 'T1'), write(Stream,'+'), write(Stream,Low)
	, write(Stream, ' /\\ ')
	, write(Stream, 'T2'), write(Stream, '<='), write(Stream, 'T1'), write(Stream,'+'), write(Stream,Up)
	, write(Stream, '.')
	, nl(Stream).
translate_interTask_constraint_one_abd((A,AEdge, B,BEdge, Dist, [Low,Up]), Options, Stream) :-
	% A =.. [ADesc, duration(_,_)]
	% , ADesc \= start, ADesc \= stop
	% , B =.. [BDesc, duration(_,_)]
	% , BDesc \= start, BDesc \= stop
	% , ! 
	atom_concat(A, AEdge, AAtom)
	, atom_concat(B, BEdge, BAtom)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'ABD'(event('TraceId',AAtom,'PosA'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'ABD'(event('TraceId',BAtom,'PosB'), 'T2'))
		;
			write(Stream, 'ABD'(event(AAtom,'PosA'),'T1'))
			, write(Stream, ' /\\ ')
			, write(Stream, 'ABD'(event(BAtom,'PosB'), 'T2'))
	)
	, write(Stream, ' /\\ ')
	, write(Stream, 'PosB'), write(Stream, '=='), write(Stream, 'PosA'), write(Stream,'+'), write(Stream,Dist)
	, write(Stream, ' ---> ')
	, write(Stream, 'T2'), write(Stream, '>='), write(Stream, 'T1'), write(Stream,'+'), write(Stream,Low)
	, write(Stream, ' /\\ '), write(Stream, 'T2'), write(Stream, '<='), write(Stream, 'T1'), write(Stream,'+'), write(Stream,Up)
	, write(Stream, '.')
	, nl(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Double Chained Translation

translate_interTask_constraint_more_double_chain([], _Options, _Stream) :- !.
translate_interTask_constraint_more_double_chain([Head | Tail], Options, Stream) :-
	Head = (A,AEdge, B,BEdge, _Dist, [Low,Up])
	, atom_concat(A, AEdge, AAtom)
	, atom_concat(B, BEdge, BAtom)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',AAtom,'_PosA'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',BAtom,'_PosB'),'T2'))
		;
			write(Stream, 'H'(event(AAtom,'_PosA'),'T1'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(BAtom,'_PosB'),'T2'))
	)
	, write(Stream, ' /\\ T2>=T1+'), write(Stream,Low)
	, write(Stream, ' /\\ T2<=T1+'), write(Stream,Up)
	, write(Stream, '.')
	, nl(Stream)
	, ( (member(events_contain_traceId(yes), Options) ) ->
			write(Stream, 'H'(event('TraceId',BAtom,'_PosB'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event('TraceId',AAtom,'_PosA'),'T1'))
		;
			write(Stream, 'H'(event(BAtom,'_PosB'),'T2'))
			, write(Stream, ' ---> ')
			, write(Stream, 'E'(event(AAtom,'_PosA'),'T1'))
	)
	, write(Stream, ' /\\ T2>=T1+'), write(Stream,Low)
	, write(Stream, ' /\\ T2<=T1+'), write(Stream,Up)
	, write(Stream, '.')
	, nl(Stream)
	, nl(Stream)
	, translate_interTask_constraint_more_double_chain(Tail, Options, Stream).