% This is the main file for the AlpBPM Prototype


:- [sciff].


:- ['AlpBPM_util'].
:- ['AlpBPM_parser'].

% Reasoning tasks
:- ['AlpBPM_log_incompleteness'].
:- ['AlpBPM_trace_event_incompleteness'].
:- ['AlpBPM_opt_incompleteness'].

:- ['AlpBPM_trace_generation'].
:- ['AlpBPM_trace_generation_declare'].
:- ['AlpBPM_spark_trace_verification'].
:- ['AlpBPM_trace_generation_negatives'].


:- ['AlpBPM_examples'].



% :- [caise_optimisation].
% :- [caise_ijcai].



%:- [caise_municipality].



% :- [university_exam].
% :- [caise_university_example].




	
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% COMMON UTILITIES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_nice_traces([]).
print_nice_traces([[]|T]) :-
	!, 
	write('Trace: '), nl,
	write('EMPTY TRACE.'),
	nl, nl,
	print_nice_traces(T).
print_nice_traces([H|T]) :-
	write('Trace: '), nl,
	determine_arity(H, N),
	(sort( [N,2],@=<, H, HSorted) ->
		print_single_trace(HSorted)
		;
		print_single_trace(H)
	),
	nl, nl,
	print_nice_traces(T).


print_single_trace([]).
print_single_trace([H|T]) :-
	write(H), nl,
	print_single_trace(T).

determine_arity([H|_], N) :- functor(H,_,M), N is M-1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The all_distinct constraint re-written...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
my_all_distinct([]).
my_all_distinct([_]).
my_all_distinct([H|[T1|T2]]) :-
	set_the_constraint(H, [T1|T2]),
	my_all_distinct([T1|T2]).

set_the_constraint(_, []).
set_the_constraint(H, [T1|T2]) :-
	H #\= T1,
	set_the_constraint(H, T2).

	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CAiSE OPTIONS management
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic(caise_option/2).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generating choice points for unbound/unknown activities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% call_caise_list([], _Obs).
% call_caise_list([h(event(Activity,Pos),T) | Tail], Obs) :-
	% var(Activity),
	% !,
	% member(obs(Activity,_), Obs),
	% % write('Member!'), nl,
	% call(h(event(Activity,Pos),T)),
	% call_caise_list(Tail, Obs).
% call_caise_list([h(event(Activity,Pos),T) | Tail], Obs) :-
	% call(h(event(Activity,Pos),T)),
	% call_caise_list(Tail, Obs).


call_smart_list([], _Candidates).
call_smart_list([h(event(Activity,Pos),T) | Tail], Candidates) :-
	var(Activity),
	!,
	member(Activity, Candidates),
	% write('Member!'), nl,
	call(h(event(Activity,Pos),T)),
	call_smart_list(Tail, Candidates).
call_smart_list([h(event(Activity,Pos),T) | Tail], Candidates) :-
	call(h(event(Activity,Pos),T)),
	call_smart_list(Tail, Candidates).


	

