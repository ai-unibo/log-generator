:- module('AlpBPM_util', [
			activityHasDurationConstraints/4
			, find_activities_in_loops/3
			, get_activity_list/2
			, get_activity_list/4
			, activity_has_start_and_end/3
] ).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

activityHasDurationConstraints(A, Durations, Min, Max) :-
	member((A, (Min,Max)), Durations)
	, ( (is_number(Min)) ->
		true
		;
		write('WARNING: minimal duration for activity '), write(A), write(' is wrongly specified.'), nl, abort
	)
	, ( (is_number(Max)) ->
		true
		;
		write('WARNING: maximal duration for activity '), write(A), write(' is wrongly specified.'), nl, abort
	)
	, ( ( Min =< Max) ->
		true
		;
		write('WARNING: max duration for activity '), write(A), write(' must be greater than minimal duration.'), nl, abort
	)
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%  activity_has_start_and_end( Activity, Durations, Options)
activity_has_start_and_end( start, _Durations, _Options) :- !, fail.
activity_has_start_and_end( stop, _Durations, _Options) :- !, fail.
activity_has_start_and_end( _Activity, _Durations, Options) :-
	member(activities_have_start_and_end, Options)
	, !
	.
activity_has_start_and_end( Activity, Durations, _Options) :-
	activityHasDurationConstraints(Activity, Durations, _Min, _Max).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Tarjan Algorithm for strongly connected components.
:- dynamic tindex/1, tindex/2, tstack/1.
:- dynamic lowlink/2, onStack/1.
:- dynamic scc/1.
find_activities_in_loops(Model, InLoop, NotInLoop) :-
	retractall(tindex(_))
	, retractall(tindex(_,_))
	, retractall(tstack(_))
	, retractall(lowlink(_,_))
	, retractall(onStack(_))
	, retractall(scc(_))
	, assert(tindex(0))
	, assert(tstack([]))
	, assert(scc([]))
	, get_activity_list(Model, LA)
	, strongconnect(LA, Model)
	, scc(SCC)
	, loops(SCC, InLoop, NotInLoop)
	.

loops([], [], []).
loops([[H]|T], InLoop, [H|TResult]) :-
	!
	, loops(T, InLoop, TResult).
loops([H|T], Result, NotInLoop) :-
	loops(T, RTemp, NotInLoop),
	append(H, RTemp, Result).

get_activity_list([], []) :- !.
get_activity_list(Model, LA) :-
	setof(A, get_activity(Model, A), LA).

% get_activity_list(Flow, DurationConstraints, InterTaskConstraints, Result).
get_activity_list(Flow, DurationConstraints, InterTaskConstraints, Result) :-
	get_activity_list(Flow, LA)
	, get_activity_list(DurationConstraints, LB)
	, get_activity_list(InterTaskConstraints, LC)
	, union(LA, LB, LD)
	, union(LC, LD, Result).


	
get_activity(M,A) :- member(seq(A,_), M).
get_activity(M,A) :- member(xor_split(A,_), M).
get_activity(M,B) :- member(xor_split(_A,LB), M), member(B, LB).
get_activity(M,B) :- member(xor_join(_A,B), M).
get_activity(M,A) :- member(xor_join(LA,_B), M), member(A, LA).
get_activity(M,A) :- member(and_split(A,_), M).
get_activity(M,B) :- member(and_split(_A,LB), M), member(B, LB).
get_activity(M,B) :- member(and_join(_A,B), M).
get_activity(M,A) :- member(and_join(LA,_B), M), member(A, LA).
get_activity(M,A) :- member(seq(_,A), M).
get_activity(M,A) :- member(obs(A,_), M), A \= start, A \= stop.
get_activity(M,A) :- member( (A,(_,_)), M).



edge(M,A,B) :- member(seq(A,B), M).
edge(M,A,B) :- member(xor_split(A,LA), M), member(B, LA).
edge(M,A,B) :- member(xor_join(LA,B), M), member(A, LA).
edge(M,A,B) :- member(and_split(A,LA), M), member(B, LA).
edge(M,A,B) :- member(and_join(LA,B), M), member(A, LA).

strongconnect([], _).
strongconnect([V | T], M) :-
	tindex(V, _VIndex)
	, !
	, strongconnect(T, M).
strongconnect([V | T], M) :-
	retractall(tindex(V, _))
	, tindex(Index)
	, assert(tindex(V, Index))
	, retractall(lowlink(V,_))
	, assert(lowlink(V,Index))
	, retractall(tindex(Index))
	, NewIndex is Index+1
	, assert(tindex(NewIndex))
	, tstack(Stack)
	, retractall(tstack(_))
	, assert(tstack([V|Stack]))
	, assert(onStack(V))
	, findall((V,W), edge(M, V,W), LE)
	%
	, successors_more(M, LE)
	%
	, lowlink(V, LowV)
	, tindex(V, VIndex)
	, ( (LowV = VIndex) ->
		scc(TailScc)
		, retractall(scc(_))
		, generateSCC(V, NewScc)
		, assert(scc([NewScc|TailScc]))
	;
		true
	)
	, strongconnect(T, M)
	.

successors_more(_M, []).
successors_more(M, [(V,W)|WT]) :-
	successors(M, (V,W))
	, successors_more(M, WT)
	.
successors(M, (V,W)) :-
	( (tindex(W, WI), ground(WI)) ->
		( (onStack(W)) ->
			lowlink(V, LowV)
			, tindex(W, WIndex)
			, NewLow is min(LowV, WIndex)
			, retractall(lowlink(V,_))
			, assert(lowlink(V, NewLow))
		;
			true
		)
	;
		strongconnect([W], M)
		, lowlink(V, LowV)
		, lowlink(W, LowW)
		, NewLow is min(LowV, LowW)
		, retractall(lowlink(V,_))
		, assert(lowlink(V, NewLow))
	).

generateSCC(V, [W|RTail]) :-
	tstack([W|Stack])
	, retractall(tstack(_))
	, assert(tstack(Stack))
	, retractall(onStack(W))
	, ( (V = W) ->
		RTail = []
	;
		generateSCC(V, RTail)
	)
	.
	

test :-
	model(M)
	, find_activities_in_loops(M, InLoop, NotInLoop)
	, write(InLoop), nl
	, write(NotInLoop), nl.
	

% model(
  % [
		% seq(start,ai)
		% , xor_split(ai,[dp,gic,fd])
		% , seq(dp,si)
		% , seq(gic,pic)
		% , seq(pic,si)
		% , seq(fd,cd)
		% , xor_split(cd,[pd,fd])
		% , seq(pd,rc)
		% , seq(rc,si)
		% , seq(si,stop)
	% ]
% ).

model(
  [
    seq(a,ex)
    , seq(pat,e)
    , xor_split(e,[sla,slpa,slr])
    , and_join([pat,tat,txj],e)
    , seq(tat,e)
    , and_split(tas,[pat,tat,txs])    
    , seq(txj,e)
    , xor_join(txj,[toe,scm])
    , seq(sla,a)
    , seq(dr,cd)
    , xor_split(txs,[scm,lcm])
    , seq(r,ex)
    , seq(sbc,cr)
    , seq(slr,r)
    , seq(wbc,sbc)
    , seq(sdp,dr)
    , xor_split(cc,[gag,bag])
    , xor_split(cd,[tas,ao])
    , seq(start,ao)
    , seq(bag,paca)
    , seq(paca,ccj)
    , seq(toe,txj)
    , seq(ccj,ex)
    , seq(slpa,ap)
    , seq(gag,paco)
    , seq(ao,sdp)
    , seq(ap,wbc)
    , seq(cr,cc)
    , seq(ex,end)
    , seq(lcm,toe)
    , seq(paco,ccj)
    , seq(scm,txj)
  ]
).
	
% model(	
% [
% seq(start, a1)
	% , seq(a1, a2)
	% % xor-split X1
		% , xor_split(a2,[ a3, a5 ])	
	% , seq(a3, a4)
	% , seq(a5, a6)
	% % xor-split X2
		% , xor_split(a6,[ a7, a8 ])			
	% % and-split P1
		% , and_split(a8, [ a9, a10 ] )
	% % and-join P2
		% , and_join( [ a9, a10 ], a11 )
	% , seq(a11, a12)
	% % xor_join X4:
		% , seq(a4, a13)
		% , seq(a7, a13)
		% , seq(a12, a13)
	% , seq(a13, a14)
	% , seq(a14, stop)
% ]).