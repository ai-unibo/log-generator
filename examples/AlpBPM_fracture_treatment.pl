:- module('AlpBPM_fracture_treatment', [find_example/8] ).

% Ex 1
find_example(
			opt_incompleteness, 
			fracture_treatment,
			Model,
			Observability,
			DurationConstraints,
			InterTasksConstraints,
			Trace,
			Options) :-
	example_model(Model)
	, observability(Observability)
	, durationConstraints(DurationConstraints)
	, inter_task_constraints(InterTasksConstraints)
	, theTrace(Trace)
	, options_ex1(Options).

find_example(
			trace_generation, 
			fracture_treatment,
			Model,
			Observability,
			DurationConstraints,
			InterTasksConstraints,
			Log,
			Options) :-
	example_model(Model)
	, observability(Observability)
	, durationConstraints(DurationConstraints)
	, inter_task_constraints(InterTasksConstraints)
	, Log=[]
	, Options = [ trace_max_length(43), time_limit(10000) , instances_for_each_path(400) ].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example

example_model([
	seq(start, a1)
	, seq(a1, a2)
	% xor-split X1
		, xor_split(a2,[ a3, a5 ])	
	, seq(a3, a4)
	, seq(a5, a6)
	% xor-split X2
		, xor_split(a6,[ a7, a8 ])			
	% and-split P1
		, and_split(a8, [ a9, a10 ] )
	% and-join P2
		, and_join( [ a9, a10 ], a11 )
	, seq(a11, a12)
	% xor_join X4:
		, seq(a4, a13)
		, seq(a7, a13)
		, seq(a12, a13)
	, seq(a13, a14)
	, seq(a14, stop)
]).


observability([
	obs(start, never)
	, obs(a1, partially)
	, obs(a2, partially)
	, obs(a3, partially)
	, obs(a4, partially)
	, obs(a5, partially)
	, obs(a6, partially)
	, obs(a7, partially)
	, obs(a8, partially)
	, obs(a9, partially)
	, obs(a10, partially)
	, obs(a11, partially)
	, obs(a12, partially)
	, obs(a13, partially)
	, obs(a14, partially)
	, obs(stop, never)
]).

durationConstraints([
	  (a1, (5,10))
	, (a2, (5,10))
	, (a3, (30,40))
	, (a4, (150,200))
	, (a5, (20,40))
	, (a6, (5,10))
	, (a7, (200,225))
	, (a8, (10,20))
	, (a9, (30,40))
	, (a10, (80,80))
	, (a11, (100,120))
	, (a12, (200,400))
	, (a13, (15,20))
	, (a14, (5,10))
]).


% itc are terms like: (A,AEdge, B,BEdge, Dist, [Low,Up])
inter_task_constraints([
	(a1, '_start', a2, '_end', 1, [0,30] )
	, (a8, '_start', a11, '_start', 2, [100,140] )
	, (a10, '_end', a11, '_start', 1, [20,10000] )
	, (a10, '_start', a11, '_end', 1, [0,250] )
]).

theTrace([
	h(event(a5_start,Pos1), _T3_start),
	h(event(a5_end,Pos1), _T3_end)
	]).


options_ex1(
	[ trace_max_length(33), opt_solution ]
).