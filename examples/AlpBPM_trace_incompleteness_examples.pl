:- module('AlpBPM_trace_incompleteness_examples', [find_example/8] ).

% Ex 1
find_example(
			trace_incompleteness,
			trace_incompleteness_ex1,
			Model,
			Observability,
			DurationConstraints,
			InterTasksConstraints,
			Trace,
			Options) :-
	example_model_ex1(Model)
	, obs_ex1(Observability)
	, DurationConstraints = []
	, InterTasksConstraints = []
	, trace_ex1(Trace)
	, options_ex1(Options).

% Ex 2
find_example(
			trace_incompleteness,
			trace_incompleteness_ex2,
			Model,
			Observability,
			DurationConstraints,
			InterTasksConstraints,
			Trace,
			Options) :-
	example_model_ex1(Model)
	, obs_ex1(Observability)
	, DurationConstraints = []
	, InterTasksConstraints = []
	, trace_ex2(Trace)
	, options_ex1(Options).

% Ex 3
find_example(
			trace_incompleteness,
			trace_incompleteness_ex3,
			Model,
			Observability,
			DurationConstraints,
			InterTasksConstraints,
			Trace,
			Options) :-
	example_model_ex1(Model)
	, obs_ex1(Observability)
	, DurationConstraints = []
	, InterTasksConstraints = []
	, trace_ex3(Trace)
	, options_ex1(Options).
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example

example_model_ex1(
	[
		seq(start,ai)
		, xor_split(ai,[dp,gic,fd])
		, seq(dp,si)
		, seq(gic,pic)
		, seq(pic,si)
		, seq(fd,cd)
		, xor_split(cd,[pd,fd])
		, seq(pd,rc)
		, seq(rc,si)
		, seq(si,stop)
	]
).

obs_ex1(
	[
		obs(start,never)
		, obs(ai, partially)
		, obs(dp, partially)
		, obs(gic, partially)
		, obs(pic, partially)
		, obs(fd, partially)
		, obs(cd, partially)
		, obs(pd, partially)
		, obs(rc, partially)
		, obs(si, partially)
		, obs(stop, never)
	]
).


trace_ex1([
	hap(event(ai,_Pos1), 123)
	, hap(event(dp,_Pos2), 234)
	, hap(event(si,_Pos3), 345)
]).

trace_ex2([
	hap(event(ai,_Pos1), 123)
	, hap(event(gic,_Pos2), 234)
	, hap(event(si,_Pos3), 345)
]).

trace_ex3([
	hap(event(pd,_Pos2), 234)
	, hap(event(si,_Pos3), 345)
]).

options_ex1(
	[ trace_max_length(16), all_solutions ]
).