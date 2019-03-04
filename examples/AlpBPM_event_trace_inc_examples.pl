:- module('AlpBPM_event_trace_inc_examples', [find_example/8] ).

% Ex 1
find_example(
			event_trace_incompleteness,
			event_trace_incompleteness_ex1,
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
			event_trace_incompleteness,
			event_trace_incompleteness_ex2,
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
	h(event(_Ev,_Pos1), 234)
	, h(event(ai,_Pos2), _T2)
	]).

trace_ex2([
	h(event(_Ev1,_Pos1), _T1)
	, h(event(_Ev2,_Pos2), 234)
	, h(event(si,_Pos3), _T3)
	, h(event(ai,_Pos4), 123)
]).


options_ex1(
	[ trace_max_length(16), all_solutions ]
).