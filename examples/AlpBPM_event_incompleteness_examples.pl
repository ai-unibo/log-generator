:- module('AlpBPM_event_incompleteness_examples', [find_example/8] ).

% Ex 1
find_example(
			event_incompleteness, 
			event_incompleteness_ex1,
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
	, event_incomplete(ex1, Trace)
	, options_ex1(Options).

% Ex 2
find_example(
			event_incompleteness,
			event_incompleteness_ex2,
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
	, event_incomplete(ex2, Trace)
	, options_ex2(Options).

% Ex 3
find_example(
			event_incompleteness,
			event_incompleteness_ex3,
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
	, event_incomplete(ex3, Trace)
	, options_ex3(Options).
	
% Ex 4
find_example(
			event_incompleteness,
			event_incompleteness_ex4,
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
	, event_incomplete(ex4, Trace)
	, options_ex4(Options).
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


event_incomplete(ex1, [
	h(event(_Ev,_Pos1), 234)
	, h(event(si,_Pos2), _T2)
	, h(event(ai,_Pos3), _T3)
]).

event_incomplete(ex2, [
	h(event(_Ev1,_Pos1), _T1)
	, h(event(_Ev2,_Pos2), 234)
	, h(event(si,_Pos3), _T3)
	, h(event(ai,_Pos4), 123)
]).

event_incomplete(ex3, [
	h(event(ai,_Pos1), 123)
	, h(event(si,_Pos2), 678)
	, h(event(cd,_Pos3), 345)
	, h(event(pd,_Pos4), _T4)
	, h(event(fd,_Pos5), _T5)
	, h(event(rc,_Pos6), _T6)
]).

event_incomplete(ex4, [
	h(event(cd,_Pos1), 345)
	, h(event(_Ev2,_Pos2), _T2)
	, h(event(_Ev3,_Pos3), _T3)
	, h(event(pd,_Pos4), _T4)
	, h(event(ai,_Pos5), 123)
	, h(event(rc,_Pos6), _T6)
	, h(event(si,_Pos7), _T7)
	, h(event(fd,_Pos8), _T8)
]).

options_ex1(
	[ trace_max_length(5), all_solutions ]
).

options_ex2(
	[ trace_max_length(6), all_solutions ]
).

options_ex3(
	[ trace_max_length(8), all_solutions ]
).

options_ex4(
	[ trace_max_length(10), all_solutions ]
).