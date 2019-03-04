:- module('AlpBPM_log_incompleteness_examples', [find_example/8] ).


find_example(
			log_incompleteness,
			log_incompleteness_ex1,
			Model,
			Observability,
			DurationConstraints,
			InterTasksConstraints,
			Log,
			Options) :-
	example_model_ex1(Model)
	, obs_ex1(Observability)
	, DurationConstraints = []
	, InterTasksConstraints = []
	, traces_ex1(Log)
	, options_ex1(Options).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Example 1	

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

traces_ex1([
	[hap(event(start,_), _)
	, hap(event(ai,_), 23)
	, hap(event(dp,_), 34)
	, hap(event(si,_), 45)
	, hap(event(stop,_), _)
	],
	[hap(event(start,_), _)
	, hap(event(ai,_), 23)
	, hap(event(fd,_), 45)
	, hap(event(cd,_), 56)
	, hap(event(pd,_), 67)
	, hap(event(rc,_), 78)
	, hap(event(si,_), 89)
	, hap(event(stop,_), _)
	]
]).

options_ex1(
	[ trace_max_length(16) ]
).