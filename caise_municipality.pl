

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples related to the log incompleteness problem

mun_log_incompleteness :-
	nl, write('Usage:'),nl,writef('\tmun_log_incompleteness(Trace_max_length).'), nl, nl,
	write('Trace_max_length must a postive integer.'), nl, nl.

mun_log_incompleteness(Max) :-
	mun_model(Flow, Obs),
	push_all_to_never(Obs, ObsNever),
	create_project(Flow, ObsNever, [], [trace_max_length(Max)]),
	project('sciff/temp'),
	mun_example_trace(Ex),
	statistics(runtime,_),
	log_incompleteness(Ex, MissingTraces),
	statistics(runtime,[_,Time]),
	write('Computed Missing Traces: '), length(MissingTraces, X), write(X), nl,
	write('Required time to compute all the missing traces: '), write(Time), write('ms.'), nl,
	nl, nl, write('Existing Traces:'), nl,
	print_nice_traces(Ex),
	nl, nl, write('Missing Traces:'), nl,
	print_nice_traces(MissingTraces),
	nl, nl, write('Computed Missing Traces: '), length(MissingTraces, X), write(X), nl,
	write('Required time to compute all the missing traces: '), write(Time), write('ms.'), nl,
	nl, nl.
	
mun_example_trace([ ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % Examples related to the trace incompleteness problem

mun_trace_incompleteness :-
	nl, write('Usage:'),nl,writef('\tmun_trace_incompleteness(Example, Trace_max_length).'), nl, nl,
	write('Available examples: '),
	findall(X, mun_incomplete(X, _), L),
	write(L), nl,
	write('Trace_max_length must a postive integer.'), nl, nl.
	
mun_trace_incompleteness(Example, Max) :-
	mun_incomplete(Example, Trace),
	mun_model(Flow, Obs),
	create_project(Flow, Obs, Trace, [trace_max_length(Max)]),
	project('sciff/temp'),
	write('Trying to complete the following trace:'), nl,
	print_nice_traces([Trace]), nl, nl,
	statistics(runtime,_),
	trace_incompleteness(ExtendedTraces),
	statistics(runtime,[_,Time]),
	write('Computed extensions: '), length(ExtendedTraces, X), write(X), nl,
	write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl,
	nl, nl, write('Extended Traces:'), nl,
	print_nice_traces(ExtendedTraces),
	nl, nl, nl, write('Computed extensions: '), write(X), nl,
	write('Required time to compute all the extensions: '), write(Time), write('ms.'), nl.
	

mun_incomplete(ex1, [
	hap(event(aligned_assisted_registry, _Pos5), 567)
]).


mun_incomplete(ex2, [
	hap(event(aligned_assisted_registry, _Pos1), 123)
	, hap(event(generate_apss_report, _Pos6), 678)
]).


mun_incomplete(ex3, [
	hap(event(municipality_logging, _Pos10), 123)
	, hap(event(register_municipality_data, _Pos11), 234)
	, hap(event(send_to_saia, _Pos12), 345)
	, hap(event(return_result, _Pos15), 456)
]).
	

mun_incomplete(ex4, [
	hap(event(aligned_assisted_registry, _Pos1), 123)
	, hap(event(generate_apss_report, _Pos2), 234)
	, hap(event(municipality_logging, _Pos3), 345)
	, hap(event(register_municipality_data, _Pos4), 456)
	, hap(event(send_to_saia, _Pos5), 567)
	, hap(event(return_result, _Pos6), 678)
]).
		

mun_incomplete(ex5, [
	hap(event(aligned_assisted_registry, _Pos1), 123)
	, hap(event(municipality_logging, _Pos2), 234)
	, hap(event(register_municipality_data, _Pos3), 345)
	, hap(event(send_to_saia, _Pos4), 456)
	, hap(event(return_result, _Pos5), 567)
]).


mun_incomplete(ex6, [
	hap(event(municipality_logging, _Pos1), 123)
	, hap(event(register_municipality_data, _Pos2), 234)
	, hap(event(send_to_saia, _Pos3), 345)
	, hap(event(return_result, _Pos4), 456)
	, hap(event(aligned_assisted_registry, _Pos5), 567)
]).
	

mun_incomplete(ex7, [
	hap(event(municipality_logging, 10), 123)
	, hap(event(register_municipality_data, 11), 234)
	, hap(event(send_to_saia, 12), 345)
	, hap(event(return_result, 15), 456)
	, hap(event(aligned_assisted_registry, 26), 567)
	, hap(event(generate_apss_report, 27), 678)
]).


mun_incomplete(ex7bis, [
	hap(event(municipality_logging, _Pos1), 123)
	, hap(event(register_municipality_data, _Pos2), 234)
	, hap(event(send_to_saia, _Pos3), 345)
	, hap(event(return_result, _Pos4), 456)
	, hap(event(aligned_assisted_registry, _Pos5), 567)
	, hap(event(generate_apss_report, _Pos6), 678)
]).


% mun_incomplete(ex7ter, [
	% hap(event(municipality_logging, _Pos1), _T1)
	% , hap(event(register_municipality_data, _Pos2), _T2)
	% , hap(event(send_to_saia, _Pos3), _T3)
	% , hap(event(return_result, _Pos4), _T4)
	% , hap(event(aligned_assisted_registry, _Pos5), _T5)
	% , hap(event(generate_apss_report, _Pos6), _T6)
% ]).



	


	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


mun_model(
	[
		% gamma path
		seq(start, execute_birth)
		, seq(execute_birth, produce_record)
		, seq(produce_record, parent_notification)
		, seq(parent_notification, parent_registration)
		, xor_split(parent_registration, [ present_at_the_municipality, present_at_the_hospital] )
		, seq(present_at_the_municipality, municipality_registration)
		, seq(present_at_the_hospital, hospital_registration)
		, xor_join([municipality_registration, hospital_registration], close_parent_registration)
		, seq(municipality_registration, close_parent_registration)
		, seq(hospital_registration, close_parent_registration)
		, seq(close_parent_registration, start_loop)
		, xor_join([close_parent_registration, close_loop], start_loop)
		, seq(start_loop, munic_apss_switch)
		, xor_split(munic_apss_switch, [municipality_logging, apss_logging])
		
		% alfa-path
		, seq(municipality_logging, register_municipality_data)
		, seq(register_municipality_data, send_to_saia)
		, seq(send_to_saia, communication_receipt)
		, seq(communication_receipt, technical_check)
		, xor_split(technical_check, [generate_fc, verify_fc, return_result])
		, seq(generate_fc, check_fc)
		, seq(check_fc, return_result)
		, seq(verify_fc, return_result)
		, xor_join([check_fc, verify_fc, technical_check], return_result)
		, seq(return_result, result_receipt)
		, seq(result_receipt, generate_card)
		, xor_split(generate_card, [report_to_asl, store_information])
		, seq(report_to_asl, municipality_notification_receipt)
		, seq(municipality_notification_receipt, close_generate_card)
		, seq(store_information, close_generate_card)
		, seq(close_generate_card, close_munic_apss)
		
		% beta path
		, seq(apss_logging, register_data)
		, seq(register_data, aligned_assisted_registry)
		, xor_split(aligned_assisted_registry, [municipality_record_receipt_by_apss, generate_apss_report])
		, seq(municipality_record_receipt_by_apss, close_aligned_assisted_registry)
		, seq(generate_apss_report, close_aligned_assisted_registry)
		, seq(close_aligned_assisted_registry, close_munic_apss)
		
		, xor_join([municipality_record_receipt_by_apss, generate_apss_report], close_aligned_assisted_registry)
		, xor_join([close_generate_card, close_aligned_assisted_registry], close_munic_apss)
		, seq(close_munic_apss, close_loop)
		, xor_split(close_loop, [start_loop, end_process])
				
		, seq(end_process, stop)
	],
	[
		obs(start,never)
		, obs(execute_birth, never)
		, obs(produce_record, never)
		, obs(parent_notification, never)
		, obs(parent_registration, never)
		, obs(present_at_the_municipality, never)
		, obs(present_at_the_hospital, never)
		, obs(municipality_registration, never)
		, obs(hospital_registration, never)
		, obs(close_parent_registration, never)
		, obs(start_loop, never)
		, obs(munic_apss_switch, never)
		, obs(municipality_logging, always)
		, obs(apss_logging, never)
		, obs(register_municipality_data, always)
		, obs(send_to_saia, always)
		, obs(communication_receipt, never)
		, obs(technical_check, never)
		, obs(generate_fc, never)
		, obs(verify_fc, never)
		, obs(return_result, always)
		, obs(check_fc, never)
		, obs(result_receipt, never)
		, obs(generate_card, never)
		, obs(report_to_asl, never)
		, obs(store_information, never)
		, obs(municipality_notification_receipt, never)
		, obs(close_generate_card, never)
		, obs(close_munic_apss, never)
		, obs(register_data, never)
		, obs(aligned_assisted_registry, always)
		, obs(municipality_record_receipt_by_apss, never)
		, obs(generate_apss_report, always)
		, obs(close_aligned_assisted_registry, never)
		, obs(close_loop, never)
		, obs(end_process, never)
		
		, obs(stop, never)
	]
).




