# log-generator
Synthetic Log Generation through Abduction

As the recent years have seen the rise of a new discipline commonly addressed as Process Mining, focused on the analysis and management of business processes, two tasks have gained increasing attention in research: process discovery and compliance monitoring. In both these fields of Process Mining, the pressing demand for event log benchmarks with predefined characteristics - useful to evaluate the process model and the techniques for its discovery - has determined the design of various methodologies and tools for synthetic log generation.

Real-life as well as artificially created logs often contain positive examples only (i.e., process instances deemed as compliant w.r.t. the model), while the presence of negative process instances (i.e., non-compliant traces) can be crucial to correctly evaluate the performance and robustness of a novel process discovery or conformance checking technique.

In this work, we report our experience in the design of a generator of synthetic logs, which takes as input a declarative or structured workflow model, encoded in the abductive declarative language SCIFF, and provides as output logs containing positive and negative traces. Our approach provides both a highly expressive notation for the description of the business model, and the ability to generate logs with various customizable features, such as the capability of generating trace templates as well as grounded traces, the possibility of taking into account user-defined constraints on data and time, and the capability of generating traces starting from a user-specified partial trace.

The work at hand presents our abductive approach to log synthesis as well as a study on positive and negative trace generation in case of open and closed model specification. The theoretical approach has been concretely employed in the implementation of a first sotware prototype for synthetic log generation.

### Pre-requisites
The prototype is built upon the SCIFF Proof Procedure, which in turn is a prolog-based program. Due to the use of some specific Prolog libraries, not all the Prolog interpreters/compilers are suitable for executing the SCIFF. The current prototype has been developed and tested using SWI-Prolog 7.2.3. More recent versions (should) work fine as well.

Download the lates SWI_Prolog interpreter suitable for your platform from the SWI-Prolog website http://www.swi-prolog.org.
Install the SWI-Prolog interpreter following the instructions provided with the interpreter.
If everything goes fine, you should be able to open a SWI-Prolog interpreter and to pose few simple queries. For example, typing `pwd.` at the SWI console, should print out the current working directory.


### Download and Execution instructions
Please clone the repository and find the sources (and executables, it's Prolog!) of the Synthetic Log Generator.

Double-click on the file `AlpBPM.pl`: if you installed SWI-pl correctly, the double click action will invoke the SWI-pl interpreter, and the `AlpBPM.pl` file will be automatically loaded/consulted.
If no error messages appear, you are ready to experiment with our prototype.

The SCIFF Trace generator is an abductive logic program which takes as input a business process model (specified through procedural or declarative contraints) and generates as output synthetic logs containing positive and negative traces according to a set of specified generation options.

You can execute a first generation example from a procedural business model by running:
```
?- procedural_example. 
```
The model used in this example is illustrated in Figure 1 of the paper **"Generating synthetic positive and negative business process traces through abduction", sumbitted to Knowledge And Information Systems. Springer London. ISSN: 0219-1377 (Print) 0219-3116 (Online)**

Alternatively, you can test the generation feature with the declarative business model in Figure 2:
```
?- declarative_example. 
```
The results of the generation can be found in the OUTPUT/ directory.

#### Run your own generation process
In order to run your generation process, you have to call the `trace_generation_negatives/4` predicate, which takes the following parameters:
```
?- trace_generation_negatives(
          Model   					%% the procedural/declarative model encoded in intermediate coding language
	  	      						%% e.g., [start(a), xor_split(a,[b,c]), seq(c,d), seq(b,e), seq(d,e) ] 
        , ActivityList				%% the list of all possible activities that may occurr 
									%% e.g., [a,b,c,d,e,f]
        , FileNameForSavingTraces	%% the path of the file where the generated log will be saved
        , Options  					%% the generation options (see the following for details)
    ).
```

The Options list allows to drive the execution process by specifying a list of desired constraints that the SCIFF proof procedure will fulfill while generating the traces. The accepted values in the list are:

- `procedural` or `declarative`: to specify the nature of the business process. These options are alternative to each other, thus they cannot be both included in the list as the same time.
- `positives` to generate positive traces only.
- `negatives` to generate negative traces only. positives and negatives options can be specified at the same time in order to obtain a single output log with both positive and negative traces
- `output(xes)` to obtain a XES formatted output log. If not specified, the output will be in the more concise but non-standard SCIFF format: `t(TraceId,h(event(ActivityName),Timestamp))`.
- `instances_for_each_path(X)` when the model admits alternative paths (e.g., when a `xor_split` constraint is present), this option allows to specify the maximum number `X` of traces to be generated for each one of paths that are envisaged by the model.
- `trace_max_length(X)` specifies the maximum number `X` of activities in each output trace.
- `time_limit(X)` to limit the maximum timestamp of the generated traces and ultimately guarantee their termination. For example, `time_limit(5)` forces the SCIFF proof procedure to generate all the traces with activities occurred in the time interval `[CurrentTime, CurrentTime + 5s]`. If not specified, `X` is set to `100s` by default.
