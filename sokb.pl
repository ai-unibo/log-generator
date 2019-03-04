:- dynamic(fdet/1).

:- dynamic(society_goal/0).

:- dynamic(chesio/2).

:- dynamic fdet/1.

:- dynamic society_goal/0.

fdet(_).

society_goal :- 
    trace_max_length(5),
    current_trace_max_length(0).

chesio(A,B) :- 
    write('CHESIO!!!!!\n'),
    A@<B.

