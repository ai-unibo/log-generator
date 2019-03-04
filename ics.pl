:-module(ics,[ics/2]).

ics([true],
        [[true],
        [abd(event(a),T),T>0,T<5,abd(nic(0),0)]]).

ics([abd(event(a),T)],
        [[true],
        [abd(event(a),T1),T1>T,T1<5,abd(nic(0),0)]]).

ics([abd(event(a),T1),abd(event(a),T2),chesio(T1,T2)],
        [[<>(T1,T2)]]).

ics([true],
        [[true],
        [abd(event(b),T),T>0,T<5,abd(nic(0),0)]]).

ics([abd(event(b),T)],
        [[true],
        [abd(event(b),T1),T1>T,T1<5,abd(nic(0),0)]]).

ics([true],
        [[true],
        [abd(event(c),T),T>0,T<5,abd(nic(0),0)]]).

ics([abd(event(c),T)],
        [[true],
        [abd(event(c),T1),T1>T,T1<5,abd(nic(0),0)]]).

ics([true],
        [[true],
        [abd(event(d),T),T>0,T<5,abd(nic(0),0)]]).

ics([abd(event(d),T)],
        [[true],
        [abd(event(d),T1),T1>T,T1<5,abd(nic(0),0)]]).

ics([true],
        [[true],
        [abd(event(e),T),T>0,T<5,abd(nic(0),0)]]).

ics([abd(event(e),T)],
        [[true],
        [abd(event(e),T1),T1>T,T1<5,abd(nic(0),0)]]).

ics([true],
        [[T>0,T<5,abd(event(a),T)]]).

ics([abd(event(a),T1)],
        [[T2>T1,T2<5,abd(event(b),T2)],
        [abd(event(c),T2),T2>T1,T2<5]]).

ics([abd(event(b),T1),abd(event(c),T2)],
        [[fail]]).

ics([abd(event(c),T1)],
        [[T2>T1,T2<5,abd(event(d),T2)]]).

ics([abd(event(b),T1)],
        [[abd(nic(0),0)]]).

ics([abd(event(b),T1),abd(event(e),T2),T2>T1],
        [[fail]]).

ics([abd(event(d),T1)],
        [[T2>T1,T2<5,abd(event(e),T2)]]).

