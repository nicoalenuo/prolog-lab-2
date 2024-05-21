1/4::palo(oros).
1/4::palo(copas).
1/4::palo(espadas).
1/4::palo(bastos).

1/12::carta(P,X) :- palo(P), between(1,12,X).

query(carta(bastos,X)).
