/* set filetype=prolog syntax=prolog */

/* Calculate champion stats at a certain point. */

champ_health_at(X, Level, Health) :-
    champ_health(X, Base),
    champ_healthg(X, Per),
    Health is Base + Per * Level.
