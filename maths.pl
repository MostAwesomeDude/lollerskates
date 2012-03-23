/* set filetype=prolog syntax=prolog */

/* Calculate champion stats at a certain point. */

champ_health_at(X, Level, Value) :-
    champ_health(X, Base),
    champ_healthg(X, Per),
    Value is Base + Per * Level.

champ_hregen_at(X, Level, Value) :-
    champ_hregen(X, Base),
    champ_hregeng(X, Per),
    Value is Base + Per * Level.

champ_mana_at(X, Level, Value) :-
    champ_mana(X, Base),
    champ_manag(X, Per),
    Value is Base + Per * Level.

champ_mregen_at(X, Level, Value) :-
    champ_mregen(X, Base),
    champ_mregeng(X, Per),
    Value is Base + Per * Level.

champ_ad_at(X, Level, Value) :-
    champ_ad(X, Base),
    champ_adg(X, Per),
    Value is Base + Per * Level.

champ_as_at(X, Level, Value) :-
    champ_as(X, Base),
    champ_asg(X, Per),
    Value is Base + Per * Level.

champ_armor_at(X, Level, Value) :-
    champ_armor(X, Base),
    champ_armorg(X, Per),
    Value is Base + Per * Level.

champ_mr_at(X, Level, Value) :-
    champ_mr(X, Base),
    champ_mrg(X, Per),
    Value is Base + Per * Level.
