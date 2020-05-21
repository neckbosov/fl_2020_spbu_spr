# Фадеева грамматика

*Дисклеймер: это будет отвратительно, так как в терминалах только маленькие
латинские буквы. Возможно это и будет отдаленно напоминать грамматику.*

Вместо пробелов будет символ `a`, вместо переносов строк - `b`, вместо стрелочки - `c`. Нетерминалы можно составлять из символов `d`. Остальные символы можно использовать как терминальные.

`GRAMMAR -> RULE`

`GRAMMAR -> RULE SPACES b GRAMMAR`

`RULE -> NONTERM SPACES c SPACES RHS`

`RHS -> TERM SPACES`

`RHS -> NONTERM SPACES`

`RHS -> TERM SPACES RHS`

`RHS -> NONTERM SPACES RHS`

`NONTERM -> d`

`NONTERM -> d NONTERM`

`TERM -> [e - z]`

`SPACES -> a`

`SPACES -> a SPACES`
