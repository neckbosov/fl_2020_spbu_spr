# Описание грамматики Филипанова

В описании двойные кавычки заменены на одинарные. У автора уточнено, что в
кавычках можно писать последовательность терминалов.

`GRAMMAR ::= <RULE>`

`GRAMMAR ::= <RULE> <SPACES> "\n" <GRAMMAR>`

`RULE ::= <SPACES> <NONTERM> <SPACES>  "::="  <SPACES> <RHS>`

`RHS ::= <TERM> <SPACES>`

`RHS ::= <NONTERM> <SPACES>`

`RHS ::= <TERM> <SPACES> <RHS>`

`RHS ::= <NONTERM> <SPACES> <RHS>`

`TERM ::= "'" SEQT "'"`

`NONTERM ::= "<" SEQN ">"`

`SEQT ::= any char exclude "`

`SEQN ::= any char exclude < and >`

`SPACES ::= " "`

`SPACES ::= " " SPACES`
