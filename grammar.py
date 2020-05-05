from ply import lex, yacc
from collections import defaultdict

tokens = (
    'TERM',
    'NONTERM',
    'TO',
    'COMMA',
    'NEWLINES'
)

t_TO = r'->'
t_NONTERM = r'[A-Z]+'
t_TERM = r'\"[^\"]*\"'
t_COMMA = r','


def t_NEWLINES(t):
    r"""(\n)+"""
    t.lexer.lineno += t.value.count('\n')
    return t


t_ignore = " \t"


def t_error(t):
    print(f"Illegal character {t.value[0]!r}")
    t.lexer.skip(1)


lex.lex()

terms = set()
nonterms = set()
rules = defaultdict(list)


def add_term(term):
    for c in term:
        terms.add(c)


def p_rule_one(p):
    """rule : NONTERM TO rhs"""
    nonterms.add(p[1])
    rules[p[1]].append(p[3])


def p_rule_one_newlines(p):
    """rule : NONTERM TO rhs NEWLINES"""
    nonterms.add(p[1])
    rules[p[1]].append(p[3])


def p_rule_many(p):
    """rule : NONTERM TO rhs NEWLINES rule"""
    nonterms.add(p[1])
    rules[p[1]].append(p[3])


def p_rhs_term_one(p):
    """rhs : TERM"""
    add_term(p[1][1:-1])
    p[0] = []
    p[0].append(p[1])


def p_rhs_nonterm_one(p):
    """rhs : NONTERM"""
    nonterms.add(p[1])
    p[0] = []
    p[0].append(p[1])


def p_rhs_term_many(p):
    """rhs : rhs COMMA TERM"""
    add_term(p[3][1:-1])
    p[0] = p[1].copy()
    p[0].append(p[3])


def p_rhs_nonterm_many(p):
    'rhs : rhs COMMA NONTERM'
    nonterms.add(p[3])
    p[0] = p[1].copy()
    p[0].append(p[3])


syntax_error = False


def p_error(p):
    global syntax_error
    syntax_error = True
    print('Syntax error at {}'.format(p))


def parse(s):
    global rules, terms, nonterms, syntax_error
    rules = defaultdict(list)
    terms = set()
    nonterms = set()
    syntax_error = False
    yacc.parse(s)
    return terms.copy(), nonterms.copy(), rules.copy(), syntax_error


yacc.yacc()
if __name__ == '__main__':
    file = input()
    with open(file, "r") as f:
        ss = f.read()
        parse(ss)
        print('Terms:', ' '.join(terms))
        print('Nonterms:', ' '.join(nonterms))
        print('Rules:')
        for key, value in rules.items():
            for rhs in value:
                print(key, '->', ' '.join(rhs))
        print("Start nonterm: S")
