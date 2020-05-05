import pytest
from collections import defaultdict

from grammar import parse


def test_single_rule():
    s = 'S -> A\n'
    terms, nonterms, rules, syntax_error = parse(s)
    assert not syntax_error
    assert terms == set()
    assert nonterms == {'S', 'A'}
    assert rules == {'S': [['A']]}


def test_multiple_rules():
    s = '''S->A,B
    A -> A, "("
    B -> B, ")"
    '''
    terms, nonterms, rules, syntax_error = parse(s)
    assert not syntax_error
    assert terms == {'(', ')'}
    assert nonterms == {'S', 'A', 'B'}
    assert rules == {'S': [['A', 'B']], 'A': [['A', '\"(\"']], 'B': [['B', '\")\"']]}


def test_syntax_error():
    s = '''S->A,B
        A -> A, "("
        B -> B ")"
        '''
    terms, nonterms, rules, syntax_error = parse(s)
    assert syntax_error


def test_multiple_terms():
    s = '''S -> "a", S, "b"
    '''
    terms, nonterms, rules, syntax_error = parse(s)
    assert not syntax_error
    assert terms == {'a', 'b'}
    assert nonterms == {'S'}
    assert rules == {'S': [['\"a\"', 'S', '\"b\"']]}


if __name__ == '__main__':
    pytest.main()
