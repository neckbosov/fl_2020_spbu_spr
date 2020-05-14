from collections import defaultdict


class NonTermsGenerator:
    def __init__(self, nonterms):
        self.nonterms = nonterms
        self.cnt = 0

    def get_nonterm(self):
        while ('N' + str(self.cnt)) not in self.nonterms:
            self.cnt += 1
        res = 'N' + str(self.cnt)
        self.cnt += 1
        self.nonterms.add(res)
        return res


def is_term(c):
    return c[0] == '\"' and c[-1] == '\"'


def replace_terms(rules, gen):
    res = defaultdict(list)
    for n, prods in rules.items():
        for seq in prods:
            new_seq = []
            for c in seq:
                if is_term(c):
                    nt = gen.get_nonterm()
                    res[nt].append([c])
                    new_seq.append(nt)
                else:
                    new_seq.append(c)
            res[n].append(new_seq)
    return res


def remove_long_rules(rules, gen):
    res = defaultdict(list)
    for n, prods in rules.items():
        for seq in prods:
            if len(seq) <= 2:
                res[n].append(seq)
            else:
                nt = gen.get_nonterm()
                res[nt].append(seq[1:])
                res[n].append([seq[0], nt])
    return res

