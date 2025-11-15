from typing import Set, Tuple

from fparser.two.utils import walk
from fparser.two.Fortran2003 import Type_Declaration_Stmt, Entity_Decl, Use_Stmt, Name


def collect_intent_in(parse_tree) -> Set[str]:
    res: Set[str] = set()
    for td in walk(parse_tree, Type_Declaration_Stmt):
        txt = str(td).upper()
        if "INTENT(IN)" in txt:
            for e in walk(td, Entity_Decl):
                try:
                    res.add(e.items[0].string.lower())
                except Exception:
                    pass
    return res


def collect_intent_out(parse_tree) -> Set[str]:
    res: Set[str] = set()
    for td in walk(parse_tree, Type_Declaration_Stmt):
        txt = str(td).upper()
        if "INTENT(OUT)" in txt:
            for e in walk(td, Entity_Decl):
                try:
                    res.add(e.items[0].string.lower())
                except Exception:
                    pass
    return res


def collect_intent_inout(parse_tree) -> Set[str]:
    res: Set[str] = set()
    for td in walk(parse_tree, Type_Declaration_Stmt):
        txt = str(td).upper()
        if "INTENT(INOUT)" in txt:
            for e in walk(td, Entity_Decl):
                try:
                    res.add(e.items[0].string.lower())
                except Exception:
                    pass
    return res


def collect_value_attr(parse_tree) -> Set[str]:
    res: Set[str] = set()
    for td in walk(parse_tree, Type_Declaration_Stmt):
        txt = str(td).upper()
        if "VALUE" in txt:
            for e in walk(td, Entity_Decl):
                try:
                    res.add(e.items[0].string.lower())
                except Exception:
                    pass
    return res


def collect_use_modules(parse_tree) -> Set[str]:
    res: Set[str] = set()
    for u in walk(parse_tree, Use_Stmt):
        names = [n.string.lower() for n in walk(u, Name)]
        if names:
            res.add(names[0])
    return res