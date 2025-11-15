from typing import Set

from fparser.two.utils import walk
from fparser.two.Fortran2003 import Type_Declaration_Stmt, Entity_Decl


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