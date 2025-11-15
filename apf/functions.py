from typing import Set

from fparser.two.Fortran2003 import Function_Subprogram, Subroutine_Subprogram, Name


def find_pure_functions(parse_tree) -> Set[str]:
    """Return names of procedures declared as PURE/ELEMENTAL within a tree.

    Args:
        parse_tree: fparser2 root parse tree.

    Returns:
        Set of lowercased function/subroutine names considered pure.

    Example:
        If a module contains `PURE SUBROUTINE foo`, returns {'foo'}.
    """
    pure: Set[str] = set()
    from fparser.two.utils import walk

    for node in walk(parse_tree, (Function_Subprogram, Subroutine_Subprogram)):
        text = str(node)
        if "PURE" in text or "ELEMENTAL" in text:
            names = [n.string for n in walk(node, Name)]
            if names:
                pure.add(names[0].lower())
    return pure