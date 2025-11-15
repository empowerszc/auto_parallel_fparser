from typing import Set, Tuple, Dict

from fparser.two.utils import walk
from fparser.two.Fortran2003 import Type_Declaration_Stmt, Entity_Decl, Use_Stmt, Name, Assignment_Stmt, Int_Literal_Constant, Real_Literal_Constant


def collect_intent_in(parse_tree) -> Set[str]:
    """Collect names declared with INTENT(IN).

    Args:
        parse_tree: fparser2 parse tree (module/subprogram).

    Returns:
        Lowercased set of variable names.
    """
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
    """Collect names declared with INTENT(OUT).

    Args:
        parse_tree: fparser2 parse tree.

    Returns:
        Lowercased set of names.
    """
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
    """Collect names declared with INTENT(INOUT)."""
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
    """Collect names declared with VALUE attribute."""
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
    """Collect module names from USE statements."""
    res: Set[str] = set()
    for u in walk(parse_tree, Use_Stmt):
        names = [n.string.lower() for n in walk(u, Name)]
        if names:
            res.add(names[0])
    return res


def collect_scalar_constants_before(loop_node) -> Dict[str, str]:
    """Collect scalar literal assignments preceding a loop within the same block.

    Args:
        loop_node: AST node of the target Block_Nonlabel_Do_Construct.

    Returns:
        Mapping from variable name to literal value (as strings), e.g., {'k': '2'}.

    Example:
        Given:
            k = 2
            do i = 1, n
        Returns {'k': '2'}.
    """
    consts: Dict[str, str] = {}
    try:
        # climb to a parent that has content list containing the loop_node
        parent = getattr(loop_node, "parent", None)
        while parent is not None and not hasattr(parent, "content"):
            parent = getattr(parent, "parent", None)
        if parent is None:
            return consts
        content = getattr(parent, "content", [])
        for item in content:
            if item is loop_node:
                break
            for a in walk(item, Assignment_Stmt):
                items = getattr(a, "items", [])
                lhs = items[0] if items else None
                rhs = items[2] if len(items) > 2 else None
                if isinstance(lhs, Name):
                    lname = lhs.string.lower()
                    # only accept literal constants
                    ival = None
                    for lit in walk(rhs, Int_Literal_Constant):
                        ival = str(lit).strip()
                        break
                    rval = None
                    for lit in walk(rhs, Real_Literal_Constant):
                        rval = str(lit).strip()
                        break
                    val = ival or rval
                    if val is not None:
                        consts[lname] = val
    except Exception:
        return consts
    return consts