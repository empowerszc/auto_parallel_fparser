from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple


@dataclass
class ArrayAccess:
    """Array reference captured from an assignment.

    Attributes:
        name: Array name (lowercased).
        subscripts: Original subscript expressions from source.
        resolved_subscripts: Subscripts after constant propagation, if applied.
        affine_map: Mapping `loop_var -> (coeff, offset)` for affine parts.
        nonconst_coeffs: Mapping `loop_var -> symbol` for variable coefficients or non-affine terms.
        complex_index: True if any subscript is function-like or otherwise complex.
    """
    name: str
    subscripts: List[str]
    affine_map: Dict[str, Tuple[int, int]]
    nonconst_coeffs: Dict[str, str] = field(default_factory=dict)
    resolved_subscripts: List[str] = field(default_factory=list)
    complex_index: bool = False


@dataclass
class StatementIR:
    """Intermediate representation for one assignment statement.

    Attributes:
        raw: Raw text of the assignment.
        writes: ArrayAccess targets being written.
        reads: ArrayAccess sources being read.
        reductions: Optional scalar or Data_Ref reductions detected ('sum' or 'product').
    """
    raw: str
    writes: List[ArrayAccess] = field(default_factory=list)
    reads: List[ArrayAccess] = field(default_factory=list)
    reductions: Dict[str, str] = field(default_factory=dict)


@dataclass
class LoopIR:
    """Loop-level IR with structure and body statements.

    Attributes:
        loop_vars: Loop variables in nesting order (outer→inner).
        bounds: Per-variable `(lb, ub, step)` textual bounds.
        body_statements: Collected StatementIRs in the loop body.
        node_ref: AST node reference for the loop.
        start_text, end_text: Canonical DO/END DO strings.
        nest_depth: Nesting depth.
        parent_start_text: Outer loop DO text when nested.
        has_complex_index: True if any statement has complex index.
    """
    loop_vars: List[str]
    bounds: Dict[str, Tuple[str, str, Optional[str]]]
    body_statements: List[StatementIR]
    node_ref: object
    start_text: str
    end_text: str
    nest_depth: int = 1
    parent_start_text: Optional[str] = None
    has_complex_index: bool = False


@dataclass
class Dependence:
    """A single dependence relation between two statements.

    Attributes:
        src_stmt, dst_stmt: Statement indices (src ≤ dst).
        array: Array name involved.
        distance_vector: Per-nesting distance (ints or '?' when unknown).
        direction_vector: Per-nesting direction ('<','>','=' or '?').
        carried_by: Loop variables that carry non-zero/'?' distance.
        notes: Optional annotations (e.g., constant propagation info).
    """
    src_stmt: int
    dst_stmt: int
    array: str
    distance_vector: List
    direction_vector: List[str]
    carried_by: List[str]
    notes: List[str] = field(default_factory=list)


@dataclass
class AnalysisResult:
    """Result of analyzing one loop.

    Attributes:
        loop: LoopIR analyzed.
        dependences: List of Dependence entries.
        is_parallel: True if loop is safe to parallelize.
        reason: Text reason when not parallel.
        reduction_vars: Detected reductions and their kinds.
        private_vars, firstprivate_vars, shared_vars, lastprivate_vars: Clause variables.
    """
    loop: LoopIR
    dependences: List[Dependence]
    is_parallel: bool
    reason: Optional[str]
    reduction_vars: Dict[str, str]
    private_vars: List[str]
    firstprivate_vars: List[str] = field(default_factory=list)
    shared_vars: List[str] = field(default_factory=list)
    lastprivate_vars: List[str] = field(default_factory=list)