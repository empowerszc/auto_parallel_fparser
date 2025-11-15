from dataclasses import dataclass, field
from typing import List, Dict, Optional, Tuple


@dataclass
class ArrayAccess:
    name: str
    subscripts: List[str]
    affine_map: Dict[str, Tuple[int, int]]
    nonconst_coeffs: Dict[str, str] = field(default_factory=dict)
    resolved_subscripts: List[str] = field(default_factory=list)
    complex_index: bool = False


@dataclass
class StatementIR:
    raw: str
    writes: List[ArrayAccess] = field(default_factory=list)
    reads: List[ArrayAccess] = field(default_factory=list)
    reductions: Dict[str, str] = field(default_factory=dict)


@dataclass
class LoopIR:
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
    src_stmt: int
    dst_stmt: int
    array: str
    distance_vector: List
    direction_vector: List[str]
    carried_by: List[str]
    notes: List[str] = field(default_factory=list)


@dataclass
class AnalysisResult:
    loop: LoopIR
    dependences: List[Dependence]
    is_parallel: bool
    reason: Optional[str]
    reduction_vars: Dict[str, str]
    private_vars: List[str]
    firstprivate_vars: List[str] = field(default_factory=list)
    shared_vars: List[str] = field(default_factory=list)
    lastprivate_vars: List[str] = field(default_factory=list)