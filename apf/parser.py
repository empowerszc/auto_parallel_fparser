from typing import Optional, Union

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader, FortranFileReader


def parse_source(source: Union[str, bytes], std: str = "f2003", ignore_comments: bool = False, include_omp_conditional_lines: bool = True):
    """Parse Fortran source (string or path) and return an fparser2 parse tree.

    Args:
        source: Source text or file path. Strings with newlines are treated as in-memory source.
        std: Fortran standard passed to ParserFactory (e.g., 'f2003').
        ignore_comments: Whether to strip comments during reading.
        include_omp_conditional_lines: Whether to include OpenMP conditional lines.

    Returns:
        fparser2 parse tree.

    Example:
        >>> tree = parse_source('program x\n do i=1,2\n end do\n end program')
    """
    if isinstance(source, (str, bytes)) and ("\n" in str(source) or not str(source).endswith((".f", ".f90", ".F90", ".F"))):
        reader = FortranStringReader(str(source), ignore_comments=ignore_comments, include_omp_conditional_lines=include_omp_conditional_lines)
    else:
        reader = FortranFileReader(str(source), ignore_comments=ignore_comments, include_omp_conditional_lines=include_omp_conditional_lines)
    parser = ParserFactory().create(std=std)
    return parser(reader)


def parse_file(path: str, std: str = "f2003", ignore_comments: bool = False, include_omp_conditional_lines: bool = True):
    """Parse a Fortran file from disk.

    Args:
        path: Absolute or relative path to a Fortran file.
        std: Fortran standard for the parser.
        ignore_comments: Whether to strip comments.
        include_omp_conditional_lines: Include OpenMP conditional lines.

    Returns:
        fparser2 parse tree.
    """
    reader = FortranFileReader(path, ignore_comments=ignore_comments, include_omp_conditional_lines=include_omp_conditional_lines)
    parser = ParserFactory().create(std=std)
    return parser(reader)