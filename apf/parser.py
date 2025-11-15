from typing import Optional, Union

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader, FortranFileReader


def parse_source(source: Union[str, bytes], std: str = "f2003", ignore_comments: bool = False, include_omp_conditional_lines: bool = True):
    if isinstance(source, (str, bytes)) and ("\n" in str(source) or not str(source).endswith((".f", ".f90", ".F90", ".F"))):
        reader = FortranStringReader(str(source), ignore_comments=ignore_comments, include_omp_conditional_lines=include_omp_conditional_lines)
    else:
        reader = FortranFileReader(str(source), ignore_comments=ignore_comments, include_omp_conditional_lines=include_omp_conditional_lines)
    parser = ParserFactory().create(std=std)
    return parser(reader)


def parse_file(path: str, std: str = "f2003", ignore_comments: bool = False, include_omp_conditional_lines: bool = True):
    reader = FortranFileReader(path, ignore_comments=ignore_comments, include_omp_conditional_lines=include_omp_conditional_lines)
    parser = ParserFactory().create(std=std)
    return parser(reader)