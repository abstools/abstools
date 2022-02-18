from logics.utils.parsers import classical_parser
from logics.utils.formula_generators.generators_biased import random_formula_generator
from logics.classes.propositional import Language
from logics.utils.parsers.standard_parser import StandardParser
lang = Language(atomics=['F1', 'F2'],
                constant_arity_dict={'!': 1, '&&': 2, '||' : 2},
                sentential_constants=['false', 'true'],
                metavariables=[],
                context_variables=[])

inf = random_formula_generator.random_formula(depth=2, atomics=['F1', 'F2'],
                                              language=lang,
                                              exact_depth=True, all_atomics=False)
parser = StandardParser(language=lang,
                        infix_cts=['&&','||'])
out = parser.unparse(inf)
print(out)            