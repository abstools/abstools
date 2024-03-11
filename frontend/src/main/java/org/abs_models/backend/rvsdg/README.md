# RVSDG

The following package implements a Regionalized Value State Dependence Graph (RVSDG) IR for ABS.

For more details see:
Reissmann, Nico, et al. "RVSDG: An intermediate representation for optimizing compilers." arXiv preprint arXiv:1912.05036 (2019).
<https://arxiv.org/abs/1912.05036>.

## Structure

- `core/`: The core RVSDG infrastructure.
  This uses the `typechecker.Type` class, but other than that it's completely independent of ABS. 
- `abs/`: Classes which implements ABS semantics.
- `builder/`: Classes which converts AST to RVSDG.
