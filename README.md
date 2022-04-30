# chr_book
Valuable code and exercises about the book "Constraint Handling Rules (2009)" by Thom Frühwirth.

Note that most of the examples in this book cannot work directly in SWI-Prolog (e.g. incomplete code, lacking necessary rules and built-ins, not compatible with Prolog flattening style, using different operation semantics, bugs, etc). The major purpose of this repository is to make them work, followed by some experiments and extensions.

## Selected Examples 

- Chapter 2: My first CHR programs
  - [Greatest common divisor](ch02/multiset_trans/gcd)
  - [Prime numbers Sieve of Eratosthenes](ch06/logic_programming/primes)
  - [Exchange sort](ch02/multiset_trans/exchange_sort)
  - [CYK algorithm](ch02/graph/transitive_closure/cyk)
  - [Merge sort](ch02/graph/merge_sort)
  - [Hamming’s problem](ch02/graph/merge_sort/hamming_problem)
- Chapter 6:  Rule-based and graph-based formalisms in CHR
  - [Production rule systems](ch06/rule_based_system/production_system)
  - [Event condition action rules](ch06/rule_based_system/event_condition_action_system)
  - [Term rewriting systems](ch06/rewriting_system/standard_trs)
- Chapter 8: Finite domain constraint solvers
  - Boolean algebra and propositional logic
    - [Boolean cardinality](https://github.com/chansey97/chr_book/blob/main/ch08/boolean/propositional_logic/boolean_cardinality.pl)
    - [Resolution](ch08/boolean/propositional_logic/resolution)
      - [Davis–Putnam procedure](https://github.com/chansey97/chr_book/blob/main/ch08/boolean/propositional_logic/resolution/6_dp.pl)
  - Consistency techniques
    - Arc consistency
      - Finite domain
        - [Interval domains](ch08/consistency_techniques/arc_consistency/fd/interval_domain)
        - [Enumeration_domain](ch08/consistency_techniques/arc_consistency/fd/enumeration_domain)
        - [N-Queen](ch08/consistency_techniques/arc_consistency/fd/nqueens)
    - Path consistency
      - Temporal reasoning
        - [PA network](ch08/consistency_techniques/path_consistency/temporal_reasoning/qualitative_time_point/pa_network)
        - [Simple-Temporal-Network (STN)](ch08/consistency_techniques/path_consistency/temporal_reasoning/quantitative_time_point)
  - [Sudoku (try-most-constrained-field-first)](https://github.com/chansey97/chr_book/blob/main/ch08/sudoku.pl)
- Chapter 9: Infinite domain constraint solvers
  - Linear polynomial equation solving
    - [Variable elimination](ch09/linear_polynomial_equation_solving/equation/variable_elimination)
    - [Gaussian elimination](ch09/linear_polynomial_equation_solving/equation/gauss_elimination)
    - [Fourier’s algorithm](ch09/linear_polynomial_equation_solving/inequation/fourier)
  - [Description logic](ch09/description_logic)
  - [Rational trees](ch09/rational_tree)
  - [Feature terms](ch09/feature_term)
- Chapter 10: Union-find algorithm
  - [Traditional union-find](ch10/1_uf)
  - [Generalized union-find](ch10/2_guf)

