Framework and DSL for Ensemble-Based Access Control
===================================================

Assignment / Annotation / Abstract
----------------------------------

Access control policies typically take the form of a set of static rules pertaining to
individual entities under control. This can be impractical in real-world scenarios:
authorization invariably depends on wider situational context which often tends to be
highly dynamic. This leads to increasingly complex rules, which have to change over time
to reflect the evolution of the controlled system.

Ensemble-based architectures allow dynamic formation of goal-oriented groups in systems
with large number of independent autonomous components. Because of the ad-hoc and
situation-aware nature of group formation, ensembles offer a novel way of approaching
access control.

The goal of this work is to design a Scala framework and internal DSL for describing
access control related situations via ensembles. In particular, the framework will
define ensemble semantics suitable for evaluating the ensembles and establishing access
control at runtime.
