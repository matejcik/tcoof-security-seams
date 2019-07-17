package cz.cuni.mff.d3s.trust

import org.chocosolver.solver.{Solution, Model => ChocoModel}
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{BoolVar, IntVar, SetVar}

import scala.collection.mutable

/** Solver instance.
  *
  * A specialization of Choco solver [[org.chocosolver.solver.Model]] class, extended
  * to work with [[Integer]] and [[Logical]] objects. It also encloses concrete
  * implementations of [[Integer]], because they require access to the solver object.
  */
class SolverModel extends ChocoModel {

  /** Upper bound for integer variables of the solver */
  private[trust] val IntMaxValue = 10000 // IntVar.MAX_INT_BOUND

  /** Lower bound for integer variables of the solver */
  private[trust] val IntMinValue = -10000 // IntVar.MIN_INT_BOUND

  /** Create a new `IntVar` with bounded range.
    *
    * Used as a shortcut and a performance improvement: enumeration-bounded `IntVars` may consume
    * a lot of memory, and pre-setting their bounds is a saving.
    *
    * @return a new bounded `IntVar`.
    */
  private[trust] def newIntVar = intVar(IntMinValue, IntMaxValue)

  /** Create a constraint conjunction.
    *
    * @param clauses Constraint clauses
    * @return a constraint satisfied if all `clauses` are satisfied.
    */
  def and(clauses: Iterable[Logical]): Logical = {
    // shortcut: if a static false value is in the list, we don't need to do anything
    if (clauses.exists {
          case LogicalBoolean(false) => true
          case _                     => false
        }) {
      LogicalBoolean(false)
    } else {
      val ilogs = for { clause <- clauses if !clause.isInstanceOf[LogicalBoolean] } yield
        clause match {
          case LogicalLogOp(value)   => value
          case LogicalBoolVar(value) => value
        }

      LogicalLogOp(LogOp.and(ilogs.toArray: _*))
    }
  }

  /** Create a constraint disjunction.
    *
    * @param clauses Constraint clauses
    * @return a constraint satisfied if at least one of `clauses` is satisfied.
    */
  def or(clauses: Iterable[Logical]): Logical = {
    // shortcut: if a static true value is in the list, we don't need to do anything
    if (clauses.exists {
          case LogicalBoolean(value) if value => true
          case _                              => false
        }) {
      LogicalBoolean(true)
    } else {
      val ilogs = for {
        clause <- clauses
        if !clause.isInstanceOf[LogicalBoolean]
      } yield
        clause match {
          case LogicalLogOp(value)   => value
          case LogicalBoolVar(value) => value
        }

      LogicalLogOp(LogOp.or(ilogs.toArray: _*))
    }
  }

  /** Post a `Logical` constraint.
    *
    * Wrapper around the underlying `post()` method that works with a [[Logical]].
    *
    * @see [[org.chocosolver.solver.Model.post]]
    */
  def post(clause: Logical): Unit = {
    clause match {
      case LogicalBoolean(value) if !value => falseConstraint().post()
      case LogicalBoolVar(value)           => addClauseTrue(value)
      case LogicalLogOp(value)             => addClauses(value)
      case _                               =>
    }
  }

  /** State that constraints are true for selected members.
    *
    * Accepts a collection of constraints, and creates a constraint stating that
    * if `i` is a member of `membersVar`, then `membersClauses[i]` must be satisfied.
    *
    * Used to set up a dependency of ensemble constraints on its selection status.
    *
    * @param membersClauses Collection of constraints
    * @param membersVar Member set
    * @return a constraint satisfied if corresponding clauses of selected members are
    *         satisfied.
    */
  def forAllSelected(
      membersClauses: Iterable[Logical],
      membersVar: SetVar
  ): Logical = {
    val clauses = mutable.ListBuffer.empty[ILogical]

    var idx = 0
    for (clause <- membersClauses) {
      clause match {
        case LogicalBoolean(value) =>
          if (!value) clauses += notMember(idx, membersVar).reify
        case LogicalBoolVar(value) =>
          clauses += LogOp.implies(member(idx, membersVar).reify, value)
        case LogicalLogOp(value) =>
          clauses += LogOp.implies(member(idx, membersVar).reify, value)
        case _ =>
      }

      idx = idx + 1
    }

    if (clauses.nonEmpty)
      LogicalLogOp(LogOp.and(clauses: _*))
    else
      LogicalBoolean(true)
  }

  /** Enforce membership based on provided constraints.
    *
    * Posts a constraint stating that if `memberClauses[i]` is satisfied, then `i` must
    * be a member of `membersVar`
    *
    * Used for mandatory activation of ensembles based on their situation predicates.
    *
    * @param membersClauses Collection of constraints
    * @param membersVar Member set
    */
  def postEnforceSelected(
      membersClauses: Iterable[Logical],
      membersVar: SetVar
  ): Unit = {
    var idx = 0
    for (clause <- membersClauses) {
      clause match {
        case LogicalBoolean(value) => if (value) post(member(idx, membersVar))
        case LogicalBoolVar(value) => ifThen(value, member(idx, membersVar))
        case LogicalLogOp(value) =>
          post(
            LogicalLogOp(LogOp.implies(value, member(idx, membersVar).reify))
          )
        case _ =>
      }

      idx = idx + 1
    }
  }

  /** State that at a constraint is true for at least one selected member.
    *
    * Accepts a collection of constraints, and creates a constraint stating that
    * there exists `i` which is a member of `membersVar`, and the corresponding
    * `membersClauses[i]` is satisfied.
    *
    * Used for implementing the [[MemberGroup.some()]] predicate.
    *
    * @param membersClauses Collection of constraints
    * @param membersVar Member set
    * @return a constraint satisfied if a member constraint is satisfied for at least
    *         one selected member.
    */
  def existsSelected(
      membersClauses: Iterable[Logical],
      membersVar: SetVar
  ): Logical = {
    // Room for optimization: if there are just LogicalBooleans, and just a few
    // of them are true, a formula like "A is member OR B is member OR C is member"
    // is going to be better.

    // Set up a list of BoolVars whose truth value corresponds to membersClauses
    val booleans = membersClauses.map {
      case LogicalBoolean(value) => boolVar(value)
      case LogicalBoolVar(value) => value
      case LogicalLogOp(value) => {
        val b = boolVar
        addClauses(LogOp.reified(b, value))
        b
      }
    }.toIndexedSeq

    // Set up a set whose members are indices of those constraints that are satisfied
    val channelSet = setVar(Array.emptyIntArray, booleans.indices.toArray)
    setBoolsChanneling(booleans.toArray, channelSet).post()

    // Create a constraint that the set of satisfied constraints must not be disjoint
    // with the set of selected members
    val notDisjoint = disjoint(channelSet, membersVar).reify().not().asInstanceOf[BoolVar]
    LogicalBoolVar(notDisjoint)
  }

  /** Sum of Integer values.
    *
    * Wrapper around the underlying `sum()` method that works with `Integer`s.
    *
    * @see [[org.chocosolver.solver.Model.sum]]
    * @param values Integer values
    * @return sum of `values` as an `Integer`
    */
  def sum(values: Iterable[Integer]): Integer = {
    val constValue = values.collect { case x: IntegerInt => x }.foldLeft(0)(_ + _.value)
    val intVars = values.collect { case x: IntegerIntVar => x }.map(_.value)

    if (intVars.isEmpty) IntegerInt(constValue)
    else {
      // Not sure whether this optimization has any real effect
      val sumVar =
        if (intVars.size == 1) intVars.head
        else {
          val sumVar = newIntVar
          sum(intVars.toArray, "=", sumVar).post()
          sumVar
        }

      if (constValue == 0) IntegerIntVar(sumVar)
      else arithmResult(intVar(constValue), "+", sumVar)
    }
  }

  /** Sum based on membership.
    *
    * @param membersVar Member set
    * @param values Individual member values
    * @return sum of only those values which are members of `membersVar`.
    */
  def sumBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]): Integer = {
    IntegerIntVar(
      if (values.forall(_.isInstanceOf[IntegerInt])) sumIntsBasedOnMembership(membersVar, values)
      else sumGenericBasedOnMembership(membersVar, values)
    )
  }

  /** Sum of IntVars based on membership.
    *
    * Wrapper around the underlying `sumElements` for sums of statically known values.
    * Called from [[sumBasedOnMembership()]]
    *
    * @param membersVar Member set
    * @param values Individual member values. The caller must ensure that all are of
    *               type `IntegerInt`.
    * @return sum of only those values which are members of `membersVar`.
    */
  private def sumIntsBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]): IntVar = {
    val sumVar = newIntVar
    sumElements(
      membersVar,
      values.map(_.asInstanceOf[IntegerInt].value).toArray,
      sumVar
    ).post()
    sumVar
  }

  /** Sum of any kind of Integer based on membership.
    *
    * More complicated implementation of [[sumBasedOnMembership()]], in case not all
    * values are statically known. Creates a list of `IntVar`s and conditionally sets
    * their values to 0 or the desired target value, based on whether the corresponding
    * member is selected in `membersVar`.
    *
    * @param membersVar Member set
    * @param values Individual member values, can be of any `Integer` subtype
    * @return only those values which are members of `membersVar`.
    */
  private def sumGenericBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]): IntVar = {
    val condCostVars = new Array[IntVar](values.size)

    var idx = 0
    for (value <- values) {
      val condCostVar = newIntVar
      val costVarConstraint = value match {
        case IntegerInt(intVal)    => arithm(condCostVar, "=", intVal)
        case IntegerIntVar(intVar) => arithm(condCostVar, "=", intVar)
      }

      ifThenElse(
        member(idx, membersVar),
        costVarConstraint,
        arithm(condCostVar, "=", 0)
      )
      condCostVars(idx) = condCostVar

      idx = idx + 1
    }

    val sumVar = newIntVar
    sum(condCostVars, "=", sumVar).post()
    sumVar
  }

  /** Calculate an arithmetic expression.
    *
    * @param left Left operand
    * @param op Operator
    * @param right Right operand
    * @return a new `IntVar` whose value is bound to the result of the operation.
    */
  private def arithmResult(left: IntVar, op: String, right: IntVar): IntegerIntVar = {
    val result = newIntVar
    arithm(left, op, right, "=", result).post()
    IntegerIntVar(result)
  }

  /** Statically known Integer type.
    *
    * Wrapper around the native `Int` type, interoperable with the solver-bound type.
    */
  private[trust] case class IntegerInt(value: Int) extends Integer {
    protected type ValueType = Int

    override def asInt: Int = value

    /** Shortcut implementation of arithmetic operations.
      *
      * Calculates the result directly if both sides are `IntegerInts`, creates
      * an arithmetic variable otherwise.
      */
    private def op(other: Integer, op: String, opFun: (Int, Int) => Int): Integer = other match {
      case IntegerInt(otherValue)    => IntegerInt(opFun(value, otherValue))
      case IntegerIntVar(otherValue) => arithmResult(intVar(value), op, otherValue)
    }

    override def +(other: Integer): Integer = op(other, "+", _ + _)
    override def -(other: Integer): Integer = op(other, "-", _ - _)
    override def *(other: Integer): Integer = op(other, "*", _ * _)
    override def /(other: Integer): Integer = op(other, "/", _ / _)
    override def unary_-(): Integer = IntegerInt(-value)

    /** Shortcut implementation of arithmetic comparisons.
      *
      * Calculates the result directly if both sides are `IntegerInts`, creates
      * a constraint otherwise.
      */
    private def revRelOp(num: Integer, revOp: String, revFun: (Int, Int) => Boolean): Logical = {
      num match {
        case i: IntegerInt       => LogicalBoolean(revFun(i.value, value))
        case iVar: IntegerIntVar => LogicalBoolVar(arithm(iVar.value, revOp, value).reify())
      }
    }

    override def ===(num: Integer): Logical = revRelOp(num, "=", _ == _)
    override def !=(num: Integer): Logical = revRelOp(num, "!=", _ != _)
    override def <(num: Integer): Logical = revRelOp(num, ">", _ > _)
    override def >(num: Integer): Logical = revRelOp(num, "<", _ < _)
    override def <=(num: Integer): Logical = revRelOp(num, ">=", _ >= _)
    override def >=(num: Integer): Logical = revRelOp(num, "<=", _ <= _)
  }

  /** Solver-bound Integer type.
    *
    * Wrapper around Choco solver [[IntVar]] type. Can represent values determined
    * by the solver, such as set selection cardinality.
    */
  private[trust] case class IntegerIntVar(value: IntVar) extends Integer {
    protected type ValueType = IntVar

    override def asInt: Int = solution.getIntVal(value)

    /** Shortcut implementation of arithmetic operations.
      *
      * Converts the other operand to `IntVar` if required.
      */
    def op(other: Integer, op: String): Integer = other match {
      case IntegerInt(otherValue)    => arithmResult(value, op, intVar(otherValue))
      case IntegerIntVar(otherValue) => arithmResult(value, op, otherValue)
    }

    override def +(other: Integer): Integer = op(other, "+")
    override def -(other: Integer): Integer = op(other, "-")
    override def *(other: Integer): Integer = op(other, "*")
    override def /(other: Integer): Integer = op(other, "/")
    override def unary_-(): Integer = IntegerIntVar(intMinusView(value))

    /** Shortcut implementation of arithmetic comparisons. */
    private def relOp(num: Integer, op: String) = {
      num match {
        case i: IntegerInt       => LogicalBoolVar(arithm(value, op, i.value).reify())
        case iVar: IntegerIntVar => LogicalBoolVar(arithm(value, op, iVar.value).reify())
      }
    }

    override def ===(num: Integer): Logical = relOp(num, "=")
    override def !=(num: Integer): Logical = relOp(num, "!=")
    override def <(num: Integer): Logical = relOp(num, "<")
    override def >(num: Integer): Logical = relOp(num, ">")
    override def <=(num: Integer): Logical = relOp(num, "<=")
    override def >=(num: Integer): Logical = relOp(num, ">=")
  }

  /** Reference to current solution. */
  private[trust] var solution = new Solution(this)

  /** Indicates whether a solution exists. */
  private[trust] var solutionExists = false

  /** Initialize the solver.
    *
    * Currently a no-op.
    */
  def init(): Unit = {}

  /** Find a solution.
    *
    * Invokes a search for the next solution, and records it if it exists.
    *
    * @return true if a solution was found, false otherwise.
    */
  def solveAndRecord(): Boolean = {
    val variables = getVars
    val result = getSolver.solve()

    if (result) {
      solution.record()
      solutionExists = true
    }

    result
  }

  /** True if a solution exists.
    *
    * Public reference to the internal variable.
    */
  def exists: Boolean = solutionExists
}
