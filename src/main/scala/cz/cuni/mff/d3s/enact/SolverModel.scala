package cz.cuni.mff.d3s.enact

import org.chocosolver.solver.{Solution, Model => ChocoModel}
import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.{IntVar, SetVar}

import scala.collection.mutable

class SolverModel extends ChocoModel {

  /** Upper bound for integer variables of the solver */
  private[enact] val IntMaxValue = 10000 // IntVar.MAX_INT_BOUND
  /** Lower bound for integer variables of the solver */
  private[enact] val IntMinValue = -10000 // IntVar.MIN_INT_BOUND

  private[enact] def newIntVar = intVar(IntMinValue, IntMaxValue)

  // Logical utils
  def and(clauses: Iterable[Logical]): Logical = {
    if (clauses.exists {
          case LogicalBoolean(false) => true
          case _                     => false
        }) {
      LogicalBoolean(false)
    } else {
      val ilogs = for {
        clause <- clauses
        if !clause.isInstanceOf[LogicalBoolean]
      } yield
        clause match {
          case LogicalLogOp(value)   => value
          case LogicalBoolVar(value) => value
        }

      LogicalLogOp(LogOp.and(ilogs.toArray: _*))
    }
  }

  def or(clauses: Iterable[Logical]): Logical = {
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

  def post(clause: Logical): Unit = {
    clause match {
      case LogicalBoolean(value) if !value => falseConstraint().post()
      case LogicalBoolVar(value)           => addClauseTrue(value)
      case LogicalLogOp(value)             => addClauses(value)
      case _                               =>
    }
  }

  /** Creates a clause that express the fact the membership in membersVar implies corresponding Logical in membersClauses */
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

  /** Posts clauses that enforce membership in membersVar if corresponding Logical in membersClauses is true */
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

  /** Creates a clause that express the fact that at least one Logical clause in membersClauses has to be true for a member in membersVar */
  def existsSelected(
      membersClauses: Iterable[Logical],
      membersVar: SetVar
  ): Logical = {
    val clauses = mutable.ListBuffer.empty[ILogical]

    var idx = 0
    for (clause <- membersClauses) {
      clause match {
        case LogicalBoolean(value) =>
          if (value) clauses += member(idx, membersVar).reify
        case LogicalBoolVar(value) =>
          clauses += LogOp.and(member(idx, membersVar).reify, value)
        case LogicalLogOp(value) =>
          clauses += LogOp.and(member(idx, membersVar).reify, value)
        case _ =>
      }

      idx = idx + 1
    }

    if (clauses.nonEmpty)
      LogicalLogOp(LogOp.or(clauses: _*))
    else
      LogicalBoolean(false)
  }

  // Integer utils
  def sum(values: Iterable[Integer]): Integer = {
    val constValue =
      values.collect { case x: IntegerInt => x }.foldLeft(0)(_ + _.value)
    val intVars = values.collect { case x: IntegerIntVar => x }.map(_.value)

    if (intVars.isEmpty) {
      IntegerInt(constValue)
    } else {
      val sumVar =
        if (intVars.size == 1) // Not sure whether this optimization has any real effect. However since I wrote it already, I keep it here.
          intVars.head
        else {
          val sumVar = newIntVar
          sum(intVars.toArray, "=", sumVar).post()
          sumVar
        }

      if (constValue == 0)
        IntegerIntVar(sumVar)
      else
        arithmResult(intVar(constValue), "+", sumVar)
    }
  }

  def sumBasedOnMembership(
      membersVar: SetVar,
      values: Iterable[Integer]
  ): Integer = {
    IntegerIntVar(
      if (values.forall(_.isInstanceOf[IntegerInt]))
        sumIntsBasedOnMembership(membersVar, values)
      else
        sumGenericBasedOnMembership(membersVar, values)
    )
  }

  private def sumIntsBasedOnMembership(
      membersVar: SetVar,
      values: Iterable[Integer]
  ) = {
    val sumVar = newIntVar
    sumElements(
      membersVar,
      values.map(_.asInstanceOf[IntegerInt].value).toArray,
      sumVar
    ).post()
    sumVar
  }

  private def sumGenericBasedOnMembership(
      membersVar: SetVar,
      values: Iterable[Integer]
  ): IntVar = {
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

  private def arithmResult(
      left: IntVar,
      op: String,
      right: IntVar
  ): IntegerIntVar = {
    val result = newIntVar
    arithm(left, op, right, "=", result).post()
    IntegerIntVar(result)
  }

  private[enact] case class IntegerInt(value: Int) extends Integer {
    protected type ValueType = Int

    override def asInt: Int = value

    private def op(
        other: Integer,
        op: String,
        opFun: (Int, Int) => Int
    ): Integer = other match {
      case IntegerInt(otherValue) => IntegerInt(opFun(value, otherValue))
      case IntegerIntVar(otherValue) =>
        arithmResult(intVar(value), op, otherValue)
    }

    override def +(other: Integer): Integer = op(other, "+", _ + _)
    override def -(other: Integer): Integer = op(other, "-", _ - _)
    override def *(other: Integer): Integer = op(other, "*", _ * _)
    override def /(other: Integer): Integer = op(other, "/", _ / _)
    override def unary_-(): Integer = IntegerInt(-value)

    private def revRelOp(
        num: Integer,
        revOp: String,
        revFun: (Int, Int) => Boolean
    ) = {
      num match {
        case i: IntegerInt => LogicalBoolean(revFun(i.value, value))
        case iVar: IntegerIntVar =>
          LogicalBoolVar(arithm(iVar.value, revOp, value).reify())
      }
    }

    override def ===(num: Integer): Logical = revRelOp(num, "=", _ == _)
    override def !=(num: Integer): Logical = revRelOp(num, "!=", _ != _)
    override def <(num: Integer): Logical = revRelOp(num, ">", _ > _)
    override def >(num: Integer): Logical = revRelOp(num, "<", _ < _)
    override def <=(num: Integer): Logical = revRelOp(num, ">=", _ >= _)
    override def >=(num: Integer): Logical = revRelOp(num, "<=", _ <= _)
  }

  private[enact] case class IntegerIntVar(value: IntVar) extends Integer {
    protected type ValueType = IntVar

    override def asInt: Int = solution.getIntVal(value)

    def op(other: Integer, op: String): Integer = other match {
      case IntegerInt(otherValue)    => arithmResult(value, op, intVar(otherValue))
      case IntegerIntVar(otherValue) => arithmResult(value, op, otherValue)
    }

    override def +(other: Integer): Integer = op(other, "+")
    override def -(other: Integer): Integer = op(other, "-")
    override def *(other: Integer): Integer = op(other, "*")
    override def /(other: Integer): Integer = op(other, "/")
    override def unary_-(): Integer = IntegerIntVar(intMinusView(value))

    private def relOp(num: Integer, op: String) = {
      num match {
        case i: IntegerInt => LogicalBoolVar(arithm(value, op, i.value).reify())
        case iVar: IntegerIntVar =>
          LogicalBoolVar(arithm(value, op, iVar.value).reify())
      }
    }

    override def ===(num: Integer): Logical = relOp(num, "=")
    override def !=(num: Integer): Logical = relOp(num, "!=")
    override def <(num: Integer): Logical = relOp(num, "<")
    override def >(num: Integer): Logical = relOp(num, ">")
    override def <=(num: Integer): Logical = relOp(num, "<=")
    override def >=(num: Integer): Logical = relOp(num, ">=")
  }

  private[enact] var solution = new Solution(this)
  private[enact] var solutionExists = false

  def init(): Unit = {}

  def solveAndRecord(): Boolean = {
    val variables = getVars
    val result = getSolver.solve()

    if (result) {
      solution.record()
      solutionExists = true
    }

    result
  }

  def exists = solutionExists
}
