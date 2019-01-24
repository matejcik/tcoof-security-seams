package tcof

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.search.strategy.Search
import org.chocosolver.solver.search.strategy.selectors.values.{RealDomainMax, RealDomainMin}
import org.chocosolver.solver.search.strategy.selectors.variables.Cyclic
import org.chocosolver.solver.search.strategy.strategy.AbstractStrategy
import org.chocosolver.solver.variables.{IntVar, RealVar, SetVar, Variable}
import org.chocosolver.solver.{ResolutionPolicy, Solution, Model => ChocoModel}

import scala.collection.mutable

class SolverModel extends ChocoModel {
  /** Upper bound for integer variables of the solver */
  private[tcof] val IntMaxValue = 10000 // IntVar.MAX_INT_BOUND
  /** Lower bound for integer variables of the solver */
  private[tcof] val IntMinValue = -10000 // IntVar.MIN_INT_BOUND

  private[tcof] def newIntVar = intVar(IntMinValue, IntMaxValue)

  // Logical utils
  def and(clauses: Iterable[Logical]): Logical = {
    if (clauses.exists {
      case LogicalBoolean(value) if !value => true
      case _ => false
    }) {
      LogicalBoolean(false)
    } else {
      val ilogs = for {
        clause <- clauses
        if !clause.isInstanceOf[LogicalBoolean]
      } yield clause match {
        case LogicalLogOp(value) => value
        case LogicalBoolVar(value) => value
      }

      LogicalLogOp(LogOp.and(ilogs toArray: _*))
    }
  }

  def or(clauses: Iterable[Logical]): Logical = {
    if (clauses.exists {
      case LogicalBoolean(value) if value => true
      case _ => false
    }) {
      LogicalBoolean(true)
    } else {
      val ilogs = for {
        clause <- clauses
        if !clause.isInstanceOf[LogicalBoolean]
      } yield clause match {
        case LogicalLogOp(value) => value
        case LogicalBoolVar(value) => value
      }

      LogicalLogOp(LogOp.or(ilogs toArray: _*))
    }
  }

  def post(clause: Logical): Unit = {
    clause match {
      case LogicalBoolean(value) if !value => falseConstraint().post()
      case LogicalBoolVar(value) => addClauseTrue(value)
      case LogicalLogOp(value) => addClauses(value)
      case _ =>
    }
  }

  /** Creates a clause that express the fact the membership in membersVar implies corresponding Logical in membersClauses */
  def forAllSelected(membersClauses: Iterable[Logical], membersVar: SetVar): Logical = {
    val clauses = mutable.ListBuffer.empty[ILogical]

    var idx = 0
    for (clause <- membersClauses) {
      clause match {
        case LogicalBoolean(value) => if (!value) clauses += notMember(idx, membersVar).reify
        case LogicalBoolVar(value) => clauses += LogOp.implies(member(idx, membersVar).reify, value)
        case LogicalLogOp(value) => clauses += LogOp.implies(member(idx, membersVar).reify, value)
        case _ =>
      }

      idx = idx + 1
    }

    if (clauses.nonEmpty)
      LogicalLogOp(LogOp.and(clauses : _*))
    else
      LogicalBoolean(true)
  }

  /** Posts clauses that enforce membership in membersVar if corresponding Logical in membersClauses is true */
  def postEnforceSelected(membersClauses: Iterable[Logical], membersVar: SetVar): Unit = {
    var idx = 0
    for (clause <- membersClauses) {
      clause match {
        case LogicalBoolean(value) => if (value) post(member(idx, membersVar))
        case LogicalBoolVar(value) => ifThen(value, member(idx, membersVar))
        case LogicalLogOp(value) => post(LogicalLogOp(LogOp.implies(value, member(idx, membersVar).reify)))
        case _ =>
      }

      idx = idx + 1
    }
  }

  /** Creates a clause that express the fact that at least one Logical clause in membersClauses has to be true for a member in membersVar */
  def existsSelected(membersClauses: Iterable[Logical], membersVar: SetVar): Logical = {
    val clauses = mutable.ListBuffer.empty[ILogical]

    var idx = 0
    for (clause <- membersClauses) {
      clause match {
        case LogicalBoolean(value) => if (value) clauses += member(idx, membersVar).reify
        case LogicalBoolVar(value) => clauses += LogOp.and(member(idx, membersVar).reify, value)
        case LogicalLogOp(value) => clauses += LogOp.and(member(idx, membersVar).reify, value)
        case _ =>
      }

      idx = idx + 1
    }

    if (clauses.nonEmpty)
      LogicalLogOp(LogOp.or(clauses : _*))
    else
      LogicalBoolean(false)
  }


  // Integer utils
  def sum(values: Iterable[Integer]): Integer = {
    val constValue = values.collect{case x: IntegerInt => x}.foldLeft(0)(_ + _.value)
    val intVars = values.collect{case x: IntegerIntVar => x}.map(_.value)

    if (intVars.isEmpty) {
      IntegerInt(constValue)
    } else {
      val sumVar =
        if (intVars.size == 1) // Not sure whether this optimization has any real effect. However since I wrote it already, I keep it here.
          intVars.head
        else {
          val sumVar = newIntVar
          sum(intVars toArray, "=", sumVar).post()
          sumVar
        }

      if (constValue == 0)
        IntegerIntVar(sumVar)
      else
        addIntAndIntVar(constValue, sumVar)
    }
  }

  def sumBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]): Integer = {
    IntegerIntVar(
      if (values.forall(_.isInstanceOf[IntegerInt]))
        sumIntsBasedOnMembership(membersVar, values)
      else
        sumGenericBasedOnMembership(membersVar, values)
    )
  }

  private def sumIntsBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]) = {
    val sumVar = newIntVar
    sumElements(membersVar, values.map(_.asInstanceOf[IntegerInt].value) toArray, sumVar).post()
    sumVar
  }

  private def sumGenericBasedOnMembership(membersVar: SetVar, values: Iterable[Integer]): IntVar = {
    val condCostVars = new Array[IntVar](values.size)

    var idx = 0
    for (value <- values) {
      val condCostVar = newIntVar
      val costVarConstraint = value match {
        case IntegerInt(intVal) => arithm(condCostVar, "=", intVal)
        case IntegerIntVar(intVar) => arithm(condCostVar, "=", intVar)
      }

      ifThenElse(member(idx, membersVar), costVarConstraint, arithm(condCostVar, "=", 0))
      condCostVars(idx) = condCostVar

      idx = idx + 1
    }

    val sumVar = newIntVar
    sum(condCostVars, "=", sumVar).post()

    sumVar
  }


  private def addIntAndIntVar(left: Int, right: IntVar): IntegerIntVar = {
    val sumVar = newIntVar
    arithm(sumVar, "-", right, "=", left).post()
    IntegerIntVar(sumVar)
  }

  private def subIntAndIntVar(left: Int, right: IntVar): IntegerIntVar = {
    val resultVar = newIntVar
    arithm(resultVar, "+", right, "=", left).post()
    IntegerIntVar(resultVar)
  }

  private def multiplyIntAndIntVar(left: Int, right: IntVar): IntegerIntVar = {
    val productVar = newIntVar
    times(right, left, productVar).post()
    IntegerIntVar(productVar)
  }

  private def addIntVarAndIntVar(left: IntVar, right: IntVar): IntegerIntVar = {
    val sum = newIntVar
    arithm(left, "+", right, "=", sum).post()
    IntegerIntVar(sum)
  }

  private def subIntVarAndIntVar(left: IntVar, right: IntVar): IntegerIntVar = {
    val difference = newIntVar
    arithm(left, "-", right, "=", difference).post()
    IntegerIntVar(difference)
  }

  private def multiplyIntVarAndIntVar(left: IntVar, right: IntVar): IntegerIntVar = {
    val productVar = newIntVar
    times(left, right, productVar).post()
    IntegerIntVar(productVar)
  }

  private[tcof] case class IntegerInt(value: Int) extends Integer {
    protected type ValueType = Int

    override def solutionValue: Int = value

    override def +(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => IntegerInt(value + otherValue)
      case IntegerIntVar(otherValue) => addIntAndIntVar(value, otherValue)
    }

    override def -(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => IntegerInt(value - otherValue)
      case IntegerIntVar(otherValue) => subIntAndIntVar(value, otherValue)
    }

    override def *(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => IntegerInt(value * otherValue)
      case IntegerIntVar(otherValue) => multiplyIntAndIntVar(value, otherValue)
    }

    private def revRelOp(num: Integer, revOp: String, revFun: (Int, Int) => Boolean) = {
      num match {
        case i: IntegerInt => LogicalBoolean(revFun(i.value, value))
        case iVar: IntegerIntVar => LogicalBoolVar(arithm(iVar.value, revOp, value).reify())
      }
    }

    override def ===(num: Integer): Logical = revRelOp(num, "=", (x, y) => x == y)
    override def !=(num: Integer): Logical = revRelOp(num, "!=", (x, y) => x != y)
    override def <(num: Integer): Logical = revRelOp(num, ">", (x, y) => x > y)
    override def >(num: Integer): Logical = revRelOp(num, "<", (x, y) => x < y)
    override def <=(num: Integer): Logical = revRelOp(num, ">=", (x, y) => x >= y)
    override def >=(num: Integer): Logical = revRelOp(num, "<=", (x, y) => x <= y)
  }

  private[tcof] case class IntegerIntVar(value: IntVar) extends Integer {
    protected type ValueType = IntVar

    override def solutionValue: Int = solution.getIntVal(value)

    override def +(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => addIntAndIntVar(otherValue, value)
      case IntegerIntVar(otherValue) => addIntVarAndIntVar(value, otherValue)
    }

    override def -(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => subIntAndIntVar(otherValue, value)
      case IntegerIntVar(otherValue) => subIntVarAndIntVar(value, otherValue)
    }

    override def *(other: Integer): Integer = other match {
      case IntegerInt(otherValue) => multiplyIntAndIntVar(otherValue, value)
      case IntegerIntVar(otherValue) => multiplyIntVarAndIntVar(value, otherValue)
    }

    private def relOp(num: Integer, op: String) = {
      num match {
        case i: IntegerInt => LogicalBoolVar(arithm(value, op, i.value).reify())
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


  private[tcof] var solution = new Solution(this)
  private[tcof] var solutionExists = false


  def createDefaultSearchStategy = {
    val solver = getSolver

    // 1. retrieve variables, keeping the declaration order, and put them in four groups:
    val livars = mutable.ListBuffer.empty[IntVar]
    // integer and boolean variables
    val lsvars = mutable.ListBuffer.empty[SetVar]
    // set variables
    val lrvars = mutable.ListBuffer.empty[RealVar]
    // real variables.
    val variables = getVars
    var objective: Variable = null

    for (vr <- variables) {
      val typ = vr.getTypeAndKind
      if ((typ & Variable.CSTE) == 0) {
        val kind = typ & Variable.KIND
        kind match {
          case Variable.BOOL =>
          case Variable.INT =>
            livars += vr.asInstanceOf[IntVar]
          case Variable.SET =>
            lsvars += vr.asInstanceOf[SetVar]
          case Variable.REAL =>
            lrvars += vr.asInstanceOf[RealVar]
          case _ =>
        }
      }
    }

    // 2. extract the objective variable if any (to avoid branching on it)
    if (solver.getObjectiveManager.isOptimization) {
      objective = solver.getObjectiveManager.getObjective
      if ((objective.getTypeAndKind & Variable.REAL) != 0) { //noinspection SuspiciousMethodCalls
        lrvars -= objective.asInstanceOf[RealVar] // real var objective
      } else {
        assert((objective.getTypeAndKind & Variable.INT) != 0)
        livars -= objective.asInstanceOf[IntVar] // bool/int var objective
      }
    }

    // 3. Creates a default search strategy for each variable kind
    val strats = mutable.ListBuffer.empty[AbstractStrategy[_ <: Variable]]
    if (lsvars.size > 0) strats += Search.setVarSearch(lsvars : _*)
    if (livars.size > 0) strats += Search.intVarSearch(livars : _*)
    if (lrvars.size > 0) strats += Search.realVarSearch(lrvars: _*)

    // 4. lexico LB/UB branching for the objective variable
    if (objective != null) {
      val max = solver.getObjectiveManager.getPolicy eq ResolutionPolicy.MAXIMIZE
      if ((objective.getTypeAndKind & Variable.REAL) != 0) {
        strats += Search.realVarSearch(
          new Cyclic[RealVar],
          if (max) new RealDomainMax else new RealDomainMin,
          !max,
          objective.asInstanceOf[RealVar])
      } else {
        strats += (if (max) Search.minDomUBSearch(objective.asInstanceOf[IntVar]) else Search.minDomLBSearch(objective.asInstanceOf[IntVar]))
      }
    }

    // 5. avoid null pointers in case all variables are instantiated
    if (strats.isEmpty) strats += Search.minDomLBSearch(boolVar(true))

    // 6. add last conflict
    Search.lastConflict(Search.sequencer(strats : _*))
  }

  def init(): Unit = {
    val solver = getSolver
    solver.setSearch(createDefaultSearchStategy)
  }

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
