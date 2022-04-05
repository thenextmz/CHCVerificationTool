import java.io.{File, FileWriter}
import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.JavaTokenParsers
import scala.language.postfixOps
import sys.process._
import java.nio.file.Paths


/** Definition of the programming language BOOGIE type */
case class Program(decls: List[Decl])
case class Decl(p: String, param: Parameters, S: Body)
case class Parameters(param: List[String])
case class Body(S: List[Statement])
case class WhileBody(S: List[Statement])
case class IfBody(S: List[Statement])
case class ElseBody(S: List[Statement])

/** Statements */
sealed trait Statement
case class Assignment(v: Variable, E: Expression) extends Statement
case class Havoc(v: Variable) extends Statement
case class Assert(E: Expression) extends Statement
case class Assume(E: Expression) extends Statement
case class While(E: Expression, S: WhileBody, inv_num: Int) extends Statement
case class FuncCall(v: Variable, function: Function) extends Statement
case class LocalVar(name: String) extends Statement
case class IfElse(E: Expression, S1: IfBody, S2: ElseBody) extends Statement
case class NonDet(S1: Body, S2: Body) extends Statement

/** Rules */
sealed trait Rules
case class Implies(left: Any, right: Any) extends Rules
case class And(left: Any, right: Any) extends Rules
case class ForAll(var_list: VariableList, holds: Any) extends Rules

/** Expressions */
sealed trait Expression
case class E3(l: Expression, m: String, r: Expression, negated: Boolean = false) extends Expression
case class Function(p: String, E: Expression) extends Expression
case class Variable(name: String, negated: Boolean = false) extends Expression
case class Constant(v: String) extends Expression
case class VariableList(v_list: List[Expression]) extends Expression

/**
 * Program Parser
 * ident etc:
 * https://www.scala-lang.org/api/2.11.12/scala-parser-combinators/index.html#scala.util.parsing.combinator.JavaTokenParsers
 * */
class ProgramParser extends JavaTokenParsers {

  /** Program */
  def program: Parser[Program] =
    rep1(decl) ^^ {
      decls => Program(decls)
    }

  private def decl: Parser[Decl] =
    "def " ~> ident ~ "(" ~ parameters ~ ")" ~ "{" ~ body <~ "}" ^^ {
      case p ~ _ ~ param ~ _ ~ _ ~ s => Decl(p, param, s)
    }

  private def parameters: Parser[Parameters] =
    repsep(ident, ",") ^^ {
      param => Parameters(param)
    }

  private def body: Parser[Body] =
    rep1(statement) ^^ {
      statements => Body(statements)
    }

  private def if_body: Parser[IfBody] =
    rep1(statement) ^^ {
      statements => IfBody(statements)
    }

  private def else_body: Parser[ElseBody] =
    rep1(statement) ^^ {
      statements => ElseBody(statements)
    }

  private def while_body: Parser[WhileBody] =
    rep1(statement) ^^ {
      statements => WhileBody(statements)
    }

  /** Statements */
  private def statement: Parser[Statement] =
    funcCall | assignment | havoc | if_else | while_loop | assert | assume | local_var //| non_det

  private def local_var: Parser[LocalVar] =
    "local " ~> ident ^^ {
      v => LocalVar(v)
    }

  /* ND Is not implemented as we dont want to be able to use it in program code
  // Also this gives a left-recursion endless loop
  private def non_det: Parser[NonDet] =
    body ~ "ND" ~ body ^^ {
      case s1 ~ _ ~ s2 => NonDet(s1, s2)
    }
   */

  private def funcCall: Parser[FuncCall] =
    variable ~ ":=" ~ func ^^ {
      case v ~ _ ~ p => FuncCall(v, p)
    }

  private def assignment: Parser[Assignment] =
    variable ~ ":=" ~ expression ^^ {
      case v ~ _ ~ e => Assignment(v, e)
    }

  private def havoc: Parser[Havoc] =
    "havoc " ~> variable ^^ {
      x => Havoc(x)
    }

  private def if_else: Parser[IfElse] =
    "if" ~ "(" ~> expression_logic ~ ")" ~ "{" ~ if_body ~ "}" ~ "else" ~ "{" ~ else_body <~ "}" ^^ {
      case e ~ _ ~ _ ~ s1 ~ _ ~ _ ~ _ ~ s2 => IfElse(e, s1, s2)
    }

  // Variable for the invariant name counter
  var inv_cnt = 0
  private def while_loop: Parser[While] =
    "while" ~ "(" ~> expression_logic ~ ")" ~ "{" ~ while_body <~ "}" ^^ {
      case e ~ _ ~ _ ~ s =>
        inv_cnt = inv_cnt + 1
        While(e, s, inv_cnt)
    }

  private def assert: Parser[Assert] =
    "assert " ~> expression_logic ^^ {
      e => Assert(e)
    }

  private def assume: Parser[Assume] =
    "assume " ~> expression_logic ^^ {
      e => Assume(e)
    }

  /** Expressions */
  private def expression: Parser[Expression] = func | e3 | e3_with_brackets | variable | constant
  private def subexpression: Parser[Expression] = func | e3_with_brackets | variable | constant // no e3
  private def expression_logic: Parser[Expression] = e3_logic | e3_logic_brackets
  private def one_of_logic: Parser[String] = ">=" | "<=" | "==" | "=" | "<" | ">" | "!=" | "%"
  private def one_of_arith: Parser[String] =  "+" | "-" | "/" | "*"
  private def one_of = one_of_arith | one_of_logic

  private def e3: Parser[E3] =
    subexpression ~ one_of ~ expression ^^ {
      case l ~ m ~ r => E3(l, m, r)
    }

  private def e3_logic: Parser[E3] =
    subexpression ~ one_of_logic ~ expression ^^ {
      case l ~ m ~ r => E3(l, m, r)
    }

  private def e3_logic_brackets: Parser[E3] =
    "(" ~> e3_logic <~ ")"

  private def e3_with_brackets: Parser[E3] =
    opt ("-") ~ "(" ~ subexpression ~ one_of ~ expression <~ ")" ^^ {
      case n ~ _ ~ l ~ m ~ r => E3(l, m, r, n.nonEmpty)
    }

  private def variable: Parser[Variable] =
    opt("-") ~ ident ^^ {
      case n ~ v => Variable(v, n.nonEmpty)
    }

  private def constant: Parser[Constant] =
    wholeNumber ^^ {
      c => Constant(c)
    }

  private def func: Parser[Function] =
    ident ~ "(" ~ repsep(expression, ",") <~ ")" ^^ {
      case p ~ _ ~ param =>
        if(param.nonEmpty) {
          if(param.size == 1){
            Function(p, param.head)
          } else {
            Function(p, VariableList(param))
          }
        } else {
          Function(p, VariableList(List.empty))
        }
    }
}

/** parse */
object parse extends ProgramParser {
  def apply(string: String): ParseResult[Program] = parseAll(program, string)
}

/** RUN */
object main extends App {

  /** Global Variables **/
  var variable_name_counter = 0
  var INS_BITVECTOR: Boolean = false
  var BITVEC_SIZE = 8

  /**
   * This function generates a new Integer for appending it to variable names
   *
   * @return Integer to append
   * */
  def getVarNum: Int = {
    // Adding a number to variable names for distinction
    variable_name_counter = variable_name_counter + 1
    // Return
    variable_name_counter
  }

  /**
   * This function extracts all Variables occurring in a procedure
   *
   * @param statement:    Statement possibly containing variables
   * @param var_buffer:   Buffer to store all found variables
   * @return var_buffer:  The distinct buffer with all Variables as List[Variable]
   * */
  def getAllVariables(statement: Any, Q: ListBuffer[Any], var_buffer: ListBuffer[Variable]): List[Variable] = {
    // Get all Variables from statement parameter
    // In this case we dont need variables that are digits
    statement match {
      case Assignment(v, e) =>
        var_buffer += v
        getAllVariables(e, null, var_buffer)

      case Havoc(v) => var_buffer += v

      case Assert(e) => getAllVariables(e, null, var_buffer)

      case Assume(e) => getAllVariables(e, null, var_buffer)

      case While(e, s, inv_num) =>
        getAllVariables(e, null, var_buffer)
        for (elem <- s.S) {
          getAllVariables(elem, null, var_buffer)
        }

      case FuncCall(v, function) =>
        var_buffer += v
        getAllVariables(function.E, null, var_buffer)

      case E3(l, m, r, negated) =>
        l match {
          case E3(l, m, r, negated) => getAllVariables(l, null, var_buffer)
          case Variable(name, negated) => var_buffer += Variable(name)
          case Constant(v) =>
          case Function(p, e) => // this should not occur
          case VariableList(v_list) => // this should not occur
        }
        r match {
          case E3(l, m, r, negated) => getAllVariables(r, null, var_buffer)
          case Variable(name, negated) => var_buffer += Variable(name)
          case Constant(v) => // nothing to do here
          case Function(p, e) => // this should not occur
          case VariableList(v_list) => // this should not occur
        }

      case Function(p, e) => getAllVariables(e, null, var_buffer)

      case Variable(name, negated) => var_buffer += Variable(name)

      case VariableList(v_list) =>
        for (elem <- v_list){
          getAllVariables(elem, null, var_buffer)
        }

      case LocalVar(name) => var_buffer += Variable(name)

      case IfElse(e, s1, s2) =>
        getAllVariables(e, null, var_buffer)
        for (elem <- s1.S) {
          getAllVariables(elem, null, var_buffer)
        }
        for (elem <- s2.S) {
          getAllVariables(elem, null, var_buffer)
        }

      case NonDet(s1, s2) =>
        for (elem <- s1.S) {
          getAllVariables(elem, null, var_buffer)
        }
        for (elem <- s2.S) {
          getAllVariables(elem, null, var_buffer)
        }

      case Constant(c) => // nothing to do here

      // RULES
      case Implies(left, right) =>
        getAllVariables(left, null, var_buffer)
        getAllVariables(right, null, var_buffer)

      case And(left, right) =>
        getAllVariables(left, null, var_buffer)
        getAllVariables(right, null, var_buffer)

      case ForAll(var_list, holds) =>
        getAllVariables(var_list, null, var_buffer)
        getAllVariables(holds, null, var_buffer)

      // This should not occure
      case _ =>
        println("getAllVariables: Default case - Something Went Wrong")
        sys.exit(-1)
    }

    // Get all variables from Q
    if(Q != null && Q.nonEmpty){
      for (elem <- Q){
        getAllVariables(elem, null, var_buffer)
      }
    }

    // Return, distinct filters out double occurrence of variables
    var_buffer.distinct.toList
  }

  /**
   * This function replaces Variables
   *
   * @param replace_where:  The Structure where Variables are to be replaced
   * @param replace_what:   The Variable to be replaced when found
   * @param replace_with:   The Expression (can also be Variable!) to replace the found one with
   * @return                The "same" data-structure but with replaced variables is returned
   * */
  def replaceVariables(replace_where: Any, replace_what: Variable, replace_with: Expression): Any = {

    replace_where match {
      /* Expressions */
      case E3(l, m, r, negated) =>
        return E3(replaceVariables(l, replace_what, replace_with).asInstanceOf[Expression], m,
          replaceVariables(r, replace_what, replace_with).asInstanceOf[Expression], negated)

      case Function(p, e) =>
        return Function(p, replaceVariables(e, replace_what, replace_with).asInstanceOf[Expression])

      case Variable(name, negated) =>
        if(name == replace_what.name){
          replace_with match {
            case Variable(name2, negated2) =>
              // example y := -x
              // you find -y: -y -> -(-x) == x
              if(negated && negated2){
                return Variable(name2)
              }
            case Constant(c) =>
              if(c.toInt < 0 && negated){
                return Constant((c.toInt * -1).toString)
              }

            case E3(l, m, r, negated3) =>
              if(negated && negated3){
                return E3(l, m, r)
              }
              if(negated){
                return E3(l, m, r, negated)
              }

            case _ => // Default, return replace_with
          }
          return replace_with
        } else {
          return replace_where
        }

      case Constant(v) =>
        return replace_where

      case VariableList(v_list) =>
        val tmp_buff: ListBuffer[Any] = new ListBuffer[Any]
        var tmp_list: List[Expression] = null
        for (elem <- v_list){
          tmp_buff += replaceVariables(elem, replace_what, replace_with)
        }
        tmp_list = tmp_buff.toList.asInstanceOf[List[Expression]]
        return VariableList(tmp_list)

      /* Rules */
      case Implies(left, right) =>
        return Implies(replaceVariables(left, replace_what, replace_with),
          replaceVariables(right, replace_what, replace_with))

      case And(left, right) =>
        return And(replaceVariables(left, replace_what, replace_with),
          replaceVariables(right, replace_what, replace_with))

      case ForAll(var_list, holds) =>
        val var_buffer: ListBuffer[Variable] = new ListBuffer[Variable]
        val variables = getAllVariables(var_list, null, var_buffer)

        // If the variable to replace occurs in the var_list it is captured and we do not replace anything
        for (elem <- variables){
          if(elem.name == replace_what.name){
            return replace_where
          }
        }
        //  Can this occur?
        //  Forall only occurs with multiple variables if there is inv(w)
        //  as w has all variables in the procedure we can never replace any of them
        //  can this lead to problems?
        return ForAll(var_list, replaceVariables(holds, replace_what, replace_with))

      case _ =>
        println("replaceVariables: Default case - Something Went Wrong")
        sys.exit(-1)
    }
    // Return -> SHOULD NOT BE REACHED
    replace_where
  }


  /**
   * This function changes variables in inner Foralls and removes them
   *
   * @param find_forall:  The Structure to remove foralls from
   * @return              Structure without foralls and replaced variables
   * */
  def noForall(find_forall: Any): Any = {

    find_forall match {
      /* Rules */
      case Implies(left, right) =>
        return Implies(noForall(left), noForall(right))

      case And(left, right) =>
        return And(noForall(left), noForall(right))

      case ForAll(var_list, holds) =>
        // Get all variables from the variable list
        val replace_list = new ListBuffer[Variable]
        getAllVariables(var_list, null, replace_list)

        var holds_new: Any = holds
        // replace all variables in holds with new var names into holds_new
        for (elem <- replace_list){
          holds_new = replaceVariables(holds_new, elem, Variable(elem.name + getVarNum, elem.negated))
        }

        // Using holds new here
        return noForall(holds_new)
      case _ => // do nothing
    }
    // Return
    find_forall
  }

  /**
   * This function simplifies/changes/generates CHCs
   *
   * @param change_this:  The Structure to simplify
   * @param buff:         Buffer Containing the new CHCs
   * @return              The buffer with the new "split" CHCs
   * */
  def simplify(change_this: Any, buff: ListBuffer[Any]): Any = {

    var A: Any = null
    var B: Any = null
    var C: Any = null
    val buff_new: ListBuffer[Any] = new ListBuffer[Any]

    change_this match {
      // a->(b&&c)  === (a->b)&&(a->c)
      case Implies(left, And(left2, right2)) =>
        A = left
        B = left2
        C = right2
        buff += simplify(Implies(A, B), buff_new)
        buff += simplify(Implies(A, C), buff_new)
        return buff

      // a -> (b -> c) === (a && b) -> c
      case Implies(left, Implies(left2, right2)) =>
        A = left
        B = left2
        C = right2
        return simplify(Implies(simplify(And(A, B), buff_new), C), buff_new)

      // This could only be the first forall!??
      case ForAll(var_list, holds) =>
        // Variables get searched and added to the front later on anyways so i do not care about the var_list
        return simplify(holds, buff_new)


      case _ => // return change this aka. do nothing
    }
    // return -> this happens if nothing is to be simplified
    change_this
  }

  /**
   * This function turns declarations into a lists of "wlp" for the algorithm later
   * Also it stores the variables occurring in the declaration -> procedure_variables
   *
   * @param declaration:  The declaration containing the data
   * @return              This function returns a tuple consisting of the "wlp" ListBuffer and the procedure variables
   * */
  def toHorn(declaration: Decl) : (ListBuffer[Any], List[Variable]) = {
    // Variables Needed for Transformation
    val func_name = declaration.p
    val func_param = declaration.param.param
    val func_body = declaration.S.S
    val name_pre = func_name + "_pre"
    val start_wlp = new ListBuffer[Any]()

    // Getting all variables used in a procedure
    var procedure_variables: List[Variable] = List[Variable]()
    val procedure_variables_tmp: ListBuffer[Variable] = new ListBuffer[Variable]
    for (elem <- func_param){
      procedure_variables_tmp += Variable(elem)
    }

    // If there are parameters the the variables + _0 are introduced and also part of procedure  vars
    if(func_param.nonEmpty){
      for (elem <- func_param){
        procedure_variables_tmp += Variable(elem + "_0")
      }
    }
    procedure_variables =
      getAllVariables(
        declaration.S.S.head,
        declaration.S.S.drop(1).to[ListBuffer].asInstanceOf[ListBuffer[Any]],
        procedure_variables_tmp)
    println("Procedure Vars: " + procedure_variables)

    // Case: Main Without Arguments
    if(func_name == "main" && func_param.isEmpty){
      // No havoc, no assume, no p_pre(x)
      // Fill S
      for (i <- func_body.indices) {
        start_wlp += func_body(i)
      }

      // Add p(x_0, ret) => in this case this is T
      start_wlp += Constant("True")
      return (start_wlp, procedure_variables)
    }

    // Case: function with Arguments
    if(func_name != "main" && func_param.nonEmpty){
      val var_buffer: ListBuffer[Any] = new ListBuffer[Any]
      val var_buffer_0: ListBuffer[Any] = new ListBuffer[Any]
      for (elem <- func_param){
        // Add havoc x0, y0 z0...
        start_wlp += Havoc(Variable(elem + "_0"))

        // Add assume x0 = x, y0 = y, z0 = z...
        start_wlp += Assume(E3(Variable(elem + "_0"), "=", Variable(elem)))

        // adding my variables to buffer
        var_buffer += Variable(elem)

        // same for x0...
        var_buffer_0 += Variable(elem + "_0")
      }

      // Add assume p_pre(x, y, z...)
      start_wlp += Assume(Function(name_pre, VariableList(var_buffer.toList.asInstanceOf[List[Expression]])))

      // Add S
      for (i <- func_body.indices) {
        start_wlp += func_body(i)
      }

      // Add p(x0, ret)
      // We always add ret even if it might not be used in the program
      var_buffer_0 += Variable("ret")
      val param_list = VariableList(var_buffer_0.toList.asInstanceOf[List[Expression]])
      start_wlp += Function(func_name, param_list)

      return (start_wlp, procedure_variables)
    }

    // Case: Main With Arguments
    if(func_name == "main" && func_param.nonEmpty){
      val var_buffer: ListBuffer[Any] = new ListBuffer[Any]
      val var_buffer_0: ListBuffer[Any] = new ListBuffer[Any]
      for (elem <- func_param){
        // Add havoc x0, y0 z0...
        start_wlp += Havoc(Variable(elem + "_0"))

        // Add assume x0 = x, y0 = y, z0 = z...
        start_wlp += Assume(E3(Variable(elem + "_0"), "=", Variable(elem)))

        // adding my variables to buffer
        var_buffer += Variable(elem)

        // same for x0...
        var_buffer_0 += Variable(elem + "_0")
      }

      /** Add assume p_pre(x, y, z...)
       * Not needed
       * */

      // Add S
      for (i <- func_body.indices) {
        start_wlp += func_body(i)
      }

      // Add p(x_0, ret) => in this case this is T
      start_wlp += Constant("True")
      return (start_wlp, procedure_variables)
    }

    // Case: Function Without Arguments
    if(func_name != "main" && func_param.isEmpty){
      val var_buffer: ListBuffer[Any] = new ListBuffer[Any]
      val var_buffer_0: ListBuffer[Any] = new ListBuffer[Any]

      // Add assume p_pre(x, y, z...)
      start_wlp += Assume(Function(name_pre, VariableList(List.empty)))

      // Add S
      for (i <- func_body.indices) {
        start_wlp += func_body(i)
      }

      // Add p(x0, ret)
      // We always add ret even if it might not be used in the program
      var_buffer_0 += Variable("ret")
      val param_list = VariableList(var_buffer_0.toList.asInstanceOf[List[Expression]])
      start_wlp += Function(func_name, param_list)

      return (start_wlp, procedure_variables)
    }
    // Should Not Get Here
    println("You Should Not Be Here!")
    null
  }

  /**
   * This function negates a logical expression
   *
   * @param expr: The expression to be negated
   * @return      The negated expression is returned
   * */
  def negateExpression(expr: Expression): Expression = {

    var expr_negated: Expression = null
    expr match {
      case E3(l, m, r, negated) =>
        m match {
          case ">=" =>
            expr_negated = E3(l, "<", r, negated)
          case "<=" =>
            expr_negated = E3(l, ">", r, negated)
          case "=" =>
            expr_negated = E3(l, "!=", r, negated)
          case "==" =>
            expr_negated = E3(l, "!=", r, negated)
          case "<" =>
            expr_negated = E3(l, ">=", r, negated)
          case ">" =>
            expr_negated = E3(l, "<=", r, negated)
          case "!=" =>
            expr_negated = E3(l, "==", r, negated)
        }

      case _ =>
        println("Something Went Wrong") // should not occur
        sys.exit(-1)
    }
    // Return
    expr_negated
  }

  /**
   * This function applies the wlp transformation rules
   *
   * @param statement:      Statement S -> wlp(S, Q)
   * @param Q:              Single/multiple statements -> wlp(S, Q)
   * @param procedure_vars: A list of all occuring procedure variables (needed for while-rule)
   * @return                A set of rules after transformation is finished is returned
   * */
  def wlpAlgorithm(statement: Any, Q: ListBuffer[Any], procedure_vars: List[Variable]) : Any = {
    val buffer: ListBuffer[Any] = new ListBuffer[Any]
    statement match {
      case Assignment(v, e) =>
        // wlp(x := E, Q) := let x = E in Q
        if(Q.size >= 2) {
          return replaceVariables(wlpAlgorithm(Q.head, Q.drop(1), procedure_vars), v, e)
        } else {
          return replaceVariables(Q.head, v, e)
        }

      case Havoc(v) =>
        // wlp(havoc x, Q) := ∀x . Q
        if(Q.size >= 2) {
          return ForAll(VariableList(List[Variable](Variable(v.name))), wlpAlgorithm(Q.head, Q.drop(1), procedure_vars))
        } else {
          return ForAll(VariableList(List[Variable](Variable(v.name))), Q.head)
        }

      case Assert(e) =>
        // wlp(assert ϕ, Q) := ϕ ∧ Q
        if(Q.size >= 2) {
          return And(e, wlpAlgorithm(Q.head, Q.drop(1), procedure_vars))
        } else {
          return And(e, Q.head)
        }

      case Assume(e) =>
        // wlp(assume ϕ, Q) := ϕ → Q
        if(Q.size >= 2) {
          return Implies(e, wlpAlgorithm(Q.head, Q.drop(1), procedure_vars))
        } else {
          return Implies(e, Q.head)
        }

      case While(e, s, inv_num) =>
        // wlp((while E do S), Q) := inv(w) ∧ ∀w . (((inv(w) ∧ E) → wlp(S, inv(w))) ∧ ((inv(w) ∧ ¬E) → Q))
        var var_buffer: List[Variable] = List[Variable]()
        var_buffer = procedure_vars //getAllVariables(statement, Q, var_buffer_tmp)

        val wlp_new: ListBuffer[Any] = new ListBuffer[Any]
        for (elem <- s.S){
          wlp_new += elem
        }
        val inv = "inv" + inv_num
        wlp_new += Function(inv, VariableList(var_buffer))

        if(Q.size >= 2) {
          return And(Function(inv, VariableList(var_buffer)), ForAll(VariableList(var_buffer),
            And(Implies(And(Function(inv, VariableList(var_buffer)), e),
              wlpAlgorithm(wlp_new.head.asInstanceOf[Statement], wlp_new.drop(1), procedure_vars)),
              Implies(And(Function(inv, VariableList(var_buffer)), negateExpression(e)),
                wlpAlgorithm(Q.head, Q.drop(1), procedure_vars)))))
        } else {
          return And(Function(inv, VariableList(var_buffer)), ForAll(VariableList(var_buffer),
            And(Implies(And(Function(inv, VariableList(var_buffer)), e),
              wlpAlgorithm(wlp_new.head.asInstanceOf[Statement], wlp_new.drop(1), procedure_vars)),
              Implies(And(Function(inv, VariableList(var_buffer)), negateExpression(e)), Q.head))))
        }

      case FuncCall(v, function) =>
        // wlp(y := p(E), Q) := p_pre (E) ∧ (∀r . p(E, r) → Q[r/y])
        val p_pre = function.p + "_pre"
        val new_var = "r" +  getVarNum

        // new list of procedure vars as we add r to the scope
        // This might be needed in an inner while loop?
        val procedure_vars_new = procedure_vars :+ Variable(new_var)

        if(Q.size >= 2) {
          function.E match {
            case VariableList(v_list) =>
              val new_e_list: ListBuffer[Expression] = v_list.to[ListBuffer]
              new_e_list += Variable(new_var)
              return And(Function(p_pre, function.E), ForAll(VariableList(List[Variable](Variable(new_var))),
                Implies(Function(function.p, VariableList(new_e_list.toList)),
                  replaceVariables(wlpAlgorithm(Q.head, Q.drop(1), procedure_vars_new), v, Variable(new_var)))))
            case _ =>
              return And(Function(p_pre, function.E), ForAll(VariableList(List[Variable](Variable(new_var))),
                Implies(Function(function.p, VariableList(List[Expression](function.E, Variable(new_var)))),
                  replaceVariables(wlpAlgorithm(Q.head, Q.drop(1), procedure_vars_new), v, Variable(new_var)))))
          }
        } else {
          function.E match {
            case VariableList(v_list) =>
              val new_e_list: ListBuffer[Expression] = v_list.to[ListBuffer]
              new_e_list += Variable(new_var)
              return And(Function(p_pre, function.E), ForAll(VariableList(List[Variable](Variable(new_var))),
                Implies(Function(function.p, VariableList(new_e_list.toList)),
                  replaceVariables(Q.head, v, Variable(new_var)))))
            case _ =>
              return And(Function(p_pre, function.E), ForAll(VariableList(List[Variable](Variable(new_var))),
                Implies(Function(function.p, VariableList(List[Expression](function.E, Variable(new_var)))),
                  replaceVariables(Q.head, v, Variable(new_var)))))
          }
        }

      case NonDet(s1, s2) =>
        // wlp((S1 ND S2), Q) := wlp(S1, Q) ∧ wlp(S2, Q)
        var buffer_s1: ListBuffer[Any] = new ListBuffer[Any]
        var buffer_s2: ListBuffer[Any] = new ListBuffer[Any]

        buffer_s1 = s1.S.to[ListBuffer].asInstanceOf[ListBuffer[Any]]
        buffer_s2 = s2.S.to[ListBuffer].asInstanceOf[ListBuffer[Any]]
        for (elem <- Q) {
          buffer_s1 += elem
          buffer_s2 += elem
        }

        return And(wlpAlgorithm(buffer_s1.head, buffer_s1.drop(1), procedure_vars),
          wlpAlgorithm(buffer_s2.head, buffer_s2.drop(1), procedure_vars))

      case IfElse(e, s1, s2) =>
        //wlp((if E then S1 else S2), Q) := wlp(((assume E; S1 )ND(assume ¬E; S2)), Q)
        var buffer_s1: ListBuffer[Any] = new ListBuffer[Any]
        var buffer_s2: ListBuffer[Any] = new ListBuffer[Any]

        buffer_s1 = s1.S.to[ListBuffer].asInstanceOf[ListBuffer[Any]]
        buffer_s2 = s2.S.to[ListBuffer].asInstanceOf[ListBuffer[Any]]

        for (elem <- Q) {
          buffer_s1 += elem
          buffer_s2 += elem
        }

        val e_negation = negateExpression(e)
        return And(wlpAlgorithm(Assume(e), buffer_s1, procedure_vars),
          wlpAlgorithm(Assume(e_negation), buffer_s2, procedure_vars))

      case LocalVar(v) =>
        // No need to take any action, just start new wlp run with next elements
        if(Q.size >= 2) {
          return wlpAlgorithm(Q.head, Q.drop(1), procedure_vars)
        }
        return buffer

      case Constant(c) => // This should not occur
    }
    // Return null should not be reached
    null
  }

  /**
   * This function flattens a Listbuffer containing Elements that can also Be
   * ListBuffers to be flatten into a one dimensional ListBuffer
   *
   * @param check:    The ListBuffer to Flatten
   * @param new_list: New Buffer to store the Flattened Buffer
   * @return          The Flattened ListBuffer
   * */
  def flattenListBuffer(check: ListBuffer[Any], new_list: ListBuffer[Any]): ListBuffer[Any] = {

    for (elem <- check){
      elem.getClass.getName match {
        case "scala.collection.mutable.ListBuffer" =>
          flattenListBuffer(elem.asInstanceOf[ListBuffer[Any]], new_list)
        case _ => new_list += elem
      }
    }

    // return
    new_list
  }

  /**
   * This function transforms rules and expressions to strings matching the syntax of the
   * Microsoft Z3 Python library
   * Also it builds a string containing all function declarations that are needed in Z3
   *
   * @param holds:            This is a set of expressions/rules from the outer Forall
   * @param func_str_buffer:  Buffer to store the function declarations
   * @return                  This returns a tuple with the new build string and the List of declaration strings
   * */
  def rulesExpressionsToString(holds: Any, func_str_buffer: ListBuffer[String]): (String, ListBuffer[String]) = {
    // Rules And Expressions to string
    var cnt = 0
    var tmp_string: String = ""

    holds match {
      case Implies(left, right) =>
        tmp_string += "Implies("
        tmp_string += rulesExpressionsToString(left, func_str_buffer)._1 + ","
        tmp_string += rulesExpressionsToString(right, func_str_buffer)._1
        tmp_string += ")"
      case And(left, right)  =>
        tmp_string += "And("
        tmp_string += rulesExpressionsToString(left, func_str_buffer)._1 + ","
        tmp_string += rulesExpressionsToString(right, func_str_buffer)._1
        tmp_string += ")"

      case Function(p, e) =>
        func_str_buffer.insert(cnt, p + "=" + "Function('" + p + "'" + ",")
        tmp_string += p + "("

        var num_param = 0
        e match {
          case E3(l, m, r, negated) =>
            num_param = num_param + 1
          case Function(p, e) =>
            num_param = num_param + 1
          case Variable(name, negated) =>
            num_param = num_param + 1
          case Constant(v) =>
            num_param = num_param + 1
          case VariableList(v_list) =>
            for (elem <- v_list){
              num_param = num_param + 1
            }
        }

        // Adding IntSort() according to number of params
        if(!INS_BITVECTOR){
          for(a <- 1 to num_param){
            func_str_buffer(cnt) = func_str_buffer(cnt) + "IntSort(),"
          }
        } else {
          for(a <- 1 to num_param){
            func_str_buffer(cnt) = func_str_buffer(cnt) + "BitVecSort(" + BITVEC_SIZE + "),"
          }
        }


        e match {
          case E3(l, m, r, negated) =>
            if(negated){
              tmp_string += "-"
            }
            tmp_string += "("
            tmp_string += rulesExpressionsToString(l, func_str_buffer)._1
            if(m == "=") {
              tmp_string += "=="
            } else {
              tmp_string += m
            }
            tmp_string += rulesExpressionsToString(r, func_str_buffer)._1
            tmp_string += ")"
          case Variable(name, negated) =>
            if(!negated) {
              tmp_string += name
            } else {
              tmp_string += "-" + name
            }

          case Constant(v) =>
            tmp_string += v
          case VariableList(v_list) =>
            for (elem <- v_list){
              tmp_string += rulesExpressionsToString(elem, func_str_buffer)._1  + ","
            }
          case _ =>
            println("Default 28: - Something Went Wrong")
            sys.exit(-1)
        }

        // The last "Object" in list does not need a separator
        if(tmp_string.takeRight(1) == ","){
          tmp_string = tmp_string.dropRight(1)
        }

        // Adding BoolSort() as return type to functions
        func_str_buffer(cnt) = func_str_buffer(cnt) + "BoolSort())"

        tmp_string += ")"
        cnt = cnt + 1

      case Variable(name, negated) =>
        if(!negated) {
          tmp_string += name
        } else {
          tmp_string += "-" + name
        }

      case Constant(v) =>
        tmp_string += v

      case E3(l, m, r, negated) =>
        if(negated){
          tmp_string += "-"
        }
        tmp_string += "("
        tmp_string += rulesExpressionsToString(l, func_str_buffer)._1
        if(m == "=") {
          tmp_string += "=="
        } else {
          tmp_string += m
        }
        tmp_string += rulesExpressionsToString(r, func_str_buffer)._1
        tmp_string += ")"


      case VariableList(v_list) =>
        for (elem <- v_list){
          tmp_string += rulesExpressionsToString(elem, func_str_buffer)._1  + ","
        }
        if(tmp_string.takeRight(1) == ","){
          tmp_string = tmp_string.dropRight(1)
        }

      case _ =>
        println("Default 29: Something Went Wrong")
        sys.exit(-1)
        return ("", null) // found nothing
    }
    // return
    (tmp_string, func_str_buffer.distinct)
  }

  /**
   * Generally building the Strings for Microsoft Z3 Python library
   *
   * @param constraint_horn_clauses:  A list containing all CHCs that need to be made into a string
   * @param z3_strings:               Buffer to store the strings
   * @return                          A python program stored line by line as string in a ListBuffer
   * */
  def generateZ3Input(constraint_horn_clauses: ListBuffer[ForAll], z3_strings: ListBuffer[String]): ListBuffer[String] = {

    var tmp_tuple: (String, ListBuffer[String]) = null
    var var_buffer: ListBuffer[Variable] = new ListBuffer[Variable]
    val func_str_buffer: ListBuffer[String] = new ListBuffer[String]
    var tmp_string: String = ""

    z3_strings += "from z3 import *"
    z3_strings += "s = SolverFor(\"HORN\")"

    for (elem <- constraint_horn_clauses){
      getAllVariables(elem, null, var_buffer)
    }
    var_buffer = var_buffer.distinct

    for (elem <- var_buffer){
      tmp_string += elem.name
      if(elem != var_buffer.last){
        tmp_string += ","
      }
    }

    if(!INS_BITVECTOR){
      // Ints if multiple variables
      if(var_buffer.length > 1){
        tmp_string += " = Ints('"
      }
      // Int if only one variable
      if(var_buffer.length == 1) {
        tmp_string += " = Int('"
      }
      for (elem <- var_buffer){
        tmp_string += elem.name
        if(elem != var_buffer.last){
          tmp_string += " "
        }
      }

      // only close if there was a Int/s(...
      if(var_buffer.nonEmpty){
        tmp_string += "')"
      }
    } else {
      // Ints if multiple variables
      if(var_buffer.length > 1){
        tmp_string += " = BitVecs('"
      }
      // BitVec if only one variable
      if(var_buffer.length == 1) {
        tmp_string += " = BitVec('"
      }
      for (elem <- var_buffer){
        tmp_string += elem.name
        if(elem != var_buffer.last){
          tmp_string += " "
        }
      }

      // only close if there was a BitVec/s(...
      if(var_buffer.nonEmpty){
        tmp_string += "',"  + BITVEC_SIZE + ")"
      }
    }

    // do not add tmp_string if empty as it results in a blank line
    // has no effect but unnecessary
    if(tmp_string.nonEmpty){
      z3_strings += tmp_string
    }

    for (chc <- constraint_horn_clauses){
      // only add forall if there are variables
      if(var_buffer.nonEmpty){
        tmp_string = "s.add(ForAll(["
        // Adding variables to ForAll
        for (v <- chc.var_list.v_list){
          v match {
            case Variable(name, negated) =>
              tmp_string += name
              if(v != chc.var_list.v_list.last){
                tmp_string += ","
              }
            case _ =>
              println("Default 30: Something Went Wrong")
              sys.exit(-1)
          }
        }
        tmp_string += "], "
      } else {
        // No variables -> no forall
        tmp_string += "s.add("
      }

      // Rules And Expressions to string
      //tmp_tuple = rulesExpressionsToString(chc.holds.asInstanceOf[Implies], func_str_buffer)
      tmp_tuple = rulesExpressionsToString(chc.holds, func_str_buffer)

      tmp_string += tmp_tuple._1
      // Close double if there was a forall
      if(var_buffer.nonEmpty){
        tmp_string += "))"
      } else {
        // single close -> no forall
        tmp_string += ")"
      }
      z3_strings += tmp_string
    }

    z3_strings += "res = s.check()"
    z3_strings += "print(res)"
    z3_strings += "if res == sat: print(s.model())"

    // Reordering the z3_strings as i want to input the definitions of functions before the s.add(...)
    val z3_reordered: ListBuffer[String] = new ListBuffer[String]

    var cnt = 0
    var input_index = 2

    if(var_buffer.isEmpty){
      input_index = 1
    }

    for (elem <- z3_strings){
      // Always 3 elements before i want to insert if variables exist
      // otherwise only 2 elements
      if(cnt != input_index){
        z3_reordered += elem
      } else {
        for (elem2 <- tmp_tuple._2){
          z3_reordered += elem2
        }
        z3_reordered += elem
      }
      cnt = cnt + 1
    }

    // return
    z3_reordered
  }


  /** ------------------------------------------------------------------------------------------------------------------
   * "MAIN": reading in input, calling all other functions, printing
   *
   * args: Containing params the program gets called with. In this case it is either no parameter or it is the
   *              filename of the .txt program input.
   * @return      none
   * */
  var code: String = null

  if(args.length == 1) {
    val path = args(0)
    val source = scala.io.Source.fromFile(path)
    code = try source.mkString finally source.close()
    val fileName = Paths.get(path).getFileName
    println("---------------------------------------------------------------")
    println("Running Program with: " + fileName)
    println("Using Integer Arithmetic!")
  } else {

    // MSG
    if(args.length == 0){
      println("---------------------------------------------------------------")
      println("Usage: scala -nc toHorn.scala ../FILEPATH/inputFile.txt [-bv BitVectorSize]")
      println("----------------------")
      sys.exit(-1)
    }

    if(args.length == 3){
      val path = args(0)
      val source = scala.io.Source.fromFile(path)
      code = try source.mkString finally source.close()
      val fileName = Paths.get(path).getFileName
      if(args(1) == "-bv"){
        INS_BITVECTOR = true
      } else {
        println("Usage: scala -nc toHorn.scala ../FILEPATH/inputFile.txt [-bv BitVectorSize]")
        println("----------------------")
        sys.exit(-1)
      }

      if(args(2).toInt > 0){
        BITVEC_SIZE = args(2).toInt
      } else {
        println("Bit-vector size must be greater than zero!")
        sys.exit(-1)
      }

      println("---------------------------------------------------------------")
      println("Running Program with: " + fileName)
      println("Using Bitvec Arithmetic!")
    }

    // MSG
    if(args.length > 3){
      println("----------------------")
      println("Wrong Argument Number")
      println("Usage: scala -nc toHorn.scala ../FILEPATH/inputFile.txt [-bv BitVectorSize]")
      println("----------------------")
      sys.exit(-1)
    }
  }

  // Parsing the code input
  // Start Timer for Parsing and WLP Transformation
  val t1 = System.nanoTime
  val parsed = parse(code)
  val parsed_code = try {
    parsed.get
  } catch {
    case _: Throwable =>
      println("------------------------------")
      println("Invalid Code: Could Not Parse!")
      println("------------------------------")

      print(parsed)
      sys.exit(-1)
  }

  println("---------------------------------------------------------------")
  println(code)


  println("---------------------------------------------------------------")
  println("Parsed Program:")
  println(parsed_code)
  //println("\n" + parsed_code)

  // Apply toHorn Algorithm for all declarations
  val wlp_buffer = new ListBuffer[(ListBuffer[Any], List[Variable])]
  for (elem <- parsed_code.decls){
    wlp_buffer += toHorn(elem)
  }

  println("---------------------------------------------------------------")
  println("After toHorn Transformation:")
  for (elem <- wlp_buffer){
    println(elem._1)
  }

  // Apply wlp transformation
  var s: Any = null
  var Q: ListBuffer[Any] = null
  val chc_buffer = new ListBuffer[Any]
  for (elem <- wlp_buffer){
    s = elem._1.head
    Q = elem._1.drop(1)
    chc_buffer += wlpAlgorithm(s, Q, elem._2)
  }

  println("---------------------------------------------------------------")
  println("After wlpTransformation:")
  for (elem <- chc_buffer){
    println(elem)
  }

  // "Flatten out" inner foralls before simplification
  val no_foralls = new ListBuffer[Any]
  for (elem <- chc_buffer){
    no_foralls += noForall(elem)
  }

  var buffer_simplified = new ListBuffer[Any]
  var tmp_buffer: ListBuffer[Any] = new ListBuffer[Any]
  for (elem <- no_foralls){
    buffer_simplified += simplify(elem, tmp_buffer)
  }

  println("---------------------------------------------------------------")
  println("After simplify:")
  for (elem <- buffer_simplified){
    println(elem)
  }


  tmp_buffer = new ListBuffer[Any]
  for (elem <- buffer_simplified){
    // Filter out nested lists
    if(elem.getClass.getName == "scala.collection.mutable.ListBuffer"){
      buffer_simplified = flattenListBuffer(elem.asInstanceOf[ListBuffer[Any]], tmp_buffer)
    } else {
      // If it is not in a ListBuffer i add it into one to then use the function flattenListBuffer
      // to get rid of it again + all of the others inside -> scam solution but works
      val scam_buffer = new ListBuffer[Any]
      scam_buffer += elem
      buffer_simplified = flattenListBuffer(scam_buffer, tmp_buffer)
    }
  }

  val constraint_horn_clauses = new ListBuffer[ForAll]
  for (elem <- buffer_simplified){
    val replace_list = new ListBuffer[Variable]
    constraint_horn_clauses += ForAll(VariableList(getAllVariables(elem, null, replace_list)), elem)
  }

  // Generating the Z3 Input
  var z3_strings = new ListBuffer[String]
  z3_strings = generateZ3Input(constraint_horn_clauses, z3_strings)

  println("---------------------------------------------------------------")
  println("Constraint Horn Clauses:")
  var i = 1
  for (elem <- z3_strings){
    if(elem(0) == 's' && elem(1) == '.'){
      println("CHC" + i + ":  " + elem.substring(6, elem.length()-1))
      i = i + 1
    }
  }

  // Write string to python file
  val fw = new FileWriter("z.py")

  try{
    for (elem <- z3_strings){
      fw.write(elem)
      fw.write("\n")
      //println(elem)
    }
  } finally {
    fw.close()
  }

  println("---------------------------------------------------------------")
  println("Z3 Output:")
  // End Timer for Parsing and CHC Transformation
  val duration1 = (System.nanoTime - t1) / 1e9d

  // Start Timer for Z3
  val t2 = System.nanoTime
  // execute z3
  "python3 z.py" !

  println("---------------------------------------------------------------")
  // End Timer for Z3
  val duration2 = (System.nanoTime - t2) / 1e9d
  println("Parsing and CHC-Transformation took:   " + duration1 + " Seconds")
  println("Z3 Solver took:                        " + duration2 + " Seconds")

  // Delete Python File after usage
  //new File("z.py").delete()
  println("---------------------------------------------------------------")
}