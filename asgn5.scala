// Define the ScalaApp object as a container for all components.
object ScalaApp {

  // Define an environment type as a list of bindings.
  type Env = List[Bind]

  // Define a case class for bindings in the environment.
  case class Bind(name: String, value: Value)

  // Define a sealed trait for possible values in our language.
  sealed trait Value
  case class NumV(n: Double) extends Value // Numeric value
  case class ClosV(args: List[String], body: ExprC, env: Env) extends Value // Closure value for functions
  case class PrimV(op: String) extends Value // Primitive operation
  case object TrueV extends Value // True value
  case object FalseV extends Value // False value

  // Define a sealed trait for expressions in our language.
  sealed trait ExprC
  case class NumC(n: Double) extends ExprC // Numeric expression
  case class IdC(s: String) extends ExprC // Identifier expression
  case class LamC(args: List[String], body: ExprC) extends ExprC // Lambda expression
  case class AppC(fun: ExprC, args: List[ExprC]) extends ExprC // Application expression
  case class StrC(s: String) extends ExprC // String expression
  case class CondC(i: ExprC, t: ExprC, e: ExprC) extends ExprC // Conditional expression

  // Define the top environment with predefined bindings.
  val topEnv: Env = List(
    Bind("true", TrueV),
    Bind("false", FalseV),
    Bind("+", PrimV("+")),
    Bind("-", PrimV("-")),
    Bind("*", PrimV("*")),
    Bind("/", PrimV("/"))
  )

  // Define primitive operations as methods.
  def primOpPlus(l: Value, r: Value): Value = (l, r) match {
    case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
    case _ => throw new Exception("Argument is not a number")
  }

  def primOpSubtract(l: Value, r: Value): Value = (l, r) match {
    case (NumV(n1), NumV(n2)) => NumV(n1 - n2)
    case _ => throw new Exception("Argument is not a number")
  }

  def primOpMultiply(l: Value, r: Value): Value = (l, r) match {
    case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
    case _ => throw new Exception("Argument is not a number")
  }

  def primOpDivide(l: Value, r: Value): Value = (l, r) match {
    case (_, NumV(0)) => throw new Exception("Divide by zero")
    case (NumV(n1), NumV(n2)) => NumV(n1 / n2)
    case _ => throw new Exception("Argument is not a number")
  }

  // Define comparison operations as methods.
  def lessThan(l: Value, r: Value): Value = (l, r) match {
    case (NumV(n1), NumV(n2)) => if (n1 < n2) TrueV else FalseV
    case _ => throw new Exception("Argument is not a number")
  }

  def lessThanOrEqual(l: Value, r: Value): Value = (l, r) match {
    case (NumV(n1), NumV(n2)) => if (n1 <= n2) TrueV else FalseV
    case _ => throw new Exception("Argument is not a number")
  }

  def equal(l: Value, r: Value): Value = (l, r) match {
    case (NumV(n1), NumV(n2)) => if (n1 == n2) TrueV else FalseV
    case _ => throw new Exception("Unsupported operation for non-numbers")
  }

  class Parser {
  def parse(expr: String): ExprC = {
    // Remove leading and trailing whitespaces and outer parentheses
    def removeOuterParentheses(s: String): String = s.trim match {
      case s if s.startsWith("(") && s.endsWith(")") => s.substring(1, s.length - 1).trim
      case _ => s
    }

    // Helper function to split the input into manageable parts
    def splitFirstLevel(s: String): List[String] = {
      var depth = 0
      var token = new StringBuilder
      val parts = scala.collection.mutable.ListBuffer.empty[String]

      s.foreach {
        case '(' => 
          depth += 1; token.append('(')
        case ')' => 
          depth -= 1; token.append(')')
          if (depth == 0) {
            parts += token.toString(); token.clear()
          }
        case c if depth > 0 => token.append(c)
        case c if c.isWhitespace && token.nonEmpty && depth == 0 => 
          parts += token.toString(); token.clear()
        case c if !c.isWhitespace => token.append(c)
        case _ =>
      }

      if (token.nonEmpty) parts += token.toString()
      parts.toList
    }

    // Main parse logic
    removeOuterParentheses(expr) match {
      case num if num.matches("-?\\d+(\\.\\d+)?") => NumC(num.toDouble)
      case str if str.startsWith("\"") && str.endsWith("\"") => StrC(str.substring(1, str.length - 1))
      case id if id.matches("[a-zA-Z_][a-zA-Z0-9_]*") => IdC(id)
      case lam if lam.startsWith("lam(") =>
        val bodyIndex = lam.indexOf(")(")
        val args = lam.substring(4, bodyIndex).split("\\s+").toList
        val body = parse(lam.substring(bodyIndex + 2, lam.length - 1))
        LamC(args, body)
      case app if app.startsWith("app(") =>
        val parts = splitFirstLevel(app.substring(4, app.length - 1))
        if (parts.size != 2) throw new IllegalArgumentException("app expression must have exactly 2 parts")
        AppC(parse(parts.head), splitFirstLevel(parts(1)).map(parse))
      case cond if cond.startsWith("if(") =>
        val parts = splitFirstLevel(cond.substring(3))
        if (parts.size != 3) throw new IllegalArgumentException("if expression must have exactly 3 parts")
        CondC(parse(parts(0)), parse(parts(1)), parse(parts(2)))
      case _ => throw new IllegalArgumentException("Unrecognized expression format")
    }
  }
  }
}

// Main object demonstrating usage of the Parser
object Main extends App {
  val parser = new ScalaApp.Parser()

  // Numeric expression
  val numExpr = "42"
  println(s"NumC: ${parser.parse(numExpr)}")

  // String expression
  val strExpr = "\"Hello, Scala!\""
  println(s"StrC: ${parser.parse(strExpr)}")

  // Identifier expression
  val idExpr = "x"
  println(s"IdC: ${parser.parse(idExpr)}")

  // Lambda expression
  val lamExpr = "lam(x y)(x)"
  println(s"LamC: ${parser.parse(lamExpr)}")

//These currently do not work
  // Application expression
  //val appExpr = "app(func)(x y)"
 // println(s"AppC: ${parser.parse(appExpr)}")

  // Conditional expression
  //val condExpr = "if(x)(y)(z)"
  //println(s"CondC: ${parser.parse(condExpr)}")
}



