import org.scalatest.funsuite.AnyFunSuite

class InterpTest extends AnyFunSuite {
  val interp = new ScalaApp.Interp()

  test("interp handles NumC correctly") {
    val expr = ScalaApp.NumC(42)
    assert(interp.interp(expr, ScalaApp.topEnv) == ScalaApp.NumV(42))
  }

  test("interp handles StrC correctly") {
    val expr = ScalaApp.StrC("Hello, Scala!")
    assert(interp.interp(expr, ScalaApp.topEnv) == ScalaApp.StringV("Hello, Scala!"))
  }

  test("interp handles IdC correctly") {
    val expr = ScalaApp.IdC("true")
    assert(interp.interp(expr, ScalaApp.topEnv) == ScalaApp.TrueV)
  }

  test("interp handles LamC correctly") {
    val expr = ScalaApp.LamC(List("x"), ScalaApp.IdC("x"))
    val result = interp.interp(expr, ScalaApp.topEnv)
    assert(result.isInstanceOf[ScalaApp.ClosV])
    val clos = result.asInstanceOf[ScalaApp.ClosV]
    assert(clos.args == List("x"))
    assert(clos.body == ScalaApp.IdC("x"))
  }

  test("interp handles AppC correctly") {
    val expr = ScalaApp.AppC(ScalaApp.LamC(List("x"), ScalaApp.IdC("x")), List(ScalaApp.NumC(42)))
    val result = interp.interp(expr, ScalaApp.topEnv)
    assert(result == ScalaApp.NumV(42))
  }

  test("interp handles CondC correctly") {
    val expr = ScalaApp.CondC(ScalaApp.IdC("true"), ScalaApp.NumC(42), ScalaApp.NumC(24))
    assert(interp.interp(expr, ScalaApp.topEnv) == ScalaApp.NumV(42))
  }
}