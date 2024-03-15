import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ScalaApp._

class InterpSpec extends AnyFlatSpec with Matchers {
  val interp = new ScalaApp.Interp()
  val parser = new ScalaApp.Parser()

  "interp" should "interpret numeric expressions correctly" in {
    val numExpr = parser.parse("42")
    interp.interp(numExpr, ScalaApp.topEnv) should be (ScalaApp.NumV(42))
  }

  it should "interpret string expressions correctly" in {
    val strExpr = parser.parse("\"Hello, Scala!\"")
    interp.interp(strExpr, ScalaApp.topEnv) should be (ScalaApp.StrV("Hello, Scala!"))
  }

  it should "interpret identifier expressions correctly" in {
    val idExpr = parser.parse("x")
    val env = ScalaApp.Bind("x", ScalaApp.NumV(10)) :: ScalaApp.topEnv
    interp.interp(idExpr, env) should be (ScalaApp.NumV(10))
  }

  it should "interpret lambda expressions correctly" in {
    val lamExpr = parser.parse("lam(x)(x)")
    interp.interp(lamExpr, ScalaApp.topEnv) shouldBe a [ScalaApp.ClosV]
  }

  it should "interpret application expressions correctly" in {
    val appExpr = parser.parse("app(lam(x)(x))(42)")
    interp.interp(appExpr, ScalaApp.topEnv) should be (ScalaApp.NumV(42))
  }

  it should "interpret conditional expressions correctly" in {
    val condExpr = parser.parse("if(true)(42)(24)")
    interp.interp(condExpr, ScalaApp.topEnv) should be (ScalaApp.NumV(42))
  }
}