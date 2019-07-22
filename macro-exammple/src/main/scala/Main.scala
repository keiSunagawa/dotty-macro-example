import scala.quoted._
import scala.quoted.matching._

object Main {
  import SafeDiv._
  import DSL._
  import delegate Imp._

  def main(args: Array[String]): Unit = {
    val x = 3
    val z = safeDiv(10, 5)
    println(z)
    // DoSyntax.dos{
    //   "a" </ Some(3)
    //   "b" </ Some("a".get[Int] + 3)
    //   None
    // }
    // DoSyntax.dos{
    //   Do(
    //     Bind[Int, Option]("a", Value(Some(3))),
    //     Bind[Int, Option]("b", Get[Int, Int, Option]("a", a => Value(Some(a + 4))))
    //   )
    // }
    val d = DoSyntax.dos{
        Bind[Int, Option]("a", Value(Some(3)))
    }
    println(d)
    // import Macros._
    // println(liftCompute(LitDSL(1) + LitDSL(2)))
  }
}

// object DSL {
//   def (symbol: String) </ (f: => Any): Nothing = ???
//   def (symbol: String) get[A]: A = ???
// }

// object DSL {
//   opaque type Sym[A] = A
//   object Sym {
//     def apply[A](s: String): Sym[A] = null.asInstanceOf[A]
//   }
//   def (s: Sym[A]) </[A] (f: => Any): Any = ???
//   def (s: Sym[A]) get[A]: A = null.asInstanceOf[A]
// }
