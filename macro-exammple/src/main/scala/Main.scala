object Main extends App {
  import DSL._
  import delegate Lif._

//   val d = DoSyntax.dos{
//     // Do("a" </ Option(2), "b" </ Option(3))
//     Do[Int, Int, Option](
//       Bind("a", Value(Option(2))),
//       Bind("b", Value(Option(3)))
//     )
//   }
// //    val d = DoSyntax.dos{ "a" </ Option(2) }

  //   println(d)
  import scala.quoted._
  import MacroHelper._
  import DSL2._

  val res = 1 map { a => s"${a} is number"}

  println(res)

  def hoge(): Option[Int] = None
}
