object Main extends App {
  import DSL._
  import delegate Lif._

  val d = DoSyntax.dos{
     "a" </ Option(2)
  }
  println(d)

  def hoge(): Option[Int] = None
}
