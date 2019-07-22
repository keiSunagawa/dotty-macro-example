object Main extends App {
  import DSL._
  import delegate Lif._

  val d = DoSyntax.dos{
    Bind[Int, Option]("a", Value(Some(3)))
  }
  println(d)
}
