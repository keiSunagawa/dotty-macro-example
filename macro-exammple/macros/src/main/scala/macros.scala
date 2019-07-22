import scala.quoted._
import scala.quoted.matching._

object Lif {
  import scala.quoted.autolift._

  given OptionLiftable[T: Liftable] as Liftable[Option[T]] given (t: Type[T]) {
    def toExpr(xs: Option[T]) given QuoteContext: Expr[Option[T]] = {
      xs match {
      case Some(a) => '{ Some.apply[$t](${a}) }
      case None => '{ None: Option[$t] }
      }
    }
  }
}

object DoSyntax {
  import DSL._
  inline def dos[A, F[_]](xs: => Syntax[A, F]) given Liftable[F[A]]: F[A] = {
    ${impl('xs)}
  }
  private def impl[A: Type, F[_]: Type](xs: Expr[Syntax[A, F]]) given (
    qctx: QuoteContext,
    lf: Liftable[F[A]]
  ): Expr[F[A]] = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    var env = Map.empty[String, Any]

    println(xs.show)

    def matchRHS(rhs: Expr[RHS[A, F]]): Expr[F[A]] = rhs match {
      case '{ Value($a: F[A]) } =>
        a
      case '{ Get($s, $f) } =>
        // matchRHS('{
        // val a = env($s)
        // ${f}.asInstanceOf[Any => RHS[A, F]].apply(a)
        // })
        ???
    }
    xs match {
      case '{ (Bind($a, $f: RHS[A, F]): Bind[A, F]) } => f match {
        case '{ Value($a: F[A]) } =>
          a
        case _ =>
          println("===unmatch===")
          println(f)
          ???
      }
      case '{ Bind($a, $f: Value[A, F]) } => f match {
        case '{ Value($a: F[A]) } =>
          a
        case _ =>
          println("===unmatch===")
          println(f)
          ???
      }
      case '{ $a } =>
        ???
    }
  }
}

object DSL {
  // TODO enum
  sealed trait Exp[A, F[_]]
  sealed trait Syntax[A, F[_]] extends Exp[A, F]
  sealed trait RHS[A, F[_]] extends Exp[A, F]
  case class Bind[A, F[_]](symbol: String, f: RHS[A, F]) extends Syntax[A, F]
  case class Do[A, B, F[_]](l: Bind[A, F], r: Syntax[B, F]) extends Syntax[B, F]

  case class Value[A, F[_]](v: F[A]) extends RHS[A, F]
  case class Get[A, B, F[_]](symbol: String, f: B => RHS[A, F]) extends RHS[A, F]

  inline def (symbol: => String) </[A, F[_]] (f: => F[A]): Bind[A, F] = Bind(symbol, Value(f))
  inline def bind[A, F[_]](symbol: => String, f: => F[A]): Bind[A, F] = Bind(symbol, Value(f))
}
