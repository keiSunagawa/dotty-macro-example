import scala.quoted._
import scala.quoted.matching._

object Lif {
  import scala.quoted.autolift._

  implicit def OptionLiftable[T: Liftable](implicit t: Type[T]): Liftable[Option[T]] = new Liftable[Option[T]] {
    def toExpr(xs: Option[T]) given QuoteContext: Expr[Option[T]] = xs match {
      case Some(a) => '{ Some.apply[$t](${a}) }
      case None => '{ None: Option[$t] }
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
    xs match {
      case '{ Bind($a, $f: RHS[A, F]) } => f match {
        case '{ Value($a: F[A]) } =>
          a
        case _ =>
          println("===unmatch===")
          println(f)
          ???
      }
      case '{ $a } =>
        println(a)
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
  case class Get[A, B, F[_]](symbol: String, f: A => RHS[B, F]) extends RHS[B, F]
}
