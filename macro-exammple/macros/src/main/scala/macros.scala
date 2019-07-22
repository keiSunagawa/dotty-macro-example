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

    def matchRHS[B: Type](rhs: Expr[RHS[B, F]])(implicit env: Map[Expr[String], Expr[F[Any]]]): Expr[F[B]] = rhs match {
      case '{ Value($a: F[B]) } =>
        a
      case '{ Get($s, $f) } =>
        val a = env(s)
        matchRHS('{
          ${f}.asInstanceOf[Any => RHS[B, F]].apply($a)
        })
        ???
    }
    implicit val e: Map[Expr[String], Expr[F[Any]]] = Map.empty
    ???
  }
}

object DSL {
  // TODO enum
  sealed trait Exp[+A, F[_]]
  sealed trait Syntax[+A, F[_]] extends Exp[A, F]
  sealed trait RHS[A, F[_]] extends Exp[A, F]
  case class Bind[A, F[_]](symbol: String, f: RHS[A, F]) extends Syntax[A, F]
  abstract class Do[A, F[_]] extends Syntax[A, F] {
    type B
    val l: Bind[B, F]
    val r: Syntax[A, F]
  }
  object Do {
    def apply[A, C, F[_]](lo: Bind[C, F], ro: Bind[A, F]): Do[A, F] = new Do {
      type B = C
      val l = lo
      val r = ro
    }
    def unapply[A, F[_]](d: Do[A, F]): Option[(Bind[_, F], Syntax[A, F])] = Some(d.l -> d.r)
  }

  case class Value[A, F[_]](v: F[A]) extends RHS[A, F]
  case class Get[A, B, F[_]](symbol: String, f: B => RHS[A, F]) extends RHS[A, F]

  inline def (symbol: => String) </[A, F[_]] (f: => F[A]): Bind[A, F] = Bind(symbol, Value(f))
  inline def bind[A, F[_]](symbol: => String, f: => F[A]): Bind[A, F] = Bind(symbol, Value(f))
}

object DSL2 {
  sealed trait Exp[T]
  case class Lit[T](a: T) extends Exp[T]
  case class FMap[A, B](v: Lit[A], f: A => B) extends Exp[B]

  inline def compile[A, T](x: => Exp[T]): T = {
    ${impl[A, T]('x)}
  }
  private def impl[A: Type, T: Type](x: Expr[Exp[T]]) given QuoteContext: Expr[T] = {
    x match {
      case '{ Lit($a: T) } => a
      case '{ FMap($a: Lit[A], $f: A => T) } => impl[A, T]('{Lit(${f}(${a}.a))})
    }
  }
}
