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
    def go[B: Type](ex: Expr[Syntax[B, F]])(implicit env: Map[Expr[String], Expr[F[Any]]]): Expr[F[B]] = {
      ex match {
        case '{ (Bind($a, $f: RHS[B, F]): Bind[B, F]) } =>
          println("== bind match ==")
          matchRHS(f)
        case '{ Bind($a, $f: Value[B, F]) } =>
          println("== bind match 2==")
          matchRHS(f)
        case '{ Do($a: Bind[_, F], $b: Bind[_, F])} =>
          println("== do match ==")
          go[B](b.asInstanceOf[Expr[Syntax[B, F]]])
        case _ =>
          println("== unmatch ==")
//          println(ex.show)
          ???
      }
    }
    implicit val e: Map[Expr[String], Expr[F[Any]]] = Map.empty
    go(xs)
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
