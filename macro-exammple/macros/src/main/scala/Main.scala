import scala.quoted._
import scala.quoted.matching._

object SafeDiv{
  inline def safeDiv(n: => Int, x: => Int): Int = {
    ${safeDivImpl('{n}, '{x})}
  }

  def safeDivImpl(n: Expr[Int], x: Expr[Int]) given (qctx: QuoteContext): Expr[Int] = {
    import qctx.tasty._
    val xTree: Term = x.unseal
//    println(xTree)
    xTree match {
      case Inlined(a, b, Literal(Constant(x2: Int))) if x2 != 0 =>
        '{${n} / ${x} }
      case _ =>
        throw IllegalArgumentException("second argument must non zero literal.")
    }
  }
}

// object CD {
//   inline def hogedef(c: => Any): Unit = {
//     ${~{hogedefImpl('c)}}
//   }
//   def hogedefImpl(c: Expr[Any]) given (qctx: QuoteContext): Expr[Any] = {
//     import qctx.tasty._
//     val cTree: Term = c.unseal
//     println(cTree)
//     cTree match {
//       case Inlined(a, _, Block((x: TypeDef) :: Nil, c)) =>
//         println(x)
//         Inlined(a, List(x), c).seal
//     }
//   }
// }

// object DoSyntax {
//   inline def dos(f: => Any): Any = {
//     ${dosImpl('f)}
//   }
//   def dosImpl(f: Expr[Any]) given (qctx: QuoteContext): Expr[Any] = {
//     import qctx.tasty._
//     val xTree: Term = f.unseal
//     println(xTree)
//     '{3}
//   }
// }

// object DoSyntax {
//   import DSL._

//   def dos[A, F[_]](xs: Syntax[A, F]) given (qx: QuoteContext, fa: Liftable[F[A]]): F[A] = {
//     implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
//     def go(exp: Exp[A, F], env: Map[String, Any]): Expr[F[A]] = exp match {
//       case Bind(_, f) => go(f, env)
//       case Value(a) => fa.toExpr(a)
//     }
//     go(xs, Map.empty).run
//   }
// }
object Imp {
  import scala.quoted.autolift._
  object Lists {
    implicit def ListIsLiftable[T: Liftable](implicit t: Type[T]): Liftable[List[T]] = new Liftable[List[T]] {
      def toExpr(x: List[T]) given QuoteContext: Expr[List[T]] = x match {
        case x :: xs  => '{ (${xs}).::[$t](${x}) }
        case Nil => '{ Nil: List[$t] }
      }
    }
  }
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
          println(a)
          // lf.toExpr(a)
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

object Macros {

  inline def liftString(a: => DSL): String = ${impl(StringNum, 'a)}

  inline def liftCompute(a: => DSL): Int = ${impl(ComputeNum, 'a)}

  inline def liftAST(a: => DSL): ASTNum = ${impl(ASTNum, 'a)}

  private def impl[T: Type](sym: Symantics[T], a: Expr[DSL]) given (qctx: QuoteContext): Expr[T] = {

    def lift(e: Expr[DSL])(implicit env: Map[Bind[DSL], Expr[T]]): Expr[T] = e match {

      case '{ LitDSL(${Const(c)}) } => sym.value(c)

      case '{ ($x: DSL) + ($y: DSL) } => sym.plus(lift(x), lift(y))

      case '{ ($x: DSL) * ($y: DSL) } => sym.times(lift(x), lift(y))

      case '{ ($f: DSL => DSL)($x: DSL) } => sym.app(liftFun(f), lift(x))

      case '{ val $x: DSL = $value; $body: DSL } => lift(body)(env + (x -> lift(value)))

      case Bind(b) if env.contains(b) => env(b)

      case _ =>
        import qctx.tasty._
        error("Expected explicit DSL", e.unseal.pos)
        ???
    }

    def liftFun(e: Expr[DSL => DSL])(implicit env: Map[Bind[DSL], Expr[T]]): Expr[T => T] = e match {
      case '{ ($x: DSL) => ($body: DSL) } =>
        sym.lam((y: Expr[T]) => lift(body)(env + (x -> y)))

      case _ =>
        import qctx.tasty._
        error("Expected explicit DSL => DSL", e.unseal.pos)
        ???
    }

    lift(a)(Map.empty)
  }

}

//
// DSL in which the user write the code
//

trait DSL {
  def + (x: DSL): DSL = ???
  def * (x: DSL): DSL = ???
}
case class LitDSL(x: Int) extends DSL

//
// Interpretation of the DSL
//

trait Symantics[Num] {
  def value(x: Int) given QuoteContext: Expr[Num]
  def plus(x: Expr[Num], y: Expr[Num]) given QuoteContext: Expr[Num]
  def times(x: Expr[Num], y: Expr[Num]) given QuoteContext: Expr[Num]
  def app(f: Expr[Num => Num], x: Expr[Num]) given QuoteContext: Expr[Num]
  def lam(body: Expr[Num] => Expr[Num]) given QuoteContext: Expr[Num => Num]
}

object StringNum extends Symantics[String] {
  def value(x: Int) given QuoteContext: Expr[String] = x.toString.toExpr
  def plus(x: Expr[String], y: Expr[String]) given QuoteContext: Expr[String] = '{ s"${$x} + ${$y}" } // '{ x + " + " + y }
  def times(x: Expr[String], y: Expr[String]) given QuoteContext: Expr[String] = '{ s"${$x} * ${$y}" }
  def app(f: Expr[String => String], x: Expr[String]) given QuoteContext: Expr[String] = f(x) // functions are beta reduced
  def lam(body: Expr[String] => Expr[String]) given QuoteContext: Expr[String => String] = '{ (x: String) => ${body('x)} }
}

object ComputeNum extends Symantics[Int] {
  def value(x: Int) given QuoteContext: Expr[Int] = x.toExpr
  def plus(x: Expr[Int], y: Expr[Int]) given QuoteContext: Expr[Int] = '{ $x + $y }
  def times(x: Expr[Int], y: Expr[Int]) given QuoteContext: Expr[Int] = '{ $x * $y }
  def app(f: Expr[Int => Int], x: Expr[Int]) given QuoteContext: Expr[Int] = '{ $f($x) }
  def lam(body: Expr[Int] => Expr[Int]) given QuoteContext: Expr[Int => Int] = '{ (x: Int) => ${body('x)} }
}

object ASTNum extends Symantics[ASTNum] {
  def value(x: Int) given QuoteContext: Expr[ASTNum] = '{ LitAST(${x.toExpr}) }
  def plus(x: Expr[ASTNum], y: Expr[ASTNum]) given QuoteContext: Expr[ASTNum] = '{ PlusAST($x, $y) }
  def times(x: Expr[ASTNum], y: Expr[ASTNum]) given QuoteContext: Expr[ASTNum] = '{ TimesAST($x, $y) }
  def app(f: Expr[ASTNum => ASTNum], x: Expr[ASTNum]) given QuoteContext: Expr[ASTNum] = '{ AppAST($f, $x) }
  def lam(body: Expr[ASTNum] => Expr[ASTNum]) given QuoteContext: Expr[ASTNum => ASTNum] = '{ (x: ASTNum) => ${body('x)} }
}

trait ASTNum
case class LitAST(x: Int) extends ASTNum
case class PlusAST(x: ASTNum, y: ASTNum) extends ASTNum
case class TimesAST(x: ASTNum, y: ASTNum) extends ASTNum
case class AppAST(x: ASTNum => ASTNum, y: ASTNum) extends ASTNum {
  override def toString: String = s"AppAST(<lambda>, $y)"
}
