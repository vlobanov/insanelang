import fastparse.all._

abstract class Token;
case class Symbol(value: String) extends Token

class SimpleValue extends Token;
case class IntValue(value: Integer) extends SimpleValue
case class DoubleValue(value: Double) extends SimpleValue

case class ListExpression(values: Seq[Token]) extends Token

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
  def apply(t: T) = f(t)
  override def toString() = name
}

object Reader {
  def readInt(value: String): IntValue = {
    return IntValue(value.toInt)
  }

  def readDouble(value: String): DoubleValue = {
    return DoubleValue(value.toDouble)
  }

  def readSymbol(value: String): Symbol = {
    return Symbol(value)
  }

  def readListExpression = ListExpression
}

object PredefOperations {
   def add(args: Seq[SimpleValue]): SimpleValue = {
     def two(v1: SimpleValue, v2: SimpleValue): SimpleValue = (v1, v2) match {
       case (IntValue(i1), IntValue(i2)) => IntValue(i1 + i2)
       case (DoubleValue(i1), DoubleValue(i2)) => DoubleValue(i1 + i2)
       case (IntValue(i1), DoubleValue(i2)) => DoubleValue(i1 + i2)
       case (DoubleValue(i1), IntValue(i2)) => DoubleValue(i1 + i2)
     }

     args.reduce(two)
   }

   def substract(args: Seq[SimpleValue]): SimpleValue = {
     def two(v1: SimpleValue, v2: SimpleValue): SimpleValue = (v1, v2) match {
       case (IntValue(i1), IntValue(i2)) => IntValue(i1 - i2)
       case (DoubleValue(i1), DoubleValue(i2)) => DoubleValue(i1 - i2)
       case (IntValue(i1), DoubleValue(i2)) => DoubleValue(i1 - i2)
       case (DoubleValue(i1), IntValue(i2)) => DoubleValue(i1 - i2)
     }

     args.reduce(two)
   }
}

object OpsRegistry {
  type Operation = Seq[SimpleValue] => SimpleValue
  val registry = scala.collection.mutable.Map[String, Operation](
    "+" -> PredefOperations.add,
    "-" -> PredefOperations.substract
  )

  def lookup(name: String): Operation = {
    registry.get(name) match {
      case Some(v: Operation) => v
      case None => throw new Exception(s"Symbol $name not found")
    }
  }

}

object Evaluator {
  def add(v1: Token, v2: Token): SimpleValue = (v1, v2) match {
    case (IntValue(i1), IntValue(i2)) => IntValue(i1 + i2)
    case (DoubleValue(i1), DoubleValue(i2)) => DoubleValue(i1 + i2)
    case (IntValue(i1), DoubleValue(i2)) => DoubleValue(i1 + i2)
    case (DoubleValue(i1), IntValue(i2)) => DoubleValue(i1 + i2)
  }

  def evalCall(callee: Token, args: Seq[SimpleValue]): SimpleValue = {
    val Symbol(n) = callee
    OpsRegistry.lookup(n)(args)
  }

  def lookupSymbol(name: String): SimpleValue = {
    return DoubleValue(42)
  }

  def evalArgs(args: Seq[Token]): Seq[SimpleValue] = {
    args.map(eval _)
  }

  def eval(t: Token): SimpleValue = t match {
    case t: SimpleValue => t
    case Symbol(name) => lookupSymbol(name)
    case ListExpression(callee +: args) => evalCall(callee, evalArgs(args))
  }
}

object Parser {
  val R = Reader
  val numeric = P(CharIn('0'.to('9')).rep(1))
  val sign = CharIn("+-").?
  val intParser = P(sign ~ numeric).!.map(R.readInt _)
  val doubleParser = P(sign ~ numeric ~ "." ~ numeric).!.map(R.readDouble _)
  val alphabetic = P(CharIn('a'.to('z')) | CharIn('A'.to('Z')) | CharIn("+-_*/"))
  val symbolParser = P(alphabetic ~ (alphabetic | numeric).rep(0)).!.map(R.readSymbol)
  val whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val space = P(CharsWhile(whitespace).?)
  val expressionParser = P(doubleParser | intParser | symbolParser | listExpression)
  val expressionsSeq = (space ~ expressionParser ~ space).rep(1)
  val listExpression: P[ListExpression] = P("(" ~ expressionsSeq ~ ")").map(R.readListExpression)

  def main(args: Array[String]) = {
    val Parsed.Success(ts, _) = listExpression.parse("(+ 1 2 -0.5 (- 0 11 12))")
    println(ts)
    println(Evaluator.eval(ts))
  }
}
