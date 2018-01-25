package matcher

sealed trait Currency
case object USD extends Currency

case object A extends Currency
case object B extends Currency
case object C extends Currency
case object D extends Currency

object Currency{
  def apply(s: String): Currency = {
    s match {
      case "A" => A
      case "B" => B
      case "C" => C
      case "D" => D
      case "USD" => USD
    }
  }
}

case class Money[T <: Currency](value: Double, currency: T) {
  def unary_- : Money[T] =
    Money(-value, currency)

  def *(i: Double) = Money(value * i, currency)

  def +(other: Money[T]): Money[T] =
    Money(value + other.value, currency)

  def -(other: Money[T]): Money[T] =
    Money(value - other.value, currency)

  def >=(other: Money[T]): Boolean =
    value >= other.value

  def <=(other: Money[T]): Boolean =
    value <= other.value
}
