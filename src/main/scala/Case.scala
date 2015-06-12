trait Feline {
  def colour: String
  def sound = "roar"
}

case class Cat(
    colour: String,
    food: String
  ) extends Feline {

    override def sound = "meow"
}

case class Tiger(colour: String) extends Feline

case class Lion(colour: String, maneSize: Int) extends Feline

case class Panther(colour: String) extends Feline

object ChipShop {
  def willServe(cat: Cat) = cat match {
    case Cat(_, "chips") => true
    case _ => false
  }
}

sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
  def color: Option[Color]
}

sealed trait Rectangular extends Shape {
  def width: Int
  def height: Int
  val sides = 4
  def perimeter = width * 2 + height * 2
  def area = width * height
}

final case class Rectangle(width: Int, height: Int, color: Option[Color] = None) extends Rectangular

final case class Square(side: Int, color: Option[Color] = None) extends Rectangular {
  val width = side
  val height = side
}

final case class Circle(radius: Double, color: Option[Color] = None) extends Shape {
  val sides = 0
  def perimeter = 2 * math.Pi * radius
  def area = math.Pi * radius * radius
}

object Draw {
  def apply(s: Shape): String = s match {
    case Circle(r, Some(Yellow)) => s"A yellow circle of radius $r"
    case Circle(r, Some(Red)) => s"A red circle of radius $r"
    case Circle(r, Some(Pink)) => s"A pink circle of radius $r"
    case Circle(r, _ ) => s"A circle of radius $r"
    case Rectangle(w, h, _) => s"A rectangle of width $w and height $h"
    case Square(s, _) => s"A square of side $s"
  }
}

sealed trait Color {
  def red: Int
  def green: Int
  def blue: Int
  def Option[Intensity] = None
}

sealed trait Intensity

final case object Light extends Intensity

final case object Dark extends Intensity

final case class CustomColor(red: Int, green: Int, blue: Int) extends Color

final case object Red extends Color {
  val red = 255
  val green = 0
  val blue = 0
}

final case object Yellow extends Color {
  val red = 255
  val green = 255
  val blue = 0
}

final case object Pink extends Color {
  val red = 255
  val green = 0
  val blue = 255
}

final case object Color {
  def apply (r: Int, g: Int, b: Int) = new Color {
    val red = r
    val green = g
    val blue = b
  }
}

/**
 * Here the division by zero question
 */
sealed trait DivisionResult

final case class Finite(value: Int) extends DivisionResult

final case object Infinite extends DivisionResult

object Divide {
  def apply(d1: Int, d2: Int): DivisionResult =
    if(d2 == 0) Infinite
    else Finite(d1 / d2)

  def descDiv(d1: Int, d2: Int): String = Divide(d1, d2) match {
    case Infinite => "Infinite value"
    case Finite(v) => s"Finite result of $v"
  }
}
