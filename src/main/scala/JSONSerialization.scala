package com.codinginflipflops.essentialscala

/*
 * Algebraic data type to perform JSON Scala objects representation conversion to String
 */
sealed trait JsValue {
  def stringify: String
}

final case class JsObject(values: Map[String, JsValue]) extends JsValue {
  def stringify =
    values.map
      { case (k, v) => "\"" + k + "\":\"" + v.stringify }
      .mkString("{", ",", "}")
}

final case class JsString(str: String) extends JsValue {
  def stringify = "\"" + str.replaceAll("\\|\"", "\\\\$1") + "\""
}

/*
 * Algebraic Data type hierarchy to test Json conversion
 */
import java.util.Date

sealed trait Visitor {
  def id: String
  def createdAt: Date
  def age: Long = new Date().getTime() - createdAt.getTime()
}

final case class Anonymous(id: String, createdAt: Date = new Date()) extends Visitor

final case class User(id: String, email: String, createdAt: Date = new Date()) extends Visitor

/*
 * Stuff to write Scala objects to JSON
 */
trait JsWriter[A] {
  def write(a: A): JsValue
}

object JsUtil {
  def toJson[A](a: A)(implicit writer: JsWriter[A]): JsValue =
    writer write a

  /*
   * JsWriter algebraic data type instances for Anonymous and User
   */
  implicit object AnonymousJsWriter extends JsWriter[Anonymous] {
    def write(an: Anonymous) =
      new JsObject(
        Map(
          "id" -> JsString(an.id),
          "createdAt" -> JsString(an.createdAt.toString)
        )
      )
  }

  implicit object UserJsWriter extends JsWriter[User] {
    def write(us: User) =
      new JsObject(
        Map(
          "id" -> JsString(us.id),
          "email" -> JsString(us.email),
          "createdAt" -> JsString(us.createdAt.toString)
        )
      )
  }

  implicit object VisitorJsWriter extends JsWriter[Visitor] {
    def write(v: Visitor) = v match {
      case an: Anonymous => JsUtil.toJson(an)
      case us: User => JsUtil.toJson(us)
    }
  }

  implicit object StringJsWriter extends JsWriter[String] {
    def write(str: String) =
     JsString(str)
  }

  implicit object DateJsWriter extends JsWriter[Date] {
    def write(date: Date) =
      JsString(date.toString)
  }

  //Type enrichment for Visitor (toJson capabilities) and other classes
  implicit class JsCapableVisitor[A](a: A) {
    def toJson(implicit writer: JsWriter[A]) =
      writer.write(a).stringify
  }
}
