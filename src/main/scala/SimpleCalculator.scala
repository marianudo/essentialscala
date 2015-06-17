package com.codinginflipflops.essentialscala

object SimpleCalculator {
  def calculator(operand1: String, operator: String, operand2: String): Unit = {
    val result = func(strAsInt(operand1), strAsInt(operand2), operator)
    println(result.getOrElse("Something bad happened"))
  }

  def strAsInt(str: String): Option[Int] =
    if(str matches "\\d+") Some(str.toInt)
    else None

  def func(v1: Option[Int], v2: Option[Int], op: String): Option[Int] = op match {
    case "+" => opOnOptions(v1, v2)(_+_)
    case "-" => opOnOptions(v1, v2)(_-_)
    case "*" => opOnOptions(v1, v2)(_*_)
    case "/" =>
      if(v2.isDefined && v2.get == 0) None
      else opOnOptions(v1, v2)(_/_)
    case _ => None
  }

  def opOnOptions(op1: Option[Int], op2: Option[Int])(f: (Int, Int) => Int): Option[Int] = {
    for {
      v1 <- op1
      v2 <- op2
    } yield f(v1, v2)
  }
}
