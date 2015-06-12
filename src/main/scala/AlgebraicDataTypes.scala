package essentialscala.algebraic

/**
 * Exercise 6.4.4.1
 */
sealed trait TrafficLight

final case object Red extends TrafficLight
final case object Green extends TrafficLight
final case object Yellow extends TrafficLight

/**
 * Exercise 6.4.4.2
 */
sealed trait CalculationOutcome

final case class Succeess(result: Int) extends CalculationOutcome
final case class Failure(reason: String) extends CalculationOutcome

/**
 * Exercise 6.4.4.3
 */
final case class BottledWater(size: Int, source: WaterSource, carbonated: Boolean)

sealed trait WaterSource

final case object Well extends WaterSource
final case object Spring extends WaterSource
final case object Tap extends WaterSource
