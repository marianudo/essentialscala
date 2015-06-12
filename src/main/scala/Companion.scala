case class Person(firstName: String, lastName: String) {
  def name = s"$firstName $lastName"
}

object Person {
  def apply(fullName: String) = {
    val tokens = fullName.split(" ")
    new Person(tokens(0), tokens(1))
  }
}
