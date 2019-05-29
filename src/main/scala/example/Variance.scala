package example

object Variance extends App {

  abstract class Animal { def name: String }
  case class Cat(name: String) extends Animal
  case class Dog(name: String) extends Animal

  abstract class SmallAnimal extends Animal
  case class Mouse(name: String) extends SmallAnimal

  def eats(c: Cat, f: Cat => SmallAnimal): Unit = {
    println(s"Cat ${c.name} eats ${f(c).name}")
  }


  val f = (a: Animal) => Mouse(s"Jerry (${a.name})")

  val c = Cat("Tom")


  eats(c, f)

}
