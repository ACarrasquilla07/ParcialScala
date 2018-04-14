//Hacer metodo que sume texto o numeros
/*
def sum : Int => Int => Int = a => b => a+b
def sum(a: String, b: String): String = a+b

sum("Hola", "a")
*/



//Para enviarle un parametro global con Implicit
/*
def sumaI(a: Int)(implicit b: Int): Int = a+b

implicit  val s: Int = 99

sumaI(1)
*/



//
/*
def sumToS(a: String, b: String): String = a+b
implicit val toS: Int => String = _.toString

sumToS("H", 2)
*/



//Vea el uso de los IMPLICIT
/*
def sum(a: Int)(implicit b: Int): Int = a+b
def sum(a: String)( b: String): String = a+b

implicit val i: Int = "88"
implicit def toS: Int => String = _.toString
implicit def toI: String => Int = java.lang.Integer.valueOf(_)

//Los implicit buscan un entero y con el toI lo convierte
//a entero y lo suma
sum(2)
// Convierte el 2 en String por medio del def toS
sum("H")(2)
*/



//Persona con nombre y edad
//Y sumar la edad
/*
case class Persona(nombre: String, edad: Int)

def sum(a: Int)(implicit b: Int): Int = a+b

//Este implicit si llega una persona da la edad
//Si esperaba un Int y le llego una persona use el siguiente
implicit def fPP: Persona => Int = _.edad
//Esta es una persona definida goblamente
implicit def i: Int = Persona("Andres", 22)

sum(2)
*/




//Agregar funciones donde no existian
/*
//funcion que coja dos string si el
// primero es mayor que el segundo true
// Lo que hice yo
def mayorS(a: String)(b: String): Boolean ={
  a.size >= b.size
}
mayorS("Hola")("AndresJulian")


implicit class StringOps(s: String){
  def >==(s2: String): Boolean =
    s.length >= s2.length
}

"Hola" >== "AndresJulian"
"Hola".>==("AndresJulian")
*/




//Caracteristicas de cosas que se pueden sumar
//Hacer lo siguiente implementado
//def sum[T](a: T, b: T): T = ???
//Se utilizan interfaces
/*
trait Sumable[T]{
  def sumar(a: T, b: T): T
  def zero: T
}

object SumableOps {
  implicit object IntSumable extends Sumable[Int] {
    def sumar(a: Int, b: Int): Int = a+b
    def zero: Int = 0
  }
  implicit object StringSumable extends Sumable[String] {
    def sumar(a: String, b: String): String = a+b
    def zero: String = ""
  }
}

import SumableOps._
def sum[T](a: T, b: T)(implicit s: Sumable[T]): T =
  s.sumar(a,b)

sum(1,2)
sum("1","2")
*/





//Sumar dos personas y devolver una persona
// con la suma de los patrimonios
/*
case class Persona(patrimonio: Double)

trait Sumable[T]{
  def sumar(a: T, b: T): T
  def zero: T
}

object SumableOps {
  implicit object IntSumable extends Sumable[Int] {
    def sumar(a: Int, b: Int): Int = a+b
    def zero: Int = 0
  }
  implicit object DoubleSumable extends Sumable[Double] {
    def sumar(a: Double, b: Double): Double = a+b
    def zero: Double = 0
  }
  implicit object StringSumable extends Sumable[String] {
    def sumar(a: String, b: String): String = a+b
    def zero: String = ""
  }
  //Este es el cambio para sumar personas
  implicit object PersonaSumable extends Sumable[Persona] {
    def sumar(a: Persona, b: Persona): Persona = {
      Persona(a.patrimonio + b.patrimonio)
    }
    def zero: Persona = Persona(0)
  }
}


import SumableOps._
def sum[T](a: T, b: T)(implicit s: Sumable[T]): T =
  s.sumar(a,b)

sum(1,2)
sum("1","2")
sum(1.0,2.1)

val p1: Persona = Persona(9.0)
val p2: Persona = Persona(1.0)

sum(p1, p2)

*/


List(1,2,3,4).filter(x => x % 2 == 0)
