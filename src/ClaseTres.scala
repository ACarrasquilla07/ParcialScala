object Demo {
  def main(args: Array[String]) {
    println( "Returned Value : " + factorial(10) );
    println( "Returned Value : " + fibonacci(8) );
  }

  def factorial(n: Int): Int = n match{
    case 0 => 1
    case _ => n * factorial(n-1)
  }

  def fibonacci(n: Int): Int = n match{
    case 0 => 0
    case 1 => 1
    case _ => fibonacci(n-2) + fibonacci(n-1)

  }
}