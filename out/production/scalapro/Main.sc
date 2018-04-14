println("Negro ")

//Para sumar uno al primer valor en la lista

List(1, 2, 3, 4) match {
  case h :: Nil => (h +1) :: Nil
  case h :: t => (h +1) :: t
  case _ => 0
}


//Para sumar 1 a toda la lista
def add1(l: List[Int]): List[Int] ={
  l match {
    case h :: Nil => (h +1) :: Nil
    case h :: t => (h +1) :: add1(t)
    case Nil => Nil
  }
}


add1(List(1,2,3,4))


9.toHexString

