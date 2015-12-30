/*
  Copyright http://icrus2.org/iida
*/

package algorithm.chapter3

class search[+A] {
  def linearSearch[AA >: A](a: Array[AA], key: AA): (Option[AA], Int) = {
    def recursiveMatching(acc: Int, a: Array[AA], key: AA): (Int, Boolean)  = {
      if (acc>=a.length) (acc, false)
      else if (a(acc)==key) (acc, true)
      else recursiveMatching(acc+1, a, key)
    }
     
    recursiveMatching(0, a, key) match {
      case (acc, false) => (None, acc+1)
      case (acc, true) => (Some(a(acc)), acc+1)
    }
  }

  def binarySearch(a: Array[Int], key: Int): (Option[Int], Int) = {
    def recursiveMatching(acc: Int, a: Array[Int], key: Int, head: Int, tail: Int, center: Int): (Int, Int, Boolean) = {
      if (head==tail && a(center)!=key) (-1, acc, false) 
      else if (a(center)==key) (center, acc, true)
      else recursiveMatching(acc+1, a, key,
        if (a(center)< key) center+1 else head,
        if (a(center)> key) center-1 else tail,
        if (a(center)< key) (center+1+tail)/2 else (center-1+head)/2)
    }
    
    recursiveMatching(0, a, key, 0, a.length-1, (a.length-1)/2) match {
      case (res, acc, false) => (None, acc+1)
      case (res, acc, true) => (Some(a(res)), acc+1)
    }
  }
}
