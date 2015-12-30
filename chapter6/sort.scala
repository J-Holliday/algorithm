/*
  Copyright http://icrus2.org/iida
*/

package algorithm.chapter6

class sort {
  def bubbleSort(a: Array[Int]): Array[Int] = {
    def recursiveSorting(a: Array[Int], acc: Int, border: Int): Array[Int] = {
      if (border < 1) a
      else if (acc <= border)
        else recursiveSorting(a, acc+1, border)
      else recursiveSorting(a, 0, border-1)
    }  
    recursiveSorting(a, 0, a.length-2)
  }
  
  def swap(a: Array[Int], x: Int, y: Int): Array[Int] = {
    val buf = a(x); a(x) = a(y); a(y) = buf; a
  }
}
