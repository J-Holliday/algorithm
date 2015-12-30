/*
  Copyright http://icrus2.org/iida
*/

package algorithm.chapter6

class sort {
  def bubbleSort(a: Array[Int]): (Array[Int], Int) = {
    def recursiveSorting(a: Array[Int], n: Int, border: Int, acc: Int): (Array[Int], Int) = {
      if (border < 1) (a, acc)
      else if (n < border)
        if(a(n)>a(n+1)) recursiveSorting(swap(a, n, n+1), n+1, border, acc+1)
        else recursiveSorting(a, n+1, border, acc+1)
      else recursiveSorting(a, 0, border-1, acc+1)
    }  
    recursiveSorting(a, 0, a.length-1, 0)
  }
  
  def swap(a: Array[Int], x: Int, y: Int): Array[Int] = {
    val buf = a(x); a(x) = a(y); a(y) = buf; a
  }
}
