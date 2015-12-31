/*
  Copyright http://icrus2.org/iida
*/

package algorithm.chapter6

class sort {
  def bubbleSort(a: Array[Int]): (Array[Int], Int) = {
    def recursiveSorting(a: Array[Int], n: Int, border: Int, acc: Int): (Array[Int], Int) = {
      if (border < 1) (a, acc)
      else if (n < border)
        if(a(n) > a(n+1)) recursiveSorting(swap(a, n, n+1), n+1, border, acc+1)
        else recursiveSorting(a, n+1, border, acc+1)
      else recursiveSorting(a, 0, border-1, acc+1)
    }  
    recursiveSorting(a, 0, a.length-1, 0)
  }
  
  def swap(a: Array[Int], x: Int, y: Int): Array[Int] = {
    val buf = a(x); a(x) = a(y); a(y) = buf; a
  }

  def shuttleSort(a: Array[Int]): (Array[Int], Int) = {
    def recursiveSorting(a: Array[Int], current: Int, n: Int, acc: Int): (Array[Int], Int) = {
      if (current >= a.length) (a, acc)
      else if (n < current) {
        if (a(n) > a(current)) recursiveSorting(swap(a, n, current), current, n+1, acc+1)
        else recursiveSorting(a, current, n+1, acc+1)
      }
      else recursiveSorting(a, current+1, 0, acc+1)
    }
    recursiveSorting(a, 1, 0, 0)
  }

  def shellSort(a: Array[Int]): (Array[Int], Int) = {
    def recursiveSorting(a: Array[Int], divider: Int, n: Int, acc: Int): (Array[Int], Int) = {
      if (divider < 1) (a, acc)
      else if(n >= divider) recursiveSorting(a, divider/2, 0, acc+1)
      else {
        shuttleSort(grouping(a, divider, 0, n, acc+1)._1)
        recursiveSorting(a, divider, n+1, acc+1)
      }
    }
    def grouping(a: Array[Int], divider: Int, buf: Int, start: Int, acc: Int): (Array[Int], Int) = {
      if (buf > a.length-1) (a, acc)
      //else Array.concat(Array(a(start+buf)), grouping(a, divider, buf+divider, start, acc+1)._1), acc)
      else grouping(a, divider, buf+divider, start, acc+1) match {
        case (ary, accum) => (Array.concat(Array(a(start+buf)), ary), accum)
      }
    }
    recursiveSorting(a, a.length/2, 0, 0)
  }
}
