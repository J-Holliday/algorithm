/*
  Copyright http://icrus2.org/iida
*/

package algorithm.chapter6

class sort {
  def swap(a: Array[Int], x: Int, y: Int): Array[Int] = {
    val buf = a(x); a(x) = a(y); a(y) = buf; a
  }

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

  def shuttleSort(a: Array[Int]): Array[Int] = {
    def recursiveSorting(a: Array[Int], current: Int, n: Int): Array[Int] = {
      if (current >= a.length) a
      else if (n < current) {
        if (a(n) > a(current)) recursiveSorting(swap(a, n, current), current, n+1)
        else recursiveSorting(a, current, n+1)
      }
      else recursiveSorting(a, current+1, 0)
    }
    recursiveSorting(a, 1, 0)
  }

  def shellSort(a: Array[Int]): Array[Int] = {
    def partlyShuttleSort(a: Array[Int], divider: Int, current: Int, n: Int): Array[Int] = {
      if (n > a.length-1) 
        if (current < a.length-1) partlyShuttleSort(a, divider, current+divider, current+divider*2)
        else a
      else if (a(current) > a(n)) partlyShuttleSort(swap(a, n, current), divider, current, n+divider)
      else partlyShuttleSort(a, divider, current, n+divider)
    }
    def selectSequence(a: Array[Int], divider: Int, groupSeq: Int): Array[Int] = {
   	  if (divider < 1) a
      else if(groupSeq >= divider) selectSequence(a, divider/2, 0)
      else selectSequence(partlyShuttleSort(a, divider, groupSeq, groupSeq+divider), divider, groupSeq+1)
    }
    selectSequence(a, a.length/2, 0)
  }
}
