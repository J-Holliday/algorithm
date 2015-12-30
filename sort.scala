/*
  Copyright http://icrus2.org/iida
*/

package algorithm.chapter6

class sort {
  def bubbleSort(a: Array[Int]): Array[Int] = {
    def oneItemSorting(a: Array[Int], acc: Int, border: Int): Array[Int] = {
      if (border < 1) a
      else if (acc <= border)
        if(a(acc)>a(acc+1))
          oneItemSorting(swap(a, acc, acc+1), acc+1, border)
        else
          oneItemSorting(a, acc+1, border)
      else
        oneItemSorting(a, 0, border-1)
    }  
    
    oneItemSorting(a, 0, a.length-2)
  }
  
  def swap(a: Array[Int], x: Int, y: Int): Array[Int] = {
    var ary = a
    var buf = ary(x)
    ary(x) = ary(y)
    ary(y) = buf
    ary
  }
}
