package it.unibo.u12lab.code

object Permutations extends App:

  // first an example of for-comprehension with streams..
  // note the first collection sets the type of all subsequent results

  val s: LazyList[Int] = for (i <- (10 to 50 by 10).to(LazyList); k = i + 1; j <- List(k - 1, k, k + 1)) yield j
  println(s) // LazyList(<not computed>)
  println(s.take(10).toList) // a list with the first 10 results
  println(s) // the same stream, but now we know 10 elements of it
  println(s.toList) // all on list
  println(s) // the same stream, but now we know all its elements

  // now let's do permutations
  // fill this method remove such that it works as of the next println
  // - check e.g. how method "List.split" works
  def removeAtPos[A](list: List[A], n: Int): List[A] =
    list.zipWithIndex.filter(_._2 != n).map(_._1)
  println(removeAtPos(List(10,20,30,40),1)) // 10,30,40

  def permutations[A](list: List[A]): LazyList[List[A]] = list match
    case Nil => LazyList(Nil)
    case _ =>
      (for
        //- makes i range across all indexes of list (converted as stream)
        i <- list.indices
        //- assigns e to element at position i
        e = list.apply(i)
        //- assigns r to the rest of the list as obtained from removeAtPos
        r = removeAtPos(list, i)
        //- makes pr range across all results of recursively calling permutations on r
        pr <- permutations(r)
      yield
        //- combines by :: e with pr
        e :: pr).to(LazyList)

  val list = List(10,20,30,40)
  println(permutations(list).toList)


