// Problem 1 to 28
// These problems are all related to

// P01 Find the last element of a list.
object P01 {
    // Use built-in function
    def lastBuiltin[T](ls: List[T]): T =
        if (ls.isEmpty) throw new NoSuchElementException
        else ls.last

    // Use recursion and pattern matching
    def lastRecursive[T](ls: List[T]): T = ls match {
        case h :: Nil  => h
        case _ :: tail => lastRecursive(tail)
        case _         => throw new NoSuchElementException
    }
}

// P02 Find the last but one element of a list.
object P02 {
    // Using built-in also has multiple ways:
    def penultimate1[T](ls: List[T]): T = {
        if (ls.isEmpty) throw new NoSuchElementException
        else ls.dropRight(1).last
    }

    def penultimate2[T](ls: List[T]): T = {
        if (ls.isEmpty) throw new NoSuchElementException
        else ls.init.last
    }

    // Using recursion
    def penultimateRecursive[T](ls: List[T]): T = ls match {
        case h :: _ :: Nil => h
        case _ :: tail     => penultimateRecursive(tail)
        case _             => throw new NoSuchElementException
    }
}

// P03 Find the Kth element of a list.
object P03 {
    // Using builtin
    def nth[T](n: Int, ls: List[T]): T =
        if (ls.isEmpty) throw new NoSuchElementException
        else ls(n)

    // Using pattern matching recursion
    def nthRecursive[T](n: Int, ls: List[T]): T = (n, ls) match {
        case (0, h :: tail) => h
        case (n, _ :: tail) => nthRecursive(n-1, tail)
        case (_, Nil      ) => throw new NoSuchElementException
    }
}

// P04 Find the number of elements of a list.
object P04 {
    // Trivial builtin mathod
    def length[T](ls: List[T]): T =
        ls.size

    // Using tail recursion
    def lengthRecursive[T](ls: List[T]): Int = {
        def loop(count: Int, ls: List[T]): Int = ls match {
            case Nil       => count
            case _ :: tail => loop(count + 1, tail)
        }
        loop(0, ls)
    }

    // *From answer provided by the problem site
    // Pure functional approach
    def lengthFunctional1[T](ls: List[T]): Int = ls.foldLeft(0) {
        (c, _) => c + 1
    }

    // Or, equivalently:
    def lengthFunctional2[T](ls: List[T]): Int = (0 /: ls) (//{
        (c, _) => c + 1
    )//}, the same to use () or {} here.
}

// P05 Reverse a list.
object P05 {
    // Using builtin
    def reverse[T](ls: List[T]): T =
        ls.reverse

    // Using recursion and pattern matching
    def reverseRecursive[T](ls: List[T]): List[T] = {
        def loop(res: T, cur: List[T]): List[T] = cur match {
            case Nil          => res
            case head :: tail => loop(head :: res, tail)
        }
        loop(Nil, ls)
    }

    // Functional approach
    def reverseFunctional1[T](ls: List[T]): List[T] =
        (List[T]() /: ls) {(ys, y) => y :: ys}

    // Or, equivalently,
    def reverseFunctional2[T](ls: List[T]): List[T] =
        ls.foldLeft(List[T]()) {(ys, y) => y :: ys}
}

// P06 Find out whether a list is a palindrome.
object P06 {
    // Concise implementation using builtin
    def isPalindrome[T](ls: List[T]): Boolean = ls == ls.reverse

    // recursive pattern match, maybe can be more precise?
    def isPalindrome[T](ls: List[T]): Boolean = ls match {
        case Nil                              => true
        case m :: Nil                         => true
        case l :: r :: Nil if (l == r)        => true
        case l :: r :: Nil if (l != r)        => false
        case h :: t if (h == t.tail)          => isPalindrome(t.init)
        case h :: t if (h != t.tail)          => false
    }
}

// P07 Flatten a nested list structure.
object P07 {
    // Using flatMap. The most concise way to solve this is recursion.
    def flatten1(ls: List[Any]): List[Any] = ls flatMap {
        case ms: List[_] => flatten1(ms)
        case e           => List(e)
    }
    // This code is a little bit confusing when firstly been looked at.
    // Logic explained:
    // (1) flatMap can only applied when the element of a list is also a list
    // (2) The key is to flat the list into 1 layer nested list:
    // List(List(), list(), ..., list())
    // (3) Then the normal flatMap operation can be applied.
}

// P08 Eliminate consecutive duplicates of list elements.
object P08 {
    // Using builtin dropWhile and recursion
    def compressRecursive[T](ls: List[T]): List[T] = ls match {
        case Nil       => Nil
        case h :: tail => h :: compressRecursive(tail.dropWhile(_ == h))
        // this is not tail recursion
    }

    // Tail recursion (author version)
    def compressTail1[T](ls: List[T]): List[T] = {
        def loop[T](res: List[T], cur: List[T]): List[T] = cur match {
            case h :: tail => loop(h :: res, tail.dropWhile(_ == h))
            case Nil       => res.reverse
        }
        loop(Nil, ls)
    }

    // Remove the reverse version
    def compressTail2[T](ls: List[T]): List[T] = {
        def loop[T](res: List[T], cur: List[T]): List[T] = cur match {
            case h :: tail => loop(res ::: List[T](h), tail.dropWhile(_ == h))
            case Nil       => res
        }
        loop(Nil, ls)
    }

    // Functional
    def compressFunctional1[T](ls: List[T]): List[T] =
        ls.foldRight(List[T]()) { (h, r) =>
            if (r.isEmpty || r.head != h) h :: r
            else r
        }
    // Or, using the fold right symbol:
    def compressFunction2[T](ls: List[T]): List[T] =
        (List[T]() :\ ls) { (h, r) =>
            if (r.isEmpty || r.head != h) h :: r
            else r
        }
}

// P09 Pack consecutive duplicates of list elements into sublists.
object P09 {
    // Using 'span' builtin to do the job
    // It is not tail recursion optimized
    def pack1[T](ls: List[T]): List[List[T]] = {
        if (ls.isEmpty) List(List())
        else {
            val (cur, rem) = ls.span(_ == ls.head)
            if (rem.isEmpty) List(cur)
            else cur :: pack1(rem)
        }
    }

    // Tail recursion version
    def pack2[T](ls: List[T]): List[List[T]] = {
        def loop[T](packed: List[List[T]], rest: List[T]): List[List[T]] =
            (packed, rest) match {
                case (_, Nil) => packed
                case (_, ls)  => loop(packed :+ ls.takeWhile(_ == ls.head),
                                                ls.dropWhile(_ == ls.head))
        }
        loop(Nil, ls)
    }
}

// P10 Run-length encoding of a list.
object P10 {
    // Independent implementation (Tail recursive)
    def encode[T](ls: List[T]): List[(Int, T)] = {
        def loop[T](packed: List[(Int, T)], rest: List[T]): List[(Int, T)] =
            (packed, rest) match {
                case (_, Nil) => packed
                case (_, ls)  =>
                    loop(packed :+ (ls.takeWhile(_ == ls.head).size, ls.head),
                                                ls.dropWhile(_ == ls.head))
        }
        loop(Nil, ls)
    }

    // Or, more concisely:
    import P09.pack1 // or pack2
    def encodeShort[T](ls: List[T]): List[(Int, T)] =
        pack(ls) map { e => (e.lenght, e.head) }
}

// P11 Modified run-length encoding.
object P11 {
    // Ugly implementation of mine
    def encodeModified[T](ls: List[T]): List[Any] = {
        def loop[T](packed: List[Any], rest: List[T]): List[Any] =
            (packed, rest) match {
                case (_, Nil) => packed
                case (_, h :: tail) if (h != tail.head) =>
                    loop(packed :+ h, tail)
                case (_, ls)  =>
                    loop(packed :+ (ls.takeWhile(_ == ls.head).size, ls.head),
                                                ls.dropWhile(_ == ls.head))
        }
        loop(Nil, ls)
    }

    // On the basis of P10:
    import P10.encode
    def encodeModified2[T](ls: List[T]): List[Any] =
        encode(ls) map { t => if (t._1 == 1) t._2 else t}

    // Type safer version by the author.
    def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
        encode(ls) map {
            t => if (t._1 == 1) Left(t._2) else Right(t)
        }
}

// P12 Decode a run-length encoded list.
object P12 {
    def decode[T](encoded: List[(Int, T)]): List[T] =
        encoded flatMap {
            e => List.fill(e._1)(e._2)
        }
}

// P13 Run-length encoding of a list (direct solution).
object P13 {
    // already implemented in P10.
    // To be frank, my implementation is better than author's:
    def encodeDirect[A](ls: List[A]): List[(Int, A)] =
        if (ls.isEmpty) Nil
        else {
          val (packed, next) = ls span { _ == ls.head }
          (packed.length, packed.head) :: encodeDirect(next)
        }
}

// P14 Duplicate the elements of a list.
object P14 {
    // Tail recursion version
    def duplicate[T](ls: List[T]): List[T] = {
        def loop[T](former: List[T], latter: List[T]): List[T] = {
            (former, latter) match {
                case (_, Nil)   => former
                case (res, rem) => loop(res :+ rem.head :+ rem.head, rem.tail)
            }
        }
        loop(Nil, ls)
    }

    // Using builtin 'flatMap' is most concise and simple:
    def duplicateOneline[T](ls: List[T]): List[T] = ls flatMap(e => List(e, e))
}

// P15 Duplicate the elements of a list a given number of times.
object P15 {
    // Using while loop (cumbersome)
    def duplicateN[T](n: Int, ls: List[T]): List[T] =
        ls flatMap(e => List.fill(n)(e))
    // or, ls flatMap(List.fill(n)(_)) is also ok
}

// P16 Drop every Nth element from a list.
object P16 {
    // Using builtin 'filter'. Oh dear it's tricky.
    def drop[T](n: Int, ls: List[T]): List[T] =
        List.range(1, ls.size + 1) filter {e => e % n != 0} map {i => ls(i-1)}

    // Author way of functional:
    def dropFunctional[T](n: Int, ls: List[T]): List[T] =
        ls.zipWithIndex filter {v => (v._2 + 1) % n != 0} map {_._1}
    // It's obvious my version is cleverer!

    // Always can use a recursive approach:
    def dropTailRecursive[T](n: Int, ls: List[T]): List[T] = {
        def loop[T](cd: Int, remain: List[T], result: List[T]): List[T] =
            (cd, remain) match {
                case (_, Nil)       => result.reverse
                case (1, _ :: tail) => loop(n, tail, result)
                case (_, h :: tail) => loop(cd-1, tail, h :: result)
            }
        loop(n, ls, Nil)
    }
}

// P17 Split a list into two parts.
object P17 {
    // Using builtin 'splitAt'
    def split[T](p: Int, ls: List[T]): (List[T], List[T]) = ls.splitAt(p)

    // Using recursion
    def splitR[T](p: Int, ls: List[T]): (List[T], List[T]) = {
        def loop[T](cd: Int, cur: List[T], res: List[T]): (List[T], List[T]) =
            (cd, cur) match {
                case (_, Nil)       => (res.reverse, cur)
                case (0, _ :: tail) => (res.reverse, cur)
                case (_, h :: tail) => loop(cd-1, tail, h :: res)
            }
        loop(p, ls, Nil)
    }

    // An interesting way by the author:
    def splitF[T](p: Int, ls: List[T]): (List[T], List[T]) =
        (ls.take(p), ls.drop(p))
}

// P18 Extract a slice from a list.
object P18 {
    // Using simple builtin
    def slice1[T](low: Int, high: Int, ls: List[T]): List[T] =
        (ls.drop(low).take(high-low)

    def slice2[T](low: Int, high: Int, ls: List[T]): List[T] =
        ls.slice(start, end)

    // Using tail recursion
    def sliceRecursive[T](low: Int, high: Int, ls: List[T]): List[T] = {
        def loop(count: Int, curList: List[A], result: List[A]): List[A] =
          (count, curList) match {
            case (_, Nil)                     => result.reverse
            case (c, h :: tail) if end <= c   => result.reverse
            case (c, h :: tail) if start <= c => loop(c + 1, tail, h :: result)
            case (c, _ :: tail)               => loop(c + 1, tail, result)
          }
        loop(0, ls, Nil)
        }
}