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
        case e => List(e)
    }
    // This code is a little bit confusing when firstly been looked at.
    // Logic explained:
    // (1) flatMap can only applied in the case when the element of a list is also a list
    // (2) The key idea is to flat the list into 1 layer nested list, that is of form:
    // List(List(), list(), ..., list())
    // (3) Then the normal flatMap operation can be applied.
}

// P08 Eliminate consecutive duplicates of list elements.
object P08 {
    def compress[T](ls: List[T]): List[T] = ls match {

    }
}



