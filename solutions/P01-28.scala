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

}