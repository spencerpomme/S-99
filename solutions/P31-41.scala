// Problem 31 to 41
// These problems are all related to arithmetics in Scala

// P31 (**) Determine whether a given integer number is prime.
object P31 {
    // It's generally bad to use 'return' in Scala, but I just want to finish it.
    def isPrime(num: Int): Boolean = {
        if (num < 2) return false
        for (i <- 2 until Math.sqrt(num).toInt) {
            if (num % i == 0) return false
        }
        true
    }
}

// P32 (**) Determine the greatest common divisor of two positive integer numbers
object P32 {
  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)
}

// P33 (*) Determine whether two positive integer numbers are coprime.
object P33 {
    import P32.gcd
    def isCoprimeTo(n: Int)(m: Int): Boolean = gcd(n, m) == 1
}

// P34 (**) Calculate Euler's totient function phi(m).
object P34 {
    import P33.isCoprimeTo
    def totient(num: Int): Int = {
        (List.range(1, num) filter isCoprimeTo(num)).length
    }
}

// P35 (**) Determine the prime factors of a given positive integer.
object P35 {
    def primeFactors(num: Int): List[Int] = {
        // to be done
    }
}