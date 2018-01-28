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

// P32 (**) Determine the greatest common divisor of two positive integer numbers.
object P32 {

}