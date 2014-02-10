val l = List(3, 5,8)

// Exercise 2.1.

def fibonacci(n: Int): Int = {

  @annotation.tailrec
  def go(n: Int, curn: Int, fibprev: Int, fibcur: Int): Int = {
    if (n == curn)
      fibcur
    else
      go(n, curn+1, fibcur, fibprev + fibcur)
  }

  go(n, 0, 0, 1)
}

// Exercise 2.2.
def isSorted[T](xs: Array[T], cmp: (T,T) => Boolean): Boolean = {
    if (xs.length <= 1)
        true
    else {
        val start = xs.init
        cmp(start.last, xs.last) && isSorted(start, cmp)
    }
}

// Exercise 2.3.
def partial1[A,B,C](a: A, f: (A,B) => C): B => C = {
    (b: B) => f(a, b)
}

val positive = partial1(0, (x: Int, y: Int) => x < y)

// Exercise 2.4.
def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b)
}

// Exercise 2.5.
def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
}

// Exercise 2.6.
def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
}

