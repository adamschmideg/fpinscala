sealed trait Lst[+A]

case object Empty extends Lst[Nothing]
case class Cons[+A](had: A, tail: Lst[A]) extends Lst[A]

val a0 = Empty
val a1 = Cons(1, Empty)
val a2 = Cons(2, a1)
val a3 = Cons(3, a2)

// Exercise 3.2
def tail[A](as: Lst[A]): Lst[A] = {
    as match {
        case Empty => Empty
        case Cons(h, t) => t
    }
}

// Exercise 3.3
def drop[A](as: Lst[A], n: Int): Lst[A] = {
    as match {
        case Empty => Empty
        case Cons(h, t) if n == 0 => Cons(h, t)
        case Cons(h, t) => drop(t, n - 1)
    }
}

// Exercise 3.4
def dropWhile[A](as: Lst[A], pred: A => Boolean): Lst[A] = {
    as match {
        case Empty => Empty
        case Cons(h, t) if pred(h) => dropWhile(t, pred)
        case Cons(h, t) => Cons(h, t)
    }
}

// Exercise 3.5
def setHead[A](as: Lst[A], head: A): Lst[A] = {
    as match {
        case Empty => Cons(head, Empty)
        case Cons(old, t) => Cons(head, t)
    }
}

// Exercise 3.5
def init[A](as: Lst[A]): Lst[A] = {
    as match {
        case Empty => Empty
        case Cons(h, Empty) => Empty
        case Cons(h, t) => Cons(h, init(t))
    }
}

// Exercise 3.8
def foldRight[A,B](xs: Lst[A], last: B)(f: (A,B) => B): B = {
    xs match {
        case Empty => last
        case Cons(h, t) => f(h, foldRight(t, last)(f))
    }
}

def trace[A,B](f: (A,B) => B): (A,B) => B = {
    (a: A, b: B) => {
        println(s"$a, $b")
        f(a,b)
    }
}

// Exercise 3.9
def length[A](xs: Lst[A]): Int = foldRight(xs, 0)((x,m) => m + 1)

// Exercise 3.10
@annotation.tailrec
def foldLeft[A,B](xs: Lst[A], first: B)(f: (B,A) => B): B = {
    xs match {
        case Empty => first
        case Cons(h, t) => foldLeft(t, f(first, h))(f)
    }
}

// Exercise 3.11
def sum(nums: Lst[Int]): Int = foldLeft(nums, 0)(_ + _)
def product(nums: Lst[Int]): Int = foldLeft(nums, 1)(_ * _)
def length2[A](xs: Lst[A]): Int = foldLeft(xs, 0)((m,x) => m + 1)

// Exercise 3.12
def append[A](xs: Lst[A], ys: Lst[A]): Lst[A] = {
    xs match {
        case Empty => ys
        case Cons(h, t) => Cons(h, append(t, ys))
    }
}

def reverse[A](xs: Lst[A]): Lst[A] = {
    xs match {
        case Empty => Empty
        case Cons(h, t) => append(reverse(t), Cons(h, Empty))
    }
}

def reverseLeft[A](xs: Lst[A]): Lst[A] = foldLeft(xs, Empty:Lst[A])((m,x) => Cons(x,m))
def reverseRight[A](xs: Lst[A]): Lst[A] = foldRight(xs, Empty:Lst[A])((x,m) => append(m, Cons(x,Empty)))

// Exercise 3.13
def swapArgs[A,B,C](f: (A,B) => C): (B,A) => C = (b, a) => f(a,b)

def foldLeftWithRight[A,B](xs: Lst[A], first: B)(f: (B,A) => B): B = foldRight(reverse(xs), first)(swapArgs(f))

def foldRightWithLeft[A,B](xs: Lst[A], first: B)(f: (A,B) => B): B = foldLeft(reverse(xs), first)(swapArgs(f))

// Exercise 3.14
def appendRight[A](xs: Lst[A], ys: Lst[A]): Lst[A] = foldRight(xs, ys)((x,as) => Cons(x, as))

// Exercise 3.15
def flatten[A](as: Lst[Lst[A]]): Lst[A] = foldRight(as, Empty:Lst[A])((x,m) => append(x,m))

// Exercise 3.18
def mapLeft[A,B](as: Lst[A])(f: (A) => B): Lst[B] = foldLeft(as, Empty:Lst[B])((m,x) => Cons(f(x), m))

// Exercise 3.19
def filterLeft[A](as: Lst[A])(f: (A) => Boolean): Lst[A] =
    foldLeft(as, Empty:Lst[A])((m,x) => { if (f(x)) Cons(x,m) else m })
def evens(ns: Lst[Int]): Lst[Int] = filterLeft(ns)((n) => n % 2 == 0)

// Exercise 3.20
def flatMapLeft[A,B](as: Lst[A])(f: (A) => Lst[B]): Lst[B] = foldLeft(as, Empty:Lst[B])((m,x) => append(f(x), m))

// Exercise 3.21
def filterWithFlatMap[A](as: Lst[A])(f: (A) => Boolean): Lst[A] =
    flatMapLeft(as)((x) => if (f(x)) Cons(x, Empty) else Empty)

// Exercise 3.23
def map2[A,B,C](xs: Lst[A], ys: Lst[B])(f: (A,B) => C): Lst[C] = {
    xs match {
        case Empty => Empty
        case Cons(xh, xt) => ys match {
            case Empty => Empty
            case Cons(yh, yt) => Cons(f(xh, yh), map2(xt, yt)(f))
        }
    }
}

// Exercise 3.24
def sameStart[A](l: Lst[A], start: Lst[A]): Boolean = {
    start match {
        case Empty => true
        case Cons(sh, st) => l match {
            case Empty => false
            case Cons(h, t) => h == sh && sameStart(t, st)
        }
    }
}
def hasSubsequence[A](l: Lst[A], sub: Lst[A]): Boolean = {
    l match {
        case Empty => sub == Empty
        case Cons(_, tail) => sameStart(l, sub) || hasSubsequence(tail, sub)
    }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
val t1 = Branch(
            Branch(
                Leaf(1.0),
                Branch(
                    Leaf(2.0),
                    Leaf(3.0))),
            Leaf(4.0))

def stringify[A](tree: Tree[A], indent: String = ""): String = {
    tree match {
        case Leaf(v) => s"$indent$v\n"
        case Branch(l,r) => s"$indent+\n" + stringify(l, indent + " ") + stringify(r, indent + " ")
    }
}

// Exercise 3.25
def size[A](tree: Tree[A]): Int = {
    tree match {
        case Leaf(_) => 1
        case Branch(l, r) => size(l) + size(r)
    }
}

// Exercise 3.26
def maximum(tree: Tree[Int]): Int = {
    tree match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }
}

// Exercise 3.27
def depth[A](tree: Tree[A]): Int = {
    tree match {
        case Leaf(_) => 1
        case Branch(l, r) => depth(l) max depth(r) + 1
    }
}

// Exercise 3.28
def treeMap[A,B](tree: Tree[A])(f: (A) => B): Tree[B] = {
    tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l,r) => Branch(treeMap(l)(f), treeMap(r)(f))
    }
}

// Exercise 3.29
def treeFold[A,B](tree: Tree[A])(lf: A => B)(bf: (B,B) => B): B = {
    tree match {
        case Leaf(v) => lf(v)
        case Branch(l,r) => bf(treeFold(l)(lf)(bf), treeFold(r)(lf)(bf))
    }
}

def size2[A](tree: Tree[A]): Int = treeFold(tree)(_ => 1)(_ + _)
def max2(tree: Tree[Double]): Double = treeFold(tree)(x => x)(_ max _)
def depth2[A](tree: Tree[A]): Int = treeFold(tree)(_ => 1)(_ max _ + 1)
