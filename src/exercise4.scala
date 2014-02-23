import javax.management.remote.rmi._RMIConnection_Stub

object exercise4 {

// Exercise 4.1
trait Maybe[+A] {

    def map[B](f: A => B): Maybe[B] = {
        this match {
            case Just(a: A) => Just(f(a))
            case Nonne => Nonne
        }
    }

    def getOrElse[B >: A](default: => B): B = this match {
        case Just(a: A) => a
        case Nonne => default
    }

    def flatMap[B](f: A => Maybe[B]): Maybe[B] = {
        map(f) getOrElse Nonne
    }

    def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = {
        map(Just(_)) getOrElse ob
    }

    def filter(f: A => Boolean): Maybe[A] = {
        if (this.map(f).getOrElse(false))
            this.map(identity)
        else
            Nonne
    }

    def filter_(f: A => Boolean): Maybe[A] =
        flatMap(a => if (f(a)) Just(a) else Nonne)

}
case class Just[A](value:A) extends Maybe[A]
case object Nonne extends Maybe[Nothing]


// Exercise 4.2
def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty)
        None
    else
        Some(xs.sum / xs.length)

def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
}

// Exercise 4.3
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

// Exercise 4.4
def mkMatcher(pat: String): Option[String => Boolean] = ???

def bothMatch(pat1: String, pat2: String, s: String): Option[Boolean] = ???
    //map2(mkMatcher(pat1), mkMatcher(pat2))((m1, m2) => m1(s) && m2(s))

// Exercise 4.5
def appendItem[A](xs: List[A], item: A): List[A] = xs :+ item

def f[A](xs: Option[List[A]], x: Option[A]): Option[List[A]] = map2(xs, x)(appendItem _)

def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as.foldLeft[Option[List[A]]](
        Some(List[A]()))  /* break line here to cause an error */((xs: Option[List[A]], x: Option[A]) => map2(xs, x)(appendItem _))
    //(f _)
}




}
