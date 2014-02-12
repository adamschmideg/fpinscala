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

}
