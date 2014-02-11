trait Option[+A] {

    def map[B](f: A => B): Option[B] = {
        this match {
            case Some(a: A) => Some(f(a))
            case _ => None
        }
    }

    def getOrElse[B >: A](default: => B): B = {
        this match {
            case Some(a: A) => a
            case _ => default
        }
    }

    def _flatMap[B](f: A => Option[B]): Option[B] = {
        this match {
            case Some(a: A) => f(a)
            case _ => None
        }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        this.map(f).getOrElse(None)
    }

    def _orElse[B >: A](ob: => Option[B]): Option[B] = {
        this match {
            case Some(a: A) => Some(a)
            case _ => ob
        }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = ???

    def _filter(f: A => Boolean): Option[A] = {
        this match {
            case Some(a: A) if f(a) => Some(a)
            case _ => None
        }
    }

    def filter(f: A => Boolean): Option[A] = {
        if (this.map(f).getOrElse(false))
            this.map(identity)
        else
            None
    }
}


