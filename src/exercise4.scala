trait Option[+A] {

    def map[B](f: A => B): Option[B] = {
        this match {
            case Some(a: A) => Some(f(a))
            case None => None
        }
    }

    def getOrElse[B >: A](default: => B): B = this match {
        case Some(a: A) => a
        case None => default
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
        map(f) getOrElse None
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
        map(Some(_)) getOrElse ob
    }

    def filter(f: A => Boolean): Option[A] = {
        if (this.map(f).getOrElse(false))
            this.map(identity)
        else
            None
    }

    def filter_(f: A => Boolean): Option[A] =
        flatMap(a => if (f(a)) Some(a) else None)

}


