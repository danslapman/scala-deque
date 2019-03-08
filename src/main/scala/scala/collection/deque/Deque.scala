package scala.collection.deque

final class Deque[T](private val cons: List[T], private val snoc: List[T]) {
  val isEmpty: Boolean = cons.isEmpty && snoc.isEmpty
  val nonEmpty: Boolean = cons.nonEmpty || snoc.nonEmpty
  def +:(el: T): Deque[T] = new Deque(el :: cons, snoc)
  def :+(el: T): Deque[T] = new Deque(cons, el :: snoc)
  def reverse: Deque[T] = new Deque(snoc.reverse, cons.reverse)
}

object Deque {
  def empty[T] = new Deque[T](Nil, Nil)

  def unapply[T](arg: Deque[T]): Option[(List[T], List[T])] = Some(arg.cons, arg.snoc)
}

object Uncons {
  def unapply[T](deque: Deque[T]): Option[(T, Deque[T])] =
    deque match {
      case Deque(Nil, Nil) => None
      case snd @ Deque(Nil, _) => unapply(snd.reverse)
      case Deque(h :: Nil, sn) => Some(h, new Deque(sn.reverse, Nil))
      case Deque(h :: t, sn) => Some(h, new Deque(t, sn))
    }
}

object Unsnoc {
  def unapply[T](deque: Deque[T]): Option[(Deque[T], T)] =
    deque match {
      case Deque(Nil, Nil) => None
      case cnd @ Deque(_, Nil) => unapply(cnd.reverse)
      case Deque(cn, h :: Nil) => Some(new Deque(Nil, cn.reverse), h)
      case Deque(cn, h :: t) => Some(new Deque(cn, t), h)
    }
}