package phronesis
package internal

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

/** Adapted from scalaz:
 * https://github.com/scalaz/scalaz/blob/series/7.3.x/concurrent/src/main/scala/scalaz/concurrent/Actor.scala
 */
private[phronesis] final case class Actor[A](handler: A => Unit, onError: Throwable => Unit = ActorUtils.rethrowError)(
    implicit val strategy: Strategy) {

  private val head = new AtomicReference[Node[A]]

  def !(a: A): Unit = {
    val n = new Node(a)
    val h = head.getAndSet(n)
    if (h ne null) h.lazySet(n)
    else schedule(n)
  }

  def apply(a: A): Unit = this ! a

  def contramap[B](f: B => A): Actor[B] = new Actor[B](b => this ! f(b), onError)(strategy)

  private def schedule(n: Node[A]): Unit = strategy(act(n))

  @tailrec private def act(n: Node[A], i: Int = 1024): Unit = {
    try handler(n.a) catch {
      case ex: Throwable => onError(ex)
    }
    val n2 = n.get
    if (n2 eq null) scheduleLastTry(n)
    else if (i == 0) schedule(n2)
    else act(n2, i - 1)
  }

  private def scheduleLastTry(n: Node[A]): Unit = strategy(lastTry(n))

  private def lastTry(n: Node[A]): Unit = if (!head.compareAndSet(n, null)) act(next(n))

  @tailrec private def next(n: Node[A]): Node[A] = {
    val n2 = n.get
    if (n2 ne null) n2
    else next(n)
  }

}


private[phronesis] object ActorUtils {
  val rethrowError: Throwable => Unit = throw _
}

private class Node[A](val a: A) extends AtomicReference[Node[A]]

private[phronesis] trait Strategy {
  def apply[A](a: => A): () => A
}

private[phronesis] object Actor {
  def actor[A](handler: A => Unit, onError: Throwable => Unit = ActorUtils.rethrowError)(
    implicit s: Strategy): Actor[A] = new Actor[A](handler, onError)(s)
}

private[phronesis] object Strategy {

  val defaultDaemonThreadFactory = new ThreadFactory {
    val defaultThreadFactory = Executors.defaultThreadFactory()
    def newThread(r: Runnable): Thread = {
      val t = defaultThreadFactory.newThread(r)
      t.setDaemon(true)
      t
    }
  }

  val defaultExecutorService: ExecutorService =
    Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors, defaultDaemonThreadFactory)

  val defaultStrategy: Strategy = executor(defaultExecutorService)

  implicit val sequential: Strategy = new Strategy {
    def apply[A](a: => A) = {
      val v = a
      () => v
    }
  }

  implicit def executor(implicit s: ExecutorService) = new Strategy {
    def apply[A](a: => A) = {
      val fut = s.submit(new Callable[A] { def call = a })
      () => fut.get
    }
  }



}
