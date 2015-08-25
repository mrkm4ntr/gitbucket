package gitbucket.core.util

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.{ReentrantLock, Lock}
import ControlUtil._

class LockWith[A](a: => A) {
  def run(key: String): A = LockUtil.lock(key)(a)
  def map[B](f: A => B): LockWith[B] = {
    lazy val b = f(a)
    LockWith(b)
  }
}

object LockWith {
  def apply[A](a: => A) = new LockWith(a)
}

object LockUtil {

  /**
   * lock objects
   */
  private val locks = new ConcurrentHashMap[String, Lock]()

  /**
   * Returns the lock object for the specified repository.
   */
  private def getLockObject(key: String): Lock = synchronized {
    if(!locks.containsKey(key)){
      locks.put(key, new ReentrantLock())
    }
    locks.get(key)
  }

  /**
   * Synchronizes a given function which modifies the working copy of the wiki repository.
   */
  def lock[T](key: String)(f: => T): T = defining(getLockObject(key)){ lock =>
    try {
      lock.lock()
      f
    } finally {
      lock.unlock()
    }
  }

}
