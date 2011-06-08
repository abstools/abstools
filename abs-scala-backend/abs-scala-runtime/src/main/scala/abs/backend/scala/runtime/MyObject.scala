package abs.backend.scala.runtime

import akka.actor.{Actor, ActorRef}
import akka.serialization.RemoteActorSerialization
import scala.util.continuations._

object MyObject {
  sealed abstract class Message
  case class RemoteSelfRef(ref: Array[Byte]) extends Message
  case class Run extends Message
}

abstract class MyObject(private val cog: ActorRef) extends Actor {
  self.id = self.uuid.toString

  /**
   * Submits a new task to this object's COG and returns a remote actor reference to it.
   */
  protected def submit(block: => Unit @suspendable) {
    val actor = Actor.actorOf(new Task(cog, () => block))

    val msg = new Cog.Run(actor)
  
    cog !! msg match {
      case None => throw new RuntimeException("No reply from cog")
      case Some(x) => 
        self reply_? x
    }
  }
  
  protected def await(fut: ActorRef) =
    Runner.currentRunner.get.await(fut)
  
  protected def await(cond: => Boolean) = 
    Runner.currentRunner.get.await(cond)
  
  def noop: Unit @suspendable = 
    shift {
      k : (Unit => Unit) => k()
    }

  def asyncCall(that: ActorRef, msg: Any) =
    that !! msg match {
      case None => throw new RuntimeException("Didn't get task reference from other actor due to timeout")
      case Some(x) => RemoteActorSerialization.fromBinaryToRemoteActorRef(x.asInstanceOf[Array[Byte]])
    }
 
  def getFuture(fut: ActorRef): Any =
    fut !! Task.Get getOrElse { throw new RuntimeException("No response from future") }
}
