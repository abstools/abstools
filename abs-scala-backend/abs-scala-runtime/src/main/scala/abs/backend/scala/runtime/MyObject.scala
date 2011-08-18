package abs.backend.scala.runtime

import akka.actor.{Actor, ActorRef}
import akka.event.EventHandler
import akka.serialization.RemoteActorSerialization
import scala.util.continuations._

object MyObject {
  sealed abstract class Message
  case class RemoteSelfRef(ref: Array[Byte]) extends Message
  case object Run extends Message
}

abstract class MyObject(private val cog: ActorRef) extends Actor {
  self.id = self.uuid.toString

  /**
   * Submits a new task to this object's COG and returns a remote actor reference to it.
   */
  protected def submit(block: => Unit @suspendable) {
    EventHandler.debug(this, "%s: Submitting block to COG".format(self))

    cog !! new Cog.Run(() => block) match {
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

  def asyncCall(that: Array[Byte], msg: Any): ActorRef =
    asyncCall(RemoteActorSerialization.fromBinaryToRemoteActorRef(that), msg)
    
  def asyncCall(that: ActorRef, msg: Any): ActorRef = {
    EventHandler.debug(this, "Invoking async call (recipient %s)".format(that.toString()))
    
    that !! msg match {
      case None => throw new RuntimeException("Didn't get task reference from other actor due to timeout")
      case Some(x) => RemoteActorSerialization.fromBinaryToRemoteActorRef(x.asInstanceOf[Array[Byte]])
    }
  }
 
  protected def getFuture(fut: ActorRef): Any @suspendable = {
    // if the future is not ready we get to await on it
    fut !! Task.Get match {
      case None => throw new RuntimeException("No response from future")
      case Some(x) => x.asInstanceOf[Option[Any]] match {
        case Some(x) => x
        case None => await(fut); getFuture(fut)
      }
    }
  }
}
