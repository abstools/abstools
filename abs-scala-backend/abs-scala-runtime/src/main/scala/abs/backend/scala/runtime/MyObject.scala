/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
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
  protected def submit(block: => Any @suspendable) {
    EventHandler.debug(this, "[%s] Submitting block to COG %s".format(self, cog))

    cog !! new Cog.Run(() => block) match {
      case None => throw new RuntimeException("[%s] No reply from cog %s".format(self, cog))
      case Some(x) => 
        self reply_? x
    }
  }
  
  protected def await(fut: ActorRef) =
    Runner.currentRunner.get.await(fut)
  
  protected def await(cond: => Boolean) = 
    if (!cond)
      Runner.currentRunner.get.await(cond)
  
  def noop: Unit @suspendable = 
    shift {
      k : (Unit => Unit) => k()
    }

  def _new(clazz: Class[_ <: Actor], args: Any*) =
    cog !! new Cog.New(clazz, args) match {
      case None    => throw new RuntimeException("[%s] No reply from COG %s".format(self, cog));
	  case Some(x) => x.asInstanceOf[Array[Byte]]
    }
  
  def _newcog(url: Option[String], clazz: Class[_ <: Actor], args: Any*) =
    url map { url =>
      val addr = url.split(":")
      
      // contact remote host
      val remoteNode = Actor.remote.actorFor("nodeManager", addr(0), addr(1).toInt)
    
      val remoteCog = remoteNode !! NodeManager.NewCog match {
      	case None    => throw new RuntimeException("[%s] No reply from remote node %s".format(self, remoteNode));
      	case Some(x) => RemoteActorSerialization.fromBinaryToRemoteActorRef(x.asInstanceOf[Array[Byte]])
      }
    
      remoteCog !! new Cog.New(clazz, args) match {
      	case None    => throw new RuntimeException("[%s] No reply from remote COG %s".format(self, remoteCog));
      	case Some(x) => x.asInstanceOf[Array[Byte]]
      }
    } getOrElse (
      cog !! new Cog.NewCog(clazz, args) match {
      	case None    => throw new RuntimeException("[%s] No reply from COG %s".format(self, cog));
      	case Some(x) => x.asInstanceOf[Array[Byte]]
      }
    )
  
  def asyncCall(that: Array[Byte], msg: Any): ActorRef =
    asyncCall(RemoteActorSerialization.fromBinaryToRemoteActorRef(that), msg)
    
  def asyncCall(that: ActorRef, msg: Any): ActorRef = {
    EventHandler.debug(this, "[%s] Invoking async call %s (recipient %s)".format(self, msg.toString, that))
    
    that !! msg match {
      case None => throw new RuntimeException("%s didn't get task reference from other actor %s due to timeout".format(self, that))
      case Some(x) => RemoteActorSerialization.fromBinaryToRemoteActorRef(x.asInstanceOf[Array[Byte]])
    }
  }
 
  protected def getFuture(fut: ActorRef): Any @suspendable = {
    fut !! Task.Get match {
      case None => throw new RuntimeException("%s: No response from future %s".format(self, fut))
      case Some(x) => x.asInstanceOf[Option[Any]] match {
        case Some(x) => x
        case None =>
          // block the whole COG until the future gets resolved
          await(fut) 
          getFuture(fut)
      }
    }
  }
}
