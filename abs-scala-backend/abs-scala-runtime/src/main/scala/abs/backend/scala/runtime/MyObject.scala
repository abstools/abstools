/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala.runtime

import akka.actor.{Actor, ActorRef}
import scala.util.continuations._
import akka.event.Logging
import akka.pattern.ask
import akka.dispatch.Await
import akka.util.Timeout
import akka.util.duration._
import akka.actor.ActorLogging

object MyObject {
  sealed abstract class Message
  case object Run extends Message
}

abstract class MyObject(private val cog: ActorRef) extends Actor with ActorLogging {
  private val taskCounter = new java.util.concurrent.atomic.AtomicLong(0);
  /**
   * Submits a new task to this object's COG and returns a remote actor reference to it.
   */
  protected def submit(block: => Any @suspendable) {
    log.debug("Submitting block to COG %s".format(cog))

    implicit val timeout = Timeout(5 seconds)
    
    val result = Await.result(cog ? new Cog.Run(getClass().getName() + "T" + taskCounter.incrementAndGet , () => block), timeout.duration)
    
    sender ! result
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

  def _new(clazz: Class[_ <: Actor], args: Any*) = {
    implicit val timeout = Timeout(5 seconds)
    
    Await.result (cog ? new Cog.New(clazz, args), timeout.duration).asInstanceOf[ActorRef]
  }
  
  def _newcog(url: Option[String], clazz: Class[_ <: Actor], args: Any*) =
    url map { url =>
      // contact remote host
      val remoteNode = context.actorFor("akka://ABS-Scala@%s/NodeManager".format(url))
    
      implicit val timeout = Timeout(5 seconds)
      
      val remoteCog = Await.result(remoteNode ? NodeManager.NewCog, timeout.duration).asInstanceOf[ActorRef]

      Await.result(remoteCog ? Cog.New(clazz, args), timeout.duration).asInstanceOf[ActorRef]
    } getOrElse {
      implicit val timeout = Timeout(5 seconds)
      Await.result(cog ? Cog.NewCog(clazz, args), timeout.duration).asInstanceOf[ActorRef]
    }
  
  def ignore[A](x : A) : Unit = {}
  
  def asyncCall(that: ActorRef, msg: Any): ActorRef = {
    log.debug("Invoking async call %s".format(msg.toString, that))
    
    implicit val timeout = Timeout(5 seconds)
    
    Await.result(that ? msg, timeout.duration).asInstanceOf[ActorRef]
  }
 
  def syncCall(that: ActorRef, msg: Any): ActorRef = {
    // FIXME
    log.debug("FIXME sync call %s to %s".format(msg.toString, that))
    throw new UnsupportedOperationException()
    null
  }
  
  protected def getFuture(fut: ActorRef): Any @suspendable = {
    implicit val timeout = Timeout(5 seconds)
    
    Await.result(fut ? Task.Get, timeout.duration).asInstanceOf[Option[Any]] match {
      case Some(x) => x
      case None =>
        // block the whole COG until the future gets resolved
        await(fut) 
        getFuture(fut)
    }
  }
}
