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
  case class Call(method: Any, async: Boolean) extends Message
  
  type Resolve = PartialFunction[Any, (String, () => Any @suspendable)]
}

abstract class MyObject(private val cog: ActorRef) extends Actor with ActorLogging {
  import MyObject._
  //type Resolve = MyObject.Resolve
  
  def fut2bool(fut: ActorRef): Boolean = {
    implicit val timeout = Timeout(5 seconds)
    Await.result(fut ? Task.Get, timeout.duration).asInstanceOf[Option[Any]] match {
      case None    => false
      case Some(_) => true
    }
  }
  
  private val taskCounter = new java.util.concurrent.atomic.AtomicLong(0);
  
  /**
   * Submits a new task to this object's COG and returns a remote actor reference to it.
   */
  protected def submit(name: String)(block: () => Any @suspendable) {
    log.debug("Submitting block to COG %s".format(cog))

    implicit val timeout = Timeout(5 seconds)
    
    val result = Await.result(cog ? new Cog.Run(getClass().getName() + "." + name + "$" + taskCounter.incrementAndGet , block), timeout.duration)
    
    sender ! result
  }
  
  /*
  protected def submit(name: String)(block: => Any @suspendable) {
    log.debug("Submitting block to COG %s".format(cog))

    implicit val timeout = Timeout(5 seconds)
    
    val result = Await.result(cog ? new Cog.Run(getClass().getName() + "." + name + "$" + taskCounter.incrementAndGet , () => block), timeout.duration)
    
    sender ! result
  }
  */
  
  /*protected def await(fut: ActorRef) =
    Runner.currentRunner.get.await(fut)
  */
  
  protected def suspend() = 
    await(true);
  
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
    
    Await.result(that ? new MyObject.Call(msg, true), timeout.duration).asInstanceOf[ActorRef]
  }
 
  def syncCall[T](that: ActorRef, msg: Any): T = {
    log.debug("Invoking sync call %s".format(msg.toString, that))
    
    implicit val timeout = Timeout(5 seconds)
    
    val fun = Await.result(that ? new MyObject.Call(msg, false), timeout.duration).asInstanceOf[() => T]
    fun()
  }
  
  protected def resolve: Resolve
  
  final def receive = {
    case MyObject.Run =>
      val (name, fun) = resolve(MyObject.Run)
      log.debug("received invocation for Run")
      submit(name)(fun)
      
    case MyObject.Call(m, async) => {
      val (name, fun) = resolve(m)
      log.debug("received invocation for %s".format(name))
      
      if (async)
        submit(name)(fun)
      else
    	sender ! fun
    }
  }
  /*
  def receive = {
                case MyObject.Run => log.debug("%s: received Run".format(self)); 
                case Random.IRandomBool.GetNextBool => log.debug("received %s".format(self)); submit("GetNextBool") { getNextBool() }
                case CRandomBool.SetValue(arg0) => log.debug("received %s".format(self)); submit("SetValue") { setValue(arg0) }
                case CRandomBool.ChangeValue => log.debug("received %s".format(self)); submit("ChangeValue") { changeValue() }
                case CRandomBool.GetNextBool => log.debug("received %s".format(self)); submit("GetNextBool") { getNextBool() }
                case e => throw new RuntimeException("Unknown incoming message: %s".format(e.toString))
        }
  */
  
  protected def getFuture(fut: ActorRef): Any @suspendable = {
    implicit val timeout = Timeout(5 seconds)
    
    Await.result(fut ? Task.Get, timeout.duration).asInstanceOf[Option[Any]] match {
      case Some(x) => x
      case None =>
        // block the whole COG until the future gets resolved
        await(fut2bool(fut)) 
        getFuture(fut)
    }
  }
}
