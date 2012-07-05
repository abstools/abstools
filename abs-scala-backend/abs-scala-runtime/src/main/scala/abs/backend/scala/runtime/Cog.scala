/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala.runtime

import akka.actor.{Actor, ActorRef, FSM}
import scala.util.continuations._
import akka.event.Logging
import akka.pattern.ask
import akka.dispatch.Await
import akka.util.Timeout
import akka.util.duration._
import akka.actor.Props

object Cog {
  sealed abstract class Message
  case class Run(block: () => Any @suspendable) extends Message
  case object Work extends Message
  case class Done(finished: Boolean) extends Message
  case object Blocked extends Message
  case class New[T <: Actor](clazz: Class[T], args: Seq[Any]) extends Message
  case class NewCog[T <: Actor](clazz: Class[T], args: Seq[Any]) extends Message
  
  sealed trait State
  case object IDLE extends State
  case object BUSY extends State
  case object BLOCKED extends State
}

class Cog(val server: NodeManager) extends Actor with FSM[Cog.State, Option[ActorRef]] {
  //private val log = Logging(context.system, this)
  import Cog._
  import FSM._
  
  private var tasks: List[ActorRef] = Nil
  
  private def pickTask: Option[ActorRef] = {
    implicit val timeout = Timeout(5 seconds)
    for (task <- tasks) {
      Await.result(task ? Task.CanRun, timeout.duration).asInstanceOf[Boolean] match {
          case true => return Some(task)
          case false => 
      }
    }
    
    None
  }
      
  private def newobj[T <: Actor](clazz: Class[T], args: Seq[Any]) {
    log.debug("[%s] creating new concurrent object for %s".format(self, clazz))
    
    val actor: ActorRef = context.actorOf(Props({
      val obj = clazz.getConstructor(classOf[ActorRef]).newInstance(self)
      // does it have an init method?
      try {
        log.debug("[%s] invoking init method with args %s".format(self, args.mkString(", ")))
    	clazz.getMethod("init", classOf[Array[Any]]).invoke(obj, args.toArray)
      } catch {
        case e: NoSuchMethodException => // nope
          log.debug("[%s] no init method in class".format(self))
      }
      obj
    }))
    
    actor ! MyObject.Run
    sender ! actor
  }
  
  private def newtask(block: () => Any @suspendable) {
    val task = context.actorOf(Props(new Task(self, block)))
    tasks ::= task
    
    sender ! task
  }
  
  private def newcog[T <: Actor](clazz: Class[T], args: Seq[Any]) = {
    // ask for a new cog from node manager
    val newCog = server.newCog
    newCog.forward(New(clazz, args))
  }
  
  startWith(IDLE, None)
  
  when(IDLE) {
    case Event(Work, _) =>
      log.debug("[%s] Awakened, looking for something to do".format(self))
      // pick something that can run
      pickTask match {
        case None =>
          log.debug("[%s] Nothing to do, sleeping".format(self))
          stay
        case Some(task) =>
          log.debug("[%s] Activating task %s".format(self, task))
          //task ! Task.Run(remoteSelfRef)
          task ! Task.Run
          goto(BUSY) using Some(task)
      }
            
    case Event(Run(block), _) =>
      log.debug("[%s] New task received (while passive)".format(self))
      
      newtask(block)
      self ! Work
      stay
      
    case Event(New(clazz, args), _) =>
      newobj(clazz, args)
       stay
      
    case Event(NewCog(clazz, args), _) =>
      newcog(clazz, args)
      stay
  }
  
  when(BUSY) {
    case Event(Work, _) => // ignore
      log.debug("[%s] Work received while already working".format(self))
      stay
      
    case Event(Run(block), _) =>
      log.debug("[%s] New task received (while working)".format(self))
      
      newtask(block)
      stay
      
    case Event(New(clazz, args), _) =>
      newobj(clazz, args)
      stay
      
    case Event(NewCog(clazz, args), _) =>
      newcog(clazz, args)
      stay
      
    case Event(Done(finished), Some(task)) =>
      log.debug("[%s] Task finished, awakening myself".format(self))
      
      if (finished) 
        tasks -= task
        
      self ! Work
      goto(IDLE) using None
      
    case Event(Blocked, t) =>
      log.debug("[%s] COG blocked by task %s".format(self, t))
      goto(BLOCKED)
  }
  
  when(BLOCKED) {
    case Event(Work, Some(task)) ⇒
      log.debug("[%s] Work received while blocked, checking task".format(self))

      implicit val timeout = Timeout(5 seconds)
      
      Await.result(task ? Task.CanRun, timeout.duration).asInstanceOf[Boolean] match {
        case true =>
          goto(BUSY)
        case false =>
          stay
      }
      
    case Event(Run(block), _) =>
      log.debug("[%s] New task received (while blocked)".format(self))
      newtask(block)
      stay
      
    case Event(New(clazz, args), _) =>
      newobj(clazz, args)
      stay
      
    case Event(NewCog(clazz, args), _) =>
      newcog(clazz, args)
      stay
  }
}
