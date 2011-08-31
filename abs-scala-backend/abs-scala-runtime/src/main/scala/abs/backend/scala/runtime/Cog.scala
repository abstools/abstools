/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala.runtime

import akka.actor.{Actor, ActorRef, FSM}
import akka.event.EventHandler
import akka.remoteinterface.RemoteServerModule
import akka.serialization.RemoteActorSerialization
import scala.util.continuations._

object Cog {
  sealed abstract class Message
  case class RemoteSelfRef(ref: Array[Byte]) extends Message
  case class Run(block: () => Any @suspendable) extends Message
  case object Work extends Message
  case object Done extends Message
  case class New[T <: Actor](clazz: Class[T], args: Seq[Any]) extends Message
  case class NewCog[T <: Actor](clazz: Class[T], args: Seq[Any]) extends Message
  
  sealed trait State
  case object INIT extends State
  case object IDLE extends State
  case object BUSY extends State
}

class Cog(val server: NodeManager) extends Actor with FSM[Cog.State, Unit]{
  import Cog._
  import FSM._
  
  private var tasks: List[ActorRef] = Nil
  private var remoteSelfRef: Array[Byte] = _
  
  self.id = self.uuid.toString
  
  private def pickTask: Option[ActorRef] =
    tasks.filter(_ !! Task.CanRun match {
      case None =>
        EventHandler.warning(this, "Error: no reply from task for IsSatisfied")
        false
      case Some(x) => x.asInstanceOf[Boolean]
      }).headOption
  
      
  private def newobj[T <: Actor](clazz: Class[T], args: Seq[Any]) {
    EventHandler.debug(this, "creating new concurrent object for %s".format(clazz))
    
    val actor = Actor.actorOf {
      val obj = clazz.getConstructor(classOf[ActorRef]).newInstance(self)
      // does it have an init method?
      try {
        EventHandler.debug(this, "invoking init method with args %s".format(args.mkString(", ")))
    	  clazz.getMethod("init", classOf[Array[Any]]).invoke(obj, args.toArray)
      } catch {
        case e: NoSuchMethodException => // nope
          EventHandler.debug(this, "no init method in class")
      }
      obj
    }.start()
    server.registerByUuid(actor)
    
    
    val remoteActor = Actor.remote.actorFor(actor.uuid.toString, server.host, server.port)
    
    val remoteRef = RemoteActorSerialization.toRemoteActorRefProtocol(remoteActor).toByteArray
    
    actor ! new MyObject.RemoteSelfRef(remoteRef)
    actor ! MyObject.Run
    
    self reply_? remoteRef 
  }
  
  private def newtask(block: () => Any @suspendable) {
    val task = Actor.actorOf(new Task(remoteSelfRef, block))
    tasks ::= task.start
    server.registerByUuid(task)
    self reply_? RemoteActorSerialization.toRemoteActorRefProtocol(
      Actor.remote.actorFor(task.uuid.toString, 
          server.host, 
          server.port)).toByteArray
  }
  
  private def newcog[T <: Actor](clazz: Class[T], args: Seq[Any]) = {
    // ask for a new cog from node manager
    val (newCog, newCogRemote) = server.newCog
    newCog.forward(New(clazz, args))
  }
  
  startWith(INIT, Unit)
  
  when(INIT) {
    case Ev(RemoteSelfRef(ref)) =>
      EventHandler.debug(this, "COG initializing")
      this.remoteSelfRef = ref
      goto(IDLE)
  }
  
  when(IDLE) {
    case Ev(Work) =>
      EventHandler.debug(this, "Awakened, looking for something to do")
      // pick something that can run
      pickTask match {
        case None =>
          EventHandler.debug(this, "Nothing to do, sleeping")
          stay
        case Some(task) =>
          EventHandler.debug(this, "Activating task " + task)
          //task ! Task.Run(remoteSelfRef)
          task ! Task.Run
          goto(BUSY)  
      }
            
    case Ev(Run(block)) =>
      EventHandler.debug(this, "New task received (while passive)")
      
      newtask(block)
      self ! Work
      stay
      
    case Ev(New(clazz, args)) =>
      newobj(clazz, args)
      stay
      
    case Ev(NewCog(clazz, args)) =>
      newcog(clazz, args)
      stay
  }
  
  when(BUSY) {
    case Ev(Work) => // ignore
      EventHandler.debug(this, "Work received while already working")
      stay
      
    case Ev(Run(block)) =>
      EventHandler.debug(this, "New task received (while working)")
      
      newtask(block)
      stay
      
    case Ev(New(clazz, args)) =>
      newobj(clazz, args)
      stay
      
    case Ev(NewCog(clazz, args)) =>
      newcog(clazz, args)
      stay
      
    case Ev(Done) =>
      EventHandler.debug(this, "Task finished, awakening myself")
      self ! Work
      goto(IDLE)
  }
}
