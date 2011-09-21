/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala.runtime

import akka.actor.FSM
import akka.actor.{Actor, ActorRef}
import akka.event.EventHandler
import akka.serialization.RemoteActorSerialization
import scala.util.continuations._

object Runner {
  sealed abstract class Message
  case object Run extends Message
  
  val currentRunner: ThreadLocal[Runner[_]] = new ThreadLocal()
}

class Runner[A](private val cog: Array[Byte], private val task: ActorRef, private val f: () => A @suspendable) extends Actor {
  
  private var schedCont: Unit => Unit = null
  private var taskCont: Unit => Unit = null
  
  private var result: A = _
  
  def await(cond: => Boolean): Unit @suspendable = {
    EventHandler.debug(this, "Runner awaiting")
    task ! new Task.Await(() => cond, false)

    shift {
      k : (Unit => Unit) => {        
        taskCont = k
        Runner.currentRunner.remove
        //if (schedCont != null)
        //  schedCont()
      }
    }
  }
  
  def await(fut: ActorRef, blocked: Boolean = false): Unit @suspendable = {
    EventHandler.debug(this, "Runner %s".format(if (blocked) "blocked" else "waiting"))
    
    fut ! new Task.Listen(cog)
    task ! new Task.Await(() => fut !! Task.Get match {
      case None => false
      case Some(x) => x.asInstanceOf[Option[Any]] match {
        case None => false
        case Some(_) => true
      }
    }, blocked)

    shift {
      k : (Unit => Unit) => {        
        taskCont = k
        Runner.currentRunner.remove
      }
    }
  }
  
  private def start = reset {
    EventHandler.debug(this, "starting task")
    result = f()
    EventHandler.debug(this, "task done, retval: %s".format(result))
    Runner.currentRunner.remove
    task ! Task.Done(result)
    //schedCont()
  }

  private def work {
    if (taskCont == null) {
      EventHandler.debug(this, "task not started, starting")
      start
    }
    else {
      EventHandler.debug(this, "continuing suspended task")
      taskCont()
    }
  }
    
  def receive = {
    case Runner.Run =>
      EventHandler.debug(this, "Runner running")
      Runner.currentRunner.set(this)
      work
  
    case x =>
      println("Runner: unknown message " + x)
  }
}

object Task {
  sealed abstract class TaskMessage
  case object Run extends TaskMessage
  case class Done(result: Any) extends TaskMessage
  case object Get extends TaskMessage
  case object CanRun extends TaskMessage
  case class Listen(task: Array[Byte]) extends TaskMessage
  case class Await(guard: () => Boolean, blocked: Boolean) extends TaskMessage
  
  sealed trait State
  case object IDLE extends State
  case object RUNNING extends State
  case object DONE extends State
}

class Task[A](private val cogRef: Array[Byte], f: () => A @suspendable) extends Actor with FSM[Task.State, Unit] {
  import FSM._
  import Task._
  
  private var listeners: Set[ActorRef] = Set.empty
  
  private var result: Option[Any] = None
  
  private var guard: () => Boolean = (() => true)
 
  private var cog: ActorRef = RemoteActorSerialization.fromBinaryToRemoteActorRef(cogRef) 
  private var runner: ActorRef = Actor.actorOf(new Runner(cogRef, self, f)).start

  self.id = self.uuid.toString
  
  // Initial state = IDLE
  startWith(IDLE, Unit)
  
  when(IDLE) {
    case Ev(CanRun) =>
      EventHandler.debug(this, "[%s] checking guard".format(self))
      if (guard != null) {
        val b = guard()
        EventHandler.debug(this, "[%s] guard = %s".format(self, b))
        self reply_? guard()
      }
      else
        self reply_? false
      stay
      
    case Ev(Get) =>
      EventHandler.debug(this, "[%s] get while task not complete".format(self))
      self reply_? None
      stay
      
    case Ev(Listen(cog)) =>
      EventHandler.debug(this, "[%s] listen request from %s".format(self, cog))
      listeners += RemoteActorSerialization.fromBinaryToRemoteActorRef(cog)
      stay
    
    case Ev(Task.Run) =>
      EventHandler.debug(this, "[%s] running".format(self))
      runner ! Runner.Run
      goto(RUNNING)
  }
  
  when(RUNNING) {
    case Ev(CanRun) =>
      EventHandler.debug(this, "[%s] CanRun while already running".format(self))
      self reply_? false
      stay 
      
    case Ev(Get) =>
      EventHandler.debug(this, "[%s] get while task not complete".format(self))
      self reply_? None
      stay
    
    case Ev(Listen(cog)) =>
      EventHandler.debug(this, "[%s] listen request from %s".format(self, cog))
      listeners += RemoteActorSerialization.fromBinaryToRemoteActorRef(cog)
      stay
        
    case Ev(Done(result)) =>
      EventHandler.debug(this, "[%s] Task done, result = %s".format(self, result))
      this.result = Some(result)
      listeners foreach (_ ! Cog.Work)
      cog ! Cog.Done(true)
      goto(DONE)
      
    case Ev(Task.Await(guard, blocked)) =>
      EventHandler.debug(this, "[%s] Task blocked".format(self))
      this.guard = guard
      
      cog ! (if (blocked) Cog.Blocked else Cog.Done(false))
      
      goto(IDLE)
  }
  
  when(DONE) {
    case Ev(CanRun) =>
      EventHandler.debug(this, "[%s] CanRun when task finished".format(self))
      self reply_? false
      stay
      
    case Ev(Get) => 
      EventHandler.debug(this, "[%s] get".format(self))
      self reply_? result
      stay
      
    case Ev(Listen(cog)) =>
      EventHandler.debug(this, "[%s] listen while task complete".format(self))
      RemoteActorSerialization.fromBinaryToRemoteActorRef(cog) ! Cog.Work
      stay
  }
  
  initialize
}
