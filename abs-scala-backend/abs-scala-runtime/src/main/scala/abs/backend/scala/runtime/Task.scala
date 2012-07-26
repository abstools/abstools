/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala.runtime

import akka.actor.FSM
import akka.actor.{Actor, ActorRef}
import akka.event.Logging
import scala.util.continuations._
import akka.actor.Props
import akka.pattern.ask
import akka.dispatch.Await
import akka.util.Timeout
import akka.util.duration._
import akka.actor.ActorLogging

object Runner {
  sealed abstract class Message
  case object Run extends Message
  
  val currentRunner: ThreadLocal[Runner[_]] = new ThreadLocal()
}

class Runner[A](private val cog: ActorRef, private val task: ActorRef, private val f: () => A @suspendable) extends Actor with ActorLogging {
  private var schedCont: Unit => Unit = null
  private var taskCont: Unit => Unit = null
  
  private var result: A = _
  
  def await(cond: => Boolean): Unit @suspendable = {
    log.debug("Runner awaiting")
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
    log.debug("Runner %s".format(if (blocked) "blocked" else "waiting"))
    
    fut ! new Task.Listen(cog)
    task ! new Task.Await(() => {
      implicit val timeout = Timeout(5 seconds)
      Await.result(fut ? Task.Get, timeout.duration).asInstanceOf[Option[Any]] match {
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
    log.debug("starting task")
    result = f()
    log.debug("task done, retval: %s".format(result))
    Runner.currentRunner.remove
    task ! Task.Done(result)
    //schedCont()
  }

  private def work {
    if (taskCont == null) {
      log.debug("task not started, starting")
      start
    }
    else {
      log.debug("continuing suspended task")
      taskCont()
    }
  }
    
  def receive = {
    case Runner.Run =>
      log.debug("Runner running")
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
  case class Listen(task: ActorRef) extends TaskMessage
  case class Await(guard: () => Boolean, blocked: Boolean) extends TaskMessage
  
  sealed trait State
  case object IDLE extends State
  case object RUNNING extends State
  case object DONE extends State
  
  
  sealed trait Data
  case class Idle(listeners: Set[ActorRef], guard: () => Boolean) extends Data
  case class Running(listeners: Set[ActorRef]) extends Data
  case class Result(result: Any) extends Data
}

class Task[A](private val cog: ActorRef, f: () => A @suspendable) extends Actor with FSM[Task.State, Task.Data] {
  import FSM._
  import Task._
  
  //private val log = Logging(context.system, this)
  private val runner: ActorRef = context.actorOf(Props(new Runner(cog, self, f)), name = "runner")

  
  // Initial state = IDLE
  startWith(IDLE, Idle(Set.empty, () => true))
  
  when(IDLE) {
    case Event(CanRun, Idle(_, guard)) =>
      log.debug("checking guard")
      val b = if (guard != null) guard() else false
      log.debug("guard = %s".format(b))
      
      stay replying b
      
    case Event(Get, _) =>
      log.debug("get while task not complete")
      stay replying None
      
    case Event(Listen(cog), Idle(listeners, guard)) =>
      log.debug("listen request from %s".format(cog))
      stay using Idle(listeners + cog , guard)
    
    case Event(Task.Run, Idle(listeners, guard)) =>
      log.debug("running")
      runner ! Runner.Run
      goto(RUNNING) using Running(listeners)
  }
  
  when(RUNNING) {
    case Event(CanRun, _) =>
      log.debug("CanRun while already running")
      stay replying false 
      
    case Event(Get, _) =>
      log.debug("get while task not complete")
      stay replying None
    
    case Event(Listen(cog), Running(listeners)) =>
      log.debug("listen request from %s".format(cog))
      stay using Running(listeners + cog)
        
    case Event(Done(result), Running(listeners)) =>
      log.debug("Task done, result = %s".format(result))
      listeners foreach (_ ! Cog.Work)
      cog ! Cog.Done(true)
      goto(DONE) using Result(result)
      
    case Event(Task.Await(guard, blocked), Running(listeners)) =>
      log.debug("Task blocked")
      
      cog ! (if (blocked) Cog.Blocked else Cog.Done(false))
      
      goto(IDLE) using Idle(listeners, guard)
  }
  
  when(DONE) {
    case Event(CanRun, _) =>
      log.debug("CanRun when task finished")
      stay replying false
      
    case Event(Get, Result(result)) => 
      log.debug("get")
      stay replying Some(result)
      
    case Event(Listen(cog), _) =>
      log.debug("listen while task complete")
      cog ! Cog.Work
      stay
      
  }
  
  initialize
}
