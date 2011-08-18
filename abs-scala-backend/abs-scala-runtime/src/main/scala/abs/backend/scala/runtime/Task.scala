package abs.backend.scala.runtime

import akka.actor.FSM
import akka.actor.{Actor, ActorRef}
import akka.event.EventHandler
import akka.serialization.RemoteActorSerialization
import scala.util.continuations._

object Runner {
  sealed abstract class Message
  case object Run extends Message
  
  val currentRunner: ThreadLocal[Runner] = new ThreadLocal()
}

//class Runner(private val cog: ActorRef, private val task: ActorRef, private val f: () => Any @suspendable, cogRemote: Array[Byte]) extends Actor {
class Runner(private val cog: Array[Byte], private val task: ActorRef, private val f: () => Any @suspendable) extends Actor {
  
  private var schedCont: Unit => Unit = null
  private var taskCont: Unit => Unit = null
  
  private var result: Any = _
  
  def await(cond: => Boolean): Unit @suspendable = {
    EventHandler.debug(this, "Runner awaiting")
    task ! new Task.Await(() => cond)

    shift {
      k : (Unit => Unit) => {        
        taskCont = k
        if (schedCont != null)
          schedCont()
      }
    }
  }
  
  def await(fut: ActorRef): Unit @suspendable = {
    EventHandler.debug(this, "Runner awaiting")
    fut ! new Task.Listen(cog)
    task ! new Task.Await(() => fut !! Task.Get match {
      case None => false
      case Some(x) => x.asInstanceOf[Option[Any]] match {
        case None => false
        case Some(_) => true
      }
    })

    shift {
      k : (Unit => Unit) => {        
        taskCont = k
        if (schedCont != null)
          schedCont()
      }
    }
  }
  
  private def start = reset {
    EventHandler.debug(this, "starting task")
    result = f()
    EventHandler.debug(this, "task done, retval: %s".format(result))
    task ! Task.Done(result)
    schedCont()
  }

  private def work {
    reset {
      EventHandler.debug(this, "task working")
      Runner.currentRunner.set(this)
			
	  shift {
        k : (Unit => Unit) => {
          schedCont = k
          
          if (taskCont == null) {
            EventHandler.debug(this, "task not started, starting")
            start
          }
          else {
            EventHandler.debug(this, "continuing suspended task")
            taskCont()
          }
        }
      }
      
      EventHandler.debug(this, "work done")
      Runner.currentRunner.remove
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
  //case class Run(cogRemote: Array[Byte]) extends TaskMessage
  case object Run extends TaskMessage
  case class Done(result: Any) extends TaskMessage
  case object Get extends TaskMessage
  case object CanRun extends TaskMessage
  case class Listen(task: Array[Byte]) extends TaskMessage
  case class Await(guard: () => Boolean) extends TaskMessage
  
  sealed trait State
  case object IDLE extends State
  case object RUNNING extends State
  case object DONE extends State
}

//class Task(private val cog: ActorRef, f: () => Any @suspendable) extends Actor with FSM[Task.State, Unit] {
class Task(private val cogRef: Array[Byte], f: () => Any @suspendable) extends Actor with FSM[Task.State, Unit] {
  import FSM._
  import Task._
  
  private var listeners: Set[ActorRef] = Set.empty
  
  private var result: Option[Any] = None
  
  private var guard: () => Boolean = (() => true)
 
  private var cog: ActorRef = RemoteActorSerialization.fromBinaryToRemoteActorRef(cogRef.asInstanceOf[Array[Byte]]) 
  private var runner: ActorRef = Actor.actorOf(new Runner(cogRef, self, f)).start

  self.id = self.uuid.toString
  
  // Initial state = IDLE
  startWith(IDLE, Unit)
  
  when(IDLE) {
    case Ev(CanRun) =>
      if (guard != null)
        self reply_? guard()
      else
        self reply_? false
      stay
      
    case Ev(Get) =>
      self reply_? None
      stay
      
    case Ev(Listen(cog)) =>
      EventHandler.debug(this, "listen request from %s".format(cog))
      listeners += RemoteActorSerialization.fromBinaryToRemoteActorRef(cog)
      stay
    
    case Ev(Task.Run) =>
      runner ! Runner.Run
      goto(RUNNING)
      
      /*
    case Ev(Task.Run(cogRemote)) =>
      if (runner == null)
        runner = Actor.actorOf(new Runner(cog, self, f, cogRemote)).start
      runner ! Runner.Run
      goto(RUNNING)
      */
  }
  
  when(RUNNING) {
    case Ev(CanRun) =>
      self reply_? false
      stay 
      
    case Ev(Get) =>
      self reply_? None
      stay
    
    case Ev(Listen(cog)) =>
      EventHandler.debug(this, "listen request from %s".format(cog))
      listeners += RemoteActorSerialization.fromBinaryToRemoteActorRef(cog)
      stay
        
    case Ev(Done(result)) =>
      EventHandler.debug(this, "Task done, result = %s".format(result))
      this.result = Some(result)
      listeners foreach (_ ! Cog.Work)
      cog ! Cog.Done
      goto(DONE)
      
    case Ev(Task.Await(guard)) =>
      this.guard = guard
      cog ! Cog.Done
      goto(IDLE)
  }
  
  when(DONE) {
    case Ev(CanRun) =>
      self reply_? false
      stay
      
    case Ev(Get) => 
      self reply_? result
      stay
      
    case Ev(Listen(cog)) =>
      RemoteActorSerialization.fromBinaryToRemoteActorRef(cog) ! Cog.Work
      stay
  }
  
  initialize
}
