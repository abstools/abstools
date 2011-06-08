package abs.backend.scala.runtime

import akka.actor.{Actor, ActorRef, FSM}
import akka.event.EventHandler
import akka.remoteinterface.RemoteServerModule
import akka.serialization.RemoteActorSerialization

object Cog {
  sealed abstract class Message
  case class RemoteSelfRef(ref: Array[Byte]) extends Message
  case class Run(task: ActorRef) extends Message
  case object Work extends Message
  case object Done extends Message
  case class New[T <: Actor](clazz: Class[T], args: Seq[Any]) extends Message
  
  sealed trait State
  case object INIT extends State
  case object IDLE extends State
  case object BUSY extends State
}

class Cog(val server: RemoteServerModule) extends Actor with FSM[Cog.State, Unit]{
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
      clazz.getMethod("init", classOf[Seq[Any]]).invoke(obj, args)
      obj
    }
    server.registerByUuid(actor)
    
    val remoteRef = RemoteActorSerialization.toRemoteActorRefProtocol(Actor.remote.actorFor(actor.uuid.toString, server.address.getAddress.getHostAddress(), server.address.getPort)).toByteArray 
    
    actor ! MyObject.RemoteSelfRef(remoteRef)
    
    actor ! MyObject.Run
    
    self reply_? remoteRef 
  }
  
  startWith(INIT, Unit)
  
  when(INIT) {
    case Ev(RemoteSelfRef(ref)) =>
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
          task ! Task.Run(remoteSelfRef)
          goto(BUSY)  
      }
            
    case Ev(Run(task)) =>
      EventHandler.debug(this, "New task received (while passive)")
      tasks ::= task.start
      server.registerByUuid(task)
      self reply_? RemoteActorSerialization.toRemoteActorRefProtocol(
        Actor.remote.actorFor(task.uuid.toString, server.address.getAddress.getHostAddress(), server.address.getPort)).toByteArray
      self ! Work
      stay
      
    case Ev(New(clazz, args)) =>
      newobj(clazz, args)
      stay
  }
  
  when(BUSY) {
    case Ev(Work) => // ignore
      EventHandler.debug(this, "Work received while already working")
      stay
      
    case Ev(Run(task)) =>
      EventHandler.debug(this, "New task received (while working)")
      tasks ::= task.start
      server.registerByUuid(task)
      self reply_? RemoteActorSerialization.toRemoteActorRefProtocol(
        Actor.remote.actorFor(task.uuid.toString, server.address.getAddress.getHostAddress(), server.address.getPort)).toByteArray
      stay
      
    case Ev(New(clazz, args)) =>
      newobj(clazz, args)
      stay
      
    case Ev(Done) =>
      EventHandler.debug(this, "Task finished, awakening myself")
      self ! Work
      goto(IDLE)
  }
}
