package abs.backend.scala.runtime

import akka.actor.{Actor, ActorRef}
import akka.event.EventHandler
import akka.serialization.RemoteActorSerialization

object NodeManager {
  sealed abstract class Message
  case object NewCog extends Message
}

class NodeManager(private val host: String, private val port: Int) extends Actor {
  import NodeManager._
  
  private val server = Actor.remote.start(host, port)
  
  def receive = {
    case NewCog => 
      EventHandler.debug(this, "Allocating new COG")
      
      val cog = Actor.actorOf(new Cog(server))
      server.registerByUuid(cog)
      val remoteRef = RemoteActorSerialization.toRemoteActorRefProtocol(Actor.remote.actorFor(cog.uuid.toString, host, port)).toByteArray
      
      cog ! new Cog.RemoteSelfRef(remoteRef)
      
      self reply_? remoteRef
  }
}
