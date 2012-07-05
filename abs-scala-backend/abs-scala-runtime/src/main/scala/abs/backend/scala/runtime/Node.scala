/**
 * Copyright (c) 2009-2011, The HATS Consortium. All rights reserved. 
 * This file is licensed under the terms of the Modified BSD License.
 */
package abs.backend.scala.runtime

import akka.actor.{Actor, ActorRef}
import akka.event.Logging
import java.net.ServerSocket
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import akka.actor.Props
import akka.event.Logging

object NodeManager {
  sealed abstract class Message
  case object NewCog extends Message
  case object NewLocalCog extends Message
  private[runtime] case object Init extends Message
  
  def newNode(name : String): ActorRef = {
    val config = ConfigFactory.load()
    val system = ActorSystem("ABS-Scala", config.getConfig(name).withFallback(config))
    
    val actor = system.actorOf(Props[NodeManager], name = "NodeManager")
    
    actor
  }
  
  def main(argv: Array[String]) {
    newNode(argv(1))
  }
}

class NodeManager extends Actor {
  private val log = Logging(context.system, this)
  import NodeManager._
  
  private[runtime] def newCog = {
    log.debug("Allocating new COG")
      
    val cog = context.actorOf(Props[Cog])
    
    cog
  }
  
  def receive = {
    case NewCog =>
      
      sender ! newCog
      //self reply_? newCog
  }
}
