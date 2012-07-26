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
import akka.dispatch.Await
import akka.util.Timeout
import akka.util.duration._
import akka.pattern.ask
import akka.actor.ActorLogging

object NodeManager {
  sealed abstract class Message
  case object NewCog extends Message
  private[runtime] case object Init extends Message
  
  def newNode(name: Option[String]) : ActorRef = {
  //def newNode(name : String): ActorRef = {
    val config = ConfigFactory.load()
    val system = ActorSystem("ABS-Scala", (name match {
      case Some(name) => config.getConfig(name).withFallback(config)
      case None => config
    }))
    
    val actor = system.actorOf(Props[NodeManager], name = "NodeManager")
    
    actor
  }
  
  def main(argv: Array[String]) {
    //newNode(argv(1))
    newNode(argv.headOption)
  }
  
  def bootstrap(clazz: Class[_ <: Actor], args: Seq[String]) {
    val node = newNode(args.headOption)
    
    implicit val timeout = Timeout(5 seconds)
    
    val cog = Await.result(node ? NewCog, timeout.duration).asInstanceOf[ActorRef]
    
    cog ! Cog.New(clazz, Seq.empty)
  }

}

class NodeManager extends Actor with ActorLogging {
  import NodeManager._
  
  private val cogIdCounter = new java.util.concurrent.atomic.AtomicInteger(0)
  
  private[runtime] def newCog = {
    log.debug("Allocating new COG")
      
    val cog = context.actorOf(Props(new Cog(this)), name = "COG-%d".format(cogIdCounter.incrementAndGet))
    
    cog
  }
  
  def receive = {
    case NewCog =>      
      sender ! newCog
      //self reply_? newCog
  }
}