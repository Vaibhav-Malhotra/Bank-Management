package BankingPkg.app

import BankingPkg.Actors.Bank
import BankingPkg.Actors.PersistentBankAccount.Command
import BankingPkg.http.BankRouter
import akka.NotUsed
import akka.actor.TypedActor.context
import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
//import akka.remote.transport.TestTransport.Behavior
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
//import akka.http.javadsl.Http
import akka.util.Timeout
import akka.http.scaladsl.Http

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object BankApp {

  def startHttpServer(bank: ActorRef[Command])(implicit system:ActorSystem[_])={
  implicit  val ec:ExecutionContext=system.executionContext
    val router=new BankRouter(bank)
     val routes=router.routes

    val httpBindingFuture=Http().newServerAt("localhost",8080).bind(routes)

    httpBindingFuture.onComplete{
      case Success(binding)=>
        val address=binding.localAddress
        system.log.info(s"Server online at http://{${address.getHostString}:${address.getPort}")

      case Failure(ex)=>
        system.log.error(s"Failed to bind HTTP server endpoint ,$ex ")
        system.terminate()

    }


  }

  def main(args:Array[String]): Unit =
  {
    trait RootCommand
    case class RetrieveBankAccount(replyTo:ActorRef[ActorRef[Command]]) extends RootCommand

    val rootBehavior:Behavior[RootCommand]=Behaviors.setup { context =>
      val bankActor=context.spawn(Bank(),"bank")

      Behaviors.receiveMessage{
        case RetrieveBankAccount(replyTo)=>
          replyTo! bankActor
          Behaviors.same
      }
    }

    implicit val system=ActorSystem(rootBehavior,"BankSystem")
    implicit val timeout:Timeout=Timeout(5.seconds)
    implicit  val ec:ExecutionContext=system.executionContext
    implicit val scheduler = system.scheduler

    val bankActorFuture:Future[ActorRef[Command]]=system.ask(replyTo => RetrieveBankAccount(replyTo))

    bankActorFuture.foreach(startHttpServer)

  }

}
