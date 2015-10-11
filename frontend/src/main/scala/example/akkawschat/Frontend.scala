package example.akkawschat

import org.scalajs.dom.raw._
import scala.scalajs.js
import org.scalajs.dom
import upickle._
import shared.Protocol._
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react._

object Frontend extends js.JSApp {

  // just a dumb websocket client
  class SocketService(name: String) {
    private def getWebsocketUri(document: Document, nameOfChatParticipant: String): String = {
      val wsProtocol = if (dom.document.location.protocol == "https:") "wss" else "ws"

      s"$wsProtocol://${dom.document.location.host}/chat?name=$nameOfChatParticipant"
    }

    private val client = new WebSocket(getWebsocketUri(dom.document, name))

    def listener(f: MessageEvent ⇒ Unit) =
      client.onmessage = { (event: MessageEvent) ⇒ f(event) }

    def sendMessage(m2send: String): Unit = client.send(m2send)

    def close(): Unit = client.close()
  }

  case class State(name: String, messageToSend: String, messages: List[ChatMessage], socket: Option[SocketService])

  class Backend($: BackendScope[Unit, State]) {
    def handleSubmit(e: ReactEventI) = {
      $.modState(_.copy(name = e.target.value))
    }

    def onNameChange(e: ReactEventI) =
      $.modState(_.copy(name = e.target.value))

    def onMessageChange(e: ReactEventI) =
      $.modState(_.copy(messageToSend = e.target.value))

    def sendMessage(e: ReactEventI) = {
      e.preventDefault()
      $.get().socket foreach { ws ⇒
        ws.sendMessage($.get().messageToSend)
      }
      $.modState(_.copy(messageToSend = ""))
    }

    def joinChat(e: ReactEventI) = {
      e.preventDefault()
      val socket = new SocketService($.get().name)
      $.modState(_.copy(socket = Some(socket)))
      // todo - attach to reactjs 'EventListener' api
      socket.listener(event ⇒ {
        val wsMsg = read[ChatMessage](event.data.toString)
        $.modState(s ⇒ s.copy(messages = s.messages :+ wsMsg))
      })
    }

    def isJoined = $.get().socket.isDefined
  }

  val JoinChat =
    ReactComponentB[(State, Backend)]("ChatName")
      .render(P ⇒ {
        val (s, b) = P
        <.form(^.onSubmit ==> b.joinChat,
          <.input(^.placeholder := "Who?", ^.onChange ==> b.onNameChange, ^.value := s.name),
          <.button("Join Chat"))
      })
      .build

  val SendMessage =
    ReactComponentB[(State, Backend)]("SendMessage")
      .render(P ⇒ {
        val (s, b) = P
        <.form(^.onSubmit ==> b.sendMessage,
          <.input(^.placeholder := "What?", ^.onChange ==> b.onMessageChange, ^.value := s.messageToSend),
          <.button("Send Message"))
      }).build

  val ConversationHistory =
    ReactComponentB[List[ChatMessage]]("ConversationHistory")
      .render(props ⇒ {
        def showMessage(message: ChatMessage) = <.li(s"[${message.sender}] - ${message.message}")
        <.ul(props map showMessage)
      }).build

  val Chat =
    ReactComponentB[Unit]("Chat")
      .initialState(State("", "", List(ChatMessage("admin", "Not connected yet")), None))
      .backend(new Backend(_))
      .render((_, S, B) ⇒ {
        <.div(
          JoinChat((S, B)),
          SendMessage((S, B)),
          ConversationHistory(S.messages))
      }).build

  def main(): Unit = {
    val reactground = dom.document.getElementById("reactground")
    React.render(Chat(Unit), reactground)
  }
}