package gatis.g500.server.http

import gatis.g500.game.PlayerIndex

trait Command

object Command {
  final case object DoNothing extends Command
  final case object OpenTable extends Command
  final case class JoinTable(tableId: TableId) extends Command
  final case class LeaveTable(tableId: TableId) extends Command
  final case class PlayTurn(tableId: TableId, input: String) extends Command
}

object WsMsgParser {
  import Command._

  def parse(input: String): Command =
    input match {
      case s"join_table ${tableId}" => JoinTable(TableId(tableId)) // "join_table 5s2S4FAD2SF"
      case s"leave_table ${tableId}" => LeaveTable(TableId(tableId)) // "leave_table 5s2S4FAD2SF"
      case "open_table" => OpenTable // "open_table"
      case s"play_turn ${tableId} ${rest}" =>
        PlayTurn(TableId(tableId), rest) // "play_turn 5s2S4FAD2SF 100" , "play_turn 5s2S4FAD2SF Ad"
      case _ => DoNothing
    }

}
