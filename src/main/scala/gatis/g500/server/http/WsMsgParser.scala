package gatis.g500.server.http

import main.PlayerIndex

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

  def parse(input: String): Command = {
    val parts = input.split(" ")
    val base = parts(0)
    // TODO. use regex pattern matching to extract base and arguments
    base match {
      case "join_table" => JoinTable(TableId(parts(1))) // "join_table 5s2S4FAD2SF"
      case "leave_table" => LeaveTable(TableId(parts(1))) // "leave_table 5s2S4FAD2SF"
      case "open_table" => OpenTable // "open_table"
      case "play_turn" =>
        PlayTurn(TableId(parts(1)), parts(2)) // "play_turn 5s2S4FAD2SF 100" , "play_turn 5s2S4FAD2SF Ad"
      case _ => DoNothing
    }
  }

}
