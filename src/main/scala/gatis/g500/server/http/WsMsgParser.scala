package gatis.g500.server.http

trait Command

object Command {
  final case object DoNothing extends Command
  final case object OpenTable extends Command
  final case class JoinTable(tableId: TableId) extends Command
  final case class LeaveTable(tableId: TableId) extends Command
}

object WsMsgParser {
  import Command._

  def parse(input: String): Command = {
    val parts = input.split(" ")
    val base = parts(0)
    // TODO. use regex pattern matching to extract base and arguments
    base match {
      case "join_table" => JoinTable(TableId(parts(1)))
      case "leave_table" => LeaveTable(TableId(parts(1)))
      case "open_table" => OpenTable
      case _ => DoNothing
    }
  }

}
