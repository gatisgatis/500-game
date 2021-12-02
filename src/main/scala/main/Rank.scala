package main

sealed trait Rank {
  def toString: String
  val value: Int
}

object Rank {
  object Nine extends Rank {
    override def toString: String = "9"
    val value: Int = 0
  }
  object Ten extends Rank {
    override def toString: String = "T"
    val value: Int = 10
  }
  object Jack extends Rank {
    override def toString: String = "J"
    val value: Int = 2
  }
  object Queen extends Rank {
    override def toString: String = "Q"
    val value: Int = 3
  }
  object King extends Rank {
    override def toString: String = "K"
    val value: Int = 4
  }
  object Ace extends Rank {
    override def toString: String = "A"
    val value: Int = 11
  }
  val all: Set[Rank] = Set(Nine, Ten, Jack, Queen, King, Ace)
}
