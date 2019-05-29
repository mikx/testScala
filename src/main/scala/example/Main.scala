package example

import scala.collection.mutable

object Main extends App {

  val addr0 = "address_0"
  val addr1 = "address_1"
  val addr2 = "address_2"

  val c0 = new Coin(addr0, 100)
  val c1 = new Coin(addr1, 210)
  val c2 = new Coin(addr2, 110)
  val c3 = new Coin(addr0, 90)
  val c4 = new Coin(addr1, 210)

  val t0 = new Transaction(isCoinbase = true)
    .addOutput(c0)
    .addOutput(c1)

  val t1 = new Transaction()
    .addInput(c1)
    .addOutput(c2)
    .addOutput(c3)

  val t2 = new Transaction()
    .addInput(c0)
    .addInput(c2)
    .addOutput(c4)

  val b0 = new Block(1000)
    .addTransaction(t0)

  val b1 = new Block(2000)
    .addTransaction(t1)
    .addTransaction(t2)

  // blocks are in block_time order
  val blockchain = new Blockchain()
    .addBlock(b0)
    .addBlock(b1)

  /**
    * Find an address that received the most volume
    * Note1: interval is inclusive
    * Note2: there could be a tie
    * @param blockchain
    * @param interval_start
    * @param interval_end
    * @return address with maximum sum of amount of created coins
    */
  def find_maximum_inbound_volume_address(blockchain: Blockchain, interval_start: Long, interval_end: Long): String = {
    // holder for each address total
    val totals = mutable.HashMap[String, Long]().withDefault(_ => 0L)
    // select all blocks in range
    val blocks = blockchain.blocks.dropWhile(_.block_time < interval_start).takeWhile(_.block_time <= interval_end)
    // aggregate totals
    blocks.foreach { b =>
      b.transactions.foreach { t =>
        // add outputs (coins created)
        t.outputs.foreach { coin =>
          val current = totals(coin.address)
          totals.update(coin.address, current + coin.amount)
        }
      }
    }
    // find max value, return key
    val (address, value) = totals.maxBy(_._2)
    // return
    address
  }

  // test run
  val address = find_maximum_inbound_volume_address(blockchain, 0, 9999)
  assert(address == addr1)

  /**
    * Find all ancestors for a given coin
    * coins always have a back reference to a creating transaction
    * creating transaction inputs are parent coins for the current coin
    * @param coin
    * @return ancestors
    */
  def find_coinbase_ancestors(coin: Coin): Seq[Coin] = {

    // remember visited transactions
    val visited = mutable.HashSet[Transaction]()

    // return an empty collection if we have seen that transaction already
    val empty = Seq[Coin]()

    // helper method to recursively add ancestors
    def helper(current: Coin): Seq[Coin] = {
      val transaction = current.creator_transaction.get  // can not be null
      if (visited.contains(transaction) || transaction.isCoinbase) empty  // return empty sequence
      else {
        // add transaction to the visited set
        visited.add(transaction)
        // add inputs to ancestors + recursively search for ancestors
        transaction.inputs ++ transaction.inputs.map(helper).flatten
      }
    }
    // call our helper method, return its result
    helper(coin)
  }

  // test run
  assert(find_coinbase_ancestors(c4).size == 3)
  assert(find_coinbase_ancestors(c3).size == 1)
  assert(find_coinbase_ancestors(c2).size == 1)
  assert(find_coinbase_ancestors(c1).size == 0)
  assert(find_coinbase_ancestors(c0).size == 0)

}


//
//  Model
//

class Blockchain(val blocks: mutable.ArrayBuffer[Block] = mutable.ArrayBuffer())
{
  def addBlock(block: Block): Blockchain = {
    blocks += block
    this
  }
}

class Block(
  val block_time: Long,
  val transactions: mutable.ArrayBuffer[Transaction] = mutable.ArrayBuffer())
{
  def addTransaction(transaction: Transaction): Block = {
    transaction.block = Some(this)
    transactions += transaction
    this
  }
}

class Transaction(
  var block: Option[Block] = None,
  val isCoinbase: Boolean = false,
  val inputs: mutable.ArrayBuffer[Coin] = mutable.ArrayBuffer(),
  val outputs: mutable.ArrayBuffer[Coin] = mutable.ArrayBuffer())
{
  def addInput(coin: Coin): Transaction = {
    coin.spender_transaction = Some(this)
    inputs += coin
    this
  }
  def addOutput(coin: Coin): Transaction = {
    coin.creator_transaction = Some(this)
    outputs += coin
    this
  }
}

class Coin(
   val address: String,
   val amount: Long,
   var creator_transaction: Option[Transaction] = None,
   var spender_transaction: Option[Transaction] = None)
{
  override def toString() = s"Address: $address, amount: $amount"
}
