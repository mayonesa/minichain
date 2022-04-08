package io.iog.minichain.models

import zio.{Task, UIO}
import zio.test.*
import zio.test.Assertion.*
import org.scalactic.TripleEquals.convertToEqualizer
import Miner.StdMiningTargetNumber
import zio.test.TestAspect.*

object BlockchainSpec extends DefaultRunnableSpec:
  private val Genesis = Block(0, Hash("hello".getBytes), Seq("1.1", "1.2"), StdMiningTargetNumber, 1)

  private val singleFiber = suite("single-fiber fast blockchain spec")(
    test("empty") {
      assertM(Blockchain.empty.flatMap(_.findByIndex(0)))(isNone)
    },
    test("append") {
      for
        blockchain <- Blockchain.empty
        _          <- blockchain.append(Genesis)
        res        <- blockchain.findByIndex(Genesis.index)
      yield assertTrue(res.contains(Genesis))
    } @@ nonFlaky(10),
    test("not find-by-index") {
      for
        blockchain <- Blockchain.empty
        _          <- blockchain.append(Genesis)
        res        <- blockchain.findByIndex(1)
      yield assertTrue(res.isEmpty)
    } @@ nonFlaky(10),
    test("not find-by-hash") {
      for
        blockchain <- Blockchain.empty
        _          <- blockchain.append(Genesis)
        res        <- blockchain.findByHash(Hash("wrong hash".getBytes))
      yield assertTrue(res.isEmpty)
    } @@ nonFlaky(10),
    test("not find-by-hash") {
      for
        blockchain <- Blockchain.empty
        _          <- blockchain.append(Genesis)
        res        <- blockchain.findByHash(Hash("wrong hash".getBytes))
      yield assertTrue(res.isEmpty)
    } @@ nonFlaky(10),
    test("common ancestor") {
      for
        blockchain1  <- Blockchain.empty
        _            <- blockchain1.append(Genesis)
        block2       =  Block(1, Hash("hello".getBytes), Seq("2.1", "2.2"), StdMiningTargetNumber, 1)
        _            <- blockchain1.append(block2)
        _            <- blockchain1.append(Block(2, Hash("good-bye".getBytes), Seq("1.3"), StdMiningTargetNumber, 2))
        blockchain2  <- Blockchain.empty
        _            <- blockchain2.append(Genesis)
        _            <- blockchain2.append(block2)
        _            <- blockchain2.append(Block(2, Hash("good-bye, pt. 2".getBytes), Seq("2.3", "xsaction3"),
          StdMiningTargetNumber, 3))
        latestCommon <- blockchain1.latestCommon(blockchain2)
      yield assertTrue(latestCommon === block2)
    } @@ nonFlaky(10),
  )

  private val multiFiber = suite("multi-fiber fast blockchain spec")(
    test("multi find-by-index") {
      for
        blockchain <- Blockchain.empty
        _          <- Task.foreachDiscard(0 until 100) { i =>
          val block = Block(i, Hash("hello".getBytes), Seq("2.1", "2.2"), StdMiningTargetNumber, i * 5)
          blockchain.append(block)
        }
        results    <- UIO.foreachPar(0 until 100) { i =>
          blockchain.findByIndex(i).map(_.get.index === i)
        }
      yield assertTrue(results.forall(bool => bool))
    } @@ nonFlaky(10),
  )

  def spec = suite("fast blockchain spec")(singleFiber, multiFiber)
