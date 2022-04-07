package io.iog.minichain.models

import zio.{UIO, Task}
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.flaky

import org.scalactic.TripleEquals.convertToEqualizer

import Miner.{Genesis, StdMiningTargetNumber}

object BlockchainSpec extends DefaultRunnableSpec:
  private val singleFiber = suite("single-fiber fast blockchain spec")(
    test("empty") {
      assertM(Blockchain.empty.flatMap(_.findByIndex(0)))(isNone)
    },
    test("append") {
      for
        blockchain <- Blockchain.empty
        genesis    <- Genesis
        _          <- blockchain.append(genesis)
        res        <- blockchain.findByIndex(genesis.index)
      yield assertTrue(res.contains(genesis))
    },
    test("not find-by-index") {
      for
        blockchain <- Blockchain.empty
        genesis    <- Genesis
        _          <- blockchain.append(genesis)
        res        <- blockchain.findByIndex(1)
      yield assertTrue(res.isEmpty)
    },
    test("not find-by-hash") {
      for
        blockchain <- Blockchain.empty
        genesis    <- Genesis
        _          <- blockchain.append(genesis)
        res        <- blockchain.findByHash(Hash("wrong hash".getBytes))
      yield assertTrue(res.isEmpty)
    },
    test("not find-by-hash") {
      for
        blockchain <- Blockchain.empty
        genesis    <- Genesis
        _          <- blockchain.append(genesis)
        res        <- blockchain.findByHash(Hash("wrong hash".getBytes))
      yield assertTrue(res.isEmpty)
    },
    test("common ancestor") {
      for
        blockchain1  <- Blockchain.empty
        genesis      <- Genesis
        _            <- blockchain1.append(genesis)
        block2       =  Block(1, Hash("hello".getBytes), Seq("2.1", "2.2"), StdMiningTargetNumber, 1)
        _            <- blockchain1.append(block2)
        _            <- blockchain1.append(Block(2, Hash("good-bye".getBytes), Seq("1.3"), StdMiningTargetNumber, 2))
        blockchain2  <- Blockchain.empty
        _            <- blockchain2.append(genesis)
        _            <- blockchain2.append(block2)
        _            <- blockchain2.append(Block(2, Hash("good-bye, pt. 2".getBytes), Seq("2.3", "xsaction3"),
          StdMiningTargetNumber, 3))
        latestCommon <- blockchain1.latestCommon(blockchain2)
      yield assertTrue(latestCommon === block2)
    },
  )

  private val multiFiber = suite("multi-fiber fast blockchain spec")(
    test("multi find-by-index") {
      for
        blockchain <- Blockchain.empty
        genesis    <- Genesis
        _          <- blockchain.append(genesis)
        _          <- Task.foreachDiscard(1 until 100) { i =>
          val block = Block(i, Hash("hello".getBytes), Seq("2.1", "2.2"), StdMiningTargetNumber, i * 5)
          blockchain.append(block)
        }
        results    <- UIO.foreachPar(0 until 100) { i =>
          blockchain.findByIndex(i).map(_.get.index === i)
        }
      yield assertTrue(results.forall(identity))
    } @@ flaky(100),
  )

  def spec = suite("fast blockchain spec")(singleFiber, multiFiber)
