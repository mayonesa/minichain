package io.iog.minichain.models

import zio.Task
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.*

object MinerSpec extends DefaultRunnableSpec:
  def spec = suite("miner spec")(
    test("multi-fiber mines") {
      Task.foreachPar(0 until 100) { i =>
        Miner.mine(i, Hash("hello".getBytes), Seq("2.1", "2.2"), Miner.StdMiningTargetNumber)
      }.map { blocks =>
        assertTrue(blocks.forall(_.minedProperly))
      }
    } @@ nonFlaky(5)
  )
