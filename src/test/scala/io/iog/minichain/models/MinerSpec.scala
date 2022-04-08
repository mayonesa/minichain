package io.iog.minichain.models

import zio.Task
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.nonFlaky

object MinerSpec extends DefaultRunnableSpec:
  def spec = suite("miner spec")(
    test("mines") {
      assertM(Task.foreach(0 until 100) { i =>
        Miner.mine(i, Hash("hello".getBytes), Seq("2.1", "2.2"), Miner.StdMiningTargetNumber).map(_.minedProperly)
      })(forall(isTrue))
    } @@ nonFlaky(100)
  )