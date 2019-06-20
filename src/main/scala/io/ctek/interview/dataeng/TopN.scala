package io.ctek.interview.dataeng

import com.yahoo.sketches.frequencies.{ErrorType, ItemsSketch}

import scala.annotation.tailrec
import scala.collection.immutable.Stream.Empty

class TopN {
  def findTopN(n: Int)(stream: Stream[Int]): List[Int] = {

    @tailrec
    def processStream(sketch: ItemsSketch[Int], currentStream: Stream[Int]): ItemsSketch[Int] = {
      currentStream match {
        case Empty =>
          sketch
        case head #:: tail =>
          sketch.update(head, head.asInstanceOf[Long])
          processStream(sketch, tail)
      }
    }

    val maxMapSize = 1024
    val sketch = new ItemsSketch[Int](maxMapSize)

    processStream(sketch, stream)
      .getFrequentItems(ErrorType.NO_FALSE_POSITIVES)
      .take(n)
      .map(_.getItem)
      .toList
  }
}
