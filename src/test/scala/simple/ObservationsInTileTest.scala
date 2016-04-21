import simple._
import org.scalatest._

import breeze.linalg._
import breeze.numerics._
import breeze.stats._

class ObservationsInTileTest extends FlatSpec with Matchers {
    "The observations in tiles " should "work" in {
        val matrix = DenseMatrix((-1.0, 4.0), (4.0, 0.0), (5.0, 3.0), (3.0, 5.0), (3.0, 6.0))
        val tile = Tile(0, 0, 5, 5)
        val axis = 0 // First column
        val result = observationsInTile(matrix, tile, axis)
        val correctObservations = DenseVector(4.0, 5.0, 3.0)
        result should be (correctObservations)
    }
}
