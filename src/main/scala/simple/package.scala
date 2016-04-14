import breeze.linalg._
import breeze.numerics._
import breeze.stats._

package object simple {
    val meuh = "a"
    def numberOfObservationsInTile(matrix: DenseMatrix[Double], tile: Tile): Int = {
        var observations = 0
        (0 until matrix.rows).map{ row =>
            if (matrix(row, 0) >= tile.minX && matrix(row, 0) <= tile.maxX && matrix(row, 1) >= tile.minY && matrix(row, 1) <= tile.maxY) {
                observations += 1
            }
        }
        return observations
    }

    def minMeanMax(matrix: DenseMatrix[Double], tile: Tile): (DenseVector[Double], DenseVector[Double], DenseVector[Double]) = {
        var min = DenseVector.fill(2){scala.Double.PositiveInfinity}
        var mean = DenseVector.zeros[Double](2)
        var max = DenseVector.fill(2){scala.Double.NegativeInfinity}
        var observations = 0
        (0 until matrix.rows).map{ row =>
            if (matrix(row, 0) >= tile.minX && matrix(row, 0) <= tile.maxX && matrix(row, 1) >= tile.minY && matrix(row, 1) <= tile.maxY) {
                observations += 1
                mean(0) += matrix(row, 0)
                mean(1) += matrix(row, 1)
                if (min(0) > matrix(row, 0)) {
                    min(0) = matrix(row, 0)
                }
                if (min(1) > matrix(row, 1)) {
                    min(1) = matrix(row, 1)
                }
                if (max(0) < matrix(row, 0)) {
                    max(0) = matrix(row, 0)
                }
                if (max(1) < matrix(row, 1)) {
                    max(1) = matrix(row, 1)
                }
            }
        }
        mean(0) /= observations
        mean(1) /= observations
        return (min, mean, max)
    }

    def cut(matrix: DenseMatrix[Double], parentTile: Tile, maxPerTile: Int): List[Tile] = {
        // If there are too many observations in the tile, we divide it.
        if (numberOfObservationsInTile(matrix, parentTile) > maxPerTile) {
            val (minCols, meanCols, maxCols) = minMeanMax(matrix, parentTile)
            // val ratioX = ratio(dist(minCols(0), meanCols(0)), dist(meanCols(0), maxCols(0)))
            // val ratioY = ratio(dist(minCols(1), meanCols(1)), dist(meanCols(1), maxCols(1)))
            if (dist(minCols(0), maxCols(0)) >= dist(minCols(1), maxCols(1))) { // Dividing the tile vertically is better
                val tileLeft = Tile(parentTile.minX, parentTile.minY, meanCols(0), parentTile.maxY)
                val tileRight = Tile(meanCols(0), parentTile.minY, parentTile.maxX, parentTile.maxY)
                // We use matrix instead of observations to introduce neighborhoods later.
                return List.concat(cut(matrix, tileLeft, maxPerTile), cut(matrix, tileRight, maxPerTile))
            } else { // Dividing the title horizontally makes more sense.
                val tileBottom = Tile(parentTile.minX, parentTile.minY, parentTile.maxX, meanCols(1))
                val tileTop = Tile(parentTile.minX, meanCols(1), parentTile.maxX, parentTile.maxY)
                return List.concat(cut(matrix, tileBottom, maxPerTile), cut(matrix, tileTop, maxPerTile))
            }
        } else {
            return List(parentTile)
        }
    }

    def dist(a: Double, b: Double): Double = {
        return sqrt(pow(a - b, 2))
    }

    def ratio(a: Double, b: Double): Double = {
        if (a > b) {
            return a / b
        } else {
            return b / a
        }
    }
}
