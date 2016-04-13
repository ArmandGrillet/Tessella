import breeze.linalg._
import breeze.numerics._
import breeze.stats._

package object simple {
    def numberOfObservationsInTile(matrix: DenseMatrix[Double], tile: Tile): Int = {
        var observations = 0
        (0 until matrix.rows).map{ row =>
            if (matrix(row, 0) >= tile.minX && matrix(row, 0) <= tile.maxX && matrix(row, 1) >= tile.minY && matrix(row, 1) <= tile.maxY) {
                observations += 1
            }
        }
        return observations
    }

    def cut(matrix: DenseMatrix[Double], parentTile: Tile, maxPerTile: Int): List[Tile] = {
        // If there are too many observations in the tile, we divide it.
        if (numberOfObservationsInTile(matrix, parentTile) > maxPerTile) {
            val minCols = min(matrix(::, *))
            val meanCols = mean(matrix(::, *))
            val maxCols = max(matrix(::, *))
            val ratioX = ratio(dist(maxCols(0), meanCols(0)), dist(meanCols(0), maxCols(0)))
            val ratioY = ratio(dist(maxCols(1), meanCols(1)), dist(meanCols(1), maxCols(1)))
            if (dist(ratioX, 1.0) < dist(ratioY, 1.0)) { // Dividing the tile vertically is better
                val tileLeft = Tile(parentTile.minX, parentTile.minY, meanCols(0), parentTile.maxY)
                val tileRight = Tile(meanCols(0), parentTile.minY, parentTile.maxX, parentTile.maxY)
                return cut(matrix, tileLeft, maxPerTile) ::: cut(matrix, tileRight, maxPerTile)
            } else { // Dividing the title horizontally makes more sense.
                val tileBottom = Tile(parentTile.minX, parentTile.minY, parentTile.maxX, meanCols(1))
                val tileTop = Tile(parentTile.minX, meanCols(1), parentTile.maxX, parentTile.maxY)
                return cut(matrix, tileBottom, maxPerTile) ::: cut(matrix, tileTop, maxPerTile)
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
