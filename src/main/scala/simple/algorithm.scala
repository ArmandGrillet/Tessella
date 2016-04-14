package simple

import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import breeze.stats._
import java.awt.{Color, Paint}
import java.io.File
import scala.io.Source

case class Tile(minX: Double, minY: Double, maxX: Double, maxY: Double)

object Algorithm {
    // Parameters.
    val maxPerTile = 2 // Maximum number of observations per tile.

    def main(args: Array[String]) = {
        // Choose the dataset to cluster.
        val pathToMatrix = getClass.getResource("/noob.csv").getPath()
        val matrixFile = new File(pathToMatrix)

        // Create a DenseMatrix from the CSV.
        val matrix = breeze.linalg.csvread(matrixFile)

        val firstTile = Tile(scala.Double.NegativeInfinity, scala.Double.NegativeInfinity, scala.Double.PositiveInfinity, scala.Double.PositiveInfinity)
        val tiles: List[Tile] = cut(matrix, firstTile, maxPerTile)

        val f = Figure()
        val p = f.subplot(0)
        val minCols = min(matrix(::, *))
        val maxCols = max(matrix(::, *))
        p.xlim(minCols(0) - 2, maxCols(0) + 2)
        p.ylim(minCols(1) - 2, maxCols(1) + 2)

        p += scatter(matrix(::, 0), matrix(::, 1), {(_:Int) => 0.3}, {(_:Int) => Color.BLACK}) // Display the observations.


        for (tile <- tiles) {
            var (modifiableMinX, modifiableMinY, modifiableMaxX, modifiableMaxY) = (tile.minX, tile.minY, tile.maxX, tile.maxY)

            if (modifiableMinX == scala.Double.NegativeInfinity) {
                modifiableMinX = minCols(0) - 2
            }
            if (modifiableMinY == scala.Double.NegativeInfinity) {
                modifiableMinY = minCols(1) - 2
            }
            if (modifiableMaxX == scala.Double.PositiveInfinity) {
                modifiableMaxX = maxCols(0) + 2
            }
            if (modifiableMaxY == scala.Double.PositiveInfinity) {
                modifiableMaxY = maxCols(1) + 2
            }

            val x = linspace(modifiableMinX, modifiableMaxX)
            val top = DenseVector.fill(x.length){modifiableMaxY}
            val bottom = DenseVector.fill(x.length){modifiableMinY}
            p += plot(x, top)
            p += plot(x, bottom)

            val y = linspace(modifiableMinY, modifiableMaxY)
            val left = DenseVector.fill(y.length){modifiableMinX}
            val right = DenseVector.fill(y.length){modifiableMaxX}
            p += plot(left, y)
            p += plot(right, y)
        }

        p.title = "Tesselation tree"
        // f.saveas("image.png")
    }
}
