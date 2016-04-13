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
    val maxPerTile = 1 // Maximum number of observations per tile.

    def main(args: Array[String]) = {
        // Choose the dataset to cluster.
        val pathToMatrix = getClass.getResource("/4.csv").getPath()
        val matrixFile = new File(pathToMatrix)

        // Create a DenseMatrix from the CSV.
        val matrix = breeze.linalg.csvread(matrixFile)

        val firstTile = Tile(scala.Double.NegativeInfinity, scala.Double.NegativeInfinity, scala.Double.PositiveInfinity, scala.Double.PositiveInfinity)
        val tiles: List[Tile] = cut(matrix, firstTile, maxPerTile)

        println(tiles)

        val f = Figure()
        val p = f.subplot(0)
        p += scatter(matrix(::, 0), matrix(::, 1), {(_:Int) => 0.3}, {(_:Int) => Color.BLACK})

        val x = linspace(-10.0,10.0)
        p += plot(x, sin(x), '.')
        p += plot(x, cos(x))
        val y = DenseVector.fill(x.length){5.0}
        p += plot(x, y)

        p.title = "Tesselation tree"
        // f.saveas("image.png")
    }
}
