package dct

import scala.math._

/**
 * Created by kamyar on 8/25/15.
 */


trait DCT8 {

  val w1 = Seq(
    64, 64,    0,   0, 0, 0, 0, 0,
    89, 75,   50,  18, 0, 0, 0, 0,
    83, 36,    0,   0, 0, 0, 0, 0,
    75, -18, -89, -50, 0, 0, 0, 0,
    64, -64,   0,   0, 0, 0, 0, 0,
    50, -89,  18,  75, 0, 0, 0, 0,
    36, -83,   0,   0, 0, 0, 0, 0,
    18, -50,  75, -89, 0, 0, 0, 0)


  val coefficients = Array.fill(8*8)(0.0)
  for (j <- 0 until 4) {
    if(j<2)coefficients(j) = sqrt(1.0/8.0)*sqrt(0.5)
    for (i <- 8 until 64 by 8) {
      if(i/8%2==1 || j <2)
        coefficients(i + j) = sqrt(0.5) * 0.5 * cos(i * (j + 0.5) * Pi / 64.0)
    }
  }
  val w2 = coefficients.map(c => ((c + 0.5) *256).round.toInt - 128)


  val w = w1.toArray

  var addsub = 0

}