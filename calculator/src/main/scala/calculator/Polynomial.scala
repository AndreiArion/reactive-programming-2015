package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal( b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val d = computeDelta(a,b,c)()
      if (d==0) {Set(-b() / (2 * a()))}
      else if(d>0) Set((-b() +Math.sqrt(d))/(2 * a()),(-b() -Math.sqrt(d))/(2 * a()))
      else if(d==d)Set.empty else Set.empty
    }
  }
}
