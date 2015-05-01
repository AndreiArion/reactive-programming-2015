package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal( b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal{
      val d = delta()
      val aa = a()
      val bb = b()
      val cc = c()
      if (d==0) {Set(-bb/2*aa)}
      else if(d>0) Set((-bb+Math.sqrt(d))/2*aa,(-bb-Math.sqrt(d))/2*aa)
      else if(d==d)Set.empty else Set.empty
    }
  }
}
