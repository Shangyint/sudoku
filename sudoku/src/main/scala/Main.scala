// based on http://www.cs.umd.edu/class/spring2019/cmsc388F/lectures/sudoku.html

object Sudoku extends App{
  type Grid = Matrix[Value]
  type Matrix[A] = List[Row[A]]
  type Row[A] = List[A]
  type Value = Char

  val boxSize: Int = 3
  val values: List[Char] = List.range(1, 10).map(i => i.toString()(0))
  val empty: Value => Boolean = _ == '.'
  def single[A]: List[A] => Boolean = _.length == 1
  
  val puzzle: Grid = List(
    "2....1.38",
    "........5",
    ".7...6...",
    ".......13",
    ".981..257",
    "31....8..",
    "9..8...2.",
    ".5..69784",
    "4..25...."
  ).map(_.toList)

  def valid(g: Grid): Boolean = {
    rows(g).forall(noDups(_)) &&
    cols(g).forall(noDups(_)) &&
    boxes(g).forall(noDups(_))  
  } 
  def noDups[A](a: List[A]): Boolean = a.distinct == a 

  def rows[A]: Matrix[A] => List[Row[A]] = identity
  def cols[A]: Matrix[A] => List[Row[A]] = _.transpose
  def boxes[A]: Matrix[A] => List[Row[A]] = {
    def split[B]: List[B] => List[List[B]] = chop(boxSize)
    def pack[B] = split.compose((x:Matrix[A]) => x.map(split))
    pack andThen (_.map(cols)) andThen (_.flatten) andThen (_.map(_.flatten)) 
  }
  def chop[A](n: Int)(a: List[A]): List[List[A]] = a match {
    case Nil => Nil
    case xs => xs.take(n) :: chop(n)(xs.drop(n))
  }

  type Choices = List[Value]

  def choices: Grid => Matrix[Choices] = g =>
    g.map(_.map(v => if (empty(v)) values else List(v)))

  def sequence[A]: List[List[A]] => List[List[A]] = x => x match {
    case head :: next => for {
      x <- head
      xs <- sequence(next)
    } yield (x::xs)
    case Nil => List(Nil)
  }

  def collapse[A]: Matrix[List[A]] => List[Matrix[A]] =
    sequence compose (_.map(sequence))

  def solveBrute: Grid => List[Grid] =
    choices andThen collapse andThen (_.filter(valid))

  def prune: Matrix[Choices] => Matrix[Choices] =
    pruneBy(rows) andThen pruneBy(cols) andThen pruneBy(boxes)

  def pruneBy[A]: (Matrix[Choices] => List[Row[Choices]]) =>
    Matrix[Choices] => Matrix[Choices] =
    f => f andThen (_.map(reduce)) andThen f

  def reduce: Row[Choices] => Row[Choices] =
    x => {
      val singles = x.filter(single).flatten.toSet
      def minus(x: Choices):Choices =
        if (single(x)) x else x.filterNot(singles)
      x.map(minus(_))
    }

  def solvePrune: Grid => List[Grid] =
    choices andThen prune andThen collapse andThen (_.filter(valid))

  def solveFixPrune: Grid => List[Grid] =
    choices andThen fix(prune) andThen collapse andThen (_.filter(valid))

  def fix[A]: (A => A) => A => A =
    f => (x => {
      val xprime = f(x)
      if (x == xprime) x else fix(f)(xprime)
    })
  
  def printSudoku: Grid => Unit = g =>
    g.foreach(r => {
      r.foreach(c => print(c + " "))
      println()
    })

  solveFixPrune(puzzle).foreach(printSudoku(_))

  // TODO refinement: search, backtracking, etc
}



object Main extends App {
  println("Hello, World!")
}