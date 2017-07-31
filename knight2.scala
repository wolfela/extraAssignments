// Part 2 about finding a single tour for a board
//================================================

// copy any function you need from file knight1.scala

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions


//(2a) Implement a first-function that finds the first 
// element, say x, in the list xs where f is not None. 
// In that case return f(x), otherwise none.

def first(xs: List[Pos], f: Pos => Option[Path]): Option[Path] = xs match {
  case Nil => None
  case x::xs => {
    val fx = f(x)
    if (fx != None) fx else first(xs, f)
  }
}

//(2b) Implement a function that uses the first-function for
// trying out onward moves, and searches recursively for an 
// *open* tour on a dim * dim-board.

def first_tour(dim: Int, path: Path): Option[Path] = {
 tour(dim, path, path(0)) 
}

def tour(dim: Int, path: Path, pos: Pos): Option[Path] = {
  val legalmoves = legal_moves(dim, path, pos)
  if (path.size == dim*dim) {
    Some(path)
  }
  else 
    first(legalmoves, pos => tour(dim, pos::path, pos))
}

def is_legal(dim: Int, path: Path)(x: Pos): Boolean = 
    if ((x._1 >= 0) && (x._1 < dim) && (x._2 >= 0) && (x._2 < dim) && !path.contains(x)) 
      true
    else false

def legal_moves(dim: Int, path: Path, x: Pos): List[Pos] = {
  val possiblemoves = List(upright(x), rightup(x), rightdown(x), downright(x), downleft(x), leftdown(x), leftup(x), upleft(x))
  for (move <- possiblemoves if (is_legal(dim, path)(move))) yield move
}

def upright(x: Pos): Pos = 
  ((x._1 + 1), (x._2 + 2))
def upleft(x: Pos): Pos = 
  ((x._1 - 1), (x._2 + 2))
def leftup(x: Pos): Pos = 
  ((x._1 - 2), (x._2 + 1))
def leftdown(x: Pos): Pos = 
  ((x._1 - 2), (x._2 - 1))
def downright(x: Pos): Pos = 
  ((x._1 + 1), (x._2 - 2))
def downleft(x: Pos): Pos = 
  ((x._1 - 1), (x._2 - 2))
def rightup(x: Pos): Pos = 
  ((x._1 + 2), (x._2 + 1))
def rightdown(x: Pos): Pos = 
  ((x._1 + 2), (x._2 - 1))
