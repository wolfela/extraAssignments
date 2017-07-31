// Part 1 about finding and counting Knight's tours
//==================================================

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(1a) Complete the function that tests whether the position 
// is inside the board and not yet element in the path.

def is_legal(dim: Int, path: Path)(x: Pos): Boolean = 
  if ((x._1 >= 0) && (x._1 < dim) && (x._2 >= 0) && (x._2 < dim) && !path.contains(x)) 
    true
  else false


//(1b) Complete the function that calculates for a position 
// all legal onward moves that are not already in the path. 
// The moves should be ordered in a "clockwise" order.
 
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

//assert(legal_moves(8, Nil, (2,2)) == List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
//assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
//assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


//(1c) Complete the two recursive functions below. 
// They exhaustively search for open tours starting from the 
// given path. The first function counts all possible open tours, 
// and the second collects all open tours in a list of paths.

def count_tours(dim: Int, path: Path): Int = {
  findpaths(dim, path, path(0)).size
}

def enum_tours(dim: Int, path: Path): List[Path] = {
  findpaths(dim, path, path(0))
}


def findpaths(dim: Int, path: Path, x: Pos): List[Path] = {
  val legalmoves = legal_moves(dim, path, x)
    if (path.size == dim*dim)
      List(path)
    else if (!legalmoves.isEmpty) {
      val pathlist = for(legalmove <- legalmoves) yield findpaths(dim, legalmove :: path, legalmove)
      pathlist.flatten
    }
    else {
      List[Path]()
    }
}

