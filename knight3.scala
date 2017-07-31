// Part 3 about finding a single tour using the Warnsdorf Rule
//=============================================================

// copy any function you need from files knight1.scala and
// knight2.scala

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

//(3a) Complete the function that calculates a list of onward
// moves like in (1b) but orders them according to the Warnsdorf’s 
// rule. That means moves with the fewest legal onward moves 
// should come first.

def ordered_moves(dim: Int, path: Path, x: Pos): List[Pos] = {
	val legalmoves = legal_moves(dim, path, x)
	val movesnumbers = for (move <- legalmoves) yield (move, legal_moves(dim, move::path, move).length)
	val orderedmoves = movesnumbers.sortBy(_._2)
	orderedmoves.unzip._1
}

//(3b) Complete the function that searches for a single *closed* 
// tour using the ordered moves function.

def first_closed_tour_heuristic(dim: Int, path: Path): Option[Path] = {
	closed_tour(dim, path, path(0))
}

def first(xs: List[Pos], f: Pos => Option[Path]): Option[Path] = xs match {
  case Nil => None
  case x::xs => {
    val fx = f(x)
    if (fx != None) fx else first(xs, f)
  }
}

def closed_tour(dim: Int, path: Path, pos: Pos): Option[Path] = {
	if (path.size == dim*dim-1) {
		val possiblemoves = List(upright(pos), rightup(pos), rightdown(pos), downright(pos), downleft(pos), leftdown(pos), leftup(pos), upleft(pos))
		if (possiblemoves.contains(path.last))
			Some(path.last::path)
		else
			None
	}
	else {
		val orderedmoves = ordered_moves(dim, path, path(0))
		first(orderedmoves, pos => closed_tour(dim, pos::path, pos))
	}
}

//(3c) Same as (3b) but searches for *open* tours.

def first_tour_heuristic(dim: Int, path: Path): Option[Path] = {
	tour_tail(dim, path, path(0))
}

def tour_tail(dim: Int, path: Path, pos: Pos): Option[Path]= {
	if (path.size == dim*dim)
		Some(path)
	else {
		val orderedmoves = ordered_moves(dim, path, path(0))
		tour_tail(dim, orderedmoves(0)::path, orderedmoves(0))
	}	
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

