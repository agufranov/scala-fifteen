package fifteen.algorithms

import fifteen.Board
import collection.mutable._

/** Узел поиска для алгоритма A* для задачи пятнашек
  */
class FifteenAStarNode(override val Value: Board) extends AStar.Node[Board, FifteenAStarNode](Value) {
    /** Соседние узлы - это поля, полученные ходами во всех возможных направлениях */
    override lazy val Adj: Seq[FifteenAStarNode] = Value.PossibleMoves map(m => new FifteenAStarNode(Value.To(m)))
    override var Parent: FifteenAStarNode = null
    /** Расстояние до родительского узла всегда равно 1 (один ход) */
    override def DistTo(node: FifteenAStarNode) = 1
    /** В качестве функции, оценивающей расстояние до целевого узла, будем использовать манхэттенское расстояние */
    override def H = Value.ManhHeuristic
}
