package fifteen

import scala.language.postfixOps

/** immutable-класс, представляющий состояние игрового поля
  */
class Board private (val Grid: Seq[Seq[Int]]) {
    import Board._

    /** Значение в указанной позиции поля */
    def At(pos: Position) = Grid(pos._1)(pos._2)

    /** Позиция указанного значения */
    def PosOf(value: Int) = util.indexOf2d(Grid, value)

    /** Позиция пустого квадрата */
    lazy val EmptyPos = PosOf(EmptyVal)

    /** Является ли данное состояние приводимым к решению */
    private lazy val isSolvable = Size match {
        case 2 | 4 => inversionsCount(Grid flatten, EmptyPos) % 2 == 0 // только для 4x4!
        case 3 => (inversionsCount(Grid flatten, EmptyPos) - EmptyPos._1) % 2 == 1
        case _ => throw new Exception("Невозможно вычислить разрешаемость.")
    }

    /** Является ли данное состояние решенным */
    lazy val IsTerminal = this.equals(Board.Terminal)

    /** Возможные направления ходов */
    lazy val PossibleMoves = possibleMovePositions.keys.toArray

    /** Находится ли указанная позиция в рамках поля */
    private def inBounds(movePos: Position) = (0 until Size contains movePos._1) && (0 until Size contains movePos._2)

    /** Возможные направления ходов и позиции, в которые перейдет пустой квадрат */
    private lazy val possibleMovePositions = Map(
        Move.Up -> new Position(EmptyPos._1 + 1, EmptyPos._2),
        Move.Down -> new Position(EmptyPos._1 - 1, EmptyPos._2),
        Move.Left -> new Position(EmptyPos._1, EmptyPos._2 + 1),
        Move.Right -> new Position(EmptyPos._1, EmptyPos._2 - 1)
    ) filter(t => inBounds(t._2))

    /** Манхэттенское расстояние фишки на указанной позиции до позиции в решении */
    def ManhDistToTerminal(pos: Position) = {
        val value = At(pos)
        if(value == EmptyVal) 0 else util.manhDist(pos, Board.Terminal.PosOf(value))
    }

    /** Манхэттенское расстояние указанного значения до позиции в решении */
    def ManhDistToTerminal(value: Int) = util.manhDist(PosOf(value), Board.Terminal.PosOf(value))

    /** Эвристическая функция, использующая манхэттенское расстояние.
      * Используется в алгоритме поиска решения
      */
    lazy val ManhHeuristic = (1 until Size2) map ManhDistToTerminal sum
    
    /** Возможен ли ход в указанном направлении */
    def CanMoveTo(move: Move.Move) = possibleMovePositions.contains(move)

    /** Новое поле, представляющее состояние данного поля после хода в указанном направлении */
    def To(move: Move.Move): Board = {
        val maybeMovePos = possibleMovePositions.get(move)
        maybeMovePos match {
            case Some(movePos: Position) => new Board(util.swap(Grid, EmptyPos, movePos))
            case None => throw new Exception("Невозможный ход.")
        }
    }

    lazy val Up = To(Move.Up)

    lazy val Down = To(Move.Down)

    lazy val Left = To(Move.Left)

    lazy val Right = To(Move.Right)

    /** Вывод поля в консоль, используется при отладке */
    def PrintToConsole = { Grid foreach { row => println(row map(c => c match { case EmptyVal => " " case _ => c.toString }) mkString("\t")) }; println }

    override def equals(that: Any) = that match {
        case that: Board => Grid == that.Grid
        case _ => false
    }

    override def hashCode = Grid.hashCode
}

object Board {
    import Board._

    type Position = Tuple2[Int, Int]

    private val rand = scala.util.Random

    /** Перечисление направлений ходов */
    object Move extends Enumeration {
        type Move = Value
        val Up, Down, Left, Right = Value
    }

    /** Размер игрового поля */
    val Size = 4

    val Size2 = Size * Size

    /** Значение, представляющее пустой квадрат */
    val EmptyVal = Size2

    /** Поле, являющееся решением
      * (фишки упорядочены по возрастанию слева направо и снизу вверх,
      * пустой квадрат в нижнем левом углу)
      */
    val Terminal: Board = new Board(util.slice2d((1 to Size2 - 1) :+ EmptyVal, Size))

    /** Число беспорядка для последовательности.
      * Используется для определения решаемости комбинации
      */
    private def inversionsCount(s: Seq[Int], emptyPos: Position): Int = s match {
        case EmptyVal +: xs => emptyPos._1 + 1 + inversionsCount(xs, emptyPos)
        case x +: xs => xs.filter(_x => _x < x && x != EmptyVal).length + inversionsCount(xs, emptyPos)
        case _ => 0
    }

    /** Случайное игровое поле с гарантией решаемости */
    def Random: Board = {
        val board = new Board(util.slice2d(rand.shuffle((1 to Size2 - 1) :+ EmptyVal), Size))
        if(board.isSolvable) board else Random
    }
}
