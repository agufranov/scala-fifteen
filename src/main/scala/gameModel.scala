package fifteen

/** Модель игры
  */
class GameModel {
    private var board = Board.Random

    /** Состояние игрового поля */
    def GameBoard = board

    /** Инициализация игрового поля случайным образом */
    def Reset { board = Board.Random }

    /** Получение решения из текущей позиции */
    def Solve = {
        import algorithms._
        val startNode = new FifteenAStarNode(GameBoard)
        val terminalNode = new FifteenAStarNode(Board.Terminal)
        AStar.Solve[Board, FifteenAStarNode](startNode, terminalNode)
    }

    /** Установка поля в определенную позицию, используется при прорисовке решения */
    def SetBoard(_board: Board) { board = _board }

    /** Перемещение с изменением состояния поля */
    def Move(move: Board.Move.Move) = {
        val canMove = board.CanMoveTo(move)
        if(canMove)
            board = board.To(move)
        canMove
    }
}
