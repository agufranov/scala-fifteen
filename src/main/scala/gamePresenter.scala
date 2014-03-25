package fifteen

import swing._
import event._

/** Presenter игры
  */
class GamePresenter(model: GameModel, view: GameView) {
    private val labels = view.CreateLabels(Board.Size2)

    private var keysEnabled = true

    /** Отрисовка игрового поля на форме */
    private def RefreshView {
        labels.zip(model.GameBoard.Grid.flatten).foreach(k => k._1.text = if(k._2 == Board.EmptyVal) " " else k._2.toString)
    }

    /** Проверка состояния и оповещение в случае победы */
    private def CheckState {
        if(model.GameBoard.IsTerminal) {
            Dialog.showMessage(null, "Победа", "", Dialog.Message.Info)
            StartNewGame
        }
    }

    /** Перемещение */
    private def Move(move: Board.Move.Move)  {
        if(model.Move(move)) { RefreshView; CheckState }
    }

    /** Начало новой игры */
    private def StartNewGame { model.Reset; RefreshView; CheckState }

    /** Запуск решалки с пошаговой отрисовкой ходов */
    private def Solve {
        keysEnabled = false
        val infoText = view.InfoPanel.text
        view.InfoPanel.text = "Решение..."
        import java.util.{Timer, TimerTask}
        var solution: collection.mutable.Stack[Board] = null
        Swing.onEDT({
            solution = model.Solve
            val t = new Timer
            def scheduleNext: Unit = { t.schedule(new TimerTask { def run = {
                if(solution.isEmpty) { CheckState; view.InfoPanel.text = infoText; keysEnabled = true }
                else { model.SetBoard(solution.pop); RefreshView; scheduleNext }
            } }, 200) }
            scheduleNext
        })
    }

    /** Выход из игры */
    private def ExitGame { System.exit(0); }

    /** Отрисовка фишек на форме и подвешивание событий UI */
    private def InitView {
        view.CheckersPanel.contents ++= labels
        view.MainPanel.reactions += {
            case event.KeyPressed(_, key, _, _) => if(keysEnabled) key match {
                case Key.Up => Move(Board.Move.Up)
                case Key.Down => Move(Board.Move.Down)
                case Key.Left => Move(Board.Move.Left)
                case Key.Right => Move(Board.Move.Right)
                case Key.N => StartNewGame
                case Key.S => Solve
                case Key.Escape => ExitGame
                case _ => 
            }
            case _ => 
        }
    }

    InitView
    StartNewGame
}
