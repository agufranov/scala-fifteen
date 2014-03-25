package fifteen

import scala.swing._
import event._

/** Главное окно игры
  */
class GameView extends SimpleSwingApplication {
    val CheckersPanel = new GridPanel(Board.Size, Board.Size) {
        preferredSize = new Dimension(600, 600)
    }

    val InfoPanel = new TextArea {
        editable = false
        text = "N - новая игра\nСтрелки - перемещение\nS - решение\nEsc - выход"
    }

    val MainPanel = new BoxPanel(Orientation.Vertical) {
        contents += CheckersPanel
        contents += new Separator
        contents += InfoPanel
        listenTo(keys)
        focusable = true
    }

    val top = new MainFrame {
        title = "Пятнашки"
        contents = MainPanel
    }

    def CreateLabels(count: Int) = 0 until count map(_ => new Label { font = new Font("Arial", 0, 36) })
}

