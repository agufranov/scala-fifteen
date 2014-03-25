package fifteen

import algorithms._

object Game extends App {
    val gameView = new GameView
    val gameModel = new GameModel
    val gamePresenter = new GamePresenter(gameModel, gameView)
    gameView.main(args)
}
