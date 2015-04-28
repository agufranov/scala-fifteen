package fifteen

import algorithms._

object Game extends App {
    /*
    val t1 = System.nanoTime
    val s = AStar.Solve[Board, FifteenAStarNode](new FifteenAStarNode(Board.Debug), new FifteenAStarNode(Board.Terminal))
    val t2 = System.nanoTime
    while(!s.isEmpty) {
        val n = s.pop
        n.PrintToConsole
    }
    println((t2 - t1) / 1000000)
    */

    val gameView = new GameView
    val gameModel = new GameModel
    val gamePresenter = new GamePresenter(gameModel, gameView)
    gameView.main(args)
}
