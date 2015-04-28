package fifteen.algorithms

import collection.mutable._

object AStar {
    /** Узел поиска для алгоритма A*, общий вид
      */
    abstract class Node[T, TNode <: Node[T, TNode]](val Value: T) extends Ordered[Node[T, TNode]] {
        /** Получение соседних узлов */
        val Adj: Seq[TNode]

        /** Родительский узел */
        var Parent: TNode

        /** Функция, определяющая расстояние до родительского узла */
        def DistTo(node: TNode): Int

        /** Стоимость пути от начальной вершины */
        def G: Int = if(Parent != null) Parent.G + DistTo(Parent) else 0

        /** Стоимость (предполагаемая) пути до целевой вершины */
        def H: Int

        /** Функция для оценки приоритета вершины */
        def F: Int = G + H

        /** Реализация трейта Ordered нужна для помещения узла в очередь с приоритетом */
        def compare(that: Node[T, TNode]) = F - that.F

        override def equals(that: Any) = that match {
            case that: Node[T, TNode] => Value == that.Value
            case _ => false
        }
        override def hashCode = Value.hashCode
    }

    /** Поиск пути из узла @param startNode в узел @param terminalNode по алгоритму A* */
    def Solve[T, TNode <: Node[T, TNode]](startNode: TNode, terminalNode: TNode) = {
        /* Список открытых узлов */
        val open = HashSet[TNode](startNode)
        /* Список закрытых узлов */
        val closed = HashSet[TNode]()
        /* Индикатор выхода из цикла обхода */
        var break = false
        var node = startNode
        var k = 0

        /* Цикл обхода графа */
        while(!open.isEmpty && !break) {
            k += 1
            /* Проверка на достижение целевого узла */
            if(node == terminalNode) break = true
            else {
                /* Удаление текущего узла из открытого списка и помещение в закрытый */
                open -= node
                closed += node
                /* Обход всех соседних узлов, которых еще нет в закрытом списке */
                node.Adj filter(childNode => !closed.contains(childNode)) foreach { childNode =>
                    val dist = childNode.DistTo(node)
                    if(open.contains(childNode)) {
                        /* Если соседний узел уже есть в открытом списке, пересчитать для него расстояние.
                         * Если расстояние через текущий узел окажется меньше,
                         * установить текущий узел как родительский.
                         */
                        if(childNode.G > node.G + dist) {
                            childNode.Parent = node
                        }
                    }
                    else {
                        /* Если соседнего узла нет в открытом списке, добавить его туда */
                        open += childNode
                        childNode.Parent = node
                    }
                }
                /* Взять из открытого списка узел с наименьшим значением целевой функции */
                node = open.minBy(_.F)
            }
        }
        /* Решение будем возвращать в виде стека с вершиной в исходном узле */
        val solution = Stack[T]()
        while(node != null) {
            /* Проходим путь назад от целевого узла к исходному и добавляем в стек */
            solution.push(node.Value)
            node = node.Parent
        }
        println("Iterations: " + k)
        println("Length: " + solution.length)
        solution
    }
}
