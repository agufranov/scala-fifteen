package fifteen

import scala.reflect.ClassTag

/** Вспомогательные функции для работы с двумерными последовательностями */
package object util {
    /** Получение из последовательности длины n^2 двумерной последовательности n*n */
    def slice2d[T: ClassTag](s: Seq[T], size: Int): Seq[Seq[T]] = {
        val l = s.toList
        l match {
            case Nil => Nil.toVector
            case l => l.slice(0, size).toVector +: slice2d(l.slice(size, l.length), size)
        }
    }

    //def slice2d[T](s: Seq[T], size: Int) = s.zipWithIndex.groupBy(_._2 / size).values.map(_.map(_._1).toVector).toVector

    /** Индекс элемента в двумерном массиве */
    def indexOf2d[T](s: Seq[Seq[T]], t: T) = {
        var subseq = s.find(_.contains(t)).get
        (s.indexOf(subseq), subseq.indexOf(t))
    }

    /** Создание новой двумерной последовательности из исходной с обменянными элементами на двух позициях */
    def swap[T: ClassTag](s: Seq[Seq[T]], pos1: Tuple2[Int, Int], pos2: Tuple2[Int, Int]) = {
        val arr = s.map(_.toArray).toArray
        val t = arr(pos1._1)(pos1._2)
        arr(pos1._1)(pos1._2) = arr(pos2._1)(pos2._2)
        arr(pos2._1)(pos2._2) = t
        arr.map(_.toVector).toVector
    }

    /** Манхэттенское расстояние между двумя точками на плоскости */
    def manhDist(pos1: Tuple2[Int, Int], pos2: Tuple2[Int, Int]) = math.abs(pos2._1 - pos1._1) + math.abs(pos2._2 - pos1._2)
}
