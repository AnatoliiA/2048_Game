import scala.util.Random
import scala.io.StdIn

class Game {
  private var GRIDS: Array[Array[Int]] = Array.ofDim[Int](4, 4)
  private val RANDOM: Random = new Random()
  private var SCORE: Int = 0
  private var GAME_STATE: Boolean = false

  def startGame(): Array[Array[Int]] = {
    var GRIDS: Array[Array[Int]] = Array.ofDim[Int](4, 4)
    GAME_STATE = true
    GRIDS = Array.ofDim[Int](4, 4)
    SCORE = 0
    //generateNewValue()
    //generateNewValue()

    GRIDS(3)(0) = 4
    GRIDS(3)(1) = 2
    GRIDS(3)(2) = 2
    GRIDS(3)(3) = 4
    //    GRIDS(0)(2) = 2
    GRIDS(2)(0) = 4
    GRIDS(2)(1) = 4
    GRIDS(2)(2) = 0
    GRIDS(2)(3) = 4

    GRIDS(0)(0) = 4
    GRIDS(0)(1) = 16
    GRIDS(0)(2) = 0
    GRIDS(0)(3) = 4

    GRIDS(1)(0) = 0
    GRIDS(1)(1) = 0
    GRIDS(1)(2) = 0
    GRIDS(1)(3) = 4
    grid_dispaly(GRIDS)
    //    while (GAME_STATE) {
    //      userInput()
    //      //generateNewValue()
    //      grid_dispaly(GRIDS)
    //    }
    return GRIDS
  }

  def grid_dispaly(arr: Array[Array[Int]]): Unit = {
    var s: String = ""
    for (i <- arr) {
      s += i.mkString("|", " ", "|\n")
    }
    println(s)
  }

  // Check if user's input is valid, if not then call the same function again until it's valid
  def userInput(): Unit = {
    println("Please, choose a direction to move tiles. ('w' - up, 'a' - left, 's' - down, 'd' - right)")
    val inputs: Array[Char] = Array('w', 'a', 's', 'd', 'W', 'A', 'S', 'D')
    var state: Boolean = true
    var input: Char = '0'
    //    while (state) {
    //      try {
    //        input = StdIn.readChar()
    //      } catch {
    //        case e: java.lang.StringIndexOutOfBoundsException => println()
    //      }
    //      if (inputs.contains(input)) {
    //        state = false
    //      } else {
    //        println("Please, enter a valid direction.")
    //      }
    //    }
    gridMover(input)
  }

  // Generate a new value of 2 with 90% chance or 4 with 10% chance
  def newTileValue(): Int = {
    val value: Int = RANDOM.nextInt(10)
    if (value == 9) {
      4
    } else {
      2
    }
  }

  // Check if there are any empty positions (zeroes) inside our GRIDS
  def checkForZero(): Array[Array[Int]] = {
    var counter: Int = 0
    for (i <- GRIDS(0).indices) {
      for (j <- GRIDS(i).indices) {
        if (GRIDS(i)(j) == 0) {
          counter += 1
        }
      }
    }
    val result: Array[Array[Int]] = new Array[Array[Int]](counter)
    var pos: Int = 0
    for (i <- GRIDS(0).indices) {
      for (j <- GRIDS(i).indices) {
        if (GRIDS(i)(j) == 0) {
          result(pos) = Array[Int](i, j)
          pos += 1
        }
      }
    }
    result
  }

  def possibleMove(): Boolean = {
    println("possible move function")
    true
  }

  def gameOver(): Unit = {

  }

  def counterNonZero(arr: Array[Array[Int]]): Int = {
    var counter: Int = 0
    for (i <- arr(0).indices) {
      for (j <- arr(i).indices) {
        if (arr(i)(j) != 0) {
          counter += 1
        }
      }
    }
    counter
  }

  /**
   * def gridMover(direction: Char): Unit = {
   * val temp: Array[Array[Int]] = rotateArray(GRIDS)
   * var bool: Boolean = true
   * var counter: Int = 0
   * grid_dispaly(temp)
   * for (i <- temp.indices) {
   * var chg = 0
   *
   * bool = true
   * for (j <- temp(i).indices.reverse if bool) {
   * counter = counterNonZero(temp(i))
   * if (counter == 1 && bool) {
   * for (y <- j until -1 by -1) {
   * if (temp(i)(y) > 1) {
   * chg = temp(i)(y)
   * temp(i)(y) = 0
   * }
   * }
   * temp(i)(0) = chg
   * }
   * bool = false
   * }
   * }
   * grid_dispaly(temp)
   * }
   * */
  def gridMover(direction: Char): Unit = {
    val counter: Int = 0
    var c = 0
    val positions: Array[Array[Int]] = new Array[Array[Int]](counter)
    val moveable: Array[Array[Int]] = Array.ofDim(4, 4)
    var pos: Int = 0
    var bool = false
    //    for (i <- GRIDS(0).indices) {
    //      for (j <- GRIDS(i).indices) {
    //        if (GRIDS(i)(j) != 0) {
    //          positions(pos) = Array[Int](i, j)
    //          pos += 1
    //        }
    //      }
    //    }

    if (direction == 'w' || direction == 'W') {
      for (i <- GRIDS.indices.reverse) {
        val arrChanged = Array.fill(4)(-1)
        var counterChanged = 0
        for (j <- GRIDS(i).indices) {
          if (j == 1)
            println()
          for (y <- 1 to 3) {
            if (GRIDS(y)(j) > 0 && GRIDS(counterChanged)(j) == 0) {
              GRIDS(counterChanged)(j) = GRIDS(y)(j)
              GRIDS(y)(j) = 0
              counterChanged += 1
            }
            //                if (GRIDS(y)(j) > 0 && GRIDS(y-1)(j)==0) {
            //                  GRIDS(y - 1)(j) = GRIDS(y)(j)
            //                  GRIDS(y)(j) = 0
            //                }
          }
          for (y <- 1 to i) {
            if (GRIDS(y)(j) > 0 && GRIDS(y - 1)(j) != 0 && !arrChanged.contains(y - 1)) {
              if (GRIDS(y)(j) == GRIDS(y - 1)(j)) {
                GRIDS(y - 1)(j) *= 2
                GRIDS(y)(j) = 0
                arrChanged(counterChanged) = y - 1
                arrChanged(counterChanged) = y
                counterChanged += 1
              }
              //                    for (q <- 1 to i) {
              //                      if (GRIDS(q)(j) > 0 && GRIDS(q-1)(j)==0) {
              //                        GRIDS(q - 1)(j) = GRIDS(q)(j)
              //                        GRIDS(q)(j) = 0
              //                      }
              //                    }
            }
          }
        }
        println("stop")
      }
    } else if (direction == 'a' || direction == 'A') {
      for (i <- positions.indices) {
        val row = positions(i)(0)
        val col = positions(i)(1)
        //Check if pos is not zero
        if (col != 0) {
          for (x <- col to 0 by -1) {
            if (GRIDS(x)(col) == 0) {
              GRIDS(x)(col) = GRIDS(row)(col)
              GRIDS(row)(col) = 0
            } else if (GRIDS(x)(col) == GRIDS(row)(col) && x != row && moveable(x)(col) != 1) {
              GRIDS(x)(col) *= 2
              GRIDS(row)(col) = 0
              moveable(x)(col) = 1
            }
          }
        }
      }
    } else if (direction == 's' || direction == 'S') {
      for (i <- positions.indices) {
        println(" grids value " + GRIDS(positions(i)(0))(positions(i)(1)))
        println(" position value f  " + positions(i)(0) + " s " + (positions(i)(1)))
        val row = positions(i)(0)
        val col = positions(i)(1)
        //Check if pos is not 3
        if (row != 3) {
          for (y <- 3 to row by -1) {
            println(GRIDS(y)(positions(i)(1)))
          }
          //          for (y <- 3 to row by -1) {
          //            if (GRIDS(y)(col) == 0) {
          //              GRIDS(y)(col) = GRIDS(row)(col)
          //              GRIDS(row)(col) = 0
          //            } else if (GRIDS(y)(col) == GRIDS(row)(col) && moveable(y)(col) != 1) {
          //              GRIDS(y)(col) *= 2
          //              GRIDS(row)(col) = 0
          //              moveable(y)(col) = 1
          //            }
          //          }
        }
      }
      grid_dispaly(moveable)
    }
  }

  /**
   * def rotateArray(arr: Array[Array[Int]]): Array[Array[Int]] = {
   * val rotate: Array[Array[Int]] = Array.ofDim(4, 4)
   * var c: Int = 0
   * for (counter <- arr.indices) {
   * rotate(counter) = new Array[Int](4)
   * for (i <- arr.indices) {
   * for (j <- arr.indices) {
   * if (j == counter) {
   * rotate(counter)(c) = arr(i)(j)
   * c += 1
   * if (c == 4) c = 0
   * }
   * }
   * }
   * }
   * rotate
   * }
   * */

  // Check if there are empty grids to generate a new value. If not, then check for possible moves and if there are none, game over.
  def generateNewValue(): Unit = {
    val temp: Array[Array[Int]] = checkForZero()
    val positions: Array[Int] = temp(RANDOM.nextInt(temp.length))
    if (temp.length == 0) {
      possibleMove()
    } else {
      GRIDS(positions(0))(positions(1)) = newTileValue()
    }
  }
}


object Game extends App {
  val game: Game = new Game
  //  grid_dispaly(game.startGame())
  var resultGrid: Array[Array[Int]] = moveRight(game.startGame())

  def grid_dispaly(arr: Array[Array[Int]]): Unit = {
    var s: String = ""
    for (i <- arr) {
      s += i.mkString("|", " ", "|\n")
    }
    println(s)
  }

  grid_dispaly(resultGrid)

  def moveUp(arrArrInt: Array[Array[Int]]): Array[Array[Int]] = {
    var bool = true
    var countChanged = 0
    var moveInt = 0
    var moveIntBase = 0
    var arrChanged = Array.fill(4)(-1)
    for (counter <- 0 to 3) {
      arrChanged = Array.fill(4)(-1)

      for (i <- arrArrInt.indices) {
        for (m <- 0 to arrArrInt.length - 2) {
          moveInt = 0
          moveIntBase = 0
          try {
            while (arrArrInt(moveIntBase)(counter) != 0 && bool) {
              moveIntBase += 1
              moveInt += 1
              if (moveIntBase > arrArrInt.length - 1) {
                moveIntBase = 3
                moveInt = 3
                bool = false
              }
            }
          } catch {
            case e: Exception => println(s"Base исключение: ${e.getMessage} counter ${counter} moveint ${moveIntBase}")
          }
          bool = true
          try {
            while (moveInt < 4) {
              if (arrArrInt(moveIntBase)(counter) == 0 && arrArrInt(moveInt)(counter) != 0) {
                arrArrInt(moveIntBase)(counter) = arrArrInt(moveInt)(counter)
                arrArrInt(moveInt)(counter) = 0
                moveIntBase += 1
                moveInt = moveIntBase
              }
              moveInt += 1
            }
          } catch {
            case e: Exception => println(s"Second исключение: ${e.getMessage} counter ${counter} moveint ${moveIntBase}")
          }


          if (arrArrInt(m)(counter) == 0 && arrArrInt(m + 1)(counter) != 0) {
            arrArrInt(m)(counter) = arrArrInt(m + 1)(counter)
            arrArrInt(m)(counter) = 0
          } else if (arrArrInt(m)(counter) != 0 && arrArrInt(m + 1)(counter) != 0) {
            if (arrArrInt(m)(counter) == arrArrInt(m + 1)(counter) && !arrChanged.contains(m)) {
              arrArrInt(m)(counter) *= 2
              arrArrInt(m + 1)(counter) = 0
              arrChanged(countChanged) = m
              countChanged += 1
              if (countChanged > 3) {
                countChanged = 0
              }
            }
          }
        }
      }
    }
    countChanged = 0
    moveIntBase = 0
    moveInt = 0
    for (counter <- 0 to 3) {
      moveInt = 0
      moveIntBase = 0
      if (counter == 2) {
        println()
      }
      while (arrArrInt(moveIntBase)(counter) != 0) {
        moveIntBase += 1
        moveInt += 1
      }
      while (moveInt < 4) {
        if (arrArrInt(moveIntBase)(counter) == 0 && arrArrInt(moveInt)(counter) != 0) {
          arrArrInt(moveIntBase)(counter) = arrArrInt(moveInt)(counter)
          arrArrInt(moveInt)(counter) = 0
          moveIntBase += 1
          moveInt = moveIntBase
        }
        moveInt += 1
      }
    }
    return arrArrInt
  }

  def moveDown(arrArrInt: Array[Array[Int]]): Array[Array[Int]] = {
    println("MoveDown")
    var bool = true
    var countChanged = 0
    var moveInt = 0
    var moveIntBase = 0
    var arrChanged = Array.fill(4)(-1)
    for (counter <- 3 to 0 by -1) {
      arrChanged = Array.fill(4)(-1)

      for (i <- arrArrInt.indices) {
        for (m <- arrArrInt.length - 1 to 0 by -1) {
          if (m == 2) {
            println("")
          }
          moveInt = 3
          moveIntBase = 3
          bool = true
          try {
            while (arrArrInt(moveIntBase)(counter) != 0 && bool) {
              moveIntBase -= 1
              moveInt -= 1
              if (moveIntBase < 0) {
                moveIntBase = 0
                moveInt = 0
                bool = false
              }
            }
          } catch {
            case e: Exception => println(s"Base исключение: ${e.getMessage} counter ${counter} moveint ${moveIntBase}")
          }
          bool = true
          try {
            while (moveInt >= 0) {
              if (arrArrInt(moveIntBase)(counter) == 0 && arrArrInt(moveInt)(counter) != 0) {
                arrArrInt(moveIntBase)(counter) = arrArrInt(moveInt)(counter)
                arrArrInt(moveInt)(counter) = 0
                moveIntBase -= 1
                moveInt = moveIntBase
              }
              moveInt -= 1
            }
          } catch {
            case e: Exception => println(s"Second исключение: ${e.getMessage} counter ${counter} moveint ${moveIntBase}")
          }
          if (arrArrInt(m)(counter) > 0) {
            if (arrArrInt(m)(counter) != 0 && arrArrInt(m - 1)(counter) != 0) {
              if (arrArrInt(m)(counter) == arrArrInt(m - 1)(counter) && !arrChanged.contains(m)) {
                arrArrInt(m)(counter) *= 2
                arrArrInt(m - 1)(counter) = 0
                arrChanged(countChanged) = m
                countChanged += 1
                if (countChanged > 3) {
                  countChanged = 0
                }
              }
            }
          }
        }
      }
    }
    countChanged = 0
    moveIntBase = 3
    moveInt = 3
    for (counter <- 3 to 0 by -1) {
      moveInt = 3
      moveIntBase = 3
      if (counter == 2) {
        println()
      }
      try {
        while (arrArrInt(moveIntBase)(counter) != 0 && bool) {
          moveIntBase -= 1
          moveInt -= 1
          if (moveIntBase < 0) {
            moveIntBase = 0
            moveInt = 0
            bool = false
          }
        }
      } catch {
        case e: Exception => println(s"Base исключение: ${e.getMessage} counter ${counter} moveint ${moveIntBase}")
      }
      bool = true
      try {
        while (moveInt >= 0) {
          if (arrArrInt(moveIntBase)(counter) == 0 && arrArrInt(moveInt)(counter) != 0) {
            arrArrInt(moveIntBase)(counter) = arrArrInt(moveInt)(counter)
            arrArrInt(moveInt)(counter) = 0
            moveIntBase -= 1
            moveInt = moveIntBase
          }
          moveInt -= 1
        }
      } catch {
        case e: Exception => println(s"Second исключение: ${e.getMessage} counter ${counter} moveint ${moveIntBase}")
      }
    }
    return arrArrInt
  }



  def moveRight(arrArrInt: Array[Array[Int]]): Array[Array[Int]] = {
    println("MoveRight")

    def shiftAndMergeRow(row: Int): Unit = {
      var moveIntBase = 3
      var moveInt = 3
      val arrChanged = Array.fill(4)(false) // Флаг изменений для каждого элемента строки

      // Сдвиг чисел вправо
      while (moveIntBase >= 0) {
        if (arrArrInt(row)(moveIntBase) == 0) {
          moveInt -= 1
          while (moveInt >= 0 && arrArrInt(row)(moveInt) == 0) moveInt -= 1
          if (moveInt >= 0) {
            arrArrInt(row)(moveIntBase) = arrArrInt(row)(moveInt)
            arrArrInt(row)(moveInt) = 0
          }
        }
        moveIntBase -= 1
        moveInt = moveIntBase
      }

      // Объединение чисел
      for (col <- 3 until 0 by -1) {
        if (arrArrInt(row)(col) == arrArrInt(row)(col - 1) && arrArrInt(row)(col) != 0 && !arrChanged(col)) {
          arrArrInt(row)(col) *= 2
          arrArrInt(row)(col - 1) = 0
          arrChanged(col) = true
        }
      }

      // Повторный сдвиг после объединения
      moveIntBase = 3
      moveInt = 3
      while (moveIntBase >= 0) {
        if (arrArrInt(row)(moveIntBase) == 0) {
          moveInt -= 1
          while (moveInt >= 0 && arrArrInt(row)(moveInt) == 0) moveInt -= 1
          if (moveInt >= 0) {
            arrArrInt(row)(moveIntBase) = arrArrInt(row)(moveInt)
            arrArrInt(row)(moveInt) = 0
          }
        }
        moveIntBase -= 1
        moveInt = moveIntBase
      }
    }

    // Обработка каждой строки
    for (row <- arrArrInt.indices) {
      shiftAndMergeRow(row)
    }

    arrArrInt
  }




  def moveLeft(arrArrInt: Array[Array[Int]]): Array[Array[Int]] = {
    println("MoveLeft")

    def shiftAndMergeRow(row: Int): Unit = {
      var moveIntBase = 0
      var moveInt = 0
      val arrChanged = Array.fill(4)(false) // Флаг изменений для каждого элемента строки

      // Сдвиг чисел влево
      while (moveIntBase < 4) {
        if (arrArrInt(row)(moveIntBase) == 0) {
          moveInt += 1
          while (moveInt < 4 && arrArrInt(row)(moveInt) == 0) moveInt += 1
          if (moveInt < 4) {
            arrArrInt(row)(moveIntBase) = arrArrInt(row)(moveInt)
            arrArrInt(row)(moveInt) = 0
          }
        }
        moveIntBase += 1
        moveInt = moveIntBase
      }

      // Объединение чисел
      for (col <- 0 until 3) {
        if (arrArrInt(row)(col) == arrArrInt(row)(col + 1) && arrArrInt(row)(col) != 0 && !arrChanged(col)) {
          arrArrInt(row)(col) *= 2
          arrArrInt(row)(col + 1) = 0
          arrChanged(col) = true
        }
      }

      // Повторный сдвиг после объединения
      moveIntBase = 0
      moveInt = 0
      while (moveIntBase < 4) {
        if (arrArrInt(row)(moveIntBase) == 0) {
          moveInt += 1
          while (moveInt < 4 && arrArrInt(row)(moveInt) == 0) moveInt += 1
          if (moveInt < 4) {
            arrArrInt(row)(moveIntBase) = arrArrInt(row)(moveInt)
            arrArrInt(row)(moveInt) = 0
          }
        }
        moveIntBase += 1
        moveInt = moveIntBase
      }
    }

    // Обработка каждой строки
    for (row <- arrArrInt.indices) {
      shiftAndMergeRow(row)
    }

    arrArrInt
  }


  def moveUpG(arrArrInt: Array[Array[Int]]): Array[Array[Int]] = {
    println("MoveUp")

    def shiftAndMergeColumn(col: Int): Unit = {
      var moveIntBase = 0
      var moveInt = 0
      val arrChanged = Array.fill(4)(false) // Флаг изменений для каждой строки

      // Сдвиг чисел вверх
      while (moveIntBase < 4) {
        if (arrArrInt(moveIntBase)(col) == 0) {
          moveInt += 1
          while (moveInt < 4 && arrArrInt(moveInt)(col) == 0) moveInt += 1
          if (moveInt < 4) {
            arrArrInt(moveIntBase)(col) = arrArrInt(moveInt)(col)
            arrArrInt(moveInt)(col) = 0
          }
        }
        moveIntBase += 1
        moveInt = moveIntBase
      }

      // Объединение чисел
      for (row <- 0 until 3) {
        if (arrArrInt(row)(col) == arrArrInt(row + 1)(col) && arrArrInt(row)(col) != 0 && !arrChanged(row)) {
          arrArrInt(row)(col) *= 2
          arrArrInt(row + 1)(col) = 0
          arrChanged(row) = true
        }
      }

      // Повторный сдвиг после объединения
      moveIntBase = 0
      moveInt = 0
      while (moveIntBase < 4) {
        if (arrArrInt(moveIntBase)(col) == 0) {
          moveInt += 1
          while (moveInt < 4 && arrArrInt(moveInt)(col) == 0) moveInt += 1
          if (moveInt < 4) {
            arrArrInt(moveIntBase)(col) = arrArrInt(moveInt)(col)
            arrArrInt(moveInt)(col) = 0
          }
        }
        moveIntBase += 1
        moveInt = moveIntBase
      }
    }

    // Обработка каждой колонки
    for (col <- 0 to 3) {
      shiftAndMergeColumn(col)
    }

    arrArrInt
  }

  def moveDownG(arrArrInt: Array[Array[Int]]): Array[Array[Int]] = {
    println("MoveDown")

    def shiftAndMergeColumn(col: Int): Unit = {
      var moveIntBase = 3
      var moveInt = 3
      val arrChanged = Array.fill(4)(false) // Для отслеживания изменений в колонке

      // Сдвиг чисел вниз
      while (moveIntBase >= 0) {
        if (arrArrInt(moveIntBase)(col) == 0) {
          moveInt -= 1
          while (moveInt >= 0 && arrArrInt(moveInt)(col) == 0) moveInt -= 1
          if (moveInt >= 0) {
            arrArrInt(moveIntBase)(col) = arrArrInt(moveInt)(col)
            arrArrInt(moveInt)(col) = 0
          }
        }
        moveIntBase -= 1
        moveInt = moveIntBase
      }

      // Объединение чисел
      for (row <- 3 until 0 by -1) {
        if (arrArrInt(row)(col) == arrArrInt(row - 1)(col) && arrArrInt(row)(col) != 0 && !arrChanged(row)) {
          arrArrInt(row)(col) *= 2
          arrArrInt(row - 1)(col) = 0
          arrChanged(row) = true
        }
      }

      // Повторный сдвиг после объединения
      moveIntBase = 3
      moveInt = 3
      while (moveIntBase >= 0) {
        if (arrArrInt(moveIntBase)(col) == 0) {
          moveInt -= 1
          while (moveInt >= 0 && arrArrInt(moveInt)(col) == 0) moveInt -= 1
          if (moveInt >= 0) {
            arrArrInt(moveIntBase)(col) = arrArrInt(moveInt)(col)
            arrArrInt(moveInt)(col) = 0
          }
        }
        moveIntBase -= 1
        moveInt = moveIntBase
      }
    }

    // Применяем логику для каждой колонки
    for (counter <- 3 to 0 by -1) {
      shiftAndMergeColumn(counter)
    }

    arrArrInt
  }

}

mmm