package ui

import mandelbrot._

import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.canvas.Canvas
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint._
import scalafx.scene.shape.Rectangle
import scalafx.scene.{Cursor, Group, Scene}
import scalafx.geometry.Point2D

object MandelbrotUI extends JFXApp {

  object SelectedMandelbrotValues {

    def update(xPixelRange : Range, yPixelRange : Range) : Unit = {

      val left = Math.linearTransform(xPixelRange.min, Config.XScreenRange, xMandelbrotRange)
      val right = Math.linearTransform(xPixelRange.max, Config.XScreenRange, xMandelbrotRange)

      val up = Math.linearTransform(yPixelRange.min, Config.YScreenRange, yMandelbrotRange)
      val down = Math.linearTransform(yPixelRange.max, Config.YScreenRange, yMandelbrotRange)

      xMandelbrotRange = Range(left, right)
      yMandelbrotRange = Range(up, down)
    }

    def getXMandelbrotRange = xMandelbrotRange
    def getYMandelbrotRange = yMandelbrotRange

    private var xMandelbrotRange = Config.XMandelbrotRange
    private var yMandelbrotRange = Config.YMandelbrotRange
  }

  object ZoomRectangle {

    val rectangle = new Rectangle {
      fill = Color.Transparent
      stroke = Color.White
      strokeWidth = 2
    }

    def update(start: Point2D = _start, end: Point2D = _end) {
      _start = start
      _end = end
      rectangle.x = math.min(_start.x, _end.x)
      rectangle.y = math.min(_start.y, _end.y)
      rectangle.width = math.abs(_start.x - _end.x)
      rectangle.height = math.abs(_start.y - _end.y)
    }

    def getStart = _start
    def getEnd = _end

    private var _start = new Point2D(0, 0)
    private var _end = new Point2D(0, 0)
  }

  val canvas = new Canvas(Config.XScreenRange.width, Config.YScreenRange.width)

  val rootPane = new Group
  rootPane.children = List(canvas, ZoomRectangle.rectangle)

  val theScene = new Scene(Config.XScreenRange.width, Config.YScreenRange.width) {
    root = rootPane
  }

  stage = new PrimaryStage {
    title = "Mandelbrot set"
    scene = theScene
  }

  val gc = canvas.graphicsContext2D

  val pixels = for {
    x <- (0 until Config.XScreenRange.width.toInt)
    y <- (0 until Config.YScreenRange.width.toInt)
  } yield (Point(x, y))

  rootPane.handleEvent(MouseEvent.Any) {
    me: MouseEvent => {
      me.eventType match {
        case MouseEvent.MousePressed => {
          val p = new Point2D(me.x, me.y)
          ZoomRectangle.update(p, p)
        }
        case MouseEvent.MouseDragged => {
          ZoomRectangle.update(end = new Point2D(me.x, me.y))
          ZoomRectangle.rectangle.setVisible(true)
        }

        case MouseEvent.MouseReleased => {
          theScene.cursor = Cursor.Wait

          val task = new javafx.concurrent.Task[Unit] {

            private var pixelColors: List[(ColorIntensity, Point)] = List()

            override def call(): Unit = {
              val xPixelRange = Range(math.min(ZoomRectangle.getStart.x, ZoomRectangle.getEnd.x),
                                 math.max(ZoomRectangle.getStart.x, ZoomRectangle.getEnd.x))
              val yPixelRange = Range(math.min(ZoomRectangle.getStart.y, ZoomRectangle.getEnd.y),
                                 math.max(ZoomRectangle.getStart.y, ZoomRectangle.getEnd.y))

              SelectedMandelbrotValues.update(xPixelRange, yPixelRange)

              pixelColors = Mandelbrot.getColorIntensityPerPoint(pixels.toList,
                SelectedMandelbrotValues.getXMandelbrotRange, SelectedMandelbrotValues.getYMandelbrotRange)
            }

            override def succeeded(): Unit = {
              ZoomRectangle.rectangle.setVisible(false)
              theScene.cursor = Cursor.Default
              drawPixels(pixelColors)
            }

            override def failed(): Unit = {
              ZoomRectangle.rectangle.setVisible(false)
              theScene.cursor = Cursor.Default
            }
          }

          val t = new Thread(task, "Mandelbrot calculation task")
          t.setDaemon(true)
          t.start()

        }

        case _ => {}
      }
    }
  }

  private def drawPixels(pixelColors : List[(ColorIntensity, Point)]): Unit = {
    pixelColors.foreach(cp => {
      gc.setFill(Color.rgb((cp._1.intensity * 255.0).toInt, 0, 0))
      gc.fillRect(cp._2.x, cp._2.y, 1, 1)
    })
  }

  private def drawMandelbrot(): Unit = {
    val pixelColors = Mandelbrot.getColorIntensityPerPoint(pixels.toList,
      SelectedMandelbrotValues.getXMandelbrotRange, SelectedMandelbrotValues.getYMandelbrotRange)
    drawPixels(pixelColors)
  }

  drawMandelbrot()

}