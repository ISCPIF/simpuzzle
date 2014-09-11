package fr.iscpif.flocking.visualisation

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.geom.{AffineTransform, Line2D}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, Dimension, Graphics2D, GraphicsEnvironment, RenderingHints, Shape, Point => JPoint}
import javax.swing.Timer

import fr.iscpif.flocking.model.datatypes._
import fr.iscpif.flocking.model.engine._

import scala.swing.RichWindow.Undecorated
import swing._
import swing.event._
import scala.util.Random
import math._

trait Visu {
  val model: Model
  val pixelWidth: Int
  val pixelHeight: Int
  val frameDelay: Int
  val birdLength:Double
  val birdWidth:Double

  val seed = 42

  lazy val ge = GraphicsEnvironment.getLocalGraphicsEnvironment()
  lazy val gd = ge.getDefaultScreenDevice()
  lazy val gc = gd.getDefaultConfiguration()

  lazy val backgroundImage: BufferedImage = new BufferedImage(model.env.nCellsWide, model.env.nCellsHigh, BufferedImage.TYPE_INT_RGB)
  lazy val scaleBackgroundImage: AffineTransform = AffineTransform.getScaleInstance(pixelWidth / model.env.nCellsWide.toDouble, pixelHeight / model.env.nCellsHigh.toDouble)
  lazy val backgroundColorRGB:Int = new Color(0,0,0).getRGB()
  lazy val obstacleColorRGB:Int = new Color(0,0,255).getRGB()
  lazy val birdColor: Color = new Color(255,255,255)

  lazy val timer = new Timer(frameDelay, Surface)
  lazy val modelStepByStep = new ModelIterator(model)(new Random(seed))

  lazy val visuMainFrame = new MainFrame(gc) {
    preferredSize = new Dimension(pixelWidth, pixelHeight)
    contents = Surface
    background = Color.black
  }

  def shapeBird(x: Double,y: Double,heading: Heading): Shape = {
    new Line2D.Double(
      (x / model.worldWidth * pixelWidth + (birdLength / 2) * cos(heading.toDouble)),
      (y / model.worldHeight * pixelHeight + (birdLength / 2) * sin(heading.toDouble)),
      (x / model.worldWidth * pixelWidth - (birdLength / 2) * cos(heading.toDouble)),
      (y / model.worldHeight * pixelHeight - (birdLength / 2) * sin(heading.toDouble))
    )
  }

  def drawBirds(xyheading: Seq[(Double, Double, Heading)]) =
    xyheading.map(t => shapeBird(t._1, t._2, t._3))

  def birdsShapes(): Iterable[Shape] =
    drawBirds(modelStepByStep.currentState.birds.map(b => (b.position.x, b.position.y, b.heading)))

  // def birdsTransform(): Iterable[AffineTransform] =
  //   modelStepByStep.currentState.birds.map(b => {
  //       //b.position.x, b.position.y, b.heading
  //       val af = new AffineTransform()
  //       val xpos = b.position.x / model.worldWidth * pixelWidth
  //       val ypos = b.position.y / model.worldHeight * pixelHeight
  //       af.translate(xpos - birdImageWidth / 2.0, ypos - birdImageHeight / 2.0)
  //       // af.rotate(b.heading.toDouble + Pi/2, xpos, ypos)
        
  //       af
  //     }
  //   )

  def updateBackground() {
    val envWidth: Double = model.env.width
    val envHeight: Double = model.env.height
    // var i: Int = 0
    // var j: Int = 0
    // i = 0
    // while (i < pixelWidth) { 
    //   j = 0
    //   while (j < pixelHeight) {
    //     if (model.env.get((i + 0.5) * envWidth / pixelWidth, (j + 0.5) * envHeight / pixelHeight) != 0) backgroundImage.setRGB(i,j,obstacleColorRGB)
    //     else backgroundImage.setRGB(i,j,backgroundColorRGB)
    //     j += 1
    //   }
    //   i += 1
    // }
    backgroundImage.setRGB(0,0, model.env.nCellsWide, model.env.nCellsHigh, model.env.pixels, 0, model.env.nCellsWide)
  }

  object Surface extends Panel with ActionListener  {
    var lastFrameTime: Long = System.currentTimeMillis()
    var framesSinceLastTimeMeasure: Int = 0
    var deltaTime: Long = 0

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, // Anti-alias!
        RenderingHints.VALUE_ANTIALIAS_ON);
      g.setBackground(Color.black)
      g.clearRect(0,0,pixelWidth,pixelHeight)
      g.setColor(birdColor)
      //for {s <- obstaclesShapes} g.fill(s)
      updateBackground
      g.drawImage(backgroundImage, scaleBackgroundImage, null)
      g.setStroke(new BasicStroke(birdWidth.toFloat))
      for {shape <- birdsShapes} g.draw(shape)
      //for {af <- birdsTransform} g.drawImage(birdImage, af, null)
      // val af = new AffineTransform()
      // af.translate(-birdImageWidth / 2.0, -birdImageHeight / 2.0)
      // af.scale(0.5, 0.5)
      // af.translate(birdImageWidth * (1/0.5) / 2.0, birdImageHeight * (1/0.5) / 2.0)
      // af.rotate(Pi/4.0, birdImageWidth / 2.0, birdImageHeight / 2.0)
      // g.drawImage(birdImage, af, null)
      framesSinceLastTimeMeasure += 1
      deltaTime = System.currentTimeMillis() - lastFrameTime
      if (deltaTime >= 1000) {
        print((framesSinceLastTimeMeasure / deltaTime.toDouble) * 1000)
        println(" fps")
        framesSinceLastTimeMeasure = 0
        lastFrameTime = System.currentTimeMillis()
      }
    }


    var iteration=0
    def actionPerformed(e: ActionEvent) {
      println(iteration)
      iteration += 1
      modelStepByStep.step
      repaint()
    }

    listenTo(keys)
    reactions += {
      case KeyPressed(_, Key.Q, _, _) => {
        Skeleton.quit()
      }
      case KeyPressed(_, Key.Escape, _, _) => {
        Skeleton.quit()
      }
    }
    focusable = true
    requestFocus
  }


  object Skeleton extends SimpleSwingApplication {
    def top = {
      timer.start()
      visuMainFrame
    }

    override def shutdown() = {
      timer.stop
      super.shutdown
    }
  }
}

trait Fullscreen <: Visu {

  lazy val pixelWidth: Int = gd.getDisplayMode().getWidth() 
  lazy val pixelHeight: Int = gd.getDisplayMode().getHeight()

  override lazy val visuMainFrame = new MainFrame(gc) with Undecorated {
    preferredSize = new Dimension(pixelWidth, pixelHeight)
    contents = Surface
    background = Color.black
    gd.setFullScreenWindow(this.peer)
  }
}

// trait NoFullscreen <: Visu {

//   override lazy val visuMainFrame = new MainFrame(gc) {
//     preferredSize = new Dimension(pixelWidth, pixelHeight)
//     contents = Surface
//     background = Color.black
//   }
// }
