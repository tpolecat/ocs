package jsky.app.ot.gemini.editor.targetComponent.details

import java.awt.{GridBagConstraints, GridBagLayout, Insets}
import javax.swing.JPanel

import edu.gemini.pot.sp.ISPNode
import edu.gemini.shared.util.immutable.{Option => JOption}
import edu.gemini.spModel.obs.context.ObsContext
import edu.gemini.spModel.target.SPTarget
import edu.gemini.spModel.target.system.CoordinateParam
import jsky.app.ot.gemini.editor.targetComponent.TelescopePosEditor
import jsky.app.ot.gemini.editor.targetComponent.details.NumericPropertySheet.Prop
import jsky.util.gui.{NumberBoxWidget, TextBoxWidget, TextBoxWidgetWatcher}
import squants.{Quantity, UnitOfMeasure}

import scala.swing.ListView.Renderer
import scala.swing.event.SelectionChanged
import scala.swing._
import scalaz.std.list._
import scalaz.syntax.functor._
import scalaz.syntax.id._

// An editor for a list of doubles, with a titled border
case class NumericPropertySheet[A](title: Option[String], f: SPTarget => A, props: NumericPropertySheet.Prop[A, _]*)
  extends JPanel with TelescopePosEditor with Publisher with ReentrancyHack {

  private[this] var spt: SPTarget = new SPTarget // never null

  val pairs: List[(NumericPropertySheet.Prop[A, _], NumberBoxWidget)] = props.toList.fproduct(editField)

  title.foreach(t => setBorder(titleBorder(t)))
  setLayout(new GridBagLayout)
  pairs.zipWithIndex.foreach { case ((p, w), row) =>
    val ins = new Insets(0, 2, 0, 2)
    add(p.leftComponent.peer, new GridBagConstraints <| { c =>
      c.gridx = 0
      c.gridy = row
      c.fill = GridBagConstraints.HORIZONTAL
      c.insets = ins
    })
    add(w, new GridBagConstraints <| { c =>
      c.gridx = 1
      c.gridy = row
      c.insets = ins
    })
    add(p.rightComponent.peer, new GridBagConstraints <| { c =>
      c.gridx = 2
      c.gridy = row
      c.weightx = 1
      c.fill = GridBagConstraints.HORIZONTAL
      c.insets = ins
    })

    // if the right component is a unit selector we need to
    // update the value every time the unit is changed
    Option(p.rightComponent).foreach {
      case c: ComboBox[_] =>
        listenTo(c.selection)
        reactions += {
          case SelectionChanged(`c`) =>
            updateField(p, w)
        }
      case _ =>
    }
    // if the left side component is a selector we need to support
    // transforming the value on the text without writing it back to the
    // model until the user edits it
    Option(p.leftComponent).foreach {
      case c: ComboBox[_] =>
        listenTo(c.selection)
        reactions += {
          case SelectionChanged(`c`) =>
            transformField(p, w)
        }
      case _ =>
    }

  }


  def edit(ctx: JOption[ObsContext], target: SPTarget, node: ISPNode): Unit =
    nonreentrant {
      spt = target
      pairs.foreach { case (p, w) => p.init(w, f(target)) }
    }

  private def editField(p: Prop[A, _]) =
    new NumberBoxWidget {
      setColumns(15)
      setMinimumSize(getPreferredSize)
      addWatcher(new TextBoxWidgetWatcher {
        def textBoxKeyPress(tbwe: TextBoxWidget): Unit = updateField(p, tbwe)
        def textBoxAction(tbwe: TextBoxWidget): Unit   = updateField(p, tbwe)

      })
    }

  private def updateField(p: Prop[A, _], tbwe: TextBoxWidget) =
    try
      nonreentrant {
        p.edit(f(spt), tbwe.getValue.toDouble)
        spt.notifyOfGenericUpdate()
      }
    catch { case _: NumberFormatException => }

  private def transformField(p: Prop[A, _], tbwe: TextBoxWidget) =
    try {
      pairs.find(_._1 == p).foreach { w =>
        p.transform(w._2, f(spt), tbwe.getValue.toDouble)
      }
    } catch { case _: NumberFormatException => }

}

object NumericPropertySheet {

  sealed trait Prop[A, B] {
    def leftComponent: Component
    def rightComponent: Component
    def edit: (A, Double)                       => Unit
    def transform: (NumberBoxWidget, A, Double) => Unit = (_, _, _) => {}
    def init: (NumberBoxWidget, A)              => Unit
  }

  case class DoubleProp[A](leftCaption: String, rightCaption: String, get: A => Double, edit: (A, Double) => Unit) extends Prop[A, Double] {
    val leftComponent  = new Label(leftCaption)
    val rightComponent = new Label(rightCaption)
    def init: (NumberBoxWidget, A) => Unit  =  (w,a) => {
      w.setValue(get(a))
    }
  }

  case class TransformableProp[A, B](leftOptions: List[B], rightCaptions: Map[B, String], initial: B, render: B => String, get: (A, B) => Double, set: (A, B, Double) => Unit) extends Prop[A, Double] {
    val rightComponent = new Label(rightCaptions.getOrElse(initial, ""))
    val leftComponent  = new ComboBox[B](leftOptions) {
      selection.item = initial
      renderer = Renderer(render)
      listenTo(selection)
      reactions += {
        case SelectionChanged(_) =>
          rightComponent.text = rightCaptions.getOrElse(selection.item, "")
      }
    }

    def edit: (A, Double)          => Unit  =  (a, d) => {
      set(a, leftComponent.selection.item, d)
    }
    override def transform: (NumberBoxWidget,  A, Double)          => Unit  =  (w, a, d) => {
      w.setValue(get(a, leftComponent.selection.item))
    }
    def init: (NumberBoxWidget, A) => Unit  =  (w, a) => {
      w.setValue(get(a, leftComponent.selection.item))
    }
  }

  case class SingleUnitQuantity[A, B <: Quantity[B]](leftCaption: String, unit: UnitOfMeasure[B], get: A => B, set: (A, B) => Unit) extends Prop[A, B] {
    val leftComponent  = new Label(leftCaption)
    val rightComponent = new Label(unit.symbol)
    def edit: (A, Double)          => Unit  =  (a, d) => set(a, unit(d))
    def init: (NumberBoxWidget, A) => Unit  =  (w, a) => {
      w.setValue(get(a).value)
    }
  }

  case class MultiUnitQuantity[A, B <: Quantity[B]](leftCaption: String, units: Seq[UnitOfMeasure[B]], get: A => B, set: (A, B) => Unit) extends Prop[A, B] {
    val leftComponent  = new Label(leftCaption)
    val rightComponent = new ComboBox[UnitOfMeasure[B]](units) {renderer = Renderer(_.symbol)}
    def unit           = rightComponent.selection.item
    def edit: (A, Double)          => Unit  =  (a, d) => set(a, unit(d))
    def init: (NumberBoxWidget, A) => Unit  =  (w, a) => {
      w.setValue(get(a).value)
      rightComponent.selection.item = get(a).unit
    }
  }

  object Prop {

    def apply[A](leftCaption: String, rightCaption: String, f: A => Double, g: (A, Double) => Unit): Prop[A, Double] =
      DoubleProp(leftCaption, rightCaption, f, g)

    def apply[A](leftCaption: String, rightCaption: String, f: A => CoordinateParam): Prop[A, Double] =
      DoubleProp(leftCaption, rightCaption, a => f(a).getValue, (a, b) => f(a).setValue(b))

    def apply[A, B](leftOptions: List[B], rightCaptions: Map[B, String], initial: B, render: B => String, f: (A, B) => Double, g: (A, B, Double) => Unit): Prop[A, Double] =
      TransformableProp(leftOptions, rightCaptions, initial, render, f, g)

    def apply[A, B <: Quantity[B]](leftCaption: String, get: A => B, set: (A, B) => Unit, units: UnitOfMeasure[B]*): Prop[A, B] =
      units.length match {
        case 0      => sys.error("quantity property needs at least one unit")
        case 1      => SingleUnitQuantity(leftCaption, units.head, get, set)
        case _      => MultiUnitQuantity(leftCaption,  units,      get, set)
      }

  }

}
