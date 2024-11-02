package io.github.edadma.logo

import pprint.pprintln

import java.awt.event.{ActionEvent, InputEvent, KeyEvent}
import java.awt.image.BufferedImage
import java.awt.{Color, Font, Toolkit}
import java.io.{ByteArrayOutputStream, File, PrintWriter, StringWriter}
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.undo.UndoManager
import javax.swing.{AbstractAction, BorderFactory, ImageIcon, KeyStroke}
import scala.Console.withOut
import scala.io.Source
import scala.language.postfixOps
import scala.swing.*
import scala.swing.event.*

class MultiPagePanel extends BoxPanel(Orientation.Vertical):
  def setImages(images: List[BufferedImage]): Unit =
    contents.clear()

    for (img <- images)
      val label = new Label:
        icon = new ImageIcon(img)
        border = BorderFactory.createLineBorder(Color.BLACK, 1)

      contents +=
        new BoxPanel(Orientation.Vertical):
          contents += label
          border = BorderFactory.createEmptyBorder(10, 10, 0, 10)

    revalidate()
    repaint()

object LogoPlayground extends SimpleSwingApplication:
  val screenSize = Toolkit.getDefaultToolkit.getScreenSize

  def top: Frame = new MainFrame:
    title = "Scriptura Playground"

    // Left panel components
    private val inputArea = new TextArea {
      rows = 20
      lineWrap = true
      wordWrap = true
      font = new Font("Monospaced", Font.PLAIN, 14)
      lineWrap = false
    }

    val undoManager = new UndoManager()

    inputArea.peer.getDocument.addUndoableEditListener { event =>
      undoManager.addEdit(event.getEdit)
    }

    private val undoKey = KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.CTRL_DOWN_MASK)
    inputArea.peer.getInputMap.put(undoKey, "Undo")
    inputArea.peer.getActionMap.put(
      "Undo",
      new javax.swing.AbstractAction {
        override def actionPerformed(e: ActionEvent): Unit = {
          if (undoManager.canUndo) undoManager.undo()
        }
      },
    )

    val redoKey = KeyStroke.getKeyStroke(KeyEvent.VK_Z, InputEvent.SHIFT_DOWN_MASK | InputEvent.CTRL_DOWN_MASK)
    inputArea.peer.getInputMap.put(redoKey, "Redo")
    inputArea.peer.getActionMap.put(
      "Redo",
      new javax.swing.AbstractAction {
        override def actionPerformed(e: ActionEvent): Unit = {
          if (undoManager.canRedo) undoManager.redo()
        }
      },
    )

    val errorOutput = new TextArea {
      rows = 5
      editable = false
    }
    val runButton = new Button("Run")

    // Define the key stroke for Ctrl-R
    val ctrlRKeyStroke = KeyStroke.getKeyStroke("control R")

    // Get the InputMap and ActionMap from the root pane
    val inputMap  = peer.getRootPane.getInputMap(javax.swing.JComponent.WHEN_IN_FOCUSED_WINDOW)
    val actionMap = peer.getRootPane.getActionMap

    // Bind the Ctrl-R keystroke to the "pushButton" action
    inputMap.put(ctrlRKeyStroke, "pushButton")

    // Define the action to perform when Ctrl-R is pressed
    actionMap.put(
      "pushButton",
      new AbstractAction {
        override def actionPerformed(e: ActionEvent): Unit = {
          runAction()
        }
      },
    )

    val multiPagePanel   = new MultiPagePanel
    val outputScrollPane = new ScrollPane(multiPagePanel)

    // Adding components to the left panel
    val leftPanel = new BoxPanel(Orientation.Vertical) {
      border = Swing.EmptyBorder(10, 10, 10, 10)

      contents += new ScrollPane(inputArea)
      contents += new ScrollPane(errorOutput)
      contents += new FlowPanel(FlowPanel.Alignment.Center)(runButton)
    }

    // Define the File menu with Open and Save items
    val fileMenu = new Menu("File") {
      // Open MenuItem
      val openItem = new MenuItem(Action("Open") {
        openFile()
      }) {
        mnemonic = Key.O
        peer.setAccelerator(KeyStroke.getKeyStroke("ctrl O"))
      }

      // Save MenuItem
      val saveItem = new MenuItem(Action("Save") {
        saveFile()
      }) {
        mnemonic = Key.S
        peer.setAccelerator(KeyStroke.getKeyStroke("ctrl S"))
      }

      contents += openItem
      contents += saveItem
    }

    // Set the menu bar
    menuBar = new MenuBar {
      contents += fileMenu
      contents += Swing.HStrut(20)
      contents += runButton
    }

    // Function to handle opening a file
    def openFile(): Unit = {
      val chooser = new FileChooser(new File("."))
      chooser.title = "Open Script File"
      chooser.fileFilter = new FileNameExtensionFilter("Script Files (*.script)", "script")
      val result = chooser.showOpenDialog(this)
      if (result == FileChooser.Result.Approve) {
        val file = chooser.selectedFile
        try {
          val source = Source.fromFile(file)
          inputArea.text = source.mkString
          source.close()
        } catch {
          case ex: Exception =>
            Dialog.showMessage(this, s"Failed to open file:\n${ex.getMessage}", "Error", Dialog.Message.Error)
        }
      }
    }

    // Function to handle saving a file
    def saveFile(): Unit = {
      val chooser = new FileChooser(new File("."))
      chooser.title = "Save Script File"
      chooser.fileFilter = new FileNameExtensionFilter("Script Files (*.script)", "script")
      val result = chooser.showSaveDialog(this)
      if (result == FileChooser.Result.Approve) {
        var file = chooser.selectedFile
        // Ensure the file has a .script extension
        if (!file.getName.toLowerCase.endsWith(".script")) {
          file = new File(file.getAbsolutePath + ".script")
        }
        try {
          val writer = new PrintWriter(file)
          writer.write(inputArea.text)
          writer.close()
        } catch {
          case ex: Exception =>
            Dialog.showMessage(this, s"Failed to save file:\n${ex.getMessage}", "Error", Dialog.Message.Error)
        }
      }
    }

    // Main split pane
    val splitPane = new SplitPane(Orientation.Vertical, leftPanel, outputScrollPane) {
      oneTouchExpandable = true
      continuousLayout = true
      dividerLocation = screenSize.width / 2
    }

    def captureStdOut(block: => Unit): String =
      val outputStream = new ByteArrayOutputStream()

      withOut(outputStream) {
        block // Execute the block with stdout redirected
      }

      outputStream.toString

    def runAction(): Unit =
      try {
        val t = new Graphics2DTypesetter {
          // debug = true
          // ligatures = false
          setDocument(new ZFoldedDocument)
        }
        val p   = new ScripturaParser
        val r   = new ScripturaRenderer(t, Map.empty, p)
        val ast = p.parse(inputArea.text)

        errorOutput.text = captureStdOut {
          r.render(ast)
          t.end()
        }

        val pages = t.getDocument.printedPages.toList.asInstanceOf[List[BufferedImage]]
        val maxDividerLocation =
          splitPane.size.width - splitPane.rightComponent.minimumSize.width - 2 /*border*/ - 2 * 10 /*margin*/ - 7

        multiPagePanel.setImages(pages)

        val pagesWidth =
          pages.map(_.getWidth) match
            case Nil => 0
            case ws  => ws.max

        splitPane.dividerLocation = maxDividerLocation - pagesWidth
      } catch
        case error: Throwable =>
          val sw = new StringWriter
          val pw = new PrintWriter(sw)

          error.printStackTrace(pw)
          errorOutput.text = sw.toString
          multiPagePanel.setImages(Nil)
    end runAction

    // Event handling for the Run button
    listenTo(runButton)
    reactions += { case ButtonClicked(`runButton`) => runAction() }

    // Set up the main frame
    contents = splitPane
    size = new Dimension(screenSize.width, screenSize.height)
    inputArea.requestFocus()

    override def closeOperation(): Unit = dispose()
