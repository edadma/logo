package io.github.edadma.logo

import pprint.pprintln

import java.awt.event.{ActionEvent, InputEvent, KeyEvent}
import java.awt.{Frame => awtFrame, Color, Font, Toolkit}
import java.io.{ByteArrayOutputStream, File, PrintWriter, StringWriter}
import javax.swing.filechooser.FileNameExtensionFilter
import javax.swing.undo.UndoManager
import javax.swing.{AbstractAction, KeyStroke, SwingUtilities}
import scala.Console.withOut
import scala.io.Source
import scala.language.postfixOps
import scala.swing.*
import scala.swing.Swing.*
import scala.swing.event.*
import java.awt.{GridBagConstraints, Insets}
import scala.swing.GridBagPanel

object LogoPlayground extends SimpleSwingApplication:
  val screenSize   = Toolkit.getDefaultToolkit.getScreenSize
  var dev: Boolean = false

  def top: Frame = new MainFrame:
    title = "Logo Playground"

    // Left panel components
    private val inputArea = new TextArea {
      rows = 20
      lineWrap = true
      wordWrap = true
      font = new Font("Monospaced", Font.PLAIN, 16)
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

    // Define the one-line input field for immediate Logo command execution
    val commandInput = new TextField {
      font = new Font("Monospaced", Font.PLAIN, 16)
      listenTo(keys)
      reactions += {
        case KeyPressed(_, Key.Enter, _, _) =>
          if (text.nonEmpty) {
            executeCommand(text)
          }
      }
    }

    // Function to handle command execution from the one-line input field
    def executeCommand(command: String): Unit = {
      var success = false

      try {
        errorOutput.text = captureStdOut {
          logo.interp(command)
        }
        success = true
      } catch {
        case error: Throwable =>
          if dev then
            val sw = new StringWriter
            val pw = new PrintWriter(sw)

            error.printStackTrace(pw)
            errorOutput.text = sw.toString
          else
            errorOutput.text = error.getMessage
      }

      if success then commandInput.text = ""
    }

    val errorOutput = new TextArea {
      rows = 5
      editable = false
      font = new Font("Monospaced", Font.PLAIN, 14)
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

    val turtlePanel =
      new TurtlePanel:
        preferredSize = (3000, 3000)

    val logo =
      new Logo:
        def event(): Unit = turtlePanel.repaint()

    class TurtlePanel extends Panel:
//      preferredSize = (3000, 3000)
      background = Color.WHITE

      override protected def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)

        // Translate the origin to the center of the panel
        g.translate(size.width / 2, size.height / 2)

        // Invert the y-axis
        g.scale(1, -1)

        logo.drawing foreach {
          case Line(x1, y1, x2, y2, color) =>
            g.setColor(Color.black)

            val roundedX1 = Math.round(x1).toInt
            val roundedY1 = Math.round(y1).toInt
            val roundedX2 = Math.round(x2).toInt
            val roundedY2 = Math.round(y2).toInt

            g.drawLine(roundedX1, roundedY1, roundedX2, roundedY2)
        }
    end TurtlePanel

    val outputScrollPane = new ScrollPane(turtlePanel)

    SwingUtilities.invokeLater(() =>
      peer.setExtendedState(awtFrame.MAXIMIZED_BOTH),
    )
    SwingUtilities.invokeLater(() =>
      val viewport   = outputScrollPane.peer.getViewport
      val viewSize   = viewport.getViewSize
      val extentSize = viewport.getExtentSize

      // Calculate the top-left corner position to center the content
      val x = (viewSize.width - extentSize.width) / 2
      val y = (viewSize.height - extentSize.height) / 2

      // Set the position
      viewport.setViewPosition(new Point(x, y)),
    )

    // Adding components to the left panel
    val leftPanel = new GridBagPanel {
      border = Swing.EmptyBorder(10, 10, 10, 10)

      val c = new Constraints

      // Input label
      c.gridx = 0
      c.gridy = 0
      c.anchor = GridBagPanel.Anchor.West
      c.insets = new Insets(0, 0, 5, 0) // Add some space below the label
      layout(new Label("Input Area")) = c

      // Input area
      c.gridy = 1
      c.fill = GridBagPanel.Fill.Both
      c.weightx = 1.0
      c.weighty = 1.0
      layout(new ScrollPane(inputArea)) = c

      // Command label
      c.gridy = 2
      c.fill = GridBagPanel.Fill.None
      c.weighty = 0
      layout(new Label("Command Input")) = c

      // Command input field
      c.gridy = 3
      c.fill = GridBagPanel.Fill.Both
      layout(commandInput) = c

      // Error label
      c.gridy = 4
      c.fill = GridBagPanel.Fill.None
      layout(new Label("Error Output")) = c

      // Error output area
      c.gridy = 5
      c.fill = GridBagPanel.Fill.Both
      c.weighty = 1.0
      layout(new ScrollPane(errorOutput)) = c

      // Run button
      c.gridy = 6
      c.anchor = GridBagPanel.Anchor.Center
      c.fill = GridBagPanel.Fill.None
      c.weightx = 0
      c.weighty = 0
      layout(new FlowPanel(FlowPanel.Alignment.Center)(runButton)) = c
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
      chooser.title = "Open Logo File"
      chooser.fileFilter = new FileNameExtensionFilter("Logo Files (*.logo)", "logo")
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
      chooser.title = "Save Logo File"
      chooser.fileFilter = new FileNameExtensionFilter("Logo Files (*.logo)", "logo")
      val result = chooser.showSaveDialog(this)
      if (result == FileChooser.Result.Approve) {
        var file = chooser.selectedFile
        // Ensure the file has a .logo extension
        if (!file.getName.toLowerCase.endsWith(".logo")) {
          file = new File(file.getAbsolutePath + ".logo")
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
        logo.reset()
        errorOutput.text = captureStdOut {
          logo.interp(inputArea.text)
        }
      } catch
        case error: Throwable =>
          if dev then
            val sw = new StringWriter
            val pw = new PrintWriter(sw)

            error.printStackTrace(pw)
            errorOutput.text = sw.toString
          else
            errorOutput.text = error.getMessage
    end runAction

    // Event handling for the Run button
    listenTo(runButton)
    reactions += { case ButtonClicked(`runButton`) => runAction() }

    // Set up the main frame
    contents = splitPane
    size = new Dimension(screenSize.width, screenSize.height)
    commandInput.requestFocus()

    override def closeOperation(): Unit = dispose()
