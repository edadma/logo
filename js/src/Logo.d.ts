/** Drawing data returned by the Logo interpreter */
export interface LogoDrawing {
  lines: Array<{
    x1: number
    y1: number
    x2: number
    y2: number
    color: string
    width: number
  }>
  labels: Array<{
    x: number
    y: number
    heading: number
    text: string
  }>
}

/** Current state of the turtle */
export interface TurtleState {
  x: number
  y: number
  heading: number
  visible: boolean
}

/** Logo interpreter class from Scala.js */
export declare class Logo {
  constructor(canvas: HTMLCanvasElement)

  /** Run a complete Logo program */
  run(program: string): void

  /** Execute a single Logo command */
  execute(command: string): void

  /** Clear the canvas and reset turtle position */
  clear(): void

  /** Render the current drawing to the canvas */
  render(): void

  /** Enable or disable path-based rendering (smoother curves) */
  setPathRendering(enabled: boolean): void

  /** Enable or disable automatic rendering after each command */
  setAutoRender(enabled: boolean): void

  /** Set a callback for print output */
  setOutputHandler(handler: (text: string) => void): void

  /** Get the current drawing data */
  getDrawing(): LogoDrawing

  /** Get the current turtle state */
  getTurtle(): TurtleState
}
