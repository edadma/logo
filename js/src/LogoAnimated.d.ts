import type { LogoDrawing, TurtleState } from './Logo'

/** Animated Logo interpreter with async execution support */
export declare class LogoAnimated {
  constructor(canvas: HTMLCanvasElement)

  // === Animation Control ===

  /** Set the animation speed in milliseconds between yield points. 0 = instant (no animation). */
  setSpeed(ms: number): void

  /** Get the current animation speed */
  getSpeed(): number

  /** Check if execution is paused */
  isPaused(): boolean

  /** Check if execution is running */
  isRunning(): boolean

  /** Pause execution */
  pause(): void

  /** Resume execution after pause */
  resume(): void

  /** Stop execution completely */
  stop(): void

  // === Execution ===

  /** Run a Logo program with animation (respects speed setting) */
  run(program: string): void

  /** Execute a single command synchronously (always instant, for REPL-style use) */
  execute(command: string): string | undefined

  /** Clear the canvas and reset turtle */
  clear(): void

  // === Callbacks ===

  /** Set callback for each animation step (called after each yield point) */
  onStep(callback: () => void): void

  /** Set callback for completion (receives result or undefined) */
  onComplete(callback: (result: string | undefined) => void): void

  /** Set callback for errors */
  onError(callback: (error: string) => void): void

  /** Set output handler for print statements */
  setOutputHandler(handler: (text: string) => void): void

  /** Clear the output handler */
  clearOutputHandler(): void

  // === Rendering Settings ===

  /** Enable or disable path-based rendering (smoother curves) */
  setPathRendering(enabled: boolean): void

  /** Set the canvas background color */
  setBackgroundColor(color: string): void

  /** Set the default pen color (for theme-aware drawing) */
  setForegroundColor(color: string): void

  /** Force a render */
  render(): void

  // === Variables ===

  /** Set a global variable */
  setVariable(name: string, value: any): void

  /** Get a global variable */
  getVariable(name: string): string | undefined
}
