/** A line segment in the drawing */
export interface LineData {
  x1: number;
  y1: number;
  x2: number;
  y2: number;
  /** CSS color string, e.g., "rgb(0,0,0)" */
  color: string;
  /** Line width in pixels */
  width: number;
}

/** A text label in the drawing */
export interface LabelData {
  x: number;
  y: number;
  /** Heading in radians */
  heading: number;
  text: string;
}

/** The complete drawing data */
export interface LogoDrawing {
  lines: LineData[];
  labels: LabelData[];
}

/** The turtle's current state */
export interface TurtleState {
  x: number;
  y: number;
  /** Heading in radians (0 = east, π/2 = north) */
  heading: number;
  visible: boolean;
}

/**
 * Logo programming language interpreter
 *
 * @example
 * ```javascript
 * const canvas = document.getElementById('myCanvas');
 * const logo = new Logo(canvas);
 * logo.run("repeat 4 [fd 100 rt 90]");
 * ```
 */
export declare class Logo {
  /**
   * Create a new Logo interpreter that renders to the given canvas
   * @param canvas - The HTML canvas element to render to
   */
  constructor(canvas: HTMLCanvasElement);

  /**
   * Run a Logo program
   * @param program - The Logo code to execute
   */
  run(program: string): void;

  /**
   * Execute a single Logo command
   * @param command - The command to execute
   */
  execute(command: string): void;

  /**
   * Clear the canvas and reset the turtle to home position
   */
  clear(): void;

  /**
   * Force a render of the current drawing state
   */
  render(): void;

  /**
   * Enable or disable path-based rendering
   * Path rendering groups consecutive lines with the same style
   * for smoother curves
   * @param enabled - Whether to use path rendering (default: true)
   */
  setPathRendering(enabled: boolean): void;

  /**
   * Enable or disable automatic rendering after each command
   * @param enabled - Whether to auto-render (default: true)
   */
  setAutoRender(enabled: boolean): void;

  /**
   * Get the current drawing data for custom rendering
   * @returns The drawing data including all lines and labels
   */
  getDrawing(): LogoDrawing;

  /**
   * Get the current turtle state
   * @returns The turtle's position, heading, and visibility
   */
  getTurtle(): TurtleState;
}

export default Logo;
