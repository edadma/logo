import React, { useRef, useEffect, useImperativeHandle, forwardRef } from 'react';

// Type definitions for the Logo class from Scala.js
interface LogoDrawing {
  lines: Array<{
    x1: number;
    y1: number;
    x2: number;
    y2: number;
    color: string;
    width: number;
  }>;
  labels: Array<{
    x: number;
    y: number;
    heading: number;
    text: string;
  }>;
}

interface TurtleState {
  x: number;
  y: number;
  heading: number;
  visible: boolean;
}

interface LogoInstance {
  run(program: string): void;
  execute(command: string): void;
  clear(): void;
  render(): void;
  setPathRendering(enabled: boolean): void;
  setAutoRender(enabled: boolean): void;
  getDrawing(): LogoDrawing;
  getTurtle(): TurtleState;
}

declare const Logo: new (canvas: HTMLCanvasElement) => LogoInstance;

export interface LogoCanvasProps {
  /** Width of the canvas in pixels */
  width?: number;
  /** Height of the canvas in pixels */
  height?: number;
  /** Logo program to run */
  program?: string;
  /** Whether to use path-based rendering (smoother curves) */
  pathRendering?: boolean;
  /** Callback when an error occurs */
  onError?: (error: Error) => void;
  /** Callback when program execution completes */
  onComplete?: () => void;
  /** Additional CSS class for the canvas */
  className?: string;
  /** Additional inline styles for the canvas */
  style?: React.CSSProperties;
}

export interface LogoCanvasRef {
  /** Execute a Logo command */
  execute: (command: string) => void;
  /** Run a full Logo program */
  run: (program: string) => void;
  /** Clear the canvas and reset turtle */
  clear: () => void;
  /** Get the current drawing data */
  getDrawing: () => LogoDrawing;
  /** Get the current turtle state */
  getTurtle: () => TurtleState;
  /** Get the underlying Logo instance */
  getLogo: () => LogoInstance | null;
}

export const LogoCanvas = forwardRef<LogoCanvasRef, LogoCanvasProps>(
  (
    {
      width = 600,
      height = 400,
      program,
      pathRendering = true,
      onError,
      onComplete,
      className,
      style,
    },
    ref
  ) => {
    const canvasRef = useRef<HTMLCanvasElement>(null);
    const logoRef = useRef<LogoInstance | null>(null);

    // Initialize Logo instance
    useEffect(() => {
      if (canvasRef.current && typeof Logo !== 'undefined') {
        logoRef.current = new Logo(canvasRef.current);
        logoRef.current.setPathRendering(pathRendering);
      }
    }, []);

    // Update path rendering setting
    useEffect(() => {
      if (logoRef.current) {
        logoRef.current.setPathRendering(pathRendering);
      }
    }, [pathRendering]);

    // Run program when it changes
    useEffect(() => {
      if (logoRef.current && program !== undefined) {
        try {
          logoRef.current.clear();
          logoRef.current.run(program);
          onComplete?.();
        } catch (e) {
          onError?.(e instanceof Error ? e : new Error(String(e)));
        }
      }
    }, [program, onError, onComplete]);

    // Expose methods via ref
    useImperativeHandle(ref, () => ({
      execute: (command: string) => {
        if (logoRef.current) {
          try {
            logoRef.current.execute(command);
          } catch (e) {
            onError?.(e instanceof Error ? e : new Error(String(e)));
          }
        }
      },
      run: (prog: string) => {
        if (logoRef.current) {
          try {
            logoRef.current.run(prog);
            onComplete?.();
          } catch (e) {
            onError?.(e instanceof Error ? e : new Error(String(e)));
          }
        }
      },
      clear: () => {
        logoRef.current?.clear();
      },
      getDrawing: () => {
        return logoRef.current?.getDrawing() ?? { lines: [], labels: [] };
      },
      getTurtle: () => {
        return (
          logoRef.current?.getTurtle() ?? {
            x: 0,
            y: 0,
            heading: Math.PI / 2,
            visible: true,
          }
        );
      },
      getLogo: () => logoRef.current,
    }));

    return (
      <canvas
        ref={canvasRef}
        width={width}
        height={height}
        className={className}
        style={{ backgroundColor: 'white', ...style }}
      />
    );
  }
);

LogoCanvas.displayName = 'LogoCanvas';

export default LogoCanvas;
