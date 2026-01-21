import React from 'react';
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
export declare const LogoCanvas: React.ForwardRefExoticComponent<LogoCanvasProps & React.RefAttributes<LogoCanvasRef>>;
export default LogoCanvas;
