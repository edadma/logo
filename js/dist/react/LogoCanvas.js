import { jsx as _jsx } from "react/jsx-runtime";
import { useRef, useEffect, useImperativeHandle, forwardRef } from 'react';
export const LogoCanvas = forwardRef(({ width = 600, height = 400, program, pathRendering = true, onError, onComplete, className, style, }, ref) => {
    const canvasRef = useRef(null);
    const logoRef = useRef(null);
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
            }
            catch (e) {
                onError?.(e instanceof Error ? e : new Error(String(e)));
            }
        }
    }, [program, onError, onComplete]);
    // Expose methods via ref
    useImperativeHandle(ref, () => ({
        execute: (command) => {
            if (logoRef.current) {
                try {
                    logoRef.current.execute(command);
                }
                catch (e) {
                    onError?.(e instanceof Error ? e : new Error(String(e)));
                }
            }
        },
        run: (prog) => {
            if (logoRef.current) {
                try {
                    logoRef.current.run(prog);
                    onComplete?.();
                }
                catch (e) {
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
            return (logoRef.current?.getTurtle() ?? {
                x: 0,
                y: 0,
                heading: Math.PI / 2,
                visible: true,
            });
        },
        getLogo: () => logoRef.current,
    }));
    return (_jsx("canvas", { ref: canvasRef, width: width, height: height, className: className, style: { backgroundColor: 'white', ...style } }));
});
LogoCanvas.displayName = 'LogoCanvas';
export default LogoCanvas;
