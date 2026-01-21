# @edadma/logo

A Logo programming language interpreter for JavaScript/TypeScript with React component support.

## Installation

```bash
npm install @edadma/logo
```

## Basic Usage

```javascript
import { Logo } from '@edadma/logo';

const canvas = document.getElementById('myCanvas');
const logo = new Logo(canvas);

// Run a Logo program
logo.run(`
  repeat 4 [
    forward 100
    right 90
  ]
`);

// Execute single commands
logo.execute('forward 50');
logo.execute('right 90');

// Clear and reset
logo.clear();
```

## React Component

```jsx
import { LogoCanvas } from '@edadma/logo/react';
import { useRef } from 'react';

function App() {
  const logoRef = useRef(null);
  const [code, setCode] = useState('repeat 4 [fd 100 rt 90]');

  return (
    <div>
      <textarea value={code} onChange={e => setCode(e.target.value)} />
      <button onClick={() => logoRef.current?.run(code)}>Run</button>
      <button onClick={() => logoRef.current?.clear()}>Clear</button>

      <LogoCanvas
        ref={logoRef}
        width={600}
        height={400}
        pathRendering={true}
        onError={(e) => console.error(e)}
      />
    </div>
  );
}
```

### LogoCanvas Props

| Prop | Type | Default | Description |
|------|------|---------|-------------|
| `width` | number | 600 | Canvas width in pixels |
| `height` | number | 400 | Canvas height in pixels |
| `program` | string | - | Logo program to run (auto-runs on change) |
| `pathRendering` | boolean | true | Use smooth path rendering |
| `onError` | function | - | Error callback |
| `onComplete` | function | - | Completion callback |
| `className` | string | - | CSS class for canvas |
| `style` | object | - | Inline styles for canvas |

### LogoCanvas Ref Methods

```typescript
interface LogoCanvasRef {
  execute(command: string): void;
  run(program: string): void;
  clear(): void;
  getDrawing(): LogoDrawing;
  getTurtle(): TurtleState;
  getLogo(): Logo | null;
}
```

## API Reference

### Logo Class

```typescript
class Logo {
  constructor(canvas: HTMLCanvasElement);

  run(program: string): void;
  execute(command: string): void;
  clear(): void;
  render(): void;
  setPathRendering(enabled: boolean): void;
  setAutoRender(enabled: boolean): void;
  getDrawing(): LogoDrawing;
  getTurtle(): TurtleState;
}
```

### Drawing Data

If you want to implement custom rendering:

```typescript
interface LogoDrawing {
  lines: Array<{
    x1: number; y1: number;
    x2: number; y2: number;
    color: string;  // e.g., "rgb(0,0,0)"
    width: number;
  }>;
  labels: Array<{
    x: number; y: number;
    heading: number;  // radians
    text: string;
  }>;
}

interface TurtleState {
  x: number;
  y: number;
  heading: number;  // radians, π/2 = north
  visible: boolean;
}
```

## Supported Logo Commands

### Movement
- `forward <dist>` / `fd` - Move forward
- `back <dist>` / `bk` - Move backward
- `right <angle>` / `rt` - Turn right (degrees)
- `left <angle>` / `lt` - Turn left (degrees)
- `setxy <x> <y>` - Move to position
- `home` - Return to center
- `towards [x y]` - Return heading toward point
- `distance [x y]` - Return distance to point
- `arc <angle> <radius>` - Draw an arc

### Pen Control
- `penup` / `pu` - Lift pen
- `pendown` / `pd` - Lower pen
- `setpensize <size>` - Set line width
- `setcolor <color>` - Set pen color (name, number, or [r g b])

### Turtle
- `hideturtle` / `ht` - Hide turtle
- `showturtle` / `st` - Show turtle

### Control Flow
- `repeat <n> [commands]` - Repeat commands
- `for [var start end step] [commands]` - For loop
- `while [condition] [commands]` - While loop
- `until [condition] [commands]` - Until loop
- `forever [commands]` - Loop until stop/output
- `if <cond> [commands]` - Conditional
- `ifelse <cond> [yes] [no]` - If-else

### Procedures
- `to <name> <:params> ... end` - Define procedure
- `output <value>` / `op` - Return value
- `stop` - Stop procedure

### Math
- `sum`, `difference`, `product`, `quotient`, `remainder`
- `sin`, `cos`, `tan`, `sqrt`, `pow`, `exp`, `ln`, `log10`
- `asin`, `acos`, `atan`, `atan2` - inverse trig (complex-aware)
- `abs`, `int`, `round`, `floor`, `ceiling`, `sign`
- `min`, `max` - variadic min/max
- `random` - random number
- `pi`, `e` - constants
- Infix operators: `+`, `-`, `*`, `/`, `^`, `=`, `<`, `>`, `<=`, `>=`

### Strings
- `lowercase`, `uppercase` - case conversion
- `ascii`, `char` - character codes

### Variables
- `make "name <value>` - Set variable
- `:name` - Get variable value
- `thing "name` - Get variable value
- `local "name` - Declare procedure-local variable
- `localmake "name <value>` - Declare and set local variable

### Lists
- `first`, `last`, `butfirst` (`bf`), `butlast` (`bl`)
- `fput`, `lput`, `item`, `count`
- `list`, `sentence` (`se`), `word`
- `range` / `iseq` - generate sequences: `range 5` → `[0 1 2 3 4]`
- `reverse`, `pick` - list utilities
- `emptyp`, `listp`, `wordp`, `numberp`, `memberp` - predicates

### Time
- `time` - Returns `[hours minutes seconds]` (UTC)
- `date` - Returns `[year month day]` (UTC)
- `timemilli` - Milliseconds since epoch

### Workspace Inspection
- `namep "name` / `name? "name` - Is variable defined?
- `definedp "name` / `defined? "name` - Is procedure defined?
- `primitivep "name` / `primitive? "name` - Is it a primitive?
- `procedurep "name` / `procedure? "name` - Is it a user procedure?
- `procedures` - List all user procedures
- `primitives` - List all primitives
- `names` - List all variables

### Higher-Order Functions
- `map <fn> <list>` - Apply function to each element
- `filter <fn> <list>` - Keep elements where fn returns true
- `reduce <fn> <list>` - Reduce list to single value
- `foreach <list> <fn>` - Execute fn for each element
- `apply <fn> <list>` - Apply fn with list as arguments

## License

ISC
