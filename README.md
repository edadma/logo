# Logo

A cross-platform Logo programming language interpreter written in Scala, targeting JVM, JavaScript, and Native platforms.

## Features

- **Cross-platform**: Runs on JVM, JavaScript (browser/Node.js), and Native
- **UCB Logo compatible**: Implements many procedures from Berkeley Logo
- **Turtle graphics**: Full turtle graphics support with canvas rendering
- **Infix expressions**: Mathematical expressions with proper precedence (`2 + 3 * 4`)
- **Control structures**: `if`, `ifelse`, `repeat`, `for`, `while`, `until`, `forever`
- **Procedures**: User-defined procedures with required, optional, and rest parameters
- **Local variables**: `local` and `localmake` for procedure-scoped variables
- **Lists**: Full list manipulation (`first`, `butfirst`, `last`, `butlast`, `fput`, `lput`, etc.)
- **Higher-order functions**: `map`, `filter`, `reduce`, `foreach`, `apply`
- **Workspace inspection**: `namep`, `definedp`, `primitivep`, `procedures`, `names`

## Installation

### Scala (JVM/Native)

Add to your `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "logo" % "0.1.0"
```

### JavaScript/TypeScript (npm)

```bash
npm install @edadma/logo
```

## Basic Usage

### Scala

```scala
import io.github.edadma.logo._

val logo = new Logo {
  def event(): Unit = ()
}

logo.interp("""
  to square :size
    repeat 4 [fd :size rt 90]
  end
  square 100
""")

// Get drawing commands
val drawing: Seq[Draw] = logo.drawing
```

### JavaScript/TypeScript

```typescript
import { Logo } from '@edadma/logo';

const logo = new Logo(() => {});

logo.interp(`
  to spiral :size :angle
    if :size > 100 [stop]
    fd :size
    rt :angle
    spiral :size + 2 :angle
  end
  spiral 1 91
`);

// Get drawing as array of commands
const drawing = logo.drawing();
```

### React Component

```tsx
import { LogoCanvas } from '@edadma/logo/react';

function App() {
  const code = `
    repeat 36 [
      repeat 4 [fd 50 rt 90]
      rt 10
    ]
  `;

  return <LogoCanvas code={code} width={400} height={400} />;
}
```

## Turtle Graphics Commands

| Command | Description |
|---------|-------------|
| `forward n` / `fd n` | Move forward n steps |
| `back n` / `bk n` | Move backward n steps |
| `right n` / `rt n` | Turn right n degrees |
| `left n` / `lt n` | Turn left n degrees |
| `penup` / `pu` | Lift pen (stop drawing) |
| `pendown` / `pd` | Lower pen (start drawing) |
| `home` | Return to center, heading north |
| `clearscreen` / `cs` | Clear and reset |
| `setxy x y` | Move to position |
| `setx x` | Set x coordinate |
| `sety y` | Set y coordinate |
| `setheading n` / `seth n` | Set heading in degrees |
| `setpencolor c` / `setpc c` | Set pen color |
| `setpensize n` | Set pen width |
| `hideturtle` / `ht` | Hide turtle |
| `showturtle` / `st` | Show turtle |
| `arc angle radius` | Draw an arc |
| `towards [x y]` | Heading toward point |
| `distance [x y]` | Distance to point |

## Arithmetic

| Operator/Procedure | Description |
|-------------------|-------------|
| `+` `-` `*` `/` | Infix operators |
| `^` | Power (right-associative) |
| `sum`, `difference`, `product`, `quotient` | Prefix arithmetic |
| `remainder`, `modulo` | Division remainder |
| `sqrt`, `sin`, `cos`, `tan`, `abs` | Math functions |
| `random n` | Random integer 0 to n-1 |
| `round`, `int` | Rounding |

## Control Flow

```logo
if :x > 0 [print "positive]

ifelse :x > 0 [print "positive] [print "non-positive]

repeat 4 [fd 100 rt 90]

for [i 1 10] [print :i]

while [:count > 0] [make "count :count - 1]

forever [fd 1 rt 1]  ; exits on stop or output
```

## Procedures

```logo
to square :size
  repeat 4 [fd :size rt 90]
end

to greet :name [:greeting "Hello]  ; optional parameter
  print (sentence :greeting :name)
end

to average [:nums]  ; rest parameter
  output (apply "sum :nums) / count :nums
end
```

## Local Variables

```logo
to example
  local "x           ; declare local
  make "x 10         ; assign value
  localmake "y 20    ; declare and assign
  print :x + :y
end
```

## License

ISC License - see [LICENSE](LICENSE) for details.
