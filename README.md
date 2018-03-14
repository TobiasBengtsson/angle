# Angle

Allows you to calculate with angles in a type-safe manner in Haskell.

## Import

`import Data.Angle`

## Create an angle

Create an angle from radians:

`radians (2 * pi)`

Create an angle from degrees:

`degrees 180`

## Perform arithmetic operations on angles

Add two angles

`angle1 ∠+∠ angle2`

Subtract angles

`angle1 ∠-∠ angle2`

Multiply an angle with a number

`angle ∠* n`

Divide an angle with a number

`angle ∠/ n`

Divide two angles (and get a number)

`angle1 ∠/∠ angle2`

## Perform trigonometric operations with angles

`sine angle`

`cosine angle`

`tangent angle`

Inverses gives an angle given a number:

`arcsine x`

`arccosine x`

`arctangent x`

## Get an angle as a number

Get an angle in radians

`getRadians angle`

Get an angle in degrees

`getDegrees angle`
