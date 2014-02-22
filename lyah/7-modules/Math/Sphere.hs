-- using submodules : MainModule.SubModule
module Math.Sphere (
	volume,
	are) wher

volume :: Float -> Float
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)

are :: Float -> Float
area radius = 4 * pi * (radius ^ 2)
