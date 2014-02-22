-- one module : one file
import Geometry

-- module and submodules : folder
import qualified Math.Cube as Cube

-- using our Geometry module
s = sphereVolume 1.0

-- using our Math.Cube submodule
c = Cube.volume 1.0
