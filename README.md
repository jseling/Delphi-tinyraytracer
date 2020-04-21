# Delphi-tinyraytracer
A raytracer implementation in Delphi translated from [ssloy/tinyraytracer](https://github.com/ssloy/tinyraytracer/wiki/Part-1:-understandable-raytracing)

- Optional use of [Delphi PPL library](http://docwiki.embarcadero.com/RADStudio/Rio/en/Using_the_Parallel_Programming_Library)
- Build with [Delphi 10.3 Rio Community Edition](https://www.embarcadero.com/products/delphi/starter)
- None third party library dependency

![](https://raw.githubusercontent.com/ssloy/tinyraytracer/master/out.jpg)

## To Do
- Better OOP approach
- Better scene building
- Checkboard as a scene object
- Refactor TRaytracer class (especially CastRay method)

## Not To Do
- Change records to objects: Performance questions. Memory stack and heap.
