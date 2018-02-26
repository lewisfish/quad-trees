# Quad-trees
Pure Fortran implementation of [quad-trees](https://en.wikipedia.org/wiki/Quadtree)
 for images.

Creates quadtree of input image based upon some error metric for each 'pixel/leaf'. Stopping condition is when quadtree's level is 1, 
i.e the width of the pixel is 1.

Requirements:
* Gfortran (>=5.4.1)
* ImageMagick (>=6.8.9-9) required for input/output files that are not ppm.

Compilation:
* gfortran utils.f90 Image_mod.f90 quadclass.f90 main.f90
     
     
# Example
### Input image
![input](https://raw.githubusercontent.com/lewisfish/quad-trees/master/owl.png)

### Output image

![output](https://raw.githubusercontent.com/lewisfish/quad-trees/master/outowl.png)  

### Output with 1 pixel wide border on each leaf  

![output-border](https://raw.githubusercontent.com/lewisfish/quad-trees/master/outowl-border.png)

