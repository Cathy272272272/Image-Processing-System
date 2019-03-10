# Image-Processing-System
Implement a mini image processing system for PPM images, with the following functions.
a PPM image has the following format:

P3

2 2

255

0 0 0 100 0 0
0 0 0 0 255 175

where 
"
    P3 -- magic number, only has the types P3(“plain PPM file) or P6(“rare PPM file).
    2 2 -- image dimensions
    255 --max color
"
is the header. 
The rest are the body, which is filled with RGB values accoring to image dimensions, (x, y, z) every 3 samples form a RGB, where each sample is greater or equal to 0 and less or equal to max color.

The system has the following functions:

grayscale: make the PPM image into a gray PPM image by taking average of each RGB in PPM.

autoThresholding: giving a threshold value, turn the PPM image into a black and white PPM image accoring to the threshold.(the PPM image should be graysclaed first)

blend: adding each RGB of several PPM together, with the maximum not exceeded.

findContours: find the contours of a thresholded image, and find its contours.
This is implemented by Square Tracing Algorithm with the help of Jacob's stopping criterion, which refers to the condition that a position is visited twice with the same entering direction.  


Helper notes for undrstanding the code:

Edgecase handling:
I assume if touching the edge, it only turn direction and remain at the same position.
e.g. if current position is (0, 0), given direction up, which would be out of bound (0, -1).
at this point, we stay at current position and change its next direction.
If current position is black, then the new direction would be left(up turn left is left).
Otherwise, the new direction would be right.

Direction Recorded Vector:
I use a dirvec in ImageProc.hs to record the first-time entering direction of each position, with values -1, 0, 1, 2, 3 only, where -1 means the position hasn't been entered, and other values are the first-time entering direction.
When entering a position where its recorded direction is the same as its current position, the loop will be finished.
That is because if given the same position and same starting dirction, the path is determined, and in this case, the path has already been visited once, so we don't need to loop again.

