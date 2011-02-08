import hypermedia.video.*;

OpenCV opencv;
int threshold = 80;
int startX = -1; int startY = -1;
int foundX = -1; int foundY = -1;

int tentacles_num = 8;

class ncephalo { // cephalopoda
   int cx = -1; int cy = -1;
   int radius = -1;
   ncephalo[] children = new ncephalo[tentacles_num];
}

void setup() {
  
    size(320,240,P2D);
    
    opencv = new OpenCV(this);
    opencv.capture(width,height);
    
}

void draw() {
  
    opencv.read();
    opencv.convert(OpenCV.GRAY);    
    
    opencv.threshold(threshold);
    PImage curImage = opencv.image();
    image(curImage, 0, 0);
   
    curImage.loadPixels();
    color[] pxs = curImage.pixels; 
    
    if ((startX < 0) || (startY < 0)) {
        foundX = 0; foundY = 0;
        while (lookForPossibleCephalo(pxs)) {
            drawChain(buildChain(pxs, foundX, foundY));
        }        
    } else {
        drawChain(buildChain(pxs, startX, startY));
    }
    
}

void keyPressed() {
    if (key == '[') threshold--;
    else if (key == ']') threshold++;    
}

void mousePressed() {
    startX = mouseX; 
    startY = mouseY;
}

boolean lookForPossibleCephalo(color[] pxs) {
    return false;
}

ncephalo buildChain(color[] pxs, int x, int y) {
    return null;
}

void drawChain(ncephalo head) {
    if (head == null) return;
}
