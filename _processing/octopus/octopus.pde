import hypermedia.video.*;

int MAX_CHAINS = 8; // maximum number of cephalopoda chains to search

OpenCV opencv;
int threshold = 80;
int minRad = 8; // minimum cephalo radius
int maxRad = 60; // maximum cephalo radius
int startX = -1; int startY = -1;
int[] foundPoints = new int[MAX_CHAINS * 2]; // found chains' start points

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
    
    // if start point is not set
    if ((startX < 0) || (startY < 0)) {
        // store possible chains' start points (not more than MAX_CHAINS) in foundPoints array 
        lookForPossibleChains(pxs);
        // for each of these start points
        for (int fIdx = 0; fIdx < MAX_CHAINS; fIdx++) {
            // if found point is set
            if ((foundPoints[fIdx] >= 0) && (foundPoints[fIdx + 1] >= 0)) {
                // try to build and draw a chain growing from this point
                drawChain(buildChain(pxs, foundPoints[fIdx], foundPoints[fIdx + 1]));
            }
        }        
    } else {
        // try to build and draw a chain growing from current start point
        drawChain(buildChain(pxs, startX, startY));
    }
    
}

void keyPressed() {
    if (key == '[') threshold--;
    else if (key == ']') threshold++;    
}

void mousePressed() {
    // store new start point
    startX = mouseX; 
    startY = mouseY;
}

void lookForPossibleChains(color[] pxs) {
    // clear found points cache
    for (int i = 0; i < (MAX_CHAINS * 2); i++) {
        foundPoints[i] = -1;
    }
    
    stroke(color(255, 0, 0));
    fill(255);
    //ellipse(50, 50, 20, 20);
    
    int x; int y;
    int pointsCount = 0;
    int foundCount = 0; 
    
    // left edge
    for (x = 0, y = 0; y < height; y++) { 
        // TODO: check if points follow each other
        // red(pxs[...]) == 255 means pixel is white
        if (red(pxs[y*width+x]) == 255) pointsCount++;          
        if (pointsCount >= (minRad * 2)) {
            pointsCount = 0;
            foundPoints[foundCount] = x;
            foundPoints[foundCount + 1] = y;
            foundCount++;
            if (foundCount >= MAX_CHAINS) return;
        }
    }
    // right edge
    for (x = (width - 1), y = 0; y < height; y++) { }
    // top edge
    for (x = 0, y = 0; x < width; x++) { }
    // bottom edge
    for (x = 0, y = (height - 1); x < width; x++) { }
}

ncephalo buildChain(color[] pxs, int x, int y) {
    return null;
}

void drawChain(ncephalo head) {
    if (head == null) return;
}
