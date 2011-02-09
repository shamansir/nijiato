import hypermedia.video.*;

int MAX_CHAINS = 8; // maximum number of cephalopoda chains to search

OpenCV opencv;

int threshold = 80;
int minRad = 12; // minimum cephalo radius
int maxRad = 60; // maximum cephalo radius
int startX = -1; int startY = -1;
int[] foundPoints = new int[MAX_CHAINS << 1]; // found chains' start points

int tentacles_num = 8;

ChainsDetector detect = new ChainsDetector();

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
        detect.lookForPossibleChains(pxs);
        detect.drawFoundPoints();
        // for each of these start points
        for (int fIdx = 0; fIdx < MAX_CHAINS; fIdx++) {
            // if found point is set
            if ((foundPoints[fIdx << 1] >= 0) && (foundPoints[(fIdx << 1) + 1] >= 0)) {
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

class ChainsDetector {
  
    int x; int y;
    int pointsCount = 0;
    int foundCount = 0;  
  
    void lookForPossibleChains(color[] pxs) {

        // clear found points cache
        for (int i = 0; i < (MAX_CHAINS << 1); i++) {
            foundPoints[i] = -1;
        }
      
        stroke(color(255, 0, 0));
        fill(255);
        //ellipse(50, 50, 20, 20);
      
        try {
            // left edge
            for (x = 0, y = 0; y < height; y++) _scanPoint(x, y, pxs[y*width+x], false); pointsCount = 0;
            // right edge
            for (x = (width - 1), y = 0; y < height; y++) _scanPoint(x, y, pxs[y*width+x], false); pointsCount = 0;
            // bottom edge
            for (x = 0, y = (height - 1); x < width; x++) _scanPoint(x, y, pxs[y*width+x], true); pointsCount = 0;
            // top edge
            for (x = 0, y = 0; x < width; x++) _scanPoint(x, y, pxs[y*width+x], true); pointsCount = 0;
        } catch(AllPointsAreFound apaf) { return; }
        
    }
    
    void _scanPoint(int x, int y, color _clr, boolean horz) throws AllPointsAreFound {
        if (red(_clr) == 255) pointsCount++;
        else if (pointsCount >= (minRad << 1)) {
            foundPoints[foundCount << 1] = horz ? (x - (pointsCount >> 1)) : x;
            foundPoints[(foundCount << 1) + 1] = horz ? y : (y - (pointsCount >> 1));
            pointsCount = 0;            
            foundCount++;
            if (foundCount >= MAX_CHAINS) {
                pointsCount = 0;
                foundCount = 0;
                throw new AllPointsAreFound();
            }
        }
    }  
    
    void drawFoundPoints() {
        stroke(color(255, 0, 0));
        fill(255);
        for (int fIdx = 0; fIdx < MAX_CHAINS; fIdx++) {
            if ((foundPoints[fIdx << 1] >= 0) && (foundPoints[(fIdx << 1) + 1] >= 0)) {
                ellipse(foundPoints[fIdx << 1], foundPoints[(fIdx << 1) + 1], 10, 10); 
            }
        }         
    }
    
}

class AllPointsAreFound extends Exception { };

ncephalo buildChain(color[] pxs, int x, int y) {
    return null;
}

void drawChain(ncephalo head) {
    if (head == null) return;
}
