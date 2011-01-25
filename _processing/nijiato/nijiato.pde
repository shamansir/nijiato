import hypermedia.video.*;

OpenCV opencv;
boolean calibrating = false;
// DroidSans-28.vlw
// DroidSans-42.vlw

class H {
    static final byte LEFT_HAND = 0;
    static final byte RIGHT_HAND = 1;
    
    static final byte HANDS_COUNT = 2;
    // change to 1 to detect just one hand    
}

class F {
    static final byte LEFT_LITTLE = 0;
    static final byte LEFT_RING = 1;
    static final byte LEFT_MIDDLE = 2;
    static final byte LEFT_INDEX = 3;
    static final byte LEFT_THUMB = 4;
   
    static final byte RIGHT_THUMB = 5;
    static final byte RIGHT_INDEX = 6;
    static final byte RIGHT_MIDDLE = 7;
    static final byte RIGHT_RING = 8;
    static final byte RIGHT_LITTLE = 9;
    
    static final byte FINGERS_COUNT = 10; 
    // change to 5 to detect just one hand
}

class NPositions {
    
    ncoord[] fingers;
    ncoord[] hands;

    NPositions() {
        fingers = new ncoord[F.FINGERS_COUNT];
        hands = new ncoord[H.HANDS_COUNT];
    }

}

class ncoord { int x, y, z; 
    ncoord() { x = -1; y = -1; z = -1; }
    ncoord(int _x, int _y, int _z) { x = _x; y = _y; z = _z; }
    void reset() { x = -1; y = -1; z = -1; }
    void update(int _x, int _y, int _z) { x = _x; y = _y; z = _z; }
    void update(ncoord other) { x = other.x; y = other.y; z = other.z; }
    boolean is_set() { return (x != -1) && (y != -1) && (z != -1); }
}

class ndelta { float dr, dg, db;
    ndelta() { dr = 4; dg = 4; db = 4; }
    ndelta(float _dr, int _dg, int _db) { dr = _dr; dg = _dg; db = _db; }
    boolean matches(color current, color wanted) {
        float cr = red(current); float cg = green(current); float cb = blue(current);
        // wanted
        float wr = red(wanted); float wg = green(wanted); float wb = blue(wanted);
        return ((cr <= wr + dr) && (cr >= wr - dr) &&
                (cg <= wg + dg) && (cg >= wg - dg) &&
                (cb <= wb + db) && (cb >= wb - db));      
    }
}

class nclist { ArrayList coords; int length;
    nclist() { coords = new ArrayList(); length = 0; }
    void reset() { coords.clear(); length = 0; }
    ncoord add(ncoord coord) { coords.add(coord); length++; return coord; } 
    ncoord get(int i) { return ((ncoord)coords.get(i)); }
    /* void sortpolar() { } */
}

NPositions positions = new NPositions();
NCalibration calibration = new NCalibration();
NDetection detection = new NDetection(calibration, positions);
PFont _font;

void setup() {
  
    size(320,240,P2D);
    
    calibration.init();
    
    _font = loadFont("DroidSans-28.vlw");
    textFont(_font, 28);
    
    //try {
    byte b[] = loadBytes("calibration.dat");
    if (b != null) {
        println("calibration file found");
        // TODO: read calibration data       
        calibrating = false;
    //} catch (IOException ioe) {
    } else {
        println("calibration file not found, calibrating");
        calibrating = true;
    }
    
    opencv = new OpenCV(this);
    opencv.capture(width,height);
    
}

void draw() {
  
    opencv.read();
    
    PImage curImage = opencv.image(OpenCV.SOURCE);
    image(curImage, 0, 0);    
    
    if (!calibrating) {
        NPositions positions = detection.detect(curImage);
        detection.showPolys(); // TODO: remove
    } else {
        calibration.frame(curImage);
        int curFinger;
        int curHand;        
        if ((curFinger = calibration.getLastNotCalibratedFinger()) >= 0) {
            calibration.calibrateFinger(curFinger);
        } else if ((curHand = calibration.getLastNotCalibratedHand()) >= 0) {
            calibration.calibrateHand(curHand);
        } else {
            noStroke();
            fill(255);
            text("Start!", 150, 150);
            calibrating = false;
        }
        // TODO: save calibration data
    }
       
}

void keyReleased() {
    if (key == 'r') {
        if (calibration != null) calibration.reset();
        calibrating = true;
    }
}

void keyPressed() {
    if (key == 'y') if (detection != null) detection.showPolys();   
    if (key == 'm') if (calibration != null) calibration.showAllDetected();
}
