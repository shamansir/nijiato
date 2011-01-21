import hypermedia.video.*;
import java.awt.Rectangle;

OpenCV opencv;
boolean calibrating = false;
// DroidSans-28.vlw
// DroidSans-42.vlw

class H {
    static final int RIGHT_HAND = 0;
    static final int LEFT_HAND = 1;  
}

class F {
    static final int LEFT_LITTLE = 0;
    static final int LEFT_RING = 1;
    static final int LEFT_MIDDLE = 2;
    static final int LEFT_INDEX = 3;
    static final int LEFT_THUMB = 4;
   
    static final int RIGHT_THUMB = 5;
    static final int RIGHT_INDEX = 6;
    static final int RIGHT_MIDDLE = 7;
    static final int RIGHT_RING = 8;
    static final int RIGHT_LITTLE = 9;
}

class NPositions {
    int[][] fingers = new int[10][2];
    int[][] hands = new int[2][2];
}

NPositions positions = new NPositions();
NCalibration calibration = new NCalibration();
PFont _font;

void setup() {
    size(320,240);
    
    colorMode(HSB, 255);    
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
    calibration.frame(curImage);
    image(curImage, 0, 0);    
    
    if (!calibrating) {
        // TODO: 
    } else {
        int curFinger;
        int curHand;
        if ((curFinger = calibration.getLastNotCalibratedFinger()) >= 0) {
            calibration.calibrateFinger(curFinger);
        } else if ((curHand = calibration.getLastNotCalibratedHand()) >= 0) {
            calibration.calibrateHand(curHand);
        } else {
            noStroke();
            fill(0, 0, 255);
            text("Start!", 150, 150);
            calibrating = false;
        }
    }
       
}
