class NCalibration {
  
    final int DETECTION_TIME = 5000;
  
    color[] fingers = new color[10];
    color[] hands = new color[2];
    int[][] _fingers_d = new int[10][3]; // deltas
    int[][] _hands_d = new int[2][3]; // deltas
    int _lastDetectionTime = 0;
    
    int _rTopX, _rTopY, _rWidth, _rHeight;
    
    PImage _curFrame;
    
    public NCalibration() {
        for (int i = 0; i < 10; i++) fingers[i] = -1;
        for (int i = 0; i < 2; i++) hands[i] = -1;
    }
    
    void init() {
        // rect((width / 2) - 20, (height / 2) - 16, 40, 32);
        _rTopX = (width / 2) - 20;
        _rTopY = (height / 2) - 16;
        _rWidth = 40;
        _rHeight = 32;
    }
    
    void frame(PImage curFrame) {
        _curFrame = curFrame;
    }
    
    color _colorInRect() {
        _curFrame.loadPixels();
        color[] px = _curFrame.pixels;
        color last = px[_rTopY*width+_rTopY];
        for (int x = _rTopX; x <= (_rTopX + _rWidth); x++) {
            for (int y = _rTopY; y <= (_rTopY + _rHeight); y++) {
                last = lerpColor(px[y*width+x], last, 0.5, HSB);
            }    
        }
        return last;
    }    
    
    int getLastNotCalibratedFinger() {
        int i = 0;
        while (i < 10) {
            if (fingers[i] == -1) return i;
            i++;
        }
        return -1;
    }
    
    int getLastNotCalibratedHand() {
        if (hands[0] == -1) return 0;
        if (hands[1] == -1) return 1;
        return -1;
    }    
    
    void calibrateFinger(int finger) {
        int elapsed = millis() - _lastDetectionTime;
        if (elapsed > DETECTION_TIME) {
            _showDetectionRect();          
            _detectFinger(finger);
            _lastDetectionTime = millis();
            _showAlreadyDetected();            
            _showDetected("finger", finger, fingers[finger], 0);
        } else {
            _showAlreadyDetected();
            _showDetectionRect();            
            _showDetectionState(finger, int((DETECTION_TIME - elapsed) / 1000), 0);
        }
    }
    
    void calibrateHand(int hand) {
        int elapsed = millis() - _lastDetectionTime;
        if (elapsed > DETECTION_TIME) {
            _showDetectionRect();          
            _detectHand(hand);
            _lastDetectionTime = millis();
            _showAlreadyDetected();
            _showDetected("hand", hand, hands[hand], 32);
        } else {
            _showAlreadyDetected();
            _showDetectionRect();
            _showDetectionState(hand, int((DETECTION_TIME - elapsed) / 1000), 32);
        }
    }
    
    void _showDetectionRect() {
        stroke(0, 0, 255);
        noFill();
        rect(_rTopX, _rTopY, _rWidth, _rHeight);
    }
    
    void _detectFinger(int finger) {        
        fingers[finger] = _colorInRect();
    }
  
    void _detectHand(int hand) {
        hands[hand] = _colorInRect();
    }
    
    void _showAlreadyDetected() {
        for (int i = 0; i < 10; i++) {
            if (fingers[i] != -1) {
              stroke(0, 0, 255);
              fill(fingers[i]);
              rect(i << 5, 0, 31, 31);
           }
        }
        for (int i = 0; i < 2; i++) {
            if (hands[i] != -1) {
              stroke(0, 0, 255);
              fill(hands[i]);
              rect(i << 5, 32, 31, 31);
           }
        }        
    }  
    
    void _showDetected(String name, int value, color result, int offset) {
        stroke(0, 0, 255);
        fill(result);
        rect(value << 5, offset, 31, 31);
        //println("detected " + name + " (" + value + "): " + result);
    }
    
    void _showDetectionState(int value, int elapsed, int offset) {
        stroke(255);
        noFill();
        rect(value << 5, offset, 31, 31);
        fill(255);
        text(elapsed, (value << 5) + 9, offset + 26);
        //println("detecting hand " + hand + ": " + int((DETECTION_TIME - elapsed) / 1000));
        return;      
    }  
    
}
