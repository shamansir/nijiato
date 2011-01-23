class NCalibration {
  
    final int DETECTION_TIME = 3000;
  
    color[] fingers = new color[F.FINGERS_COUNT];
    color[] hands = new color[H.HANDS_COUNT];
    ndelta[] _fingers_d = new ndelta[F.FINGERS_COUNT];
    ndelta[] _hands_d = new ndelta[F.FINGERS_COUNT];
    int _lastDetectionTime = 0;
    
    int _rTopX, _rTopY, _rWidth, _rHeight;
    
    PImage _curFrame;
    
    public NCalibration() {
        for (int f = 0; f < F.FINGERS_COUNT; f++) fingers[f] = -1;
        for (int h = 0; h < H.HANDS_COUNT; h++) hands[h] = -1;      
        for (int f = 0; f < F.FINGERS_COUNT; f++) {
            _fingers_d[f] = new ndelta();
        }
        for (int h = 0; h < H.HANDS_COUNT; h++) {
            _hands_d[h] = new ndelta();
        }
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
        color last = px[_rTopY*width+_rTopX];
        for (int x = _rTopX; x <= (_rTopX + _rWidth); x++) {
            for (int y = _rTopY; y <= (_rTopY + _rHeight); y++) {
                last = lerpColor(px[y*width+x], last, 0.5);
            }    
        }
        return last;
    }    
    
    int getLastNotCalibratedFinger() {
        int f = 0;
        while (f < 10) {
            if (fingers[f] == -1) return f;
            f++;
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
        stroke(255);
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
        for (int f = 0; f < 10; f++) {
            if (fingers[f] != -1) {
               stroke(255);
               fill(fingers[f]);
               rect(f << 5, 0, 31, 31);
            }
        }
        for (int h = 0; h < 2; h++) {
            if (hands[h] != -1) {
               stroke(255);
               fill(hands[h]);
               rect(h << 5, 32, 31, 31);
           }
        }        
    }  
    
    void _showDetected(String name, int value, color result, int offset) {
        stroke(255);
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
