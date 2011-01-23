class NDetection {
  
  NCalibration calibration;
  NPositions positions;
  nrect[] _rects;
  
  NDetection(NCalibration calibration, NPositions positions) {
      this.calibration = calibration;
      this.positions = positions;
      this._rects = new nrect[F.FINGERS_COUNT];
  }
  
  NPositions detect(PImage frame) {
      _clearRects();
      frame.loadPixels();
      color[] px = frame.pixels; 
      for (int x = 0; x < width; x++) {
          for (int y = 0; y <= height; y++) {
              color curColor = px[y*width+x];
              
              for (int f = 0; f < 10; f++) {
                  // current
                  float cR = red(curColor); 
                  float cG = green(curColor);
                  float cB = blue(curColor);
                  // wanted
                  float wR = red(calibration.fingers[f]);
                  float wG = green(calibration.fingers[f]);
                  float wB = blue(calibration.fingers[f]);
                  // delta
                  float dR = calibration._fingers_d[f].dr;
                  float dG = calibration._fingers_d[f].dg;
                  float dB = calibration._fingers_d[f].db;
                  if ((cR <= wR + dR) && (cR >= wR - dR) &&
                      (cG <= wG + dG) && (cG >= wG - dG) &&
                      (cR <= wB + dB) && (cR >= wB - dB)) {
                      // TODO: manipulate detected pixel
                  }
              }
              
              // TODO: detect hands also
          }    
      }
      return positions;      
  }
  
  void _clearRects() {
      for (int f = 0; f < 10; f++) {
          if (_rects[f] == null) _rects[f] = new nrect();
          else _rects[f].reset();
      }    
  }
  
}
