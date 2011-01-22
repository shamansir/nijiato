class NDetection {
  
  NCalibration calibration;
  NPositions positions;
  int[][][] _rects;
  
  NDetection(NCalibration calibration, NPositions positions) {
      this.calibration = calibration;
      this.positions = positions;
      this._rects = new int[10][4][3]; // finger : rect-cords : x, y, z
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
                  int dR = calibration._fingers_d[f][0];
                  int dG = calibration._fingers_d[f][1];
                  int dB = calibration._fingers_d[f][2];
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
          for (int r = 0; r < 4; r++) {
              for (int c = 0; c <= 3; c++) {
                  this._rects[f][r][c] = -1;
              }
          }
      }    
  }
  
}
