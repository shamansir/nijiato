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
              
              for (int f = 0; f < F.FINGERS_COUNT; f++) {
                  if (calibration._fingers_d[f]
                          .matches(curColor, calibration.figers[f])) {
                      _adaptInRect(f, x, y, -1);
                  }
              }
              
              // TODO: detect hands also
          }    
      }
      return positions;      
  }
  
  void _adaptInRect(int finger, int x, int y, int z) {
      nrect cur_rect = _rects[f];
      // TODO: check if it is tl corner defined, check distance
  }
  
  void _clearRects() {
      for (int f = 0; f < 10; f++) {
          if (_rects[f] == null) _rects[f] = new nrect();
          else _rects[f].reset();
      }    
  }
  
}
