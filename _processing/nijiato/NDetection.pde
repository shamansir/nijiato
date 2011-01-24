class NDetection {
  
  static final int RECT_MAX_DIST = 32;
  
  NCalibration calibration;
  NPositions positions;
  nclist[] _fpts;
  nclist[] _hpts;
  
  NDetection(NCalibration calibration, NPositions positions) {
      this.calibration = calibration;
      this.positions = positions;
      this._fpts = new nclist[F.FINGERS_COUNT];
      this._hpts = new nclist[H.HANDS_COUNT];
  }
  
  NPositions detect(PImage frame) {
      _forgetPoints();
      frame.loadPixels();
      color[] px = frame.pixels; 
      for (int x = 0; x < width; x++) {
          for (int y = 0; y < height; y++) {
              color curColor = px[y*width+x];
              
              for (int f = 0; f < F.FINGERS_COUNT; f++) {
                  if (calibration._fingers_d[f]
                          .matches(curColor, calibration.fingers[f])) {
                      _adaptInList(_fpts[f], new ncoord(x, y, -1));
                  }
              }
              
              for (int h = 0; h < H.HANDS_COUNT; h++) {
                  if (calibration._hands_d[h]
                          .matches(curColor, calibration.hands[h])) {
                      _adaptInList(_hpts[h], new ncoord(x, y, -1));
                  }
              }
          }    
      }
      // TODO: 1. filter points that too far
      // TODO: 2. sort the rest by polar angle
      // TODO: 3. build polygons on these points
      // TODO: 4. detect the center points for this polygons, return them
      _sortPoints();
      return positions;      
  }
  
  void _adaptInList(nclist cl, ncoord coord) {
  }
  
  void _sortPoints() {
      for (int f = 0; f < F.FINGERS_COUNT; f++) _fpts[f].sortpolar();
      for (int h = 0; h < H.HANDS_COUNT; h++) _hpts[h].sortpolar();
  }
  
  void showPolys() {
      /* 
      stroke(255);
      for (int f = 0; f < F.FINGERS_COUNT; f++) {
          fill(calibration.fingers[f], 225);
          quad(_frects[f].p1.x, _frects[f].p1.y,
               _frects[f].p2.x, _frects[f].p2.y,
               _frects[f].p3.x, _frects[f].p3.y,
               _frects[f].p4.x, _frects[f].p4.y);
      }
      for (int h = 0; h < H.HANDS_COUNT; h++) {
          fill(calibration.hands[h], 225);
          quad(_hrects[h].p1.x, _hrects[h].p1.y,
               _hrects[h].p2.x, _hrects[h].p2.y,
               _hrects[h].p3.x, _hrects[h].p3.y,
               _hrects[h].p4.x, _hrects[h].p4.y);
      } */
  }
  
  void _forgetPoints() {
      for (int f = 0; f < F.FINGERS_COUNT; f++) {
          if (_fpts[f] == null) _fpts[f] = new nclist();
          else _fpts[f].reset();
      }
      for (int h = 0; h < H.HANDS_COUNT; h++) {
          if (_hpts[h] == null) _hpts[h] = new nclist();
          else _hpts[h].reset();
      }      
  }
  
}
