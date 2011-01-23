class NDetection {
  
  static final int RECT_MAX_DIST = 32;
  
  NCalibration calibration;
  NPositions positions;
  nrect[] _frects;
  nrect[] _hrects;
  
  NDetection(NCalibration calibration, NPositions positions) {
      this.calibration = calibration;
      this.positions = positions;
      this._frects = new nrect[F.FINGERS_COUNT];
      this._hrects = new nrect[H.HANDS_COUNT];
  }
  
  NPositions detect(PImage frame) {
      _clearRects();
      frame.loadPixels();
      color[] px = frame.pixels; 
      for (int x = 0; x < width; x++) {
          for (int y = 0; y < height; y++) {
              color curColor = px[y*width+x];
              
              for (int f = 0; f < F.FINGERS_COUNT; f++) {
                  if (calibration._fingers_d[f]
                          .matches(curColor, calibration.fingers[f])) {
                      _adaptInRect(_frects[f], new ncoord(x, y, -1));
                  }
              }
              
              for (int h = 0; h < H.HANDS_COUNT; h++) {
                  if (calibration._hands_d[h]
                          .matches(curColor, calibration.hands[h])) {
                      _adaptInRect(_hrects[h], new ncoord(x, y, -1));
                  }
              }
          }    
      }
      _sortRects();
      // TODO: get center points from rects
      return positions;      
  }
  
  void _adaptInRect(nrect cr, ncoord coord) {
      if (cr.is_set()) return;
      cr.p1.update(5, 10, -1);
      cr.p2.update(10, 10, -1);
      cr.p3.update(10, 20, -1);
      cr.p4.update(5, 20, -1);
      /* if (!cr.p1.is_set()) {
          cr.p1.update(coord); return;
      } else if (!cr.p2.is_set()) {
          if (!cr.p1.near(coord)) cr.p1.update(coord);
          else cr.p2.update(coord); return;          
      } else if (!cr.p3.is_set()) {
          // if (!cr.p2.near(coord)) cr.p2.update(coord);
          cr.p3.update(coord);
      } else cr.p4.update(coord); */
  }
  
  void _sortRects() {
  }
  
  void showRects() {
      stroke(255);
      for (int f = 0; f < F.FINGERS_COUNT; f++) {
          fill(calibration.fingers[f], 100);
          quad(_frects[f].p1.x, _frects[f].p1.y,
               _frects[f].p2.x, _frects[f].p2.y,
               _frects[f].p3.x, _frects[f].p3.y,
               _frects[f].p4.x, _frects[f].p4.y);
      }
      for (int h = 0; h < H.HANDS_COUNT; h++) {
          fill(calibration.hands[h], 100);
          quad(_hrects[h].p1.x, _hrects[h].p1.y,
               _hrects[h].p2.x, _hrects[h].p2.y,
               _hrects[h].p3.x, _hrects[h].p3.y,
               _hrects[h].p4.x, _hrects[h].p4.y);
      }      
  }
  
  void _clearRects() {
      for (int f = 0; f < F.FINGERS_COUNT; f++) {
          if (_frects[f] == null) _frects[f] = new nrect();
          else _frects[f].reset();
      }
      for (int h = 0; h < H.HANDS_COUNT; h++) {
          if (_hrects[h] == null) _hrects[h] = new nrect();
          else _hrects[h].reset();
      }      
  }
  
}
