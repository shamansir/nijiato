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
      color[] fingers = calibration.fingers;
      color[] hands = calibration.hands;
      ndelta[] fdeltas = calibration._fingers_d;
      ndelta[] hdeltas = calibration._hands_d;
      for (int x = 0; x < width; x++) {
          for (int y = 0; y < height; y++) {
              color curColor = px[y*width+x];
              
              for (int f = 0; f < F.FINGERS_COUNT; f++) {
                  if (fdeltas[f].matches(curColor, fingers[f])) {                      
                      _storeCoord(_fpts[f], new ncoord(x, y, 0));
                  }
              }
              
              for (int h = 0; h < H.HANDS_COUNT; h++) {
                  if (hdeltas[h].matches(curColor, hands[h])) {
                      _storeCoord(_hpts[h], new ncoord(x, y, 0));
                  }
              }
          }    
      }
      // TODO: 1. filter points that too far
      // TODO: 2. sort the rest by polar angle
      // TODO: 3. build polygons on these points
      // TODO: 4. detect the center points for this polygons, return them
      //_sortPoints();
      return positions;      
  }
  
  void _storeCoord(nclist cl, ncoord coord) {
      cl.add(coord);
  }
  
  /* void _sortPoints() {
      for (int f = 0; f < F.FINGERS_COUNT; f++) _fpts[f].sortpolar();
      for (int h = 0; h < H.HANDS_COUNT; h++) _hpts[h].sortpolar();
  } */
  
  void showPolys() {
      noFill();
      for (int f = 0; f < F.FINGERS_COUNT; f++) {
          stroke(calibration.fingers[f]);
          nclist fpts = _fpts[f];
          println(f + "f: " + fpts.length);
          for (int i = 0; i < fpts.length; i++) {
              point(fpts.get(i).x, fpts.get(i).y);
          }
      }
      for (int h = 0; h < H.HANDS_COUNT; h++) {
          stroke(calibration.hands[h]);
          nclist hpts = _hpts[h];
          println(h + "h: " + hpts.length);          
          for (int i = 0; i < hpts.length; i++) {
              point(hpts.get(i).x, hpts.get(i).y);
          }
      }      
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
