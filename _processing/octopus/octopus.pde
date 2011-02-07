import hypermedia.video.*;

OpenCV opencv;
int threshold = 80;
// DroidSans-28.vlw
// DroidSans-42.vlw

int tentacles_num = 8;

class ncephalo { // cephalopoda
   int cx = -1; int cy = -1;
   int radius = -1;
   //boolean[] touched = new boolean[tentacles_num];
   ncephalo[] neighbors = new ncephalo[tentacles_num];
}

void setup() {
  
    size(320,240,P2D);
    
    opencv = new OpenCV(this);
    opencv.capture(width,height);
    
}

void draw() {
  
    opencv.read();
    opencv.convert(OpenCV.GRAY);    
    
    opencv.threshold(threshold);
    PImage curImage = opencv.image();
    image(curImage, 0, 0);
   
     
           
}

void keyPressed() {
    if (key == '[') threshold--;
    else if (key == ']') threshold++;    
}
