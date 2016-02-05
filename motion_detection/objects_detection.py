#
# This module contains different functions that read video file and
# extract moving objects
#
#

import cv2, sys
from matplotlib import pyplot as plt
import numpy as np
from scipy import signal
from math import tan,sin,cos

BLACK = (10,10,10)
WHITE = (250,250,250)
REDISH = (150,150,250)

CAM_ANGLE = 24 # camera vision angle in grads
PX_TO_HORIZONTAL_PLANE = 105 # number of pixels to the horizontal plane that intersects the frame

def main(video_file,cam_height):
    EVERY_N_FRAME = 2
    cap = cv2.VideoCapture(video_file)
    X_SIZE = int(cap.get(3))
    Y_SIZE = int(cap.get(4))
    FPS = int(cap.get(5))
    print X_SIZE, 'x', Y_SIZE
    print "fps = %i" % FPS

    fv = np.vectorize(image_binarize)
    sharp = np.vectorize(sh)

    out = cv2.VideoWriter(video_file[:-4]+'_proc.avi',cv2.cv.CV_FOURCC(*'mp4v'), FPS/EVERY_N_FRAME, (X_SIZE,Y_SIZE))

    Kernel = np.ones((7,3),np.float32)/21
    k_shape = np.shape(Kernel)
    Mx, My = k_shape[0]/2, k_shape[1]/2

    px_per_rad = X_SIZE / (2*CAM_ANGLE * np.pi/180)

    t1=cv2.getTickCount()

    ret, frame1 = cap.read()
    q = np.ones((Y_SIZE,X_SIZE),dtype='uint8')
    bkgd = cv2.cvtColor(frame1, cv2.COLOR_BGR2GRAY)
    i = 1
    mbr = Mbr(X_SIZE,Y_SIZE)

    while True:
        ret, frame1 = cap.read()
        if not ret: break
        
        if i % EVERY_N_FRAME == 0:
            frame2 = cv2.cvtColor(frame1, cv2.COLOR_BGR2GRAY)
            frame_diff = fv(frame2-bkgd)
            bkgd = cv2.add(bkgd/2,frame2/2)
            
            dst = sharp(signal.convolve(frame_diff, Kernel)[My:-My,Mx:-Mx],100)
            curr_mbr = mbr.get_mbr(dst)
            if curr_mbr != None:
                XY1,XY2 = curr_mbr
                angle = (XY2[1]-PX_TO_HORIZONTAL_PLANE)/px_per_rad
                cm_per_px = 100*cam_height / (XY2[1]-PX_TO_HORIZONTAL_PLANE)
                obj_height = int(100*cam_height + (PX_TO_HORIZONTAL_PLANE-XY1[1])*cm_per_px)
                obj_dist = round(cam_height/tan(angle),1)
                polar = (X_SIZE-XY1[0]-XY2[0])/2/px_per_rad
                obj_y = round(obj_dist*sin(polar),1)
                obj_x = round(obj_dist*cos(polar),1)
                
                cv2.rectangle(frame2,XY1,XY2,WHITE,1)
            else:
                obj_height = ''
                obj_dist = ''
                obj_y = '' 
                obj_x = ''
                
            frame = cv2.applyColorMap(frame2, cv2.COLORMAP_HOT)
            #frame = cv2.merge([q,q,frame2]) # merging red-colored frame
            font = cv2.FONT_HERSHEY_SIMPLEX
            cv2.putText(frame,'VISUAL: male',(230,100), font, 0.4,WHITE,2)
            cv2.putText(frame,'height %s'%str(obj_height),(230,120), font, 0.4,WHITE,2)
            cv2.putText(frame,'dist %s'%str(obj_dist),(230,135), font, 0.4,WHITE,2)
            cv2.putText(frame,'xy (%s, %s)'%(str(obj_x),str(obj_y)),(230,150), font, 0.4,WHITE,2)
            
            out.write(frame)
        i+=1
        
    cap.release()
    out.release()
    t2 = cv2.getTickCount()
    cv2.destroyAllWindows()

    time = (t2 - t1)/ cv2.getTickFrequency()
    print time,'sec'


def image_binarize(x):
    return 0 if x < 15 or x > 240 else 250

def sh(x,threshold):
    if x < threshold: return 0
    else: return 250

def get_mbr(img,X,Y):
    Y,X = np.shape(img)
    MinX = X
    MinY = Y
    MaxX = 0
    MaxY = 0
    for j in range(Y):
        for i in range(X):
            if img[j,i] > 0: 
                MinX = min(MinX,i)
                MinY = min(MinY,j)
                MaxX = max(MaxX,i)
                MaxY = max(MaxY,j)
    if MaxX == 0 or MaxY == 0 or MinX == X or MinY == Y: return (None,None),(None,None)
    else: return (MinX,MinY),(MaxX,MaxY)
                

class Mbr():
    def __init__(self,X,Y):
        self.i = 0
        self.X1 = [None,None,None]
        self.Y1 = [None,None,None]
        self.X2 = [None,None,None]
        self.Y2 = [None,None,None]
        self.X = X
        self.Y = Y
        
    def get_mbr(self,img):
        i = self.i
        (self.X1[i],self.Y1[i]),(self.X2[i],self.Y2[i]) = get_mbr(img,self.X,self.Y)
        self.i = (i+1) % 3
        
        if self.undef(): return None
        else: return self.mid_value()
        
    def undef(self):
        List =  self.X1+self.Y1+self.X2+self.Y2
        return any([v == None for v in List])
    
    def mid_value(self):
        X1 = sorted(self.X1)
        Y1 = sorted(self.Y1)
        X2 = sorted(self.X2)
        Y2 = sorted(self.Y2)
        return (X1[1],Y1[1]),(X2[1],Y2[1])




if __name__ == '__main__':
    Args = sys.argv
    
    if len(Args) != 3: 
        print "Usage: python objects_detection.py <video_file> <camera height in meters>"
    else: 
        video_file = Args[1]
        camera_height = float(Args[2])
        main(video_file,camera_height)
