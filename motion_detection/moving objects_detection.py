#
# This module contains different functions that read video file and
# extract moving objects
#
#

import cv2
from matplotlib import pyplot as plt
import numpy as np
from scipy import signal
from math import tan,sin,cos

GREEN = (0,250,0)
WHITE = (250,250,250)

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

    out = cv2.VideoWriter(video_file[:-4]+'proc.avi',cv2.cv.CV_FOURCC(*'mp4v'), FPS/EVERY_N_FRAME, (X_SIZE,Y_SIZE))

    Kernel = np.ones((7,3),np.float32)/21
    k_shape = np.shape(K)
    Mx, My = k_shape[0]/2, k_shape[1]/2

    px_per_rad = X_SIZE / (2*CAM_ANGLE * np.pi/180)

    t1=cv2.getTickCount()

ret, frame1 = cap.read()
q = np.zeros((Y_SIZE,X_SIZE),dtype='uint8')
bkgd = cv2.cvtColor(frame1, cv2.COLOR_BGR2GRAY)
#b,g,bkgd = cv2.split(frame1)
#bkgd = cv2.filter2D(bkgd,-1,kernel)
i = 1
mbr = Mbr()

while True:
    ret, frame1 = cap.read()
    if not ret: break
    frame2 = cv2.cvtColor(frame1, cv2.COLOR_BGR2GRAY)
    #b,g,frame2 = cv2.split(frame1)
    if i % 2 == 0:
        frame_diff = fv(frame2-bkgd)
        bkgd = cv2.add(bkgd/2,frame2/2)
        #print "Objects:",len(get_objects(frame_diff))
        
        dst = sharp(signal.convolve(frame_diff, K)[My:-My,Mx:-Mx],100)
        curr_mbr = mbr.get_mbr(dst)
        if curr_mbr != None:
            XY1,XY2 = curr_mbr
            angle = (XY2[1]-105)/px_per_rad
            cm_per_px = 100*cam_height / (XY2[1]-105)
            obj_height = int(100*cam_height + (105-XY1[1])*cm_per_px)
            obj_dist = round(cam_height/tan(angle),1)
            polar = (X_SIZE-XY1[0]-XY2[0])/2/px_per_rad
            obj_y = round(obj_dist*sin(polar),1)
            obj_x = round(obj_dist*cos(polar),1)
            
            cv2.rectangle(frame2,XY1,XY2,white,1)
        else:
            obj_height = ''
            obj_dist = ''
            obj_y = '' 
            obj_x = ''
            
        
        #dst[105,:] = 152  # horizontal line
        frame = cv2.merge([q,q,frame2])
        font = cv2.FONT_HERSHEY_SIMPLEX
        cv2.putText(frame,'VISUAL: male',(230,100), font, 0.4,(100,100,200),2)
        cv2.putText(frame,'height %s'%str(obj_height),(230,120), font, 0.4,(100,100,200),2)
        cv2.putText(frame,'dist %s'%str(obj_dist),(230,135), font, 0.4,(100,100,200),2)
        cv2.putText(frame,'xy (%s, %s)'%(str(obj_x),str(obj_y)),(230,150), font, 0.4,(100,100,200),2)
        
        out.write(frame)
        #cv2.imshow('image',frame) #, cmap = 'gray') #, interpolation = 'bicubic')
        #plt.xticks([]), plt.yticks([])  # to hide tick values on X and Y axis
        #plt.show()

    i+=1
    

cap.release()
out.release()
t2 = cv2.getTickCount()
cv2.destroyAllWindows()

time = (t2 - t1)/ cv2.getTickFrequency()
print time,'sec'



if __name__ == '__main__':
    Args = sys.argv
    
    if len(Args) != 4: 
        print "Usage: python moving_objects_detection.py <video_file> <camera height in meters>"
    else: 
        main(Args[2],float(Args[3]))
