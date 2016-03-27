
# coding: utf-8
#python -W ignore Garage48Pre.py -i box0.jpeg
# In[120]:

import sys
sys.path.append('/home/user1/opencv/build/lib')
import numpy as np
import cv2
import random 
import argparse


# In[4]:

def get_params(parameters):
    l=[]
    for rho in parameters['rho']:
        for theta in parameters['theta']:
            for threshold in parameters['threshold']:
                for minLineLength in parameters['minLineLength']:
                    for maxLineGap in parameters['maxLineGap']:
                        l.append({'rho':rho,
                                  'theta': theta, 
                                  'threshold': threshold,
                                  'minLineLength': minLineLength, 
                                  'maxLineGap': maxLineGap*minLineLength})
    return l


# In[3]:

def computeIntersect(a,b):
    a=a[0]
    b=b[0]

    x1 = a[0]
    y1 = a[1]
    x2 = a[2]
    y2 = a[3]
    x3 = b[0]
    y3 = b[1]
    x4 = b[2]
    y4 = b[3]
        
    denom = float((x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4))
    
    if (denom==0):
        return None
    
    x = ((x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4)) / denom
    y = ((x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4)) / denom
    
    return x,y                


# In[5]:

def getAngles(lines):
    angles=[]
    avgx=[]
    for line1 in lines:
        deltay=-(float)(line1[0][3]-line1[0][1])
        deltax=(float)(line1[0][2]-line1[0][0])
        a=np.arctan2(deltay,deltax)
        if (abs(a/np.pi*180)>360):
            print deltay,deltax
        angles.append(a)
        avgx.append(line1[0][2]+line1[0][0])
    avgx=np.array(avgx)
    angles=np.array(angles)
    return angles,avgx


# In[211]:

def myHL(params,img):
    return cv2.HoughLinesP(img,
                            rho=params['rho'], 
                            theta=params['theta'], 
                            threshold=params['threshold'], 
                            minLineLength=params['minLineLength'], 
                            maxLineGap=params['maxLineGap'])


# In[9]:

parameters = {'rho':[1],
              'theta': [np.pi/90,np.pi/180,np.pi/360], 
              'threshold': [10,20,40,50,60,80,100],
              'minLineLength': [5,10,20,40,50,60,80,100], 
              'maxLineGap': [0.1,0.2]}


# In[237]:

def mycos(v1,v2):
    v1=v1[0]
    v2=v2[0]
    deltax1=v1[2]-v1[0]
    deltay1=v1[3]-v1[1]
    l1=np.sqrt(deltax1**2+deltay1**2)
    deltax2=v2[2]-v2[0]
    deltay2=v2[3]-v2[1]
    l2=np.sqrt(deltax2**2+deltay2**2)
    return (deltax1*deltax2+deltay1*deltay2)/(l1*l2)


# In[10]:

def randomsample(X, n ):
    sampleix = random.sample( xrange( X.shape[0] ), int(n) )
    return X[sampleix]


# In[11]:

def my_dist(X, Y, w):
    d = np.empty( (X.shape[0], Y.shape[0]), np.float64 )
    for j, a in enumerate(X):
        for k, b in enumerate(Y):
            cross=computeIntersect(a,b)                      
            if (cross):
                x,y = cross
                d[j,k] = 1/(1+(x-w)**2+(y-w)**2)
    return d


# In[12]:

def kmeanssample( X, k,w,maxiter=10):
    return kmeans( X, randomsample( X, int(k)), w )


# In[13]:

def kmeans( X, centres, w,delta=.001, maxiter=10):
    centres = centres.copy()
    N = len(X)
    k = len(centres)
    
    allx = np.arange(N)
    prevdist = 0
    for jiter in range( 1, maxiter+1 ):
        D = my_dist(X, centres,w)  
        xtoc = D.argmin(axis=1)  # X -> nearest centre
        distances = D[allx,xtoc]
        avdist = distances.mean()  # median ?
        if jiter == maxiter:
            #print 'stop1'
            break
        #if (1 - delta) * prevdist <= avdist <= prevdist:
        #    print 'stop2',jiter
        #    break
        prevdist = avdist
        for jc in range(k):  # (1 pass in C)
            c = np.where( xtoc == jc )[0]
            if len(c) > 0:
                centres[jc] = X[c].mean( axis=0 )
    return centres, xtoc, distances


# In[204]:

def linesLoss2(lines,width):
    if lines is None:
        return 10000,[] 
    
    if len(lines)==0:
        return 10000,lines 
    
    #remove verticales
    deltaAngle=10
    angles,avgx = getAngles(lines)
    angles=angles/np.pi*180
    lines=lines[abs(abs(angles)-90)>deltaAngle]
    avgx=avgx[abs(abs(angles)-90)>deltaAngle]
    angles=angles[abs(abs(angles)-90)>deltaAngle]
    #c,ind=kmeans2(angles/np.pi*180,3)
    #err2=0.5*(100 if len(set(ind))!=3 else 0)
    #vert=np.argmin(abs(np.array([c-90,c+90])).min(axis=0))
    #err3=0*abs(c[vert]-90)
    
    linesEdge=9
    err1=abs(len(lines)-linesEdge)  

    if len(avgx)<4:
        return err1+10000,lines 
    
    #c,ind=kmeans2(avgx,2)
    centres, ind, distances=kmeanssample(lines,2,width,maxiter=50)
    n1=len(ind[ind==0])
    n2=len(ind[ind==1])
    
    #print n1,n2
    
    if (n1<=1):
        return err1+10000,lines 
    if (n2<=1):
        return err1+10000,lines 
    
    '''crossX=[]
    indeces=np.array(range(len(lines)))
    for i in range(2):
        #print i
        linesInCluster=lines[ind==i]#indeces[ind==i]]
        xarray=[]
        for line1 in linesInCluster:
            for line2 in linesInCluster:
                cross=computeIntersect(line1,line2)                      
                if (cross):
                    x,y = cross   
                    xarray.append(x)
                    #print x,y
        #print np.median(xarray)
        crossX.append(np.median(xarray))
        
    if (any(np.isnan(crossX))):
        return err1+1000,lines 
    
    if ((crossX[0]<width) & (crossX[1]<width)):
        return err1+1000,lines 
    
    if ((crossX[0]>width) & (crossX[1]>width)):
        return err1+1000,lines '''
    
    err2=0.5*(np.median(distances[ind==1])+np.median(distances[ind==0]))
    #print ind#err1,1000000*err2
    
    return err1+2*linesEdge*mycos(centres[0],centres[1]),lines       


# In[235]:

def main(image):

    intoImg=image.copy()
    width=intoImg.shape[1]
    #print width
    allParams=get_params(parameters)
    loss=[];alllines=[]
    for params in allParams:
        lines = myHL(params,intoImg)
        l,lines=linesLoss2(lines,width*0.5)
        loss.append(l)
        alllines.append(lines)
    
    lines=alllines[np.argmin(loss)]
    angles,avgx = getAngles(lines)
    #print len(lines), loss[np.argmin(loss)]
    
    indeces=np.array(range(len(lines)))
    centres, ind, distances=kmeanssample(lines,2,width*0.5,maxiter=50)
    #print ind
    crossX=[]
    for i in range(2):
        #print i
        linesInCluster=lines[indeces[ind==i]]
        xarray=[]
        for line1 in linesInCluster:
            for line2 in linesInCluster:
                cross=computeIntersect(line1,line2)                      
                if (cross):
                    x,y = cross   
                    xarray.append(x)
                    #print x,y
        #print np.median(xarray)
        crossX.append(np.median(xarray))
    
    #print crossX
    angle=np.pi*65/180
    w=float(intoImg.shape[1])
    f=w/np.tan(0.5*angle)
    x1=min(crossX);x2=max(crossX)
    a1,a2= np.arctan((w/2-x1)/f)/np.pi*180,np.arctan((x2-w/2)/f)/np.pi*180
    a1,a2=90*a1/(a1+a2),90*a2/(a1+a2)
    #print a1
    return a1


# In[236]:

def prepareImage(in_image):
    print 'Processing image: %s' % in_image

    image = cv2.imread(in_image)
    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    intoImg = cv2.Canny(gray, 35, 125)
   
    return intoImg


# In[234]:

#main(prepareImage('box1.jpeg'))


# In[ ]:

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("-i", "--image", required = True, help = "Path to the image")
    args = vars(ap.parse_args())

    #global myglobal
    #main(sys.argv)
    ang=[]
    prepImage=prepareImage(args["image"])
    for i in range(5):
        ang.append(main(prepImage))
    print np.median(ang)

# In[ ]:



