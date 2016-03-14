import numpy as np
import cv2

def get_white_spots(img):

    gray = cv2.cvtColor(img,cv2.COLOR_BGR2GRAY)
    lr = cv2.pyrDown(cv2.pyrDown(cv2.pyrDown(gray)))

    ret, thresh = cv2.threshold(lr,200,255,cv2.THRESH_BINARY)
    kernel = np.ones((3,3),np.uint8)
    opening = cv2.morphologyEx(thresh,cv2.MORPH_OPEN,kernel, iterations = 1)
    
    return get_clusters(opening)

def get_clusters(img):
    (H,W) = np.shape(img)
    last_label = 0
    clusters = {}
    prev_ranges = []
    
    for j in range(H/4,H):
        new_ranges = []
        curr_lbl = None
        for i in range(W):
            if img[j,i] == 255 and curr_lbl == None: 
                curr_x1 = i
                curr_lbl = get_lbl(i,prev_ranges)
                if curr_lbl == -1:
                    curr_lbl = last_label
                    last_label += 1
            elif img[j,i] == 255 and curr_lbl != None:
                prev_lbl = get_lbl(i,prev_ranges)
                if prev_lbl >= 0 and prev_lbl != curr_lbl:
                    # merge label
                    curr_lbl = prev_lbl
            elif img[j,i] == 0 and curr_lbl != None:
                new_ranges.append((curr_x1,i-1,curr_lbl))
                curr_lbl = None
        if curr_lbl != None:
            new_ranges.append((curr_x1,W-1,curr_lbl))
            
        for rng in new_ranges:
            clusters[rng[2]] = clusters.get(rng[2],[]) + [((rng[0]+rng[1])/2,rng[1]-rng[0],j)]
        prev_ranges = new_ranges[:]
    
    for lbl in clusters:
        Weight = float(sum([ w for (_,w,_) in clusters[lbl]]))
        clusters[lbl] = (sum([ x*w for (x,w,_) in clusters[lbl]])/Weight,
                         sum([ y*w for (_,w,y) in clusters[lbl]])/Weight,
                         Weight)
    res = []
    for lbl in clusters:
        x,y,w = clusters[lbl]
        res.append((int(x*8),int(y*8),int(w*64)))
    return res
    
def get_lbl(i,prev_ranges):
    for rng in prev_ranges:
        if i>=rng[0] and i <= rng[1]: return rng[2]
    else:
        return -1

