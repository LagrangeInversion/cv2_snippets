import io
import time, atexit
import picamera
import picamera.array
import cv2
import numpy as np
import socket
import sys
import tempfile
import os
import threading

def inthread(fn):
    def run(*k, **kw):
        t = threading.Thread(target=fn, args=k, kwargs=kw)
        t.start()
    return run

def measure(f):
    def wrap(*args):
        time1 = time.time()
        ret = f(*args)
        time2 = time.time()
        print '%s function took %0.3f ms' % (f.func_name, (time2-time1)*1000.0)
        return ret
    return wrap

def seeds(img):
    seeds = None
    display_mode = 0
    num_superpixels = 400
    prior = 2
    num_levels = 4
    num_histogram_bins = 5
    nframes = 0

    converted_img = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
    height,width,channels = converted_img.shape
    #num_superpixels_new = cv2.getTrackbarPos('Number of Superpixels', 'SEEDS')
    num_superpixels_new = 200
    #num_iterations = cv2.getTrackbarPos('Iterations', 'SEEDS')
    num_iterations = 8

    if not seeds or num_superpixels_new != num_superpixels:
        num_superpixels = num_superpixels_new
        seeds = cv2.ximgproc.createSuperpixelSEEDS(width, height, channels,
                num_superpixels, num_levels, prior, num_histogram_bins)
        color_img = np.zeros((height,width,3), np.uint8)
        color_img[:] = (0, 0, 255)

    seeds.iterate(converted_img, num_iterations)

    # retrieve the segmentation result
    labels = seeds.getLabels()

    # labels output: use the last x bits to determine the color
    num_label_bits = 2
    labels &= (1<<num_label_bits)-1
    labels *= 1<<(16-num_label_bits)

    mask = seeds.getLabelContourMask(False)

    # stitch foreground & background together
    mask_inv = cv2.bitwise_not(mask)
    result_bg = cv2.bitwise_and(img, img, mask=mask_inv)
    result_fg = cv2.bitwise_and(color_img, color_img, mask=mask)
    result = cv2.add(result_bg, result_fg)
    return result

def orb(img, orb=cv2.ORB_create()):
    # find the keypoints with ORB
    kp = orb.detect(img, None)

    # compute the descriptors with ORB
    kp, des = orb.compute(img, kp)

    ## draw only keypoints location,not size and orientation
    #img2 = cv2.drawKeypoints(img, kp, None, color=(0,255,0), flags=0)

    print '%d keypoints' % len(kp)
    return kp, des

class RGBStream(picamera.array.PiRGBAnalysis):
    def __init__(self, camera, f):
        self.f = f
        super(RGBStream, self).__init__(camera)

    @measure
    def analyse(self, array):
        return self.f(array)

def pi1():
    stream = io.BytesIO()
    with picamera.PiCamera() as camera:
        camera.resolution = (320, 240)
        camera.start_preview()
        camera.capture(stream, format='bgr')

    return np.frombuffer(stream.getvalue(), dtype=np.uint8).reshape((240, 320, 3))

def pimany(f, t=5):
    with picamera.PiCamera() as camera:
        with RGBStream(camera, f) as output:
            try:
                camera.resolution = (640, 480)
                camera.framerate = 4
                camera.start_recording(output, format='bgr')
                camera.wait_recording(t)
            finally:
                camera.stop_recording()

if __name__ == '__main__':
    t0 = time.time()
    img = pimany(orb, t=10)
    t1 = time.time()
    print t1-t0

