import matplotlib.pyplot as plt
import numpy as np
from random import random

rot90 = np.array([[0,-1],[1,0]])

class Robot:
    def __init__(self,xy,th,half_width=4,scan_range=20,view_dist=25,tick=0.5):
        self.xy = np.array(xy)
        self.th = th
        self.half_width = half_width
        self.scan_range = scan_range
        self.view_dist = view_dist
        self.tick = tick

    def set_angle(self,th):
        self.th = th

    def set_speed(self,speed):
        self.v = speed

    def set_itinarary(self,path):
        path.reverse() # the first point to visit is the last element in the list
        self.itinarary = path # the first point to visit is the last element in the list

    def start_mission(self,world,v=10):
        self.path = [self.xy.copy()]
        self.set_speed(v)
        self.visited = []
        self.missed = []
        self.found_tags = []

        while len(self.itinarary) > 0:
            target = self.itinarary.pop()
            self.go(target,world)

        print 'Found %i tags:' %len(self.found_tags)
        return self.visited, self.missed

    def go(self,target,world):
        delta = self.v * self.tick
        self.th = np.arctan2(*reversed(target - self.xy))
        dS = delta * np.array([np.cos(self.th), np.sin(self.th)])

        print 'target:', target,
        distance = np.linalg.norm(self.xy-target)
        while distance > delta:
            obs = self.get_obstacles(world.obstacles,distance)
            if obs == []:
                tags = self.seek_tags(world.tags)
                new_tags = self.get_new_tags(tags)
                sorted_tags = sorted([ (np.linalg.norm(tag-self.xy),tag) for tag in new_tags])
                if len(sorted_tags)>0:
                    new_target = np.array(sorted_tags[0][1])
                    print '- postponed. New tag found at:', new_target
                    self.itinarary.append(target)
                    self.itinarary.append(new_target)
                    return False
                self.xy += dS
                self.path.append(self.xy.copy())
                distance = np.linalg.norm(self.xy-target)
            else:
                ob_center,ob_radius = self.get_closest_obstacle(obs)

                if np.linalg.norm(ob_center-target) < ob_radius + 3*self.half_width: # target inside or close the obstacle
                    print '- missed'
                    if len(self.itinarary) > 0:
                        if np.linalg.norm(self.itinarary[-1]-target) > 5*self.half_width:
                            new_target = np.add(0.7*target, 0.3*self.itinarary[-1])
                            print 'new intermidiate target added:',new_target
                            self.itinarary.append(new_target)
                    #_ = raw_input()
                    return False
                else:
                    ob_direction = ob_center-self.xy
                    if np.cross(dS, ob_direction) > 0: # obstacle is on the left
                        ort = rot90.dot(-dS)/np.linalg.norm(dS)
                    else:
                        ort = rot90.dot(dS)/np.linalg.norm(dS)

                    new_target = ob_center + ort * (ob_radius + 2*self.half_width)
                    print '- postponed. Go round the obstacle to:',new_target
                    self.itinarary.append(target)
                    self.itinarary.append(new_target)
                    #_ = raw_input()
                    return False
        print 'reached'
        return True

    def get_obstacles(self,obstacles,distance):
        obs = []
        xy = self.xy + self.scan_range * np.array([np.cos(self.th), np.sin(self.th)])
        for ob in obstacles:
            if np.linalg.norm(xy-ob[:2]) < ob[2]+self.half_width/2 and distance > np.linalg.norm(self.xy-ob[:2])-ob[2]:
                obs.append(ob)
        return obs

    def get_closest_obstacle(self,obstacles):
        _,ob_center,ob_radius = min([ (np.linalg.norm(self.xy-ob[:2])-ob[2], ob[:2], ob[2]) for ob in obstacles])
        return ob_center,ob_radius

    def seek_tags(self,tags):
        c = self.xy + self.view_dist * np.array([np.cos(self.th), np.sin(self.th)])
        return [ tuple(tag[:2]) for tag in tags if np.linalg.norm(tag[:2]-c) < self.view_dist/2 ]

    def get_new_tags(self,tags):
        new_tags = []
        for tag in tags:
            if tag not in self.found_tags:
                new_tags.append(tag)
        self.found_tags += new_tags
        return new_tags


def get_spiral(turns=5,origin=(0.,0.),step=15.):
    xy = np.array(origin,dtype='float32')
    R = np.array([[0.,-1.], [1., 0.]])
    L = step
    vec = np.array([0,1])
    mission = []
    for i in range(2*turns):
        xy += L * vec
        mission.append(xy.copy())
        vec = R.dot(vec)
        xy += L * vec
        mission.append(xy.copy())
        vec = R.dot(vec)
        L += step
    return mission


class World:
    def __init__(self,obstacles=4,tags=10,distance=10):
        self.obstacles = []
        while len(self.obstacles) < obstacles:
            new_ob = np.array( (250*(random()-0.5),250*(random()-0.5),10+15*random() ) )
            if all([ np.linalg.norm(new_ob[:2]-ob[:2]) > new_ob[2]+ob[2]+distance for ob in self.obstacles]):
                self.obstacles.append(new_ob)

        self.tags = []
        while len(self.tags) < tags:
            new_tag = np.array( (250*(random()-0.5),250*(random()-0.5),random()>0.9 ) )
            if all([ np.linalg.norm(new_tag[:2]-ob[:2]) > ob[2] for ob in self.obstacles ]):
                self.tags.append(new_tag)
        print "%i obstacles and %i tags generated:\n" %(obstacles,tags), self.tags


def main():
    robot = Robot((0.,0.),0.77)
    world = World(obstacles=5,tags=25)

    mission = get_spiral(turns=6,origin=(0.,0.),step=20)
    mission.append(np.array([0.,0.]))

    robot.set_itinarary(mission)
    visited,missed = robot.start_mission(world)

    print 'missed points:',missed

    fig = plt.figure()
    Ox,Oy,_ = zip(*world.obstacles)
    circs = [ plt.Circle(tuple(ob[:2]),ob[2],color='r') for ob in world.obstacles ]
    for circ in circs:
        fig.gca().add_artist(circ)
    Tx,Ty,_ = zip(*world.tags)
    X,Y = zip(*robot.path)
    plt.plot(Tx,Ty,'g^',Ox,Oy,'ro',X,Y,'b-')
    plt.show()
