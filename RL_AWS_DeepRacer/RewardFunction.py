# -*- coding: utf-8 -*-
"""
Created on Mon Jul  5 11:49:23 2021

"""

import math


def dist(x, y):
    return math.sqrt((x[0]-y[0])**2 + (x[1]-y[1])**2)


def angle(x, y):
    a = math.degrees(math.atan2(
        y[1] - x[1],
        y[0] - x[0]
    ))
    return a


def up_sample(waypoints, k):
    p = waypoints
    n = len(p)
    return [[i / k * p[(j+1) % n][0] + (1 - i / k) * p[j][0],
             i / k * p[(j+1) % n][1] + (1 - i / k) * p[j][1]] for j in range(n) for i in range(k)]


def closest_waypoint_ind(p, waypoints):
    distances = [dist(wp, p) for wp in waypoints]
    min_dist = min(distances)
    return distances.index(min_dist)


def score_delta_steering(delta, worst=60):
    return max(1 - abs(delta / worst), 0)


def reward_function(params):
    # Default Reward
    reward = 1e-3
    
    
    # Read enviroment paramters.
    all_wheels_on_track = params['all_wheels_on_track']
    track_width = params['track_width']
    distance_from_center = params['distance_from_center']
    steering = abs(params['steering_angle']) # Only need the absolute steering angle
    waypoints = params["waypoints"]
    track_width = params["track_width"]
    
    ### follow center line #############################################
    
	# Give a high reward if no wheels go off the track and
	# the agent is somewhere in between the track borders
    if (all_wheels_on_track and (0.5 * track_width - distance_from_center)) >= 0.05:
        reward = 1.0

    
    ### Waypoints #############################################

    # Read states
    x, y = params["x"], params["y"]
    heading = params["heading"]
    steering_angle = params["steering_angle"]

    # Up-sample waypoints to form a series of dense racing line points.
    waypoints = up_sample(waypoints, k=30)

    # Get the closest waypoint given current position (x, y).
    which_closest = closest_waypoint_ind((x, y), waypoints)

    # Re-order the waypoints from the cloest for latter lookup.
    following_waypoints = waypoints[which_closest:] + waypoints[:which_closest]

    # Determine the desired heading angle based on a target waypoint.
    # 1. Locate the target waypoint with a search radius.
    #    Target point should be the cloest waypoint just outside the radious.
    search_radius = track_width * 0.9
    target_waypoint = waypoints[which_closest]
    for i, p in enumerate(following_waypoints):
        if dist(p, (x, y)) > search_radius:
            target_waypoint = following_waypoints[i]
            break
    # 2. Determine the desired steering angle.
    target_heading = angle((x, y), target_waypoint)
    target_steering = target_heading - heading
    delta_steering = steering_angle - target_steering

    # Reward based on difference between current and desired steering_angle.
    reward += score_delta_steering(delta_steering, worst=45)

    return float(reward)
