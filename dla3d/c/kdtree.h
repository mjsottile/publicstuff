/*
 *  kdtree.h
 *  kdtree
 *
 *  Created by Geoff Hulette on 6/3/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#include "vecmath.h"

#ifndef KD_TREE_H
#define KD_TREE_H

/*
 Node data structure for the kd-tree
 */
typedef struct tree_node {
	point *loc;
	struct tree_node *left, *right;
} tree_node;

/*
 Simple linked-list data structure for returning lists of points
 */
typedef struct list_node {
	point *loc;
	struct list_node *next;
} list_node;


typedef struct point_list {
	point **point_ptrs;
	int max_points;
	int num_points;
} point_list;

/*
 Return a new empty kd-tree data structure to use as root
 */
tree_node *kdt_new_tree();

/*
 Create an empty pointlist with a maximum size
 */
point_list *kdt_new_point_list(int n);
void kdt_clear_list(point_list *list);

/*
 Insert a new point into the kd-tree
 */
void kdt_add_point(tree_node *root, point *pt);

/*
 Search a kd-tree for all points within a given rectilinear space
 Constrain min[i] < max[i] for all i in the n-space
 Returns a linked list of the points within the subspace
 */
void kdt_range_search(tree_node *root, point *min, point *max, point_list *result);

/*
 Collision detection on a point moving from start to end
 Returns 1 if a collision occurs (and loads c with the location), 0 otherwise (with c undefined)
 */
int kdt_collision_detect(tree_node *root, point *start, point *end, point *c, float epsilon, point_list *p);

/*
 Simple test for whether the given point lies within the rectilinear space defined by min and max
 */
int kdt_is_point_within_bounds(point *pt, point *min, point *max);

/*
 Max depth of the kd-tree
 */
int kdt_max_depth(tree_node *root);

/*
 Print out a tree in Graphviz DOT language to stdout
 */
void kdt_print_tree(tree_node *root);

/*
 Dump a linked list of points to stderr
 */
void kdt_print_list(point_list *list);

#endif
