/*
 *  kdtree.c
 *  kdtree
 *
 *  Created by Geoff Hulette on 6/3/08.
 *  Copyright 2008 __MyCompanyName__. All rights reserved.
 *
 */

#include <stdio.h>
#include <stdlib.h>

#include "kdtree.h"

tree_node *kdt_new_tree() {
	tree_node *n = (tree_node *)malloc(sizeof(tree_node));
	n->loc = NULL;
	n->left = NULL;
	n->right = NULL;
	return n;
}

point_list *kdt_new_point_list(int n) {
	point_list *list = (point_list *)malloc(sizeof(point_list));
	list->max_points = n;
	list->num_points = 0;
	list->point_ptrs = (point **)malloc(sizeof(point *) * n);
	return list;
}

void kdt_add_point_to_list(point *pt, point_list *list) {
	if(list->num_points >= list->max_points) {
		fprintf(stderr, "Error: list overflowed\n");
		exit(1);
	}
	list->point_ptrs[list->num_points] = pt;
	list->num_points++;
}

void kdt_clear_list(point_list *list) {
	list->num_points = 0;
}

void kdt_add_point_with_depth(tree_node *root, point *pt, int depth) {
	if(root->loc == NULL) {
		root->loc = pt;
	}
	else {
		int axis = depth % KDT_DIM;
		if(pt->coord[axis] < root->loc->coord[axis]) {
			if(root->left == NULL) {
				root->left = kdt_new_tree();
			}
			kdt_add_point_with_depth(root->left, pt, depth + 1);
		}
		else {
			if(root->right == NULL) {
				root->right = kdt_new_tree();
			}
			kdt_add_point_with_depth(root->right, pt, depth + 1);
		}
	}
}

void kdt_add_point(tree_node *root, point *pt) {
	kdt_add_point_with_depth(root, pt, 0);
}

int kdt_is_point_within_bounds(point *pt, point *min, point *max) {
	int i, result = 1;
	for(i=0; i < KDT_DIM; i++) {
		if(pt->coord[i] < min->coord[i] || pt->coord[i] > max->coord[i]) {
			result = 0;
		}
	}
	return result;
}

void kdt_range_search_rec(tree_node *root, point *min, point *max, point_list *result, int depth) {
	if(root == NULL || root->loc == NULL) {
		return;
	}
	int axis = depth % KDT_DIM;
	if(root->loc->coord[axis] < min->coord[axis]) {
		kdt_range_search_rec(root->right, min, max, result, depth + 1);
	}
	else if(root->loc->coord[axis] > max->coord[axis]) {
		kdt_range_search_rec(root->left, min, max, result, depth + 1);
	}
	else { // min < ptk < max
		if(kdt_is_point_within_bounds(root->loc, min, max)) {
			kdt_add_point_to_list(root->loc, result);
		}
		kdt_range_search_rec(root->left, min, max, result, depth + 1);
		kdt_range_search_rec(root->right, min, max, result, depth + 1);
	}
}

void kdt_range_search(tree_node *root, point *min, point *max, point_list *result) {
	return kdt_range_search_rec(root, min, max, result, 0);
}

int kdt_collision_detect(tree_node *root, point *start, point *end, point *c, double epsilon, point_list *pts) {
	int i;
	point rmin, rmax;
	for(i=0; i < KDT_DIM; i++) {
		rmin.coord[i] = vec_fmin(start->coord[i], end->coord[i]) - epsilon;
		rmax.coord[i] = vec_fmax(start->coord[i], end->coord[i]) + epsilon;
	}
	kdt_clear_list(pts);
	kdt_range_search(root, &rmin, &rmax, pts);
	
	vec a, b, p, e;
	vec_sub(&a, end, start);
	for(i=0; i < pts->num_points; i++) {
		point *pt = pts->point_ptrs[i];
		vec_sub(&b, pt, start);
		// Collisions should be ordered, with the lowest xhat value being the first
		// currently the first collision detected is returned, with detections in random order
		double xhat = vec_dot_prod(&a, &b) / vec_dot_prod(&a, &a);
		vec_scalar_mult(&p, &a, xhat);
		vec_sub(&e, &p, &b);
		double sqrd_dist = vec_dot_prod(&e, &e);
		if(sqrd_dist < epsilon * epsilon) {
			vec_add(c, start, &p);
			return 1;
		}
	}
	return 0;
}

int kdt_max_depth(tree_node *root) {
	if(root == NULL) {
		return 0;
	}
	int max_depth_left = kdt_max_depth(root->left);
	int max_depth_right = kdt_max_depth(root->right);
	return (max_depth_left > max_depth_right ? max_depth_left : max_depth_right) + 1;
}

int kdt_print_tree_with_id(tree_node *root, int id_num) {
	char buffer[128];
	if(root->loc == NULL) {
		return id_num + 1;
	}
	if(id_num == 1) {
		printf("digraph kdt {\n");
	}
	printf("\t%d [label=\"%s\"]\n", id_num, vec_str(root->loc, buffer));
	int child_id_num = id_num + 1;
	if(root->left != NULL) {
		int left_id_num = child_id_num;
		child_id_num = kdt_print_tree_with_id(root->left, child_id_num);
		printf("\t%d -> %d\n", id_num, left_id_num);
	}
	if(root->right != NULL) {
		int right_id_num = child_id_num;
		child_id_num = kdt_print_tree_with_id(root->right, child_id_num);
		printf("\t%d -> %d\n", id_num, right_id_num);
	}
	if(id_num == 1) {
		printf("}\n");
	}
	return child_id_num;
}

void kdt_print_tree(tree_node *root) {
	kdt_print_tree_with_id(root, 1);
}

void kdt_print_list(point_list *list) {
	char buffer[128];
	int i;
	for(i=0; i < list->num_points; i++) {
		point *pt = list->point_ptrs[i];
		fprintf(stderr, "%s\n", vec_str(pt, buffer));
	}
}
