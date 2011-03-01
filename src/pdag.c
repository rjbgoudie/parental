/*
 *  pdag.c
 *  
 *
 *  Created by Robert Goudie on 14/08/2010.
 *  Copyright 2010 __MyCompanyName__. All rights reserved.
 *
 */


// Not currently correct or at least the same as R version
//structure(list(4L, integer(0), 9L, integer(0), integer(0), c(11L, 
//															 26L), integer(0), integer(0), 5L, 11L, c(22L, 28L), 9L, 5L, integer(0), 
//			   integer(0), 24L, integer(0), c(26L, 29L), c(11L, 28L), integer(0), 
//			   11L, 25L, integer(0), 30L, integer(0), c(22L, 28L), 21L, 
//			   integer(0), c(22L, 28L), 6L), class = c("bn", "parental"))

//include "pdag.h"

#include <R.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <R_ext/BLAS.h>

SEXP maximallyOrientEdges(SEXP pdag, SEXP verbose){
	
	//if (class("pdag") != "pdag"){
	//  stop("Not a pdag")
	//}

	// based on code by Imme Ebert-Uphoff (ebert@tree.com), 2007

	// uses Rules R1-R4 of Meek (1995) to complete
	// orientations in a pdag as far as possible, 
	// i.e. every compelled edge is oriented.
	//
	// (Rules R1-R4 are also summarized in Pearl (2000), p.51
	//  and Neapolitan (2004), p. 546.)
	//
	// Since the PC algorithm also uses Rules R1-R3, their implementation was 
	// copied (with some modifications) from function learn_struct_pdag_pc.
	//
	// Rule R4 is necessary here, since the orientations in the input 
	// pdag do not just represent v-structures.

	// the code below uses -1 for directed edge
	// and 1 for undirected edge
	// so need to set this up
	
	/* extract the "dim" attribute */
	SEXP dim = getAttrib(pdag, R_DimSymbol);
	SEXP old_pdag;
	int pdags_not_same;
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
	
	/* extracting the pointer just once */
	double *p = REAL(pdag);

	Rboolean verbose2 = asLogical(verbose);
	//Rprintf("verbose: %i \n", verbose);
	
	//	undirected_edges <- which(pdag == 1 & pdag != t(pdag), arr.ind = T)
	//	for (rownum in seq_len(nrow(undirected_edges))){
	//		i <- undirected_edges[rownum, 1]
	//		j <- undirected_edges[rownum, 2]
	//		pdag[i, j] <- -1
	//	}
	int i, j;
	for (i = 0; i < nrow; i++){
		for (j = 0; j < ncol; j++){
			if ((p[i + nrow * j] == 1) && (p[j + nrow * i] != p[i + nrow * j])){
				//Rprintf(" %i %i \n", i + 1, j + 1);
				p[i + nrow * j] = -1;
				//Rprintf(" %f\n", fabs(p[i + nrow * j]));
			}
		}
	};

	//
	//	n <- nrow(pdag)
	//	old_pdag <- matrix(0, nrow = n, ncol = n)
	PROTECT(old_pdag = allocMatrix(REALSXP, nrow, ncol));
	
	pdags_not_same = 1;
	//	// recurse until we don't do anything
	//	while (!identical(pdag, old_pdag)){
	//
	//		old_pdag <- pdag
	while (pdags_not_same == 1){
		old_pdag = pdag;
		double *op = REAL(old_pdag);
		//		// Rule R1
		//		directed_edges <- NULL
		//		directed_edges <- which(pdag == -1, arr.ind = T) // a -> b
		//		for (rownum in seq_len(nrow(directed_edges))){
		//			a <- directed_edges[rownum, 1]
		//			b <- directed_edges[rownum, 2]
		//			undirected <- abs(pdag) + t(abs(pdag))
		//			// Adjacency test in undirected matrix:
		//			//   a adjacent b  <=>  undirected(a,b) ==0
		//			// That's easier to use than adjacency test in pdag:
		//			//   a adjacent b  <=>  pdag(a,b)==0 and pdag(b,a)==0
		//
		//			// Find all nodes c such that  b-c  and c not adjacent a
		//			C <- which(pdag[b, ] == 1 & undirected[a,] == 0) 
		//			if (length(C) > 0){
		//				pdag[b, C] <- -1
		//				pdag[C, b] <- 0
		//				if (verbose){
		//					for (j in seq_along(C)){
		//						cat('Rule 1:', b, 'to', C[j], "\n")
		//					}
		//				}
		//			}
		//		}
		
		/*
		 Want:
		 pdag[a, b] == -1 // directed edge a -> b
		 pdag[a, c] == 0, pdag[c, a] == 0
		 pdag[b, c] == 1
		*/
		int a, b, c;
		for (a = 0; a < nrow; a++){
			for (b = 0; b < ncol; b++){
				for (c = 0; c < ncol; c++){
					if ((p[a + nrow * b] == -1) && (p[a + nrow * c] == 0) && (p[c + nrow * a] == 0) && (p[b + nrow * c] == 1)){
						if (verbose2){
							Rprintf("Rule 1: %i to %i \n", b + 1, c + 1);
						}
						p[b + nrow * c] = -1;
						p[c + nrow * b] = 0;
					}
					
				}
			}
		};
		
		
		//		// Rule R2
		//		undirected_edges <- NULL
		//		// get all undirected edges
		//		undirected_edges <- which(pdag == 1, arr.ind = T)
		//		// for each undirected edge
		//		for (rownum in seq_len(nrow(undirected_edges))){
		//			// get the end points
		//			a <- undirected_edges[rownum, 1]
		//			b <- undirected_edges[rownum, 2]
		//
		//			// it seems that we do not check for a route between the nodes
		//			// instead, it has to be a 2-stage path
		//			if (any(pdag[a, ] == -1 & pdag[, b] == -1)){
		//				pdag[a,b] <- -1
		//				pdag[b,a] <- 0
		//
		//			if (verbose){
		//					cat('Rule 2:', a, 'to', b, "\n")
		//				}
		//			}
		//		}
		/*
		 pdag[a, b] == 1
		 pdag[a, c] == -1
		 pdag[c, b] == -1
		*/
		a = 0;
		b = 0;
		c = 0;
		for (a = 0; a < nrow; a++){
			for (b = 0; b < ncol; b++){
				for (c = 0; c < ncol; c++){
					if ((p[a + nrow * b] == 1) && (p[a + nrow * c] == -1) && (p[c + nrow * b] == -1)){
						if (verbose2){
							Rprintf("Rule 2: %i to %i \n", a + 1, b + 1);
						}
						p[a + nrow * b] = -1;
						p[b + nrow * a] = 0;
					}
					
				}
			}
		};
		
		
		//		// Rule R3
		//		undirected_edges <- NULL
		//		undirected_edges <- which(pdag == 1, arr.ind = T) // unoriented a-b edge
		//		for (rownum in seq_len(nrow(undirected_edges))){
		//			a <- undirected_edges[rownum, 1]
		//			b <- undirected_edges[rownum, 2]
		//			C <- which(pdag[a,] == 1 & pdag[,b] == -1)
		//			// C contains nodes c s.t. a-c->b-a
		//
		//			// Extract lines and columns corresponding only to the set of nodes C
		//			core <- pdag[C,C]
		//
		//			// Prepare adjacency test:
		//			unoriented = abs(core) + t(abs(core)) 
		//			// Now:  a non-adjacent b <==> unoriented(a,b) == 0
		//
		//			// Prepare to detect existence of non-adjacent pairs of nodes in C.
		//			// Set diagonal to 1, to prevent finding pairs of IDENTICAL nodes:
		//			diag(unoriented) <- 1
		//
		//			// C contains 2 different non-adjacent elements
		//			if (any(unoriented == 0)){
		//				pdag[a,b] <- -1
		//				pdag[b,a] <- 0 
		//				if (verbose){
		//					cat('Rule 3:', a, 'to', b, "\n")
		//				}
		//			}
		//		}
		/*
		 pdag[a, b] == 1
		 pdag[a, c] == 1
		 pdag[c, b] == -1
		 
		 
		*/
		a = 0;
		b = 0;
		c = 0;
		int d, e;
		for (a = 0; a < nrow; a++){
			for (b = 0; b < ncol; b++){
				if (p[a + nrow * b] == 1){
					int *clist;
					clist = (int *) malloc(ncol * sizeof(int));
					int count = 0;
					
					for (c = 0; c < ncol; c++){
						if ((p[a + nrow * c] == 1) && (p[c + nrow * b] == -1)){
							if (verbose2){
								Rprintf("Rule 3 count %i \n", count);
								Rprintf("Rule 3 Cs %i \n", c + 1);
							}
							clist[count] = c;
							count++;
						}
					}
					// got set c somehow
					for (d = 0; d < count; d++) {
						for (e = 0; e < d; e++) {
							if ((p[clist[d] + nrow * clist[e]] == 0) && (p[clist[e] + nrow * clist[d]] == 0)){
								if (verbose2){
									Rprintf("Rule 3: %i to %i \n", a + 1, b + 1);
								}
								p[a + nrow * b] = -1;
								p[b + nrow * a] = 0;
							}
						}
					}
				}
			}
		};
		
		
		//		// Rule 4
		//		undirected_edges <- NULL
		//		undirected_edges <- which(pdag == 1, arr.ind = T) // unoriented a-b edge
		//		for (rownum in seq_len(nrow(undirected_edges))){
		//			a <- undirected_edges[rownum, 1]
		//			b <- undirected_edges[rownum, 2]
		//
		//			// Prepare adjacency test:
		//			// unoriented(i,j) is 0 (non-adj) or 1 (directed) or 2 (undirected)
		//			unoriented <- abs(pdag) + t(abs(pdag))
		//
		//			// Find c such that c -> b and a,c are adjacent (a-c or a->c or a<-c) 
		//			C = which((pdag[,b] == -1) & (unoriented[a,]>=1), arr.ind = T)
		//			for (j in seq_along(C)){
		//				c <- C[j]
		//				// Check whether there is any node d, such that
		//				// d->c  AND  a-d  AND  b NOT adjacent to d
		//				if (any(pdag[, c]== -1 & pdag[a, ] == 1 & unoriented[b, ] == 0)){
		//				pdag[a,b] = -1 
		//				pdag[b,a] = 0  
		//					if (verbose){
		//						cat('Rule 4:', a, 'to', b, "\n")
		//					}
		//				}
		//			}
		//		}
		/*
		 pdag[a, b] == 1
		 pdag[c, b] == -1
		 pdag[a, c] >= 1
		 pdag[c, a] >= 1
		 
		 pdag[d, c] == -1
		 pdag[a, d] == 1
		 pdag[b, d] == 0
		 pdag[d, b] == 0
		*/
		a = 0;
		b = 0;
		c = 0;
		d = 0;
		for (a = 0; a < nrow; a++){
			for (b = 0; b < ncol; b++){
				for (c = 0; c < ncol; c++){
					if ((p[a + nrow * b] == 1) && (p[c + nrow * b] == -1) && (p[a + nrow * c] >= 1) && (p[c + nrow * a] >= 1)){
						for (d = 0; d < ncol; d++){
							if ((p[d + nrow * c] == -1) && (p[a + nrow * d] == 1) && (p[b + nrow * d] == 0) && (p[d + nrow * b] == 0)){
								if (verbose2){
									Rprintf("Rule 4: %i to %i \n", a + 1, b + 1);
								}
								p[a + nrow * b] = -1;
								p[b + nrow * a] = 0;
							}
						}
					}
				}
			}
		};
		
		pdags_not_same = 0;
		for (i = 0; i < nrow; i++){
			for (j = 0; j < ncol; j++){
				if (p[i + nrow * j] != op[i + nrow * j]){
					pdags_not_same = 1;
				}
			}
		};
	}
	
	UNPROTECT(1);
	return(pdag);
	// Oriented all possible edges.  Return result.
//	pdag
};




SEXP appendToList(SEXP old, SEXP v) {
	SEXP new;
	int i = 0;
	int n = length(old);
//	Rprintf("Appending to length %i\n", n+1);
	PROTECT(new = allocVector(VECSXP, n + 1));
	for (i = 0; i < n; i++){
		SET_VECTOR_ELT(new, i, Rf_duplicate(VECTOR_ELT(old, i)));
	};
	SET_VECTOR_ELT(new, n, v);
	UNPROTECT(1);
	return new;
}

SEXP recurse_unoriented_edge(SEXP cpdag, SEXP dag_list, SEXP verbose){
//# input must be a COMPLETE pdag
	
	//	
	//	updated_list <- dag_list
	//	
	Rboolean verbose2 = asLogical(verbose);
	int dag_list_length = length(dag_list);
	SEXP updated_list;
//	PROTECT(updated_list = allocVector(VECSXP, dag_list_length));
	
	/* extract the "dim" attribute */
	SEXP dim = getAttrib(cpdag, R_DimSymbol);
	int pdags_not_same;
	int nrow = INTEGER(dim)[0];
	int ncol = INTEGER(dim)[1];
	
	/* extracting the pointer just once */
	double *p = REAL(cpdag);
	
	int foundUndirected = 0;
	
	int m;
//	Rprintf("CURRENT LIST %i\n", dag_list_length);
//	for (m = 0; m < dag_list_length; m++){
//		Rprintf("Copying %i\n", m);
//		SET_VECTOR_ELT(updated_list, m, Rf_duplicate(VECTOR_ELT(dag_list, m)));
//	};
//	PrintValue(dag_list);
	PROTECT(updated_list = duplicate(dag_list));
//	Rprintf("Copyingend \n");


	int i, j;
	i = 0;
	j = 0;
	for (j = 0; j < nrow; j++){
		for (i = 0; i < ncol; i++){
			//# find all undirected edges
			//	undirected_edges <- which(cpdag == 1, arr.ind = T)
			if ((p[i + nrow * j] == 1) && (foundUndirected == 0)){

				foundUndirected = 1;
				
				
				SEXP pdag1, pdag2;
				
				//# choose first unoriented edge
				//# (any unoriented edge could be used here)
				//		a <- undirected_edges[1, 1]
				//		b <- undirected_edges[1, 2]
				//		
				//# choose two different directions for edge and complete BOTH !      
				//# PDAG1: contains a -> b
				//		pdag1 <- cpdag
				//		pdag1[a,b] <- -1
				//		pdag1[b,a] <- 0
				//		if (verbose){
				//			cat("PDAG1: ", a, " -> ", b, "\n")
				//		}
				//# complete as far as possible using rules R1-R4:
				//		cpdag1 = maximallyOrientEdges(pdag1, verbose = verbose)
				//# Continue recursion on another unoriented edge
				//		updated_list <- recurse_unoriented_edge(cpdag1, updated_list, verbose)
				if (verbose2){
					Rprintf("PDAG1 %i -> %i \n", i + 1, j + 1);
				}
				
				PROTECT(pdag1 = duplicate(cpdag));
				double *p1 = REAL(pdag1);
				p1[i + nrow * j] = -1;
				p1[j + nrow * i] = 0;
				
				SEXP cpdag1 = maximallyOrientEdges(pdag1, verbose);
				updated_list = recurse_unoriented_edge(cpdag1, updated_list, verbose);
				
				//		
				//# PDAG1: contains b -> a
				//		if (verbose){
				//			cat("PDAG2: ", b, " -> ", a, "\n")
				//		}
				//		pdag2 = cpdag
				//		pdag2[a,b] <- 0
				//		pdag2[b,a] <- -1
				//# complete as far as possible using rules R1-R4:
				//		cpdag2 = maximallyOrientEdges(pdag2, verbose = verbose)
				//# Continue recursion on another unoriented edge
				//		updated_list <- recurse_unoriented_edge(cpdag2, updated_list, verbose)
				if (verbose2){
					Rprintf("PDAG1 %i -> %i \n", j + 1, i + 1);
				}
				PROTECT(pdag2 = duplicate(cpdag));
				double *p2 = REAL(pdag2);
				p2[i + nrow * j] = 0;
				p2[j + nrow * i] = -1;
				
				SEXP cpdag2 = maximallyOrientEdges(pdag2, verbose);
				updated_list = recurse_unoriented_edge(cpdag2, updated_list, verbose);
				UNPROTECT(2);
			}
		}
	};
				

	//	if (nrow(undirected_edges) == 0){
	//# if no undirected edges left
	//# End of recursion reached.
	//# Convert all (-1) values to (1) to yield standard DAG, add DAG to list.
	//		updated_list <- append(updated_list, list(abs(cpdag)))
	//	}
	if (foundUndirected == 0){
		if (verbose2){
			Rprintf("Found nothing %i", foundUndirected);
		}
		for (i = 0; i < nrow; i++){
			for (j = 0; j < ncol; j++){
				p[i + nrow * j] = fabs(p[i + nrow * j]);
			}
		}
		
		//SEXP new;
		//PROTECT(new = allocVector(VECSXP, 1));
		//SET_VECTOR_ELT(new, 1, cpdag);
		updated_list = appendToList(updated_list, cpdag);
		//UNPROTECT(1);
	}

	UNPROTECT(1);
	return(updated_list);
}