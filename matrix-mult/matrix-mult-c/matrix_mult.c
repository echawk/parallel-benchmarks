#include <stdio.h>
#include <stdlib.h>

typedef struct {
	int r, c;
	int** vals;
} imatrix_t;

int** calloc_dyn_2d_arr(int r, int c){
	int** arr;
	arr = calloc(r, sizeof(int*));
	for(int i = 0; i < c; i++)
		arr[i] = calloc(c, sizeof(int));
	return arr;
}

int* get_column(imatrix_t mtx, int col){
	int* column = calloc(mtx.r, sizeof(int));
	for(int i = 0; i < mtx.r; i++)
		column[i] = mtx.vals[i][col];
	return column;
}

int dot_product(int* row, int* col, int n_elements){
	int sum = 0;
	for(int i = 0; i < n_elements; i++)
		sum += row[i] * col[i];
	return sum;
}

imatrix_t matrix_mult(imatrix_t mtx1, imatrix_t mtx2){
	if(mtx1.c != mtx2.r){
		fprintf(stderr, "[ERROR] Number of columns in matrix 1 do not equal number of rows in matrix 2.\n");
		exit(EXIT_FAILURE);
	}
	else{
		imatrix_t mtx3 = (imatrix_t){mtx1.r, mtx2.c, calloc_dyn_2d_arr(mtx1.r, mtx2.c)};
		for(int i = 0; i < mtx1.r; i++){
			for(int j = 0; j < mtx2.c; j++){
				mtx3.vals[i][j] = dot_product(mtx1.vals[i], get_column(mtx2, j), mtx1.c);
			}
		}
		return mtx3;
	}
}

void free_2d_arr(int** arr, int r){
	for(int i = 0; i < r; i++){
		free(arr[i]);
	}
	free(arr);
}

void print_mtx(imatrix_t mtx){
	for(int r = 0; r < mtx.r; r++){
		for(int c = 0; c < mtx.c; c++){
			printf("%2d ", mtx.vals[r][c]);
		}
		printf("\n");
	}
}

void main(int argc, char** argv){
	imatrix_t mtx1 = (imatrix_t){2, 3, calloc_dyn_2d_arr(2, 3)};
	imatrix_t mtx2 = (imatrix_t){3, 4, calloc_dyn_2d_arr(3, 4)};

	for(int i = 0; i < mtx1.r; i++){
		for(int j = 0; j < mtx1.c; j++){
			mtx1.vals[i][j] = (i * (mtx1.r + 1)) + j;
		}
	}

	for(int i = 0; i < mtx2.r; i++){
		for(int j = 0; j < mtx2.c; j++){
			mtx2.vals[i][j] = (i * (mtx2.r + 1)) + j;
		}
	}

	print_mtx(mtx1);
	printf("\n");
	print_mtx(mtx2);
	free_2d_arr(mtx1.vals, mtx1.r);
	// free_2d_arr(mtx2.vals, mtx2.r);

	exit(EXIT_SUCCESS);
}
