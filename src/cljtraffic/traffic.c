#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>
//#include "omp.h"

double PROB_DEC = 0.25;

int* create_lane(int);
void update_velocity(int*, int size);
void update_position(int*, int size);

int main(){
  struct timeval start, end, diff;
  int size = 100000;
  int i, iter = 1;
  double elapsed_time;
  int * lane;

  lane = create_lane(size);

  gettimeofday(&start, NULL);
  for(i=0; i<iter; i++){
    update_velocity(lane, size);
    update_position(lane, size);
  }  

  //calc time
  gettimeofday(&end, NULL);
  timeval_subtract(&diff,&end,&start);
  elapsed_time = (int)diff.tv_sec + ((int)diff.tv_usec)/1000000.0;

  printf("Size: %i, Iterations: %i, Time: %lf\n", size, iter, elapsed_time);
  return 0;
}

int
timeval_subtract (result, x, y)
     struct timeval *result, *x, *y;
{
  /* Perform the carry for the later subtraction by updating y. */
  if (x->tv_usec < y->tv_usec) {
    int nsec = (y->tv_usec - x->tv_usec) / 1000000 + 1;
    y->tv_usec -= 1000000 * nsec;
    y->tv_sec += nsec;
  }
  if (x->tv_usec - y->tv_usec > 1000000) {
    int nsec = (x->tv_usec - y->tv_usec) / 1000000;
    y->tv_usec += 1000000 * nsec;
    y->tv_sec -= nsec;
  }
  
  /* Compute the time remaining to wait.
     tv_usec is certainly positive. */
  result->tv_sec = x->tv_sec - y->tv_sec;
  result->tv_usec = x->tv_usec - y->tv_usec;
  
  /* Return 1 if result is negative. */
  return x->tv_sec < y->tv_sec;
}

int* create_array(int size){
  int* arr;
  int i;

  arr = calloc(size, sizeof(int));

  for(i=0; i< size, i++){
    arr[i] = -1
    if(i%3 ==0){
      arr[i] = 0;
    }
  }
}

void update_velocity(int* arr, int size){
  int i, k, v;

  for(i=0; i < size; i++){
    //velocity of -1 means empty cell
    if (arr[i] > -1){

      //increase by 1 if not at max speed
      if(arr[i] < 5){
	v = arr[i] + 1;
      }

      //if the gap between the car in front is two close, reduce the speed
      for(k=1; k< v; k++){
	if (arr[((i+k)%size)] > -1){
	  v = k-1;
	}
      }

      //randomly reduce speed
      if ((rand() * 1.0/RAND_MAX < PROB_DEC) && v > 0){
	v = v-1;
      }
    }
  }
}

void update_position(int*arr, int size){
  int i, pos;

  for(i=0; i<size; i++){
    if(arr[i] > 0){
      pos = arr[i] + i;
      pos = pos % size;
      arr[pos] = arr[i];
      arr[i] = -1;
    }
  }
}
