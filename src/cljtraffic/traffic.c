#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <sys/time.h>
#include "omp.h"

typedef struct{
  int vel;
  int pref_vel;
} vehicle;


double PROB_DEC = 0.25;

vehicle** create_lane(int, int);
void update_velocity(vehicle**, int);
void update_position(vehicle**,vehicle**, int);
void print_cells(vehicle**, int);
int main(){
  struct timeval start, end, diff;
  int size = 100000;
  int i, k, n_threads, iter = 1000;
  double elapsed_time;
  vehicle ** lane, **next_lane, **temp;


  lane = create_lane(size, 1);
  next_lane = create_lane(size, 0);
  n_threads = 2;
  omp_set_num_threads(n_threads);

  gettimeofday(&start, NULL);
  for(i=0; i<iter; i++){
    
    update_velocity(lane, size);
    update_position(lane, next_lane, size);
    temp = lane;
    lane = next_lane;
    next_lane = temp;
    //print_cells(lane, 10);
    /*    for(k=0; k<size; k++){
      next_lane[k] = NULL;
      }*/
  }  

  //calc time
  gettimeofday(&end, NULL);
  timeval_subtract(&diff,&end,&start);
  elapsed_time = (int)diff.tv_sec + ((int)diff.tv_usec)/1000000.0;

  printf("Threads: %i, Size: %i, Iterations: %i, Time: %lf\n", n_threads, size, iter, elapsed_time);


  return 0;
}

void print_cells(vehicle** lane, int num_cells){
  int i;
  for (i=0; i<num_cells; i++){
    printf("Cell Num: %i, ", i);
    if (lane[i]== NULL){
      printf("Empty\n");
    }
    else{
      printf("Velocity: %i\n",  lane[i]->vel);
      }
  }
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

vehicle** create_lane(int size, int init){
  vehicle** arr;
  vehicle temp_car;
  int i;

  arr = (vehicle**) malloc(sizeof(vehicle*) * size);

  for(i=0; i< size; i++){
    arr[i] = NULL;
    if(i%3 ==0 && init != 0){
      arr[i] = (vehicle *) malloc(sizeof(vehicle));
      arr[i]->vel=0;
      arr[i]->pref_vel = 5;
    }
  }

  return arr;
}

void update_velocity(vehicle** arr, int size){
  int i, k, v;

#pragma omp parallel for private(k,v)
  for(i=0; i < size; i++){
    //velocity of -1 means empty cell
    if (arr[i] != NULL){

      //increase by 1 if not at max speed
      if(arr[i]->vel < arr[i]->pref_vel){
	v = arr[i]->vel + 1;
      }

      //if the gap between the car in front is two close, reduce the speed
      for(k=1; k< v; k++){
	if (arr[((i+k)%size)] != NULL){
	  v = k-1;
	}
      }

      //randomly reduce speed
      if ((rand() * 1.0/RAND_MAX < PROB_DEC) && v > 0){
	v = v-1;
      }

      //assign new velocity
      arr[i]->vel = v;
    }
  }
}

void update_position(vehicle** lane, vehicle** next_lane, int size){
  int i, pos;

#pragma omp parallel for private(pos)
  for(i=0; i<size; i++){
    if(lane[i] != NULL){
      pos = lane[i]->vel + i;
      pos = pos % size;
      next_lane[pos] = lane[i];
      lane[i] = NULL;
    }
  }
}
