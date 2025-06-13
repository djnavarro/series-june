#include <Rcpp.h>
#include <iostream>
#include <cmath>
using namespace Rcpp;
using namespace std;

double edge_length(double x1, double y1, double x2, double y2) {
  double len = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2));
  return len; 
}

NumericVector edge_noise(double x0, double y0, double dx, double dy, double noise) {
  
  // vars
  NumericVector delta (2);
  double ux, uy, x, y, r, xs, ys, xb, yb;

  // constants
  const double narrow = 0.5;
  const double wide = 1.0;
  const double bias = 0.2;
  
  // find coords (x, y) and the ratio
  x = x0 + dx/2;
  y = y0 + dy/2;
  r = fabs(x) / (fabs(x) + fabs(y));
  
  // shift and bias on the noise depends on position
  xs = (r * narrow) + ((1-r) * wide);
  ys = (r * wide) + ((1-r) * narrow);
  xb = (1-r) * bias;
  yb = r * bias;

  // sample random numbers
  ux = R::runif(xb - xs/2, xb + xs/2);
  uy = R::runif(yb - ys/2, yb + ys/2);
  
  // create delta and return
  delta(0) = ux * noise;
  delta(1) = uy * noise;
  return delta;
}

int sample_edge(NumericVector seg_len) {
  double tot_len = 0;
  for(int i = 0; i < seg_len.length(); i++) {
    tot_len += seg_len(i);
  }
  double u = R::runif(0, tot_len);
  int ind = 0;
  while(ind < seg_len.length() & u > seg_len(ind)) {
    u = u - seg_len(ind);
    ind++;
  }
  return ind;
}

// [[Rcpp::export]]
DataFrame grow_polygon(DataFrame polygon, int iterations, double noise) {
  
  NumericVector x = polygon["x"];
  NumericVector y = polygon["y"];
  NumericVector seg_len = polygon["seg_len"];
  IntegerVector lookup;
  
  int final_length = x.length() + iterations;  
  IntegerVector position (final_length);
  NumericVector x_out (final_length);
  NumericVector y_out (final_length); 
  NumericVector seg_len_out (final_length);
  
  for(int i = 0; i < x.length() - 1; i++) {
    lookup.push_back(i + 1);
  }
  lookup.push_back(0);
  
  int ind, next_ind;
  double last_x, last_y, next_x, next_y, new_x, new_y, dx, dy;
  double len, last_len, next_len;
  double split_prop;
  NumericVector delta (2);

  for(int i = 0; i < iterations; i++) {
    
    ind = sample_edge(seg_len);
    len = seg_len(ind); 
    
    last_x = x(ind);
    last_y = y(ind);
    
    next_ind = lookup(ind);
    next_x = x(next_ind);
    next_y = y(next_ind);
    
    split_prop = R::runif(0, 1);
    dx = next_x - last_x;
    dy = next_y - last_y;
    new_x = last_x + dx * split_prop;
    new_y = last_y + dy * split_prop;
    delta = edge_noise(last_x, last_y, dx, dy, noise);
    new_x = new_x + delta(0);
    new_y = new_y + delta(1);

    last_len = edge_length(last_x, last_y, new_x, new_y);
    next_len = edge_length(new_x, new_y, next_x, next_y);
    
    x.push_back(new_x);
    y.push_back(new_y);
    seg_len.push_back(next_len);
    lookup.push_back(next_ind);
    
    seg_len(ind) = last_len;
    lookup(ind) = lookup.length() - 1;
  }
  
  ind = 0;
  for(int i = 0; i < x.length(); i++) {
    position(ind) = i;  
    ind = lookup(ind);
  }

  DataFrame df = DataFrame::create(
    Named("x") = x, 
    Named("y") = y,
    Named("seg_len") = seg_len,
    Named("position") = position
  );
  return df;
}
