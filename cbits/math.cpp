#include <algorithm>
#include <boost/math/special_functions/gamma.hpp>

extern "C" float polygammaf(int n, float x) {
  return boost::math::polygamma(n, x);
}

extern "C" double polygamma(int n, double x) {
  return boost::math::polygamma(n, x);
}

extern "C" void fsort(float *x, int n) { std::sort(x, x + n); }

extern "C" void dsort(double *x, int n) { std::sort(x, x + n); }
