#include <boost/math/special_functions/gamma.hpp>

extern "C" float polygammaf(int n, float x) {
  return boost::math::polygamma(n, x);
}

extern "C" double polygamma(int n, double x) {
  return boost::math::polygamma(n, x);
}
