#include <boost/math/special_functions/gamma.hpp>
#include <cmath>
#include <fftw3.h>

extern "C" float polygammaf(int n, float x) {
  return boost::math::polygamma(n, x);
}

extern "C" double polygamma(int n, double x) {
  return boost::math::polygamma(n, x);
}

extern "C" void acfD(double *x, double *p, int n) {
  double sum = 0;
  for (int i = 0; i < n; i++)
    sum += x[i] * x[i];
  const double norm = std::sqrt(sum);
  for (int i = 0; i < n; i++)
    x[i] /= norm;

  fftw_complex *tmp =
      static_cast<fftw_complex *>(fftw_malloc(2 * sizeof(double) * n));
  fftw_plan plan = fftw_plan_dft_r2c_1d(n, x, tmp, FFTW_ESTIMATE);
  fftw_execute(plan);

  for (int i = 0; i < n; i++) {
    double x = tmp[i][0];
    double y = tmp[i][1];
    tmp[i][0] = x * x + y * y;
    tmp[i][1] = 0;
  }

  fftw_plan plan2 =
      fftw_plan_dft_c2r_1d(n, tmp, p, FFTW_ESTIMATE | FFTW_BACKWARD);
  fftw_execute(plan2);

  for (int i = 0; i < n; i++)
    p[i] = p[i] / n;

  fftw_destroy_plan(plan);
  fftw_destroy_plan(plan2);
  fftw_free(tmp);
}

extern "C" void acfF(float *x, float *p, int n) {
  double sum = 0;
  for (int i = 0; i < n; i++)
    sum += x[i] * x[i];
  const double norm = std::sqrt(sum);
  for (int i = 0; i < n; i++)
    x[i] /= norm;

  fftwf_complex *tmp =
      static_cast<fftwf_complex *>(fftw_malloc(2 * sizeof(float) * n));
  fftwf_plan plan = fftwf_plan_dft_r2c_1d(n, x, tmp, FFTW_ESTIMATE);
  fftwf_execute(plan);

  for (int i = 0; i < n; i++) {
    double x = tmp[i][0];
    double y = tmp[i][1];
    tmp[i][0] = x * x + y * y;
    tmp[i][1] = 0;
  }

  fftwf_plan plan2 =
      fftwf_plan_dft_c2r_1d(n, tmp, p, FFTW_ESTIMATE | FFTW_BACKWARD);
  fftwf_execute(plan2);

  for (int i = 0; i < n; i++)
    p[i] = p[i] / n;

  fftwf_destroy_plan(plan);
  fftwf_destroy_plan(plan2);
  fftw_free(tmp);
}
