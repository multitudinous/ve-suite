#ifndef WIN32
extern "C"
{
  void histfit_(double *ps, double *pmf, double *passed_areas, int *size,
		double *percent_through_100, double *percent_through_200);
  void createbins_(double *ps, double *pmf, double *passed_areas, int *size,
		double *m1, double *sg);
}
#else
extern "C"
{
  void HISTFIT(double *ps, double *pmf, double *passed_areas, int *size,
		double *percent_through_100, double *percent_through_200);
  void CREATEBINS(double *ps, double *pmf, double *passed_areas, int *size,
		double *m1, double *sg);
}
#endif
