
// implementation file for the grid_cell class

#include "cell.h"
#include <cmath>

grid_cell::grid_cell(){};

grid_cell::~grid_cell(){};

void grid_cell::calc_areas()
{
	x_area=(float)(fabs(yhigh-ylow)*fabs(zhigh-zlow));
	y_area=(float)(fabs(xhigh-xlow)*fabs(zhigh-zlow));
	z_area=(float)(fabs(yhigh-ylow)*fabs(xhigh-xlow));
}

float grid_cell::xplane_area()
{
	return(x_area);
}

float grid_cell::yplane_area()
{
	return(y_area);
}

float grid_cell::zplane_area()
{
	return(z_area);
}
