#ifndef REI_CELL_H
#define REI_CELL_H

class grid_cell
{

  private:

   float x_area;
   float y_area;
   float z_area;

  public:

	int type;
	int region_id;
    float xlow;
	float xhigh;
	float ylow;
	float yhigh;
	float zlow;
	float zhigh;
	int index_i;
    int index_j;
    int index_k;
    float u;
    float v;
    float w;
    float t;
    float f;
    float eta;
	char faces[6];
    
	// constructor and destructor
    grid_cell();
    ~grid_cell();

	// member functions
   	void calc_areas();
	float xplane_area();
	float yplane_area();
	float zplane_area();
		 
};

#endif
