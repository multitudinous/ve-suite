#ifndef REI_INLET_H
#define REI_INLET_H

struct region_properties{
	double mix_frac;
	double eta;
	double qratio;
	double uv_x;
	double uv_y;
	double uv_z;
};

//////////////////////////////////////////

class inlet
{

  private:

    FILE *s1;
	int ni,nj,nk;
	int active_num_prim;
	int active_num_sec;
    int pair_cell_num;
    int i,j,k,active_num,region_num_prim,region_num_sec;
    char nl,file_name[50];
    class grid_cell *cell;
    class grid_cell *pair_cell;
	class grid *grd;
	struct region_properties *region_info_prim;
	struct region_properties *region_info_sec;
	double total_prim;
	double total_sec;

    // private member functions
    void init();
	void setup_cells();
	void count_inlets();
	void calc_total_areas();

  public:
    
	// constructors and destructor
    inlet();
	inlet(char* fn);
    ~inlet();

	// member functions
	void write_inl();
	void write_inlet_list();
	void read_regions();
	void calc_vel();
	void calc_region_q();
	void load_f_eta();
	void write_region_areas();
    void read_inl_wtemp();
    
    void load_pairs();
    int active_cell(int ii,int jj,int kk);
    int already_included(int cnt,int ii,int jj,int kk, int* index);
	
	// data access functions
    int num_cells();

};

#endif
