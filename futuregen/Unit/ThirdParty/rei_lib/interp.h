#ifndef REI_INTERP_H
#define REI_INTERP_H

//#define epsi                1.0e-6

//#define MAX(a,b)   ((a) > (b) ? (a) : (b)) 

//////////////////////
struct pt_list{
   int i_index,j_index;
   struct pt_list *next;
}; 

//////////////////////
struct plane_data{
    double x;
    double y;
    int i;
    int j;
    int k;
};

//////////////////////
struct src_cell{
   double x[2];        
   double y[2];        
};

////////////////////// 
struct targ_cell{
   int src_i;   
   int src_j;    
   struct pt_list *src_nodes;  //list of rad nodes to use in interpolation 
   struct pt_list *head;       // keeps track of the first rad_nodes structure 
   struct pt_list *prev;       // the previous item in the list
};        

//////////////////////////////////////
class interp
{

  private:

      bool flip_sign;
      char *selected;
      double *distance,rfactor,alpha;
      double x1,y1,x2,y2,dx,dy,delta;
      int i,j,a,b,map[4],numlay,num_interp_nodes;
      int src_index[2],targ_index[2];
      int src_node_num[2],targ_node_num[2];
      int src_low[4],src_high[4];
      int targ_low[4],targ_high[4];
      int src_axis,targ_axis;
	  int src_plane, targ_plane;
      int src_ni;
      int src_nj;
      int targ_ni;
      int targ_nj;
      scalar *src_fun,*targ_fun;
      grid *src_grd, *targ_grd;
      struct plane_data **src_2d;
      struct plane_data **targ_2d;
      struct src_cell **scell;
      struct targ_cell **tcell;
      struct pt_list *current;
    

  public:
    
	// constructors and destructor
    interp();
    void set_inputs(bool flip_sign,grid *src_grd,grid *targ_grd,scalar *src_fun,scalar *targ_fun,FILE *fptr);
    void set_inputs(bool flip_sign,grid *src_grd,grid *targ_grd,scalar *src_fun,scalar *targ_fun,interp *intp);
    ~interp();

    void setup();
    void interpolate();
    int *get_map_array();
    int get_source_axis();
    int get_target_axis();
    int get_targ_plane();
    int* get_targ_low();
    int* get_targ_high();
    void write_interpdat(char *name);

};

#endif
