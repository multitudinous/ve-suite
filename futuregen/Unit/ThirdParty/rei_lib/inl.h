#ifndef REI_INL_H
#define REI_INL_H

class inl
{
  private:

    FILE *s1,*s2;
    int i,j,k,active,inlet_num;
    char nl,file_name[50],line[100],line1[100],line2[100];
    int *index_i,*index_j,*index_k,*type;
    float *u,*v,*w,*f,*eta,*t;

    // private member functions
    void init();
    void alloc();

  public:
    
	// constructors and destructor
    inl();
	inl(char* fn);
    ~inl();

	// file i/o member functions
	void read_inl();
	void write_inl();
	
	// data access functions
    int num_cells();
	int i_index(int index);
    int j_index(int index);
    int k_index(int index);
    void set_u(int index,float val);
	void set_v(int index,float val);
	void set_w(int index,float val);
    void set_t(int index,float val);
};

#endif
