#ifndef REI_GRID_H
#define REI_GRID_H

// cell types
#define	flow	7
#define	wall	8
#define sym		9

//////////////////////////////////////
class grid
{
  private:

    // class members
    FILE *s1,*s2;
    int ni,nj,nk,plane,active;
    int ***cell_type;
    float *x_coord,*y_coord,*z_coord;
    char file_name[350];
	char decomp_data[150];

	// private functions
	void init();
	void alloc();
    void dealloc();
    bool read_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream);
    bool write_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream) const;

  public:

    // general
    void resize(int a,int b,int c); 
    void get_fname();

	// constructors and destructor
    grid();
	grid(const char* fn);
	grid(int a,int b,int c);
    ~grid();

	// file i/o member functions
	void read_spark_grid();
	void write_spark_grid();
	void read_rei_grid();
	void write_rei_grid();
	void read_xyz_file();
	void write_xyz_file(const char* name, bool endian_flip) const;
    void write_xyz_file_face_centered();
	void read_db_file();
    bool read_db_grid(FILE *stream,int nx,int ny,int nz,bool endian_flip);


	// data access functions
    unsigned int get_offset() const;
    int size(int direction);
	int celltype(int a,int b,int c);
    void set_celltype(int a,int b,int c,int newtype);
	float x(int index);
	float y(int index);
	float z(int index);
    void set_x(int index,float xval);
	void set_y(int index,float yval);
	void set_z(int index,float zval);
};

#endif
