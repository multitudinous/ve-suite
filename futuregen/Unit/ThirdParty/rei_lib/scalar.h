#ifndef REI_SCALAR_H
#define REI_SCALAR_H

//////////////////////////////////////
class scalar
{
  private:

    FILE *s1,*s2;
	float ***func;
    int i,j,k,ni,nj,nk,nvar,active;
    char file_name[50];

	// private functions
	void init();
	int alloc();
    void dealloc();
    void convert_1d23d(float *dat);
    void flip_bytes(void* targ, void* src);
    bool read_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream);
    bool write_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream) const;


  public:
    
	// constructors and destructor
    scalar();
    scalar(char* fn);
    scalar(int a,int b,int c);
    scalar(const scalar& p);

    ~scalar();

    // misc
    void copy(const scalar&);
    void get_fname();
    void resize(int a,int b,int c);

	// file i/o member functions
	void read_fun();
    void read_db_scalar(FILE* stream, bool endian_flip);
	bool write_fun(const char* name,bool endian_flip_output);
    bool write_fun(const char* name,int nvars, bool endian_flip);
    bool write_fun_no_header(FILE *fp, bool endian_flip);
    void write_fun_rev(char* name);
    void read_p3d();
	void write_p3d();

	
	// data access functions
	int size(int direction) const;
    float func_val(int a,int b,int c) const;
	void set_func_val(int a,int b,int c,float val);
    void negate();

};

#endif
