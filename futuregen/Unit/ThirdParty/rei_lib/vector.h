#ifndef REI_VECTOR_H
#define REI_VECTOR_H

//////////////////////////////////////
class vector
{
  private:

    FILE *s1,*s2;
    class scalar *sc1, *sc2, *sc3;
    int i,j,k,ni,nj,nk,nvar,active,allocated;
    char file_name[50];

	// private functions
	void init();
	void alloc();
    void flip_bytes(void* targ, void* src);
    bool write_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream) const;
    bool read_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream);


  public:
    
	// constructors and destructor
    vector();
	vector(char* fn);
	vector(int a,int b,int c);
    vector(scalar *s1,scalar *s2,scalar *s3);
    ~vector();

    // misc
    void get_fname();
    void resize(int a,int b,int c);
    scalar* get_scalar(int i);

	// file i/o member functions
	void read_fun(bool endian_flip_output);
    void read_db_vector(FILE *stream,bool endian_flip);
	void write_fun(const char* name,bool endian_flip_output);
    void write_fun_rev(char* name);
    void read_p3d();
	void write_p3d();

	
	// data access functions
	int size(int direction);
    float func_val(int component,int a,int b,int c);
	void set_func_val(int component,int a,int b,int c,float val);

};

#endif
