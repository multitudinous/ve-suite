
// class definition for db_file class

#ifndef _db_file_h_
#define _db_file_h_

#include "string.h"
#include <stdio.h>
#include "grid.h"
#include "scalar.h"
#include "vector.h"

class db_file
{

public:

    enum function_type { SCALAR=0, VECTOR=1 };
    enum dbformat   { lesgi  = 0,  besgi = 1, pstation = 2            };
    enum order      { little = 0,  big   = 1                          };
    enum grid_types { cart   = 0,  bf    = 1, cyl      = 2, none = 3  };

    // static members

    // fseek db from beginning of file to this point and you are pointing at grid information
    unsigned int header_offset; 

    // mutators
    db_file();
    ~db_file();
    bool set_path(const char* fully_qual_path);
    bool write_function(const char* func_name, const char* file_name, bool endian_flip_output);
    bool load_function(const char* func_name, function_type* func_type);
    
    
    // accessors
    void dims();
    const grid& get_grid() const;
    scalar& get_scalar(const char* func_name,bool* error);
    vector& get_vector(const char* func_name,bool* error);
    bool isvalid() const;
    db_file::dbformat get_format() const;
    db_file::order get_machine_type() const;
    db_file::grid_types get_grid_type() const;
    void get_dims(int *arr) const;
    int get_num_scalar() const;
    int get_num_vector() const;
    const char* get_name(int index) const;
    unsigned int get_header_offset() const;
    
   grid*                db_grid;      // db file's grid
 
private:
    char                 path[200];    // fully qualified path of db file
    char                 header1[85];  // db header text lines
    char                 header2[85];  // db header text lines
    char                 header3[85];  // db header text lines 
    char                 names[100][9];// names of scalar and vector functions
    dbformat             fformat;
    order                machine;
    grid_types           grdtype;
    unsigned int         fptr_loc;
    FILE*                stream;
    bool                 endian_flip;
    bool                 error;
    int                  nx,ny,nz;
    int                  ns;           // number of scalars
    int                  nv;           // number of vectors
    bool                 valid;        // is this object valid?
    scalar               cur_scalar;   // current scalar object
    vector               cur_vector;   // current vector object
        
private:        
    bool find_format();
    bool header_lines();
    bool read_file(void*, unsigned int cnt);
   
};

////////////////////////////////////////////////
inline
db_file::dbformat db_file::get_format() const
{
    return (dbformat)(fformat);
}

////////////////////////////////////////////////
inline
db_file::grid_types db_file::get_grid_type() const
{
    return (grid_types)(grdtype);
}

////////////////////////////////////////////////
inline
db_file::order db_file::get_machine_type() const
{
    // check to see if this machine is big or little
    unsigned int test = 1;
    return((db_file::order)((int)(*(((char*)(&test))+3))));
}

////////////////////////////////////////////////
inline
bool db_file::isvalid() const
{
    return(valid);
}

#endif
