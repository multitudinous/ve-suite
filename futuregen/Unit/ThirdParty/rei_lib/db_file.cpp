#include <stdlib.h>
#include "db_file.h"
#include <assert.h>
#include <iostream>

using namespace std;

// default constructor
db_file::db_file()
:machine(get_machine_type()),stream(NULL),error(false),db_grid(NULL),valid(false)
{}

////////////////////////////////////////////////
db_file::~db_file()
{
    if(stream)fclose(stream);
    if(db_grid)delete db_grid;
}

////////////////////////////////////////////////
bool db_file::set_path(const char* fully_qual_path)
{
    // did we already have a db file loaded?
    if(stream){
        fclose(stream);
        stream=NULL;
    }
    if(db_grid){
        delete db_grid;
        db_grid=NULL;
    }
   
    // set path
    strcpy(path,fully_qual_path);

    // try to open db file
    if((stream=fopen(path,"rb"))==NULL)return(false);

    // find format of db (little/big endian)
    if(!find_format())return(false); 

    // now read header information out of file
    if(!header_lines())return(false);

    valid=true;
    
    return(true);
}

////////////////////////////////////////////////
bool db_file::find_format()
{    

    // check to be sure this isn't a powerstation file
    unsigned char bof_byte;
    if(fread(&bof_byte, sizeof(char), 1, stream)!=1)return(false);
    if(bof_byte==0x4B){
        fformat = pstation;
        return(false);
    }

    fseek(stream,0L,SEEK_SET);

    // read beginning word of file
    unsigned int bof;
    if(fread(&bof, sizeof(char), 4, stream)!=4)return(false);

    // check to see if file is big or little endian
    endian_flip = (bof>10000)?true:false;
    if(machine==big)   fformat = (endian_flip)?lesgi:besgi;
    if(machine==little)fformat = (endian_flip)?besgi:lesgi;

    return(true);
}

////////////////////////////////////////////////
bool db_file::header_lines()
{   

    // read first line
    fseek(stream,4L,SEEK_SET);
    fread(header1, sizeof(char), 80, stream);

    // second line
    fseek(stream,8L,SEEK_CUR);
    fread(header2, sizeof(char), 80, stream);

    // third line
    fseek(stream,8L,SEEK_CUR);
    fread(header3, sizeof(char), 80, stream);

     // read dimensions
    int ndim;
    fseek(stream,8L,SEEK_CUR);
    read_file((void*)(&ndim), 1);
    assert(ndim==3);
 
    fseek(stream,8L,SEEK_CUR);
    read_file((void*)(&nx), 1);
    read_file((void*)(&ny), 1);
    read_file((void*)(&nz), 1);

    fseek(stream,8L,SEEK_CUR);
    read_file((void*)(&ns), 1);
    read_file((void*)(&nv), 1);

    // set the offset to the grid data (header offset)
    header_offset = 0x14C + 0x8*(ns+nv);

    // determine the grid type
    if(strncmp(header2,"cartesian_rectangular",21)==0)             grdtype = cart;     
    else if(strncmp(header2,"body_fitted_structured_grid",27)==0)  grdtype = bf;           
    else if(strncmp(header2,"cartesian_cylindrical",21)==0)        grdtype = cyl;   
    else grdtype = none;

    // read scalar and vector names
    fseek(stream,8L,SEEK_CUR);
     
    // init func names (NULL terminate) and read names
    int i;
    char nname[9];
    nname[8] = '\0';
    for(i=0;i<100;i++)names[i][8]='\0';
    for (i=0;i<ns;i++) {
      fread(nname,sizeof(char),8,stream);
      sscanf(nname, "%s", names[i]); //# hack
    }
    fseek(stream,8L,SEEK_CUR);
    for (i=ns;i<ns+nv;i++)fread(names[i],sizeof(char),8,stream);

    // read grid info
    if(grdtype==cart)db_grid=new grid();
    if(grdtype==bf)  db_grid=new grid();   // FIX THESE!!!!!!!!!!!!!!!!!
    if(grdtype==cyl) db_grid=new grid();
    if(grdtype==none)db_grid=new grid();

    // read the grid
    fseek(stream,8L,SEEK_CUR);
    db_grid->read_db_grid(stream,nx,ny,nz,endian_flip);

    // resize the scalar and vector objects
    cur_scalar.resize(nx,ny,nz);
    cur_vector.resize(nx,ny,nz);

    return(true);
}

////////////////////////////////////////////////
const grid& db_file::get_grid() const
{
    return(*db_grid);
}

////////////////////////////////////////////////
scalar& db_file::get_scalar(const char* func_name,bool* error)
{
  function_type func_type;

    // load func_name and see if it's really a scalar
    if(!load_function(func_name,&func_type))*error = true;
    else {
        if(func_type==SCALAR) *error = false;
        else                  *error = true;
    }
    return(cur_scalar);    
}

////////////////////////////////////////////////
vector& db_file::get_vector(const char* func_name,bool* error)
{
    function_type func_type;

    // load func_name and see if it's really a vector
    if(!load_function(func_name,&func_type))*error = true;
    else {
        if(func_type==VECTOR) *error = false;
        else                  *error = true;
    }
    return(cur_vector);    
}

////////////////////////////////////////////////
bool db_file::write_function(const char* func_name, const char* file_name, bool endian_flip_output)
{

    function_type func_type;

    // load and write object    
    if(load_function(func_name,&func_type)){
        if(func_type==SCALAR)  cur_scalar.write_fun(file_name,endian_flip_output);       
        else                   cur_vector.write_fun(file_name,endian_flip_output);     
    }
    else return(false);

    return(true);
}

////////////////////////////////////////////////
bool db_file::load_function(const char* func_name, function_type* func_type)
{
  int index;
    unsigned int shift;
    bool found=false,scalarf;

    // find index of function
    for(int i=0;i<ns+nv;i++){
      if(strcmp(func_name,names[i])==0){ // was strncmp ,8
	  found   = true;
            scalarf = (i<ns);
            index=i;
            break;
        }
    }

    // now read the object    
    if(found){
        if(scalarf){
            shift = header_offset + db_grid->get_offset() + index*(nx*ny*nz*sizeof(float)+8);
            fseek(stream,shift,SEEK_SET); 
            cur_scalar.read_db_scalar(stream,endian_flip);            
        }
        else{
            shift = header_offset + db_grid->get_offset() + ns*(nx*ny*nz*sizeof(float)+8)
                    + (index-ns)*(3*(nx*ny*nz*sizeof(float)+8));
            fseek(stream,shift,SEEK_SET);
            cur_vector.read_db_vector(stream,endian_flip);
        }       
        *func_type = (scalarf)?SCALAR:VECTOR;
        return(true);
    }
    else return(false);
}

////////////////////////////////////////////////
bool db_file::read_file(void *ptr, unsigned int num)
{

    char buf[4];

    // num is the number of 4 byte blocks to be read
    if(fread(ptr,4,num,stream)!=num)return(false);

    // do we need to endian flip????
    if(endian_flip){
        for(unsigned int i=0;i<num*4;i+=4){
            for(int j=0;j<4;j++) buf[3-j]=*(((char*)ptr)+i+j);  
            memcpy(((char*)ptr)+i,buf,4);
        }                   
    }
    return(true);
}

////////////////////////////////////////////////
void db_file::get_dims(int *dms) const
{
    // return dimensions
    dms[0]=nx;
    dms[1]=ny;
    dms[2]=nz;
}

////////////////////////////////////////////////
int db_file::get_num_scalar() const
{
    return(ns);
}

////////////////////////////////////////////////
int db_file::get_num_vector() const
{
    return(nv);
}

////////////////////////////////////////////////
const char* db_file::get_name(int index) const
{
    return(names[index]);
}

////////////////////////////////////////////////
unsigned int db_file::get_header_offset() const
{
    // fseek db from beginning of file to this point and you are pointing at grid information
    return(header_offset);
}
