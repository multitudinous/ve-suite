#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "cell_types.h"
#include "grid.h"
#include <cmath>
#include <iostream>
#include "db_file.h"

using namespace std;

///////////////////////////////////////////////
// constructors

grid::grid()
{
    init();
}

///////////////////////////////////////////////
grid::grid(const char* fn)
{
  strcpy(file_name,fn);
  init();
}

///////////////////////////////////////////////
grid::grid(int a,int b,int c)
{
  ni=a;
  nj=b;
  nk=c;
  init();
  alloc();  
  active=1;
}

///////////////////////////////////////////////
void grid::get_fname()
{
    
  // get info from user
  cout<<"Enter the name of the file..."<<endl;
  //gets(file_name);
  init();
}

///////////////////////////////////////////////
void grid::resize(int a,int b,int c)
{
  dealloc();
  ni=a;
  nj=b;
  nk=c;
  init();
  alloc();  
  active=1;
}

///////////////////////////////////////////////
void grid::init()
{
  // initialize decomp_data string
  decomp_data[0]='\0';

  // initialize file pointer and active flag
  active = 0;

  x_coord=y_coord=z_coord=NULL;
  cell_type=NULL;
}

///////////////////////////////////////////////
// destructor

grid::~grid()
{
    dealloc();
} 

///////////////////////////////////////////////
void grid::alloc()
{

   int i,j;

  // allocate memory
  (x_coord)=new float[ni];
  (y_coord)=new float[nj];
  (z_coord)=new float[nk];

  (cell_type)=(int***)malloc(ni*sizeof(int**));
  if(!cell_type)cout<<"grid::alloc(): Memory allocation error"<<endl;
  for(i=0;i<ni;i++){
     cell_type[i]=(int**)malloc(nj*sizeof(int*));
     if(!cell_type[i])cout<<"grid::alloc(): Memory allocation error"<<endl;
     for(j=0;j<nj;j++){
       cell_type[i][j]=(int*)malloc(nk*sizeof(int));
       if(!cell_type[i][j])cout<<"grid::alloc(): Memory allocation error"<<endl;
     }
  }

}

///////////////////////////////////////////////
void grid::dealloc()
{

   int i,j;

   // deallocate memory
   if(x_coord)delete [] x_coord;
   if(y_coord)delete [] y_coord;
   if(z_coord)delete [] z_coord;

   if(cell_type!=NULL){
     for(i=0;i<ni;i++){
       for(j=0;j<nj;j++)free(cell_type[i][j]);
 	   free(cell_type[i]);
	 }
	 free(cell_type);
   }
   x_coord=y_coord=z_coord=NULL;
   cell_type=NULL;
}

///////////////////////////////////////////////
void grid::read_spark_grid()
{

  int i,j,k;

  // check to see if we have already read in grid info
  if(active){printf("grid::read_spark_grid(): Object is already loaded...\n"); return;}
  else active=1;

  // open file
  if((s1=fopen(file_name,"rt"))==NULL){
     fprintf(stderr, "grid::read_spark_grid(): Fail to open %s\n", "grid file");
     return;
  }

  // read dimensions
  fscanf(s1,"%d %d %d",&ni,&nj,&nk);

  // allocate memory
  alloc();

  // read x's, y's and z's
  for(i=0;i<ni;i++)fscanf(s1,"%f",x_coord+i);
  for(i=0;i<nj;i++)fscanf(s1,"%f",y_coord+i);
  for(i=0;i<nk;i++)fscanf(s1,"%f",z_coord+i);

  // read cell type information 
  int ii,llow,lhigh;

  for(i=0;i<ni;i++){
    fscanf(s1,"%d",&plane);    
    for(ii=0;(ii==0||ii<=(nj/50));ii++){
       llow=ii*50; lhigh=(((llow+50)<nj)?(llow+50):nj);
       for(k=0;k<nk;k++)
         for(j=llow;j<lhigh;j++)fscanf(s1,"%d",&(cell_type[i][j][k]));
    }
  }
  fclose(s1);
  s1=NULL;

}

////////////////////////////////////////////////////////////
void grid::write_spark_grid()
{

  int i,j,k;

  // open grid file
  if((s2=fopen("new_grid.grd","wt"))==NULL){
     fprintf(stderr, "grid::write_spark_grid(): Fail to open %s\n", "grid file");
     return;
  }

  // write out new grid file
  fprintf(s2,"\n\n\n %d %d %d\n",ni,nj,nk);
  fprintf(s2," ");
  for(i=0;i<ni;i++)fprintf(s2,"%12.6e ",x_coord[i]);
  fprintf(s2,"\n");
  fprintf(s2," ");
  for(i=0;i<nj;i++)fprintf(s2,"%12.6e ",y_coord[i]);
  fprintf(s2,"\n");
  fprintf(s2," ");
  for(i=0;i<nk;i++)fprintf(s2,"%12.6e ",z_coord[i]);
  fprintf(s2,"\n");

  // write cell type information 
  int ii,llow,lhigh;

  for(i=0;i<ni;i++){
    fprintf(s2,"%d\n",i+1);    
    for(ii=0;(ii==0||ii<=(nj/50));ii++){
       llow=ii*50; lhigh=(((llow+50)<nj)?(llow+50):nj);
       for(k=0;k<nk;k++){
         for(j=llow;j<lhigh;j++)fprintf(s2,"%d ",(cell_type[i][j][k]));
         fprintf(s2,"\n");
       }
    }
  }

  fclose(s2);
  s2=NULL;
}

//////////////////////////////////////////////////
void grid::read_rei_grid()
{
  int i,j,k;

  char nl, buf[200];

  // check to see if we have already read in grid info
  if(active){printf("grid::read_rei_grid(): Object is already loaded...\n"); return;}
  else active=1;

  // open file
  if((s1=fopen(file_name,"rt"))==NULL){
     fprintf(stderr, "grid::read_rei_grid(): Fail to open %s grid file\n", file_name);
     return;
  }

  // read 3 header lines and domain decomposition data - then grid data
  fscanf(s1,"%[^\n]",buf);  fscanf(s1,"%c",&nl);   // this reads a complete text line
  fscanf(s1,"%[^\n]",buf);  fscanf(s1,"%c",&nl);   // this reads a complete text line
  fscanf(s1,"%[^\n]",buf);  fscanf(s1,"%c",&nl);   // this reads a complete text line
  fscanf(s1,"%[^\n]",decomp_data);
  
  // read dimensions
  fscanf(s1,"%d %d %d",&ni,&nj,&nk);

  // allocate memory
  alloc();
  
  // read x's, y's and z's
  for(i=0;i<ni;i++)fscanf(s1,"%f",x_coord+i);
  for(i=0;i<nj;i++)fscanf(s1,"%f",y_coord+i);
  for(i=0;i<nk;i++)fscanf(s1,"%f",z_coord+i);

  // read cell type information 
  int ii,llow,lhigh;

  for(i=0;i<ni;i++){
    fscanf(s1,"%d",&plane);    
    for(ii=0;(ii==0||ii<=(nj/50));ii++){
       llow=ii*50; lhigh=(((llow+50)<nj)?(llow+50):nj);
       for(k=0;k<nk;k++)
         for(j=llow;j<lhigh;j++)fscanf(s1,"%d",&(cell_type[i][j][k]));
    }
  }
  fclose(s1);
  s1=NULL;
}

//////////////////////////////////////////////////
void grid::write_rei_grid()
{

  // open file
  if((s2=fopen("new_grid.grd","wt"))==NULL){
     fprintf(stderr, "grid::write_rei_grid(): Fail to open %s\n", "grid file");
     return;
  }

  // write 2 header lines and domain decomposition data - then grid data
  fprintf(s2,"%s\n","REI grid"); 
  fprintf(s2,"%s\n","Blank Line"); 
  if(strlen(decomp_data)==0)fprintf(s2,"%s\n","  1 1 0 0 0 0 0 0");
  else fprintf(s2,"%s\n",decomp_data);
  write_spark_grid();

}

//////////////////////////////////////////////////
void grid::read_xyz_file()
{

  int type;

  // check to see if we have already read in grid info
  if(active){printf("grid::read_xyz_file: Object is already loaded...\n"); return;}
  else active=1;

  // open xyz file
  if((s1=fopen(file_name,"rb"))==NULL){
     fprintf(stderr, "grid::read_xyz_file: Fail to open %s\n", "xyz file");
     return;
  }
 
  // read in ni,nj,nk
  fread(&ni,sizeof(int),1,s1);
  fread(&nj,sizeof(int),1,s1);
  fread(&nk,sizeof(int),1,s1);

  // allocate memory
  alloc();
  
  int i,j,k;

  // read in coordinate data
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	    fread(&(x_coord[i]),sizeof(float),1,s1);

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	    fread(&(y_coord[j]),sizeof(float),1,s1);

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	    fread(&(z_coord[k]),sizeof(float),1,s1);
  
  /* read iblank data */
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
	    fread(&type,sizeof(int),1,s1);
		cell_type[i][j][k]=(type==0)?wall:flow;
      }

  // clean up
  fclose(s1);
  s1=NULL;

}

////////////////////////////////////////////////////
void grid::write_xyz_file(const char *name, bool endian_flip) const // this is a binary FAST geom. file
{

  int type;
  FILE *stream;

  // open file
  if(strlen(name)==0){
     if((stream=fopen("new_grid.xyz","wb"))==NULL){
        fprintf(stderr, "grid::write_xyz_file: Fail to open %s\n", "xyz file");
        return;
     }
  }
  else{
     if((stream=fopen(name,"wb"))==NULL){
        fprintf(stderr, "grid::write_xyz_file: Fail to open %s\n", "xyz file");
        return;
     }
  }

  // write out ni,nj,nk
  write_file((void*)&ni,1,endian_flip,stream);
  write_file((void*)&nj,1,endian_flip,stream);
  write_file((void*)&nk,1,endian_flip,stream);
  
  int i,j,k;

  // write coordinate data
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	    write_file((void*)&(x_coord[i]),1,endian_flip,stream);

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	    write_file((void*)&(y_coord[j]),1,endian_flip,stream);

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	    write_file((void*)&(z_coord[k]),1,endian_flip,stream);
  
  // write iblank data 
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
        type=(cell_type[i][j][k]==wall)?0:1;
        write_file((void*)&type,1,endian_flip,stream);
      }

  // clean up
  fclose(stream);	    
}

////////////////////////////////////////////////////
void grid::write_xyz_file_face_centered()  // this is a binary FAST geom. file
{

  int i,j,k,type;

  // open grid file
  if((s2=fopen("new_grid.xyz","wb"))==NULL){
     fprintf(stderr, "grid::write_xyz_file_fc(): Fail to open %s\n", "xyz file");
     return;
  }
 
  float loc;
  int ni_tmp=ni+1;
  int nj_tmp=nj+1;
  int nk_tmp=nk+1;

  // write out ni,nj,nk
  fwrite(&ni_tmp,sizeof(int),1,s2);
  fwrite(&nj_tmp,sizeof(int),1,s2);
  fwrite(&nk_tmp,sizeof(int),1,s2);
  
  // write coordinate data
  for(k=0;k<=nk;k++)
    for(j=0;j<=nj;j++)
       for(i=0;i<=ni;i++){
          if(i==0){
            loc=x_coord[i]-(x_coord[i+1]-x_coord[i])/2.0f;
            fwrite(&loc,sizeof(float),1,s2);
          }
          else if(i<(ni)&&i!=0){
            loc=x_coord[i-1]+(x_coord[i]-x_coord[i-1])/2.0f;
	    fwrite(&loc,sizeof(float),1,s2);
          }
          if(i==(ni)){
            loc=x_coord[i-1]+(x_coord[i-1]-x_coord[i-2])/2.0f;
	    fwrite(&loc,sizeof(float),1,s2);
          }
       }

  for(k=0;k<=nk;k++)
    for(j=0;j<=nj;j++)
      for(i=0;i<=ni;i++){
	  if(j==0){
            loc=y_coord[j]-(y_coord[j+1]-y_coord[j])/2.0f;
            fwrite(&loc,sizeof(float),1,s2);
          }
          else if(j<(nj)&&j!=0){
            loc=y_coord[j-1]+(y_coord[j]-y_coord[j-1])/2.0f;
	    fwrite(&loc,sizeof(float),1,s2);
          }
          if(j==(nj)){
            loc=y_coord[j-1]+(y_coord[j-1]-y_coord[j-2])/2.0f;
	    fwrite(&loc,sizeof(float),1,s2);
          }
      }

  for(k=0;k<=nk;k++)
    for(j=0;j<=nj;j++)
      for(i=0;i<=ni;i++){
	 if(k==0){
            loc=z_coord[k]-(z_coord[k+1]-z_coord[k])/2.0f;
            fwrite(&loc,sizeof(float),1,s2);
          }
          else if(k<(nk)&&k!=0){
            loc=z_coord[k-1]+(z_coord[k]-z_coord[k-1])/2.0f;
	    fwrite(&loc,sizeof(float),1,s2);
          }
          if(k==(nk)){
            loc=z_coord[k-1]+(z_coord[k-1]-z_coord[k-2])/2.0f;
	    fwrite(&loc,sizeof(float),1,s2);
          }
      }
  
  // write iblank data 
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
        type=(cell_type[i][j][k]==wall)?0:1;
	    fwrite(&type,sizeof(int),1,s2);
      }

  // clean up
  fclose(s2);
  s2=NULL;
	    
}

////////////////////////////////////////////////
void grid::read_db_file()
{

  int i,j,k;

  // check to see if we have already read in grid info
  if(active){printf("grid::read_db_file(): Object is already loaded...\n"); return;}
  else active=1;

  int ndim,ns,nv;
  char sc[20],text[100];

  // open file
  if((s1=fopen(file_name,"rb"))==NULL){
     fprintf(stderr, "grid::read_db_file(): Fail to open %s\n", "db file");
     return;
  }
  
  // read in 3 info lines
  fread(sc, sizeof(char), 4, s1);
  fread(text, sizeof(*text), 80, s1);
  fread(sc, sizeof(char), 8, s1);
  fread(text, sizeof(*text), 80, s1);
  fread(sc, sizeof(char), 8, s1);
  fread(text, sizeof(*text), 80, s1);

  // read in number of dimensions
  fread(sc, sizeof(char), 8, s1);
  fread(&ndim, sizeof(int), 1, s1);
  
  // read in ni,nj,nk,ns,nv
  fread(sc, sizeof(char), 8, s1);
  fread(&ni, sizeof(int), 1, s1);
  fread(&nj, sizeof(int), 1, s1);
  fread(&nk, sizeof(int), 1, s1);
 
  fread(sc,sizeof(char),8,s1);
  fread(&ns,sizeof(int),1,s1);
  fread(&nv,sizeof(int),1,s1);
  
  // read scalar function names
  fread(sc,sizeof(char),8,s1);
  for(i=0;i<ns;i++)fread(text, sizeof(char), 8, s1);
  
  // read in vector function names
  fread(sc,sizeof(char),8,s1);
  for(i=0;i<nv;i++)fread(text,sizeof(char),8,s1);
  
  // allocate memory
  alloc();

  // read in x,y,z data 
  fread(sc, sizeof(char), 8, s1);
  fread(x_coord,sizeof(float),ni,s1);

  fread(sc, sizeof(char), 8, s1);
  fread(y_coord,sizeof(float),nj,s1);

  fread(sc,sizeof(char),8,s1);
  fread(z_coord,sizeof(float),nk,s1);
  
  // read in iblank data 
  long int dstart=ftell(s1);

  fseek(s1,dstart+8,SEEK_SET);
  float* giblank=(float*)malloc((ni*nj*nk)*sizeof(float));
  fread(giblank,sizeof(float),ni*nj*nk,s1);

  int cntr=0;
  for(k=0;k<nk;k++) 
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
	    cell_type[i][j][k]=(giblank[cntr]==8.0)?wall:flow;
        cntr++;
	  }

  // clean up
  free(giblank);
  fclose(s1);
  s1=NULL;

}

////////////////////////////////////////////////
bool grid::read_db_grid(FILE *stream,int nx,int ny,int nz,bool endian_flip)
{

  // check to see if we already allocated memory
  if(active)dealloc();
  else active=1;

  // set grid dims
  ni=nx;
  nj=ny;
  nk=nz;
  
  // allocate memory
  alloc();

  // read in x,y,z data 
  read_file((void*)x_coord, ni, endian_flip, stream);
  fseek(stream,8L,SEEK_CUR);
  read_file((void*)y_coord, nj, endian_flip, stream);
  fseek(stream,8L,SEEK_CUR);
  read_file((void*)z_coord, nk, endian_flip, stream);

  // read iblank data
  fseek(stream,8L,SEEK_CUR);
  float* giblank=(float*)malloc((ni*nj*nk)*sizeof(float));
  read_file((void*)giblank, ni*nj*nk, endian_flip, stream);

  int cnt=0;
  for(int k=0;k<nk;k++) 
    for(int j=0;j<nj;j++)
      for(int i=0;i<ni;i++){
	    cell_type[i][j][k]=(int)(giblank[cnt]);
        cnt++;
	  }

  // clean up
  free(giblank);
  return(true);
}

////////////////////////////////////////////////
bool grid::read_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream)
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
bool grid::write_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream) const
{

    char buf[4];    

    // num is the number of 4 byte blocks to be read
   
    // do we need to endian flip????
    if(endian_flip){
        int *tmp = new int[num];
        for(unsigned int i=0;i<num*4;i+=4){
            for(int j=0;j<4;j++) buf[3-j]=*(((char*)ptr)+i+j);  
            memcpy(((char*)tmp)+i,buf,4);
        }        
        if(fwrite(tmp,4,num,stream)!=num)return(false);
        delete tmp;
    }
    else if(fwrite(ptr,4,num,stream)!=num)return(false);
    
    return(true);
}

////////////////////////////////////////////////
int grid::celltype(int a,int b,int c)
{
  return(cell_type[a][b][c]);
}

////////////////////////////////////////////////
void grid::set_celltype(int a,int b,int c,int newtype)
{
  cell_type[a][b][c]=newtype;
}

/////////////////////////////////////////////////
float grid::x(int index)
{
  return(x_coord[index]);
}

//////////////////////////////////////////////////
float grid::y(int index)
{
  return(y_coord[index]);
}

//////////////////////////////////////////////////
float grid::z(int index)
{
  return(z_coord[index]);
}

//////////////////////////////////////////////////
void grid::set_x(int index,float xval)
{
  x_coord[index]=xval;
}

//////////////////////////////////////////////////
void grid::set_y(int index,float yval)
{
  y_coord[index]=yval;
}

//////////////////////////////////////////////////
void grid::set_z(int index,float zval)
{
  z_coord[index]=zval;
}

//////////////////////////////////////////////////
int grid::size(int direction)
{
  
  if(direction==0)return(ni);
  else if(direction==1)return(nj);
  else if(direction==2)return(nk);
  else return(-1);

}

///////////////////////////////////////////////////
unsigned int grid::get_offset() const
{
    // fseek from end of header (db_file::header_offset) to this location
    // and we are at the beginning  of the first scalar function which is iblank data (walls)!!
    return((ni+nj+nk)*sizeof(float)+24);
}

//////////////////////////////////////////////////
/*
void grid::make_cartesian(float ***x,float ***y,float ***z,
                          int cyl_orig_loc_x,int cyl_orig_loc_y,int cyl_orig_loc_z)
{
    
    // this routine is hardwired to assume that z (cyl) becomes x (cart)
    // this routine only makes sense when this grid is cylinderical

   	// this routine converts the cylinderical node pts to
	// cartesian, moves the origin of this new cart. system

	// first generate cartesian pts from cylinderical pts
	for(i=0;i<ni;i++)
	  for(j=0;j<nj;j++)
	    for(k=0;k<nk;k++){
		  x[i][j][k] = x_coord[i] + cyl_orig_loc_x; 
          y[i][j][k] = y_coord[j]*sin(z_coord[k]) + cyl_orig_loc_y;
		  z[i][j][k] = y_coord[j]*cos(z_coord[k]) + cyl_orig_loc_z;
	    }

}

*/
