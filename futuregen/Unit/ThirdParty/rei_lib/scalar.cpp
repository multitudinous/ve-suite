#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "scalar.h"
#include <cmath>
#include <iostream>

using namespace std;

///////////////////////////////////////////////
// constructors

scalar::scalar()
{ 
  init();
}

///////////////////////////////////////////////
scalar::scalar(char* fn)
{
  strcpy(file_name,fn);
  init();
}

///////////////////////////////////////////////
scalar::scalar(int a,int b,int c)
{
  
  ni=a;
  nj=b;
  nk=c;
  nvar=1;
  init();
  alloc();  
  active=1;

  for(i=0;i<ni;i++)
    for(j=0;j<nj;j++)
      for(k=0;k<nk;k++)
        func[i][j][k]=0.0;

}

///////////////////////////////////////////////
scalar::scalar(const scalar& p)
{
  init();
  copy(p);
}

///////////////////////////////////////////////
void scalar::get_fname()
{
  // get info from user
  cout<<"Enter the name of the file..."<<endl;
  gets(file_name);
  init();
}

///////////////////////////////////////////////
void scalar::init()
{

  // initialize file pointers/array and active flag
  s1=s2=NULL;
  func=NULL;
  active = 0;

}

///////////////////////////////////////////////
// destructor

scalar::~scalar()
{
   dealloc();
} 

///////////////////////////////////////////////
void scalar::copy(const scalar& p) {
  if(this == &p) return;
  
  dealloc();
  ni = p.size(0);
  nj = p.size(1);
  nk = p.size(2);
  nvar = 1;

  // p.file_name --> needs accessor

  init();
  alloc();  
  active = 1;

  for(i=0;i<ni;i++)
    for(j=0;j<nj;j++)
      for(k=0;k<nk;k++)
        func[i][j][k] = p.func_val(i, j, k);
}

///////////////////////////////////////////////
void scalar::resize(int a,int b,int c)
{
  
  dealloc();
  ni=a;
  nj=b;
  nk=c;
  nvar=1;
  init();
  alloc();  
  active=1;

  for(i=0;i<ni;i++)
    for(j=0;j<nj;j++)
      for(k=0;k<nk;k++)
        func[i][j][k]=0.0;

}

///////////////////////////////////////////////
int scalar::alloc()
{
  // allocate memory
  (func)=(float***)malloc(ni*sizeof(float**));
  if(!func) {
    cout << "Memory allocation error\n";
    return 1;
  }
  for(i=0;i<ni;i++){
     func[i]=(float**)malloc(nj*sizeof(float*));
     if(!func[i]) {
       cout << "Memory allocation error\n";
       return 1;
     }
     for(j=0;j<nj;j++){
       func[i][j]=(float*)malloc(nk*sizeof(float));
       if(!func[i][j]) {
	 cout << "Memory allocation error\n";
	 return 1;
       }
     }
  }
  
  return 0;
}

///////////////////////////////////////////////
void scalar::dealloc()
{
   // deallocate memory
  if(func!=NULL){
    for(i=0;i<ni;i++){
      if(func[i]!=NULL) {
	for(j=0;j<nj;j++)
	  if(func[i][j]!=NULL)
	    free(func[i][j]);
	free(func[i]);
      }
    }
    free(func);
  }
}

///////////////////////////////////////////////
void scalar::read_fun()
{

  // check to see if we have already read in scalar info
  if(active){printf("Object is already loaded...\n"); return;}
  else active=1;

  // open file
  if((s1=fopen(file_name,"rb"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "fun file");
     return;
  }
  
  // read dimensions
  fread(&ni, sizeof(int),1,s1);
  fread(&nj, sizeof(int),1,s1);
  fread(&nk, sizeof(int),1,s1);
  fread(&nvar, sizeof(int),1,s1);
  
  // allocate memory
  alloc();

  // read function data
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	      fread(&(func[i][j][k]),sizeof(float),1,s1);

  fclose(s1);
  s1=NULL;
	 	 
}

////////////////////////////////////////////////////////////
void scalar::read_db_scalar(FILE* stream, bool endian_flip)
{
    // allocate temporary array
    float *data = new float[ni*nj*nk];

    // read data
    read_file((void*)data,ni*nj*nk,endian_flip,stream);
    
    // load data into 3d array
    convert_1d23d(data);

    // deallocate
    delete data;
}

////////////////////////////////////////////////////////////
bool scalar::write_fun(const char* name, bool endian_flip)
{

  // open scalar function file
  if(strlen(name)==0){
     if((s2=fopen("new_scalar.fun","wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "scalar file");
        return(false);
     }
  }
  else{
     if((s2=fopen(name,"wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "scalar file");
        return(false);
     }
  }
 
  // write dimensions
  write_file((void*)&ni,1,endian_flip, s2);
  write_file((void*)&nj,1,endian_flip, s2);
  write_file((void*)&nk,1,endian_flip, s2);
  write_file((void*)&nvar,1,endian_flip, s2);

  // write function data
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	      write_file((void*)&(func[i][j][k]),1,endian_flip,s2);
  
  fclose(s2);
  s2=NULL;
  return(true);

}

////////////////////////////////////////////////////////////
bool scalar::write_fun(const char* name, int nvars, bool endian_flip)
{

  // open scalar function file
  if(strlen(name)==0){
     if((s2=fopen("new_scalar.fun","wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "scalar file");
        return(false);
     }
  }
  else{
     if((s2=fopen(name,"wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "scalar file");
        return(false);
     }
  }
 
  // write dimensions
  write_file((void*)&ni,1,endian_flip, s2);
  write_file((void*)&nj,1,endian_flip, s2);
  write_file((void*)&nk,1,endian_flip, s2);
  write_file((void*)&nvars,1,endian_flip, s2);

  // write function data
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	      write_file((void*)&(func[i][j][k]),1,endian_flip,s2);
  
  fclose(s2);
  s2=NULL;
  return(true);
}

////////////////////////////////////////////////////////////
bool scalar::write_fun_no_header(FILE *fp, bool endian_flip)
{

  // write function data
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)	      
          write_file((void*)&(func[i][j][k]),1,endian_flip,fp);

  return(true);
}

//////////////////////////////////////////////////
void scalar::write_fun_rev(char* name)
{

  // open scalar function file
  if(strlen(name)==0){
     if((s2=fopen("new_scalar.fun","wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "scalar file");
        return;
     }
  }
  else{
     if((s2=fopen(name,"wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "scalar file");
        return;
     }

  }

  // temp variables to hold byte flipped words
  int itmp;
  float ftmp;
 
  // write dimensions
  flip_bytes((void*)&itmp,(void*)&ni);
  fwrite(&itmp, sizeof(int),1,s2);
  flip_bytes((void*)&itmp,(void*)&nj);
  fwrite(&itmp, sizeof(int),1,s2);
  flip_bytes((void*)&itmp,(void*)&nk);
  fwrite(&itmp, sizeof(int),1,s2);
  flip_bytes((void*)&itmp,(void*)&nvar);
  fwrite(&itmp, sizeof(int),1,s2);

  // write function data
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
         flip_bytes((void*)&ftmp,(void*)&(func[i][j][k]));
         fwrite(&ftmp,sizeof(float),1,s2);
      }
  
  fclose(s2);
  s2=NULL;

}

/////////////////////////////////////////////////
void scalar::read_p3d()
{

  // check to see if we have already read in scalar info
  if(active){printf("Object is already loaded...\n"); return;}
  else active=1;

  // open file
  if((s1=fopen(file_name,"rt"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "scalar file");
     return;
  }

  // INCOMPLETE

  fclose(s1);
  s1=NULL;

}

//////////////////////////////////////////////////
void scalar::write_p3d()
{

  // open file
  if((s2=fopen("new_scalar.p3d","wt"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "scalar file");
     return;
  }

  // INCOMPLETE

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	      fprintf(s2,"%d %d %d %f\n",i,j,k,(func[i][j][k]));

  fclose(s2);
  s2=NULL;

}

////////////////////////////////////////////////
float scalar::func_val(int a,int b,int c) const
{
  return(func[a][b][c]);
}

//////////////////////////////////////////////////
int scalar::size(int direction) const
{
  
  if(direction==0)return(ni);
  else if(direction==1)return(nj);
  else if(direction==2)return(nk);
  else return(-1);

}

//////////////////////////////////////////////////
void scalar::set_func_val(int a,int b,int c,float val)
{
  func[a][b][c]=val;
}

//////////////////////////////////////////////////
void scalar::negate()
{
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	   func[i][j][k]*=(-1.0);   
}

//////////////////////////////////////////////////
void scalar::flip_bytes(void* targ, void* src)
{
    // Mirror 32 bit words 
    for(int i=0;i<4;i++)
        ((unsigned char*)targ)[i]=((unsigned char*)src)[3-i];
}

/////////////////////////////////////////////////
void scalar::convert_1d23d(float *dat)
{

  int i,j,k,cnt=0;

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
	    func[i][j][k] = dat[cnt];
        cnt++;
      }
} 

////////////////////////////////////////////////
bool scalar::read_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream)
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
bool scalar::write_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream) const
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
