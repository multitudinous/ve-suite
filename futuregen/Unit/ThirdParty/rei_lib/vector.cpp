
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "scalar.h"
#include "vector.h"
#include <cmath>
#include <iostream>

using namespace std;

///////////////////////////////////////////////
// constructors

vector::vector()
{ 
  init();
}

///////////////////////////////////////////////
vector::vector(char* fn)
{
  strcpy(file_name,fn);
  init();
}

///////////////////////////////////////////////
vector::vector(int a,int b,int c)
{
  
  ni=a;
  nj=b;
  nk=c;
  nvar=3;
  init();
  alloc();  
  active=1;

  for(i=0;i<ni;i++)
    for(j=0;j<nj;j++)
      for(k=0;k<nk;k++){
        sc1->set_func_val(i,j,k,0.0);
        sc2->set_func_val(i,j,k,0.0);
        sc3->set_func_val(i,j,k,0.0);
      }

}

///////////////////////////////////////////////
vector::vector(scalar *s1,scalar *s2,scalar *s3):
sc1(s1),
sc2(s2),
sc3(s3)
{
    init();
    active=1;
    nvar=3;
    ni=sc1->size(0);
    nj=sc1->size(1);
    nk=sc1->size(2);

    // also check to see if ni,nj,nk match for all 3 objs
}

///////////////////////////////////////////////
void vector::get_fname()
{
  // get info from user
  cout<<"Enter the name of the file..."<<endl;
  gets(file_name);
  init();
}

///////////////////////////////////////////////
void vector::init()
{

  // initialize file pointer and active flag
  s1=s2=NULL;
  active = 0;
  allocated=0;

}

///////////////////////////////////////////////
// destructor

vector::~vector()
{   
    // deallocate memory
    if(allocated){
        delete sc1;
        delete sc2;
        delete sc3;
    }
} 

///////////////////////////////////////////////
void vector::resize(int a,int b,int c)
{
  
  ni=a;
  nj=b;
  nk=c;
  nvar=3;
  active=1;

  if(allocated){
      sc1->resize(ni,nj,nk);
      sc2->resize(ni,nj,nk);
      sc3->resize(ni,nj,nk);
  }
  else{
      init();
      alloc();
  }

  for(i=0;i<ni;i++)
    for(j=0;j<nj;j++)
      for(k=0;k<nk;k++){
        sc1->set_func_val(i,j,k,0.0);
        sc2->set_func_val(i,j,k,0.0);
        sc3->set_func_val(i,j,k,0.0);
      }
}

///////////////////////////////////////////////
void vector::alloc()
{

  // allocate memory
  allocated=1;
  sc1=new scalar(ni,nj,nk);
  sc2=new scalar(ni,nj,nk);
  sc3=new scalar(ni,nj,nk);

}

///////////////////////////////////////////////
void vector::read_fun(bool endian_flip_output)
{
 
  // check to see if we have already read in vector info
  if(active){printf("Object is already loaded...\n"); return;}
  else active=1;

  // open file
  if((s1=fopen(file_name,"rb"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "fun file");
     return;
  }
  
  // read dimensions
  read_file((void*)&ni,1,endian_flip_output,s1);
  read_file((void*)&nj,1,endian_flip_output,s1);
  read_file((void*)&nk,1,endian_flip_output,s1);
  read_file((void*)&nvar,1,endian_flip_output,s1);
  
  // allocate memory
  alloc();

  // read function data
  float temp;
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
	      read_file((void*)&temp,1,endian_flip_output,s1);
          sc1->set_func_val(i,j,k,temp);
      }

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
	      read_file((void*)&temp,1,endian_flip_output,s1);
          sc2->set_func_val(i,j,k,temp);
      }

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
	      read_file((void*)&temp,1,endian_flip_output,s1);
          sc3->set_func_val(i,j,k,temp);
      }

  fclose(s1);
  s1=NULL;  
	 	 
}

////////////////////////////////////////////////////////////
void vector::read_db_vector(FILE *stream,bool endian_flip)
{
    // read inidividual scalars
    sc1->read_db_scalar(stream,endian_flip);
    fseek(stream,8L,SEEK_CUR);
    sc2->read_db_scalar(stream,endian_flip);
    fseek(stream,8L,SEEK_CUR);
    sc3->read_db_scalar(stream,endian_flip);
}

////////////////////////////////////////////////////////////
void vector::write_fun(const char* name, bool endian_flip_output)
{

  // open vector function file
  if(strlen(name)==0){
     if((s2=fopen("new_vector.fun","wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "vector file");
        return;
     }
  }
  else{
     if((s2=fopen(name,"wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "vector file");
        return;
     }
  }
 
  // write dimensions
  write_file((void*)&ni,1,endian_flip_output,s2);
  write_file((void*)&nj,1,endian_flip_output,s2);
  write_file((void*)&nk,1,endian_flip_output,s2);
  write_file((void*)&nvar,1,endian_flip_output,s2);

  // write function data
  float temp;
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
          temp=sc1->func_val(i,j,k);
	      write_file((void*)&temp,1,endian_flip_output,s2);
      }

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
          temp=sc2->func_val(i,j,k);
	      write_file((void*)&temp,1,endian_flip_output,s2);
      }

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
          temp=sc3->func_val(i,j,k);
	      write_file((void*)&temp,1,endian_flip_output,s2);
      }

  fclose(s2);
  s2=NULL;

  
}  // end func

////////////////////////////////////////////////////////////
void vector::write_fun_rev(char* name)
{

  // open vector function file
  if(strlen(name)==0){
     if((s2=fopen("new_vector.fun","wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "vector file");
        return;
     }
  }
  else{
     if((s2=fopen(name,"wb"))==NULL){
        fprintf(stderr, "Fail to open %s\n", "vector file");
        return;
     }

  }

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
  float temp;
  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
          temp=sc1->func_val(i,j,k);
          flip_bytes((void*)&ftmp,(void*)&temp);
	      fwrite(&ftmp,sizeof(float),1,s2);
      }

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
          temp=sc2->func_val(i,j,k);
          flip_bytes((void*)&ftmp,(void*)&temp);
	      fwrite(&ftmp,sizeof(float),1,s2);
      }

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++){
          temp=sc3->func_val(i,j,k);
          flip_bytes((void*)&ftmp,(void*)&temp);
	      fwrite(&ftmp,sizeof(float),1,s2);
      }

  fclose(s2);
  s2=NULL;

  
}  // end func

//////////////////////////////////////////////////
void vector::read_p3d()
{
   /*
  // check to see if we have already read in vector info
  if(active){printf("Object is already loaded...\n"); return;}
  else active=1;

  // open file
  if((s1=fopen(file_name,"rt"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "vector file");
     return;
  }

  // INCOMPLETE

  fclose(s1);
  s1=NULL;      */

}

//////////////////////////////////////////////////
void vector::write_p3d()
{
   /*
  // open file
  if((s2=fopen("new_vector.p3d","wt"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "vector file");
     return;
  }

  // INCOMPLETE

  for(k=0;k<nk;k++)
    for(j=0;j<nj;j++)
      for(i=0;i<ni;i++)
	      fprintf(s2,"%d %d %d %f\n",i,j,k,(func[i][j][k]));

  fclose(s2);
  s2=NULL;  */

}

////////////////////////////////////////////////
float vector::func_val(int component,int a,int b,int c)
{
    if(component==0)return(sc1->func_val(a,b,c));
    else if(component==1)return(sc2->func_val(a,b,c));
    else return(sc3->func_val(a,b,c));
}

//////////////////////////////////////////////////
int vector::size(int direction)
{
  
  if(direction==0)return(ni);
  else if(direction==1)return(nj);
  else if(direction==2)return(nk);
  else return(-1);

}

//////////////////////////////////////////////////
scalar* vector::get_scalar(int i)
{
    scalar *ptr = NULL;
    if(i==0)ptr = sc1;
    if(i==1)ptr = sc2;
    if(i==2)ptr = sc3;
    return(ptr);
}

//////////////////////////////////////////////////
void vector::set_func_val(int component,int a,int b,int c,float val)
{
    if(component==0)sc1->set_func_val(a,b,c,val);
    else if(component==1)sc2->set_func_val(a,b,c,val);
    else sc3->set_func_val(a,b,c,val);
}

//////////////////////////////////////////////////
void vector::flip_bytes(void* targ, void* src)
{
    // Mirror 32 bit words 
    for(int i=0;i<4;i++)
        ((unsigned char*)targ)[i]=((unsigned char*)src)[3-i];
}

////////////////////////////////////////////////
bool vector::read_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream)
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
bool vector::write_file(void *ptr, unsigned int num, bool endian_flip, FILE *stream) const
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
