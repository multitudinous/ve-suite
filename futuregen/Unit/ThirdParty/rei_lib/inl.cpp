#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "inl.h"
#include <cmath>
#include <iostream>

using namespace std;

///////////////////////////////////////////////
// constructors

inl::inl()
{ 
  // get info from user
  cout<<"Enter the name of the file..."<<endl;
  gets(file_name);
  init();
}

///////////////////////////////////////////////
inl::inl(char* fn)
{
  strcpy(file_name,fn);
  init();
}

///////////////////////////////////////////////
void inl::init()
{

  // initialize file pointer and active flag
  s1=s2=NULL;
  active = 0;

}

///////////////////////////////////////////////
// destructor

inl::~inl()
{
   
   // deallocate memory
   delete index_i;
   delete index_j;
   delete index_k;
   delete u;
   delete v;
   delete w;
   delete t;
   delete f;
   delete eta;
   delete type;

} 

///////////////////////////////////////////////
void inl::alloc()
{

  // allocate memory
  index_i=new int[inlet_num];
  index_j=new int[inlet_num];
  index_k=new int[inlet_num];
  u=new float[inlet_num];
  v=new float[inlet_num];
  w=new float[inlet_num];
  f=new float[inlet_num];
  eta=new float[inlet_num];
  t=new float[inlet_num];
  type=new int[inlet_num];

}

///////////////////////////////////////////////
void inl::read_inl()
{

  // check to see if we have already read in inl info
  if(active){printf("Object is already loaded...\n"); return;}
  else active=1;

  // open file
  if((s1=fopen(file_name,"rt"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "inl file");
     return;
  }

  // read title lines
  fscanf(s1,"%[^\n]",line1);
  fscanf(s1,"%c",&nl);
  fscanf(s1,"%[^\n]",line2);
  fscanf(s1,"%c",&nl);
  
  // determine number of inlet cells in inl file
  inlet_num=0;
  while(fscanf(s1,"%[^\n]",line)!=EOF){
        fscanf(s1,"%c",&nl);
        inlet_num++;
  };
  
  fseek(s1,0,0);  // rewind stream

   // reread title lines
  fscanf(s1,"%[^\n]",line);
  fscanf(s1,"%c",&nl);
  fscanf(s1,"%[^\n]",line);
  fscanf(s1,"%c",&nl);
  
  // allocate memory
  alloc();

  // initialize temperature array since it's not read in
  for(i=0;i<inlet_num;i++)t[i]=0.0;

  // read inl data
  for(i=0;i<inlet_num;i++)
	 fscanf(s1,"%d %d %d %f %f %f %f %f %d",index_i+i,index_j+i,
            index_k+i,u+i,v+i,w+i,f+i,eta+i,type+i);

  fclose(s1);
  s1=NULL;
	 	 
}

////////////////////////////////////////////////////////////
void inl::write_inl()
{

  // open inl file
  if((s2=fopen("new_inl.inl","wt"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "inl file");
     return;
  }
 
  // write header lines
  fprintf(s2,"%s\n",line1);
  fprintf(s2,"%s\n",line2);

  // write function data
  for(i=0;i<inlet_num;i++)
	 fprintf(s2,"%d\t%d\t%d\t%f\t%f\t%f\t%f\t%f\t%f\t%d\n",*(index_i+i),*(index_j+i),
            *(index_k+i),*(u+i),*(v+i),*(w+i),*(f+i),*(eta+i),*(t+i),*(type+i));

  fclose(s2);
  s2=NULL;

}

//////////////////////////////////////////////////
int inl::num_cells()
{
   return(inlet_num);
}

//////////////////////////////////////////////////
int inl::i_index(int index)
{
   return(index_i[index]);
}

//////////////////////////////////////////////////
int inl::j_index(int index)
{
   return(index_j[index]);
}

//////////////////////////////////////////////////
int inl::k_index(int index)
{
   return(index_k[index]);
}

/////////////////////////////////////////////////
void inl::set_u(int index,float val)
{
   u[index]=val;
}

/////////////////////////////////////////////////
void inl::set_v(int index,float val)
{
   v[index]=val;
}

/////////////////////////////////////////////////
void inl::set_w(int index,float val)
{
   w[index]=val;
}

/////////////////////////////////////////////////
void inl::set_t(int index,float val)
{
   t[index]=val;
}
