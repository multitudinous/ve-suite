#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "grid.h"
#include "scalar.h"
#include "interp.h"
#include <cmath>
#include <iostream>
#include "search.h"

using namespace std;

///////////////////////////////////////////////
// constructors
interp::interp()
{}

///////////////////////////////////////////////
void interp::set_inputs(bool flipsign,grid *s_grd,grid *t_grd,scalar *s_fun,scalar *t_fun,interp *intp)
{

    flip_sign= flipsign;
    src_grd  = s_grd;
    targ_grd = t_grd;
    src_fun  = s_fun;
    targ_fun = t_fun;

    for(i=0;i<3;i++)
        if(s_grd->size(i)!=src_fun->size(i)||t_grd->size(i)!=targ_fun->size(i)){
           cout<<"Dimensions of grid file and function don't match!\n";
           return;
        } 

    rfactor           = intp->rfactor;
    numlay            = intp->numlay;
    num_interp_nodes  = intp->num_interp_nodes;
    alpha             = intp->alpha;
    src_axis          = intp->src_axis;
    targ_axis         = intp->targ_axis;
    src_plane         = intp->src_plane;
    targ_plane        = intp->targ_plane;
     
    // set up map array
    for(i=1;i<4;i++)map[i]=intp->map[i];

    // set up bounding box arrays
    for(i=1;i<4;i++){    
        src_low[i]   = intp->src_low[i];        
        src_high[i]  = intp->src_high[i]; 
        targ_low[i]  = intp->targ_low[i];
        targ_high[i] = intp->targ_high[i]; 
    }
    
    setup();
}

///////////////////////////////////////////////
void interp::set_inputs(bool flipsign,grid *s_grd,grid *t_grd,scalar *s_fun,scalar *t_fun,FILE *fptr)
{

    flip_sign= flipsign;
    src_grd  = s_grd;
    targ_grd = t_grd;
    src_fun  = s_fun;
    targ_fun = t_fun;

    for(i=0;i<3;i++)
        if(s_grd->size(i)!=src_fun->size(i)||t_grd->size(i)!=targ_fun->size(i)){
           cout<<"Dimensions of grid file and function don't match!\n";
           return;
        } 

 if(fptr!=NULL){
    fscanf(fptr,"%lf %d %d %lf",&rfactor,&numlay,&num_interp_nodes,&alpha);
    fscanf(fptr,"%d %d %d %d",&src_axis,&targ_axis,&src_plane,&targ_plane);
    
    src_plane--;
    targ_plane--;
 
    // set up map array
    for(i=1;i<4;i++)fscanf(fptr,"%d",&(map[i]));
   
    // set up bounding box arrays
    for(i=1;i<4;i++){
        if(i!=src_axis){
            fscanf(fptr,"%d",&(src_low[i]));
            fscanf(fptr,"%d",&(src_high[i]));
            src_low[i]--;
            src_high[i]--;
        }
        if(i!=targ_axis){
            fscanf(fptr,"%d",&(targ_low[i]));
            fscanf(fptr,"%d",&(targ_high[i]));
            targ_low[i]--;
            targ_high[i]--;
        }
    }
 }
 else {

    // get info from user
    char def;
    cout<<"Do you want to use default interpolation values? (y/n)"; cin>>def;
 
    if(def!='y'){
      cout<<"Enter multiplicative factor for radius (used for building list of src points)"<<endl;
      cin>>rfactor;
      cout<<"Enter the number of layers to search out"<<endl;
      cin>>numlay;
      cout<<"Enter the number of nodes to use for interpolation at each point"<<endl;
      cin>>num_interp_nodes;
      cout<<"Enter the exponent used in shepard interpolation (1 or 2)"<<endl;
      cin>>alpha;
    }
    else {
      rfactor          = 5.0; 
      numlay           = 5; 
      num_interp_nodes = 3;
      alpha            = 2.0;
    }


    cout<<endl<<"Enter source plane axis (i=1,j=2,k=3): "; cin>>src_axis;
    cout<<endl<<"Enter target plane axis (i=1,j=2,k=3): "; 
    cin>>targ_axis; cout<<endl;

    cout<<"Enter source plane (all indexes are 1 based): "; cin>>src_plane;
    cout<<endl<<"Enter target plane: "; 
    cin>>targ_plane; cout<<endl;
    src_plane--;
    targ_plane--;
 
    // set up map array
    for(i=1;i<4;i++){
            cout<<"Enter target axis number corresponding to source axis "<<
               i<<": ";  cin>>map[i]; cout<<endl;
    }

    // set up bounding box arrays
    for(i=1;i<4;i++){
        if(i!=src_axis){
            cout<<"Enter low index for source axis "<<
               i<<": ";  cin>>src_low[i]; cout<<endl;
            cout<<"Enter high index for source axis "<<
               i<<": ";  cin>>src_high[i]; cout<<endl;
            src_low[i]--;
            src_high[i]--;
        }
        if(i!=targ_axis){
            cout<<"Enter low index for target axis "<<
               i<<": ";  cin>>targ_low[i]; cout<<endl;
            cout<<"Enter high index for target axis "<<
               i<<": ";  cin>>targ_high[i]; cout<<endl;
            targ_low[i]--;
            targ_high[i]--;
        }
    }
 }

 setup();

}

///////////////////////////////////////////////
void interp::setup()
{

    // find the indexes of the axes that are not the plane axis
    int cntr=0;
    for(i=1;i<4;i++){
        if(i!=src_axis){
            src_index[cntr]=i;
            cntr++;
        }
    }
    cntr=0;
    for(i=1;i<4;i++){
        if(i!=targ_axis){
            targ_index[cntr]=i;
            cntr++;
        }
    }

    // check to see if 2d axes are oriented correctly (swap if not)
    // this ensures that 0 index corresp. to x in 2d and 1 to y
    // for both source and target
    if(targ_index[0]!=abs(map[src_index[0]])){
        int temp;
        temp=targ_index[0];
        targ_index[0]=targ_index[1];
        targ_index[1]=temp;
    }

    // find number of nodes in each direction
    src_node_num[0]=(src_high[src_index[0]]-src_low[src_index[0]])+1;
    src_node_num[1]=(src_high[src_index[1]]-src_low[src_index[1]])+1;
    
    targ_node_num[0]=(targ_high[targ_index[0]]-targ_low[targ_index[0]])+1;
    targ_node_num[1]=(targ_high[targ_index[1]]-targ_low[targ_index[1]])+1;

    // allocate memory
    src_2d=new struct plane_data*[src_node_num[0]];
    if(src_2d==NULL){cout<<"Memory Alloc error type b\n"; return;}
    for(i=0;i<src_node_num[0];i++){
        src_2d[i]=new struct plane_data[src_node_num[1]];
        if(src_2d[i]==NULL){cout<<"Memory Alloc error type c\n"; return;}
    }

    targ_2d=new struct plane_data*[targ_node_num[0]];
    if(targ_2d==NULL){cout<<"Memory Alloc error type d\n"; return;}
    for(i=0;i<targ_node_num[0];i++){
        targ_2d[i]=new struct plane_data[targ_node_num[1]];
        if(targ_2d[i]==NULL){cout<<"Memory Alloc error type e\n"; return;}
    }

    // load src_2d array x values
    for(j=0;j<src_node_num[1];j++){
      src_2d[0][j].x=0.0;   // initialize each row
        for(i=1;i<src_node_num[0];i++){

            if(src_index[0]==1)delta=src_grd->x(src_low[src_index[0]]+i)-src_grd->x(src_low[src_index[0]]+i-1);
            else if(src_index[0]==2)delta=src_grd->y(src_low[src_index[0]]+i)-src_grd->y(src_low[src_index[0]]+i-1);
            else delta=src_grd->z(src_low[src_index[0]]+i)-src_grd->z(src_low[src_index[0]]+i-1);

            src_2d[i][j].x=src_2d[i-1][j].x+delta;                    
        }
    }

    // load src_2d array y values
    for(i=0;i<src_node_num[0];i++){
      src_2d[i][0].y=0.0;   // initialize each column
        for(j=1;j<src_node_num[1];j++){

            if(src_index[1]==1)delta=src_grd->x(src_low[src_index[1]]+j)-src_grd->x(src_low[src_index[1]]+j-1);
            else if(src_index[1]==2)delta=src_grd->y(src_low[src_index[1]]+j)-src_grd->y(src_low[src_index[1]]+j-1);
            else delta=src_grd->z(src_low[src_index[1]]+j)-src_grd->z(src_low[src_index[1]]+j-1);

            src_2d[i][j].y=src_2d[i][j-1].y+delta;                    
        }
    }

    // load src_2d array i,j,k indices
    for(i=0;i<src_node_num[0];i++)
        for(j=0;j<src_node_num[1];j++){
           if(src_axis==1){
             src_2d[i][j].i=src_plane;
             src_2d[i][j].j=src_low[src_index[0]]+i;
             src_2d[i][j].k=src_low[src_index[1]]+j;
           }
           if(src_axis==2){
             src_2d[i][j].i=src_low[src_index[0]]+i;
             src_2d[i][j].j=src_plane;
             src_2d[i][j].k=src_low[src_index[1]]+j;
           }
           if(src_axis==3){
             src_2d[i][j].i=src_low[src_index[0]]+i;
             src_2d[i][j].j=src_low[src_index[1]]+j;
             src_2d[i][j].k=src_plane;
           }
        }

    // load targ_2d array x values (src & targ axes oriented the same)
    if(map[src_index[0]]>0){
      for(j=0;j<targ_node_num[1];j++){
        targ_2d[0][j].x=0.0;   // initialize each row
          for(i=1;i<targ_node_num[0];i++){

            if(targ_index[0]==1)
                delta=targ_grd->x(targ_low[targ_index[0]]+i)-targ_grd->x(targ_low[targ_index[0]]+i-1);
            else if(targ_index[0]==2)
                delta=targ_grd->y(targ_low[targ_index[0]]+i)-targ_grd->y(targ_low[targ_index[0]]+i-1);
            else 
                delta=targ_grd->z(targ_low[targ_index[0]]+i)-targ_grd->z(targ_low[targ_index[0]]+i-1);

            targ_2d[i][j].x=targ_2d[i-1][j].x+delta;                    
          }
      }
    }

    // load targ_2d array x values (src & targ axes oriented opposite)
    if(map[src_index[0]]<0){
      for(j=0;j<targ_node_num[1];j++){
        targ_2d[0][j].x=0.0;   // initialize each row
          for(i=1;i<targ_node_num[0];i++){

            if(targ_index[0]==1)
                delta=targ_grd->x(targ_high[targ_index[0]]-i+1)-targ_grd->x(targ_high[targ_index[0]]-i);
            else if(targ_index[0]==2)
                delta=targ_grd->y(targ_high[targ_index[0]]-i+1)-targ_grd->y(targ_high[targ_index[0]]-i);
            else 
                delta=targ_grd->z(targ_high[targ_index[0]]-i+1)-targ_grd->z(targ_high[targ_index[0]]-i);

            targ_2d[i][j].x=targ_2d[i-1][j].x+delta;                    
          }
      }
    }

    // load targ_2d array y values (src and targ axes oriented same)
    if(map[src_index[1]]>0){
      for(i=0;i<targ_node_num[0];i++){
        targ_2d[i][0].y=0.0;   // initialize each column
          for(j=1;j<targ_node_num[1];j++){

            if(targ_index[1]==1)
                delta=targ_grd->x(targ_low[targ_index[1]]+j)-targ_grd->x(targ_low[targ_index[1]]+j-1);
            else if(targ_index[1]==2)
                delta=targ_grd->y(targ_low[targ_index[1]]+j)-targ_grd->y(targ_low[targ_index[1]]+j-1);
            else 
                delta=targ_grd->z(targ_low[targ_index[1]]+j)-targ_grd->z(targ_low[targ_index[1]]+j-1);

            targ_2d[i][j].y=targ_2d[i][j-1].y+delta;                    
          }
      }
    }

    // load targ_2d array y values (src and targ axes opposite)
    if(map[src_index[1]]<0){
      for(i=0;i<targ_node_num[0];i++){
        targ_2d[i][0].y=0.0;   // initialize each column
          for(j=1;j<targ_node_num[1];j++){

            if(targ_index[1]==1)
                delta=targ_grd->x(targ_high[targ_index[1]]-j+1)-targ_grd->x(targ_high[targ_index[1]]-j);
            else if(targ_index[1]==2)
                delta=targ_grd->y(targ_high[targ_index[1]]-j+1)-targ_grd->y(targ_high[targ_index[1]]-j);
            else 
                delta=targ_grd->z(targ_high[targ_index[1]]-j+1)-targ_grd->z(targ_high[targ_index[1]]-j);

            targ_2d[i][j].y=targ_2d[i][j-1].y+delta;                    
          }
      }
    }

    // load targ_2d array i,j,k indices
    int a,b,loop1,loop2;
    for(i=0;i<targ_node_num[0];i++)
        for(j=0;j<targ_node_num[1];j++){
           
           if(targ_axis==1){

             targ_2d[i][j].i=targ_plane;

             if(abs(map[src_index[0]])==2){a=0; b=1; loop1=i; loop2=j;}
             else {a=1; b=0; loop1=j; loop2=i;}
             
             if(map[src_index[a]]>0)
                 targ_2d[i][j].j=targ_low[targ_index[a]]+loop1;
             else 
                 targ_2d[i][j].j=targ_high[targ_index[a]]-loop1;

             if(map[src_index[b]]>0)
                 targ_2d[i][j].k=targ_low[targ_index[b]]+loop2;
             else 
                 targ_2d[i][j].k=targ_high[targ_index[b]]-loop2;
             
           }
           if(targ_axis==2){

             targ_2d[i][j].j=targ_plane;

             if(abs(map[src_index[0]])==1){a=0; b=1; loop1=i; loop2=j;}
             else {a=1; b=0; loop1=j; loop2=i;}
             
             if(map[src_index[a]]>0)
                 targ_2d[i][j].i=targ_low[targ_index[a]]+loop1;
             else 
                 targ_2d[i][j].i=targ_high[targ_index[a]]-loop1;

             if(map[src_index[b]]>0)
                 targ_2d[i][j].k=targ_low[targ_index[b]]+loop2;
             else 
                 targ_2d[i][j].k=targ_high[targ_index[b]]-loop2;
           }
           if(targ_axis==3){
             
             targ_2d[i][j].k=targ_plane;

             if(abs(map[src_index[0]])==1){a=0; b=1; loop1=i; loop2=j;}
             else {a=1; b=0; loop1=j; loop2=i;}
             
             if(map[src_index[a]]>0)
                 targ_2d[i][j].i=targ_low[targ_index[a]]+loop1;
             else 
                 targ_2d[i][j].i=targ_high[targ_index[a]]-loop1;

             if(map[src_index[b]]>0)
                 targ_2d[i][j].j=targ_low[targ_index[b]]+loop2;
             else 
                 targ_2d[i][j].j=targ_high[targ_index[b]]-loop2;

           }
        }

    // now shift coordinates of targ_2d so target and src are centered
    // this will also allow us to define a common origin
    float xshift=((src_2d[src_node_num[0]-1][0].x-src_2d[0][0].x)-(targ_2d[targ_node_num[0]-1][0].x-targ_2d[0][0].x))/2.0;
    float yshift=((src_2d[0][src_node_num[1]-1].y-src_2d[0][0].y)-(targ_2d[0][targ_node_num[1]-1].y-targ_2d[0][0].y))/2.0;

    for(i=0;i<targ_node_num[0];i++)
        for(j=0;j<targ_node_num[1];j++){
          targ_2d[i][j].x+=xshift;
          targ_2d[i][j].y+=yshift;
        }

    // define these variables for convenience
    src_ni  = src_node_num[0];
    src_nj  = src_node_num[1];
    targ_ni = targ_node_num[0];
    targ_nj = targ_node_num[1];

    // now perform interpolation
    interpolate();
   
}

///////////////////////////////////////////////
// destructor

interp::~interp()
{
   
   // deallocate tcell linked list 
   pt_list *next_pt;

   for(i=0; i<targ_ni; i++){          
      for(j=0; j<targ_nj; j++){           
            current = tcell[i][j].head;                 
            while(current!=NULL){
              next_pt=current->next;
              free(current);
              current=next_pt;
            }
      }
   }

   for(i=0;i<src_node_num[0];i++)delete [] src_2d[i];
   delete [] src_2d;

   for(i=0;i<targ_node_num[0];i++)delete [] targ_2d[i];
   delete [] targ_2d;

   for(i=0;i<src_ni;i++)delete [] scell[i];
   delete [] scell;

   for(i=0;i<targ_ni;i++)delete [] tcell[i];
   delete [] tcell;

   free(distance);
   free(selected);

} 

//////////////////////////////////////////////////////////////
void interp::interpolate()
{

    // allocate memory for scell and tcell array
    scell=new struct src_cell*[src_ni];
    if(scell==NULL){cout<<"Memory Alloc error type f\n"; return;}
    for(i=0;i<src_ni;i++){
        scell[i]=new struct src_cell[src_nj];
        if(scell[i]==NULL){cout<<"Memory Alloc error type g\n"; return;}
    }

    tcell=new struct targ_cell*[targ_ni];
    if(tcell==NULL){cout<<"Memory Alloc error type h\n"; return;}
    for(i=0;i<targ_ni;i++){
        tcell[i]=new struct targ_cell[targ_nj];
        if(tcell[i]==NULL){cout<<"Memory Alloc error type i\n"; return;}
    }

    // initialize tcell src_i and src_j elements
    for(i=0;i<targ_ni;i++)
      for(j=0;j<targ_nj;j++)
        tcell[i][j].src_i=tcell[i][j].src_j=-1;
    
    // set up cell coordinates (assuming the x,y,z arrays are ordered - monotonically increasing or decreasing) 
    for(i=0;i<src_ni;i++){          
      for(j=0;j<src_nj;j++){                 

            // set the two x coordinates of the cell 

            if(i==0){
               scell[i][j].x[0]=src_2d[i][j].x-0.5*(src_2d[i+1][j].x-src_2d[i][j].x);
               scell[i][j].x[1]=src_2d[i][j].x+0.5*(src_2d[i+1][j].x-src_2d[i][j].x);
            }
            else if(i==src_ni-1){
               scell[i][j].x[0]=src_2d[i][j].x-0.5*(src_2d[i][j].x-src_2d[i-1][j].x);
               scell[i][j].x[1]=src_2d[i][j].x+0.5*(src_2d[i][j].x-src_2d[i-1][j].x);
            }
            else{
               scell[i][j].x[0]=src_2d[i][j].x-0.5*(src_2d[i][j].x-src_2d[i-1][j].x);
               scell[i][j].x[1]=src_2d[i][j].x+0.5*(src_2d[i+1][j].x-src_2d[i][j].x);
            }

	    // set the two y coordinates of the cell 

            if(j==0){
               scell[i][j].y[0]=src_2d[i][j].y-0.5*(src_2d[i][j+1].y-src_2d[i][j].y);
               scell[i][j].y[1]=src_2d[i][j].y+0.5*(src_2d[i][j+1].y-src_2d[i][j].y);
            }
            else if(j==src_nj-1){
               scell[i][j].y[0]=src_2d[i][j].y-0.5*(src_2d[i][j].y-src_2d[i][j-1].y);
               scell[i][j].y[1]=src_2d[i][j].y+0.5*(src_2d[i][j].y-src_2d[i][j-1].y);
            }
            else{
               scell[i][j].y[0]=src_2d[i][j].y-0.5*(src_2d[i][j].y-src_2d[i][j-1].y);
               scell[i][j].y[1]=src_2d[i][j].y+0.5*(src_2d[i][j+1].y-src_2d[i][j].y);
            }
      } 
    } 

// now that we have cell loaded with the correct x,y coordinates
// of each source cell, we can now search and see which target nodes
// are in each source cell. This list of nodes is stored in the
// cell structure as a linked list. 

   // initialize the lo values to zero for the first pass    
   int dumi=0;
   int dumj=0;
        
   // search all non-iblanked target nodes to find which source cell each one is in 
  
   int *ilo=&dumi;
   int *jlo=&dumj;

   for(a=0;a<targ_ni;a++){
      for(b=0;b<targ_nj;b++){

            if(targ_grd->celltype(targ_2d[a][b].i,targ_2d[a][b].j,targ_2d[a][b].k)!=8){  // only search for nodes that are non iblanked 
               
               search_i(scell,src_ni,targ_2d[a][b].x,ilo);
               search_j(scell,src_nj,targ_2d[a][b].y,jlo);           

               // if indexes are good, put node in tcell[][].src_i & src_j
               
               if(*ilo!=-1 && *ilo!=src_ni && *jlo!=-1 && *jlo!=src_nj){ 
                  tcell[a][b].src_i=*ilo;
                  tcell[a][b].src_j=*jlo;
               }    
               else{

                  if(*ilo==-1)tcell[a][b].src_i=0;
                  else if(*ilo==src_ni)tcell[a][b].src_i=src_ni-1;
                  else tcell[a][b].src_i=*ilo;

                  if(*jlo==-1)tcell[a][b].src_j=0;
                  else if(*jlo==src_nj)tcell[a][b].src_j=src_nj-1;
                  else tcell[a][b].src_j=*jlo;

                  printf("Warning: a target node is outside range of source grid.. continuing...\n");		          
                  printf("   Assigned src indices %d %d\n",tcell[a][b].src_i,tcell[a][b].src_j);
                  printf("   targ nodes (2d) %d %d, Location: %f %f\n",a,b,targ_2d[a][b].x,targ_2d[a][b].y);
                  printf("   *ilo: %d, *jlo: %d\n",*ilo,*jlo);
                  printf("   src_ni: %d src_nj: %d\n",src_ni,src_nj);
                  printf("   targ_ni: %d targ_nj: %d\n",targ_ni,targ_nj);
                  printf("   src grid range (face to face) xlo: %f xhi: %f\n",scell[0][0].x[0],scell[src_ni-1][0].x[1]);
                  printf("   src grid range (face to face) ylo: %f yhi: %f\n",scell[0][0].y[0],scell[0][src_nj-1].y[1]);
                  printf("   targ grid range (node to node) xlo: %f xhi: %f\n",targ_2d[0][0].x,targ_2d[targ_ni-1][0].x);
                  printf("   targ grid range (node to node) ylo: %f yhi: %f\n",targ_2d[0][0].y,targ_2d[0][targ_nj-1].y);
                         
	           }

            }  // end iblank conditional 

            // initialize all heads of linked list to NULL - this is a convenient place to do this 
            tcell[a][b].head = NULL;

      }	     // end for loop for nb 
   }        // end for loop for na 

   
   // now build a linked list of source node points for each target node
   cout<<"Building point lists...\n";
   cout.flush();
   
   for(a=0;a<targ_ni;a++){
      for(b=0;b<targ_nj;b++){

            if(targ_grd->celltype(targ_2d[a][b].i,targ_2d[a][b].j,targ_2d[a][b].k)!=8){  // only interpolate at non-iblanked nodes 
               i=tcell[a][b].src_i;
               j=tcell[a][b].src_j;
               x1 = targ_2d[a][b].x;
               y1 = targ_2d[a][b].y;

               // find size of cartesian cell
               dx=fabs(scell[i][j].x[1]-scell[i][j].x[0]);
               dy=fabs(scell[i][j].y[1]-scell[i][j].y[0]);             

               float radius=sqrt(dx/2.0*dx/2.0+dy/2.0*dy/2.0)*rfactor;
               
               for(int nli=-numlay; nli<=numlay; nli++){
                  for(int nlj=-numlay; nlj<=numlay; nlj++){
         
                        // check that subscripts are in range

                        if((i+nli>=0)&&(i+nli<src_ni)&&(j+nlj>=0)&&(j+nlj<src_nj)){
                           x2 = src_2d[i+nli][j+nlj].x;
                           y2 = src_2d[i+nli][j+nlj].y;                   
                           float r = sqrt(pow((x2-x1),2.0) + pow((y2-y1),2.0));

                           if(r<=radius&&src_grd->celltype(src_2d[i+nli][j+nlj].i,src_2d[i+nli][j+nlj].j,src_2d[i+nli][j+nlj].k)!=8){
                              tcell[a][b].src_nodes=(struct pt_list *)malloc(sizeof(struct pt_list));
                              if(tcell[a][b].src_nodes==NULL){
                                  cout<<"Memory Alloc error type a\n";
                                  return;
                              }
                              
                              if(tcell[a][b].head == NULL)
                                 tcell[a][b].head=tcell[a][b].src_nodes;
                              else
                                 tcell[a][b].prev->next=tcell[a][b].src_nodes;

                              tcell[a][b].src_nodes->next = NULL;

                              // store info about src node in tcell                                                 
			                  tcell[a][b].src_nodes->i_index=i+nli;
                              tcell[a][b].src_nodes->j_index=j+nlj; 
                              tcell[a][b].prev=tcell[a][b].src_nodes;
                           }
                        }                
                  }
               }                  
               if(tcell[a][b].head==NULL){
                   printf("\nFatal Error: No head node for a tcell: %d %d\n",a,b);               
                   return; 
               }
            }
      }
   }


   // now that we have our linked list of points, lets do interpolation
   cout<<"doing interpolation...\n";
   cout.flush();

   // find max_pt_cnt 

   int max_pt_cnt=0,pt_cnt=0,num_pts;

   for(i=0;i<targ_ni;i++){
      for(j=0;j<targ_nj;j++){          
            pt_cnt=0;
            current = tcell[i][j].head;     
  
            /* loop through linked list of points */            
            while(current!=NULL){
              pt_cnt++;
              current=current->next;
            }
            if(pt_cnt>max_pt_cnt)max_pt_cnt=pt_cnt;
      }
   }

   // allocate memory 

   distance=(double*)malloc(max_pt_cnt*sizeof(double));
   selected=(char*)malloc(max_pt_cnt*sizeof(char));
   if(distance==NULL||selected==NULL){
       cout<<"MEMORY ALLOCATION ERROR! Try adjusting the search radius...\n";
       return;
   }

   // now calculate distances to all points and find top num_interp_nodes
   
   int min_index=0;
   double dist,min_dist,num,denom,beta;

   for(i=0;i<targ_ni;i++){
      for(j=0;j<targ_nj;j++){

            num=denom=pt_cnt=0;
            for(a=0;a<max_pt_cnt;a++)selected[a]=0;
            current = tcell[i][j].head;     
  
            // loop through linked list of points 
            
            while(current!=NULL){
              dist = sqrt(pow(((src_2d[current->i_index][current->j_index].x)-targ_2d[i][j].x),2.0) + pow(((src_2d[current->i_index][current->j_index].y)-targ_2d[i][j].y),2.0));
              //distance[pt_cnt] = MAX(dist,epsi);
	      if(dist>1.0e-6) distance[pt_cnt] = dist;
	      else distance[pt_cnt] = 1.0e-6;
	      pt_cnt++;
              current=current->next;
            }
            
            /* search for closest num_interp_nodes nodes */
            if(pt_cnt!=0&&targ_grd->celltype(targ_2d[i][j].i,targ_2d[i][j].j,targ_2d[i][j].k)!=8){
              if(num_interp_nodes<=pt_cnt){
                for(a=0;a<num_interp_nodes;a++){
                   min_dist=1.0e30;
                   for(b=0;b<pt_cnt;b++){
                      if(selected[b]==0&&distance[b]<min_dist){
                        min_index=b;
                        min_dist=distance[b];
                      }
                   }
                   selected[min_index]=1;
                }
                num_pts=num_interp_nodes;
              }
              else{
                for(a=0;a<pt_cnt;a++)selected[a]=1;
                num_pts=pt_cnt;
                printf("WARNING: The total number of nodes is %d less than num_interp_nodes for %d %d\n",num_interp_nodes-pt_cnt,i,j);
              }
            }
            else num_pts=0;
            
            // now actually interpolate
            
            current = tcell[i][j].head;  
            if(num_pts!=0){
              for(a=0;a<pt_cnt;a++){    
                if(selected[a]==1){
                   dist = 1.0/distance[a];
                   beta=pow(dist,alpha);     
                   num+=beta*src_fun->func_val(src_2d[current->i_index][current->j_index].i,src_2d[current->i_index][current->j_index].j,src_2d[current->i_index][current->j_index].k);
                   denom+=beta;                 
                }
                current=current->next;
              } 
              targ_fun->set_func_val(targ_2d[i][j].i,targ_2d[i][j].j,targ_2d[i][j].k,num/denom*((flip_sign)?-1.0:1.0));          
            }            
      }
   }


}  // end interpolate function

//////////////////////////////////////////////
int *interp::get_map_array()
{
    return(map);
}

//////////////////////////////////////////////
int interp::get_source_axis()
{
    return(src_axis);
}

//////////////////////////////////////////////
int interp::get_target_axis()
{
    return(targ_axis);
}

//////////////////////////////////////////////
void interp::write_interpdat(char *name)
{
    
    // write out an interp.dat file
    FILE *stream;
    if(strlen(name)==0){
       if((stream=fopen("interp.dat.new","wt"))==NULL){
          fprintf(stderr, "Fail to open %s\n", "interp.dat.new file");
          return;
       }
    }
    else{
       if((stream=fopen(name,"wt"))==NULL){
          fprintf(stderr, "Fail to open %s\n", name);
          return;
       }

    }

    fprintf(stream,"%f %d %d %f\n",rfactor,numlay,num_interp_nodes,alpha);
    fprintf(stream,"%d %d %d %d\n",src_axis,targ_axis,src_plane+1,targ_plane+1);
 
    // write map array
    for(i=1;i<4;i++)fprintf(stream,"%d\n",(map[i]));
   
    // set up bounding box arrays
    for(int i=1;i<4;i++){
        if(i!=src_axis){
            fprintf(stream,"%d\n",src_low[i]+1);
            fprintf(stream,"%d\n",src_high[i]+1);
        }
        if(i!=targ_axis){
            fprintf(stream,"%d\n",targ_low[i]+1);
            fprintf(stream,"%d\n",targ_high[i]+1);
        }
    }
    fclose(stream);
}

//////////////////////////////////////////////
int interp::get_targ_plane()
{
    return(targ_plane);
}

//////////////////////////////////////////////
int* interp::get_targ_low()
{
    return(targ_low);
}

//////////////////////////////////////////////
int* interp::get_targ_high()
{
    return(targ_high);
}

