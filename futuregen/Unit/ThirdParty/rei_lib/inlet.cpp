#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include "cell_types.h"
#include "cell.h"
#include "grid.h"
#include "inlet.h"
#include <cmath>
#include <iostream>

using namespace std;

///////////////////////////////////////////////
// constructors

inlet::inlet()
{ 
  // get info from user
  cout<<"Enter the name of the grid file..."<<endl;
  gets(file_name);
  init();
}

///////////////////////////////////////////////
inlet::inlet(char* fn)
{
  strcpy(file_name,fn);
  init();
}

///////////////////////////////////////////////
void inlet::init()
{
   
   //  initialize pointers
   grd=NULL;
   cell=NULL;
   region_info_prim=NULL;
   region_info_sec=NULL;
   pair_cell=NULL;

   // create grid object and read file
   grd=new grid(file_name);

   char ans;
   cout<<"Is grid file in spark or rei format (s/r)?\n";
   cin>>ans;

   if(ans=='s')grd->read_spark_grid();
   else grd->read_rei_grid();

   ni=grd->size(0);
   nj=grd->size(1);
   nk=grd->size(2);
   count_inlets();

   // create array of grid_cells
   cell=new grid_cell[active_num];

   // setup cells
   setup_cells();

   // calculate total areas
   calc_total_areas();

}

///////////////////////////////////////////////
// destructor

inlet::~inlet()
{
	if(grd)delete grd;
	if(cell)delete [] cell;
	if(region_info_prim)delete [] region_info_prim;
	if(region_info_sec) delete [] region_info_sec;
    if(pair_cell)delete [] pair_cell;
} 

////////////////////////////////////////////////////////////
void inlet::write_inlet_list()
{

	// write a list of the active inlet cells to a file
	if((s1=fopen("cells.dat","wt"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "cells file");
     return;
    }

	printf("Total Number of Active Cells: %d\n",active_num);
	printf("Total Number of Primary Active Cells: %d\n",active_num_prim);
	printf("Total Number of Secondary Active Cells: %d\n",active_num_sec);
	printf("Total Primary Area (faces exposed to flow cells): %f\n",total_prim);
	printf("Total Secondary Area (faces exposed to flow cells): %f\n",total_sec);

	fprintf(s1,"%d %d\n",active_num_prim,active_num_sec);
	
	for(i=0;i<active_num;i++)
	   if(cell[i].type==primary)fprintf(s1,"%d\t%d\t%d\n",cell[i].index_i+1,cell[i].index_j+1,
	                                                      cell[i].index_k+1);
	for(i=0;i<active_num;i++)
	   if(cell[i].type==secondary)fprintf(s1,"%d\t%d\t%d\n",cell[i].index_i+1,cell[i].index_j+1,
	                                                        cell[i].index_k+1);

	fclose(s1);
}

////////////////////////////////////////////////////////////
void inlet::write_inl()
{

  // open inl file
  if((s1=fopen("new_inl.inl","wt"))==NULL){
     fprintf(stderr, "Fail to open %s\n", "inl file");
     return;
  }
 
  // write header lines
  fprintf(s1,"2\n");
  fprintf(s1,"%s\n","inl file created by:");
  fprintf(s1,"%s\n","inl_gen - das");

  // write function data
  for(k=0;k<region_num_prim;k++){
    for(i=0;i<active_num;i++)
	  if(cell[i].type==primary&&cell[i].region_id==k)fprintf(s1,"%4d %4d %4d %14.3f %14.3f %14.3f %14.3f %14.3f %d %d\n",
	            cell[i].index_i+1,cell[i].index_j+1,cell[i].index_k+1,cell[i].u,
		        cell[i].v,cell[i].w,cell[i].f,cell[i].eta,cell[i].type,cell[i].region_id+1);
    for(i=0;i<pair_cell_num;i++)
	  if(pair_cell[i].type==primary&&pair_cell[i].region_id==k)fprintf(s1,"%4d %4d %4d %14.3f %14.3f %14.3f %14.3f %14.3f %d %d p\n",
	            pair_cell[i].index_i+1,pair_cell[i].index_j+1,pair_cell[i].index_k+1,pair_cell[i].u,
		        pair_cell[i].v,pair_cell[i].w,pair_cell[i].f,pair_cell[i].eta,pair_cell[i].type,pair_cell[i].region_id+1);
  }

  for(k=0;k<region_num_sec;k++){
    for(i=0;i<active_num;i++)
	  if(cell[i].type==secondary&&cell[i].region_id==k)fprintf(s1,"%4d %4d %4d %14.3f %14.3f %14.3f %14.3f %14.3f %d %d\n",
	            cell[i].index_i+1,cell[i].index_j+1,cell[i].index_k+1,cell[i].u,
			    cell[i].v,cell[i].w,cell[i].f,cell[i].eta,cell[i].type,cell[i].region_id+1);
    for(i=0;i<pair_cell_num;i++)
	  if(pair_cell[i].type==secondary&&pair_cell[i].region_id==k)fprintf(s1,"%4d %4d %4d %14.3f %14.3f %14.3f %14.3f %14.3f %d %d p\n",
	            pair_cell[i].index_i+1,pair_cell[i].index_j+1,pair_cell[i].index_k+1,pair_cell[i].u,
		        pair_cell[i].v,pair_cell[i].w,pair_cell[i].f,pair_cell[i].eta,pair_cell[i].type,pair_cell[i].region_id+1);
  }

  fclose(s1);

}

//////////////////////////////////////////////////////////
void inlet::count_inlets()
{

  char active,bounds[6];
  active_num=0;
  active_num_prim=0;
  active_num_sec=0;

  for(i=0;i<ni;i++)
    for(j=0;j<nj;j++)
      for(k=0;k<nk;k++){
      
        if(grd->celltype(i,j,k)==primary||grd->celltype(i,j,k)==secondary){  
		
		  // initialize
          active=0;
          for(int a=0;a<6;a++)bounds[a]=0; 

          // check to see if we are on a boundary 
          if(i==0)    bounds[0]=1;
		  if(i==ni-1) bounds[1]=1;
		  if(j==0)    bounds[2]=1;
		  if(j==nj-1) bounds[3]=1;
		  if(k==0)    bounds[4]=1;
		  if(k==nk-1) bounds[5]=1;

		  // check to see if we are next to a flow cell
          if(!bounds[1]&&grd->celltype(i+1,j,k)==flow)active=1;
          if(!bounds[0]&&grd->celltype(i-1,j,k)==flow)active=1;
          if(!bounds[3]&&grd->celltype(i,j+1,k)==flow)active=1; 
          if(!bounds[2]&&grd->celltype(i,j-1,k)==flow)active=1;
          if(!bounds[5]&&grd->celltype(i,j,k+1)==flow)active=1;
          if(!bounds[4]&&grd->celltype(i,j,k-1)==flow)active=1;         
          if(active){
			  active_num++;
			  if(grd->celltype(i,j,k)==primary)active_num_prim++;
	       	  else active_num_sec++;
		  }
		}
      }     
}

//////////////////////////////////////////////////
int inlet::num_cells()
{
   return(active_num);
}

///////////////////////////////////////////////////
void inlet::setup_cells()
{

   int a, cnt=0;
   char active,bounds[6];
   char temp_face[6];

   for(i=0;i<ni;i++)          
      for(j=0;j<nj;j++)                 
         for(k=0;k<nk;k++){

	        if(grd->celltype(i,j,k)==primary||grd->celltype(i,j,k)==secondary){  
		
		       // initialize
               active=0;
               for(a=0;a<6;a++){bounds[a]=0; temp_face[a]=0;} 

               // check to see if we are on a boundary 
               if(i==0)    bounds[0]=1;
		       if(i==ni-1) bounds[1]=1;
		       if(j==0)    bounds[2]=1;
		       if(j==nj-1) bounds[3]=1;
		       if(k==0)    bounds[4]=1;
		       if(k==nk-1) bounds[5]=1;

		       // check to see if we are next to a flow cell
               if(!bounds[1]&&grd->celltype(i+1,j,k)==flow){active=1; temp_face[1]=1;}
               if(!bounds[0]&&grd->celltype(i-1,j,k)==flow){active=1; temp_face[0]=1;}
               if(!bounds[3]&&grd->celltype(i,j+1,k)==flow){active=1; temp_face[3]=1;}
               if(!bounds[2]&&grd->celltype(i,j-1,k)==flow){active=1; temp_face[2]=1;}
               if(!bounds[5]&&grd->celltype(i,j,k+1)==flow){active=1; temp_face[5]=1;}
               if(!bounds[4]&&grd->celltype(i,j,k-1)==flow){active=1; temp_face[4]=1;}        
               
			   if(active){

				   for(a=0;a<6;a++)cell[cnt].faces[a]=temp_face[a];
				   cell[cnt].type=grd->celltype(i,j,k);
				   cell[cnt].index_i=i;
				   cell[cnt].index_j=j;
				   cell[cnt].index_k=k;

				   // set the two x coordinates of the cell 
                   if(i==0){
                     cell[cnt].xlow=(float)(grd->x(i)-0.5*(grd->x(i+1)-grd->x(i)));
                     cell[cnt].xhigh=(float)(grd->x(i)+0.5*(grd->x(i+1)-grd->x(i)));
                   }
                   else if(i==ni-1){
                     cell[cnt].xlow=(float)(grd->x(i)-0.5*(grd->x(i)-grd->x(i-1)));
                     cell[cnt].xhigh=(float)(grd->x(i)+0.5*(grd->x(i)-grd->x(i-1)));
                   }
                   else{
                     cell[cnt].xlow=(float)(grd->x(i)-0.5*(grd->x(i)-grd->x(i-1)));
                     cell[cnt].xhigh=(float)(grd->x(i)+0.5*(grd->x(i+1)-grd->x(i)));
                   }

	               // set the two y coordinates of the cell
                   if(j==0){
                     cell[cnt].ylow=(float)(grd->y(j)-0.5*(grd->y(j+1)-grd->y(j)));
                     cell[cnt].yhigh=(float)(grd->y(j)+0.5*(grd->y(j+1)-grd->y(j)));
                   }
                   else if(j==nj-1){
                     cell[cnt].ylow=(float)(grd->y(j)-0.5*(grd->y(j)-grd->y(j-1)));
                     cell[cnt].yhigh=(float)(grd->y(j)+0.5*(grd->y(j)-grd->y(j-1)));
                   }
                   else{
                     cell[cnt].ylow=(float)(grd->y(j)-0.5*(grd->y(j)-grd->y(j-1)));
                     cell[cnt].yhigh=(float)(grd->y(j)+0.5*(grd->y(j+1)-grd->y(j)));
                   }

	               // set the two z coordinates of the cell 
                   if(k==0){
                     cell[cnt].zlow=(float)(grd->z(k)-0.5*(grd->z(k+1)-grd->z(k)));
                     cell[cnt].zhigh=(float)(grd->z(k)+0.5*(grd->z(k+1)-grd->z(k)));
                   }
                   else if(k==nk-1){
                     cell[cnt].zlow=(float)(grd->z(k)-0.5*(grd->z(k)-grd->z(k-1)));
                     cell[cnt].zhigh=(float)(grd->z(k)+0.5*(grd->z(k)-grd->z(k-1)));
                   }
                   else{
                     cell[cnt].zlow=(float)(grd->z(k)-0.5*(grd->z(k)-grd->z(k-1)));
                     cell[cnt].zhigh=(float)(grd->z(k)+0.5*(grd->z(k+1)-grd->z(k)));
                   }
       
                   cell[cnt].calc_areas();
				   cnt++;
			   }
	        }
   
         }
         if(cnt!=active_num)cout<<"FATAL ERROR see DAS\n";
  
 }

///////////////////////////////////////////////////////////
void inlet::calc_total_areas()
{

	total_prim=0.0;
	total_sec=0.0;

	for(i=0;i<active_num;i++){

		if(cell[i].type==primary){
		   if(cell[i].faces[0]||cell[i].faces[1])total_prim+=cell[i].xplane_area();
		   if(cell[i].faces[2]||cell[i].faces[3])total_prim+=cell[i].yplane_area();
		   if(cell[i].faces[4]||cell[i].faces[5])total_prim+=cell[i].zplane_area();
		}
		else{
		   if(cell[i].faces[0]||cell[i].faces[1])total_sec+=cell[i].xplane_area();
		   if(cell[i].faces[2]||cell[i].faces[3])total_sec+=cell[i].yplane_area();
		   if(cell[i].faces[4]||cell[i].faces[5])total_sec+=cell[i].zplane_area();
		}
	}

}

/////////////////////////////////////////////////////
void inlet::read_regions()
{

	int a,b,c,num_cells;
	float mag;
	char fn[50];
	cout<<"Enter the name of the regions file: \n";
	cin>>(fn);

	// initialize region_id member of grid_cell class
	for(i=0;i<active_num;i++)cell[i].region_id=-1;

	// open regions file
    if((s1=fopen(fn,"rt"))==NULL){
      fprintf(stderr, "Fail to open %s\n", "regions file");
      return;
    }

	// read the number of regions
	fscanf(s1,"%d",&region_num_prim);
	fscanf(s1,"%d",&region_num_sec);

	// read in rest of regions file
	// primary regions
	for(i=0;i<region_num_prim;i++){
		fscanf(s1,"%d",&num_cells);
		for(j=0;j<num_cells;j++){
			fscanf(s1,"%d %d %d",&a,&b,&c);
			a--; b--; c--;
			for(k=0;k<active_num;k++){
			   if(cell[k].index_i==a&&cell[k].index_j==b&&cell[k].index_k==c){
				  cell[k].region_id=i;
				  if(cell[k].type!=primary)cout<<"Error! Non-primary cell found in regions file\n";
				  break;
			   }
			}
		}
	}

	// secondary regions
	for(i=0;i<region_num_sec;i++){
		fscanf(s1,"%d",&num_cells);
		for(j=0;j<num_cells;j++){
			fscanf(s1,"%d %d %d",&a,&b,&c);
			a--; b--; c--;
			for(k=0;k<active_num;k++){
			   if(cell[k].index_i==a&&cell[k].index_j==b&&cell[k].index_k==c){
				  cell[k].region_id=i;
				  if(cell[k].type!=secondary)cout<<"Error! Non-secondary cell found in regions file\n";
				  break;
			   }
			}
		}
	}

	fclose(s1);

	// check to be sure we have a region for each active cell
	for(i=0;i<active_num;i++)
		if(cell[i].region_id==-1)cout<<"Error: cell "<<cell[i].index_i+1<<" "
		                                             <<cell[i].index_j+1<<" "
									                 <<cell[i].index_k+1<<
													 " not found in regions file\n";

	// now read info about each region
	region_info_prim=new region_properties[region_num_prim];
	region_info_sec =new region_properties[region_num_sec];

	cout<<"Enter the name of the regions data file: \n";
	cin>>(fn);

	// open regions data file
    if((s1=fopen(fn,"rt"))==NULL){
      fprintf(stderr, "Fail to open %s\n", "regions data file");
      return;
    }

	// get info for primary regions
	for(i=0;i<region_num_prim;i++){
		fscanf(s1,"%lf %lf %lf %lf %lf %lf",
			      &region_info_prim[i].mix_frac,
			      &region_info_prim[i].eta,
				  &region_info_prim[i].qratio,
				  &region_info_prim[i].uv_x,
				  &region_info_prim[i].uv_y,
				  &region_info_prim[i].uv_z);
		mag=(float)sqrt(pow(region_info_prim[i].uv_x,2.0)+
			     pow(region_info_prim[i].uv_y,2.0)+
				 pow(region_info_prim[i].uv_z,2.0));
		region_info_prim[i].uv_x/=mag;
		region_info_prim[i].uv_y/=mag;
		region_info_prim[i].uv_z/=mag;
	}

	// get info for secondary regions
	for(i=0;i<region_num_sec;i++){
		fscanf(s1,"%lf %lf %lf %lf %lf %lf",
		          &region_info_sec[i].mix_frac,
		          &region_info_sec[i].eta,
		          &region_info_sec[i].qratio,
		          &region_info_sec[i].uv_x,
		          &region_info_sec[i].uv_y,
		          &region_info_sec[i].uv_z);
		mag=(float)sqrt(pow(region_info_sec[i].uv_x,2.0)+
			     pow(region_info_sec[i].uv_y,2.0)+
				 pow(region_info_sec[i].uv_z,2.0));
		region_info_sec[i].uv_x/=mag;
		region_info_sec[i].uv_y/=mag;
		region_info_sec[i].uv_z/=mag;
	}

	fclose(s1);
	
}

////////////////////////////////////////////////////////
void inlet::calc_vel()
{

	// this is where we determine u,v,w for the inl file
	// we start by arbitrarily setting Vmag=1.0 for the 
	// 1st primary region and the 1st secondary region
	for(i=0;i<active_num;i++){
		if(cell[i].region_id==0&&cell[i].type==primary){
			cell[i].u=(float)region_info_prim[0].uv_x;
			cell[i].v=(float)region_info_prim[0].uv_y;
			cell[i].w=(float)region_info_prim[0].uv_z;
		}
		if(cell[i].region_id==0&&cell[i].type==secondary){
			cell[i].u=(float)region_info_sec[0].uv_x;
			cell[i].v=(float)region_info_sec[0].uv_y;
			cell[i].w=(float)region_info_sec[0].uv_z;
		}
	}

	// now find reference q for primary and secondary 
	float qref_prim=0.0;
	float qref_sec=0.0;

	for(i=0;i<active_num;i++){

		if(cell[i].region_id==0&&cell[i].type==primary){
		   if(cell[i].faces[0]&&cell[i].u<0.0)qref_prim+=(float)fabs(cell[i].u*cell[i].xplane_area());
		   if(cell[i].faces[1]&&cell[i].u>0.0)qref_prim+=(float)fabs(cell[i].u*cell[i].xplane_area());

		   if(cell[i].faces[2]&&cell[i].v<0.0)qref_prim+=(float)fabs(cell[i].v*cell[i].yplane_area());
		   if(cell[i].faces[3]&&cell[i].v>0.0)qref_prim+=(float)fabs(cell[i].v*cell[i].yplane_area());
		
		   if(cell[i].faces[4]&&cell[i].w<0.0)qref_prim+=(float)fabs(cell[i].w*cell[i].zplane_area());
		   if(cell[i].faces[5]&&cell[i].w>0.0)qref_prim+=(float)fabs(cell[i].w*cell[i].zplane_area());
		}
		
		if(cell[i].region_id==0&&cell[i].type==secondary){
		   if(cell[i].faces[0]&&cell[i].u<0.0)qref_sec+=(float)fabs(cell[i].u*cell[i].xplane_area());
		   if(cell[i].faces[1]&&cell[i].u>0.0)qref_sec+=(float)fabs(cell[i].u*cell[i].xplane_area());

		   if(cell[i].faces[2]&&cell[i].v<0.0)qref_sec+=(float)fabs(cell[i].v*cell[i].yplane_area());
		   if(cell[i].faces[3]&&cell[i].v>0.0)qref_sec+=(float)fabs(cell[i].v*cell[i].yplane_area());
		
		   if(cell[i].faces[4]&&cell[i].w<0.0)qref_sec+=(float)fabs(cell[i].w*cell[i].zplane_area());
		   if(cell[i].faces[5]&&cell[i].w>0.0)qref_sec+=(float)fabs(cell[i].w*cell[i].zplane_area());
		}
	}

	// now, for each region, we can find Vmag and thus u,v,w

	float vmag,flow_area;

	// primary regions
	for(i=1;i<region_num_prim;i++){

		flow_area=0.0;

		for(j=0;j<active_num;j++){
		   if(cell[j].region_id==i&&cell[j].type==primary){
		     if(cell[j].faces[0]&&region_info_prim[i].uv_x<0.0)flow_area+=(float)fabs(region_info_prim[i].uv_x*cell[j].xplane_area());
			 if(cell[j].faces[1]&&region_info_prim[i].uv_x>0.0)flow_area+=(float)fabs(region_info_prim[i].uv_x*cell[j].xplane_area());
			 
			 if(cell[j].faces[2]&&region_info_prim[i].uv_y<0.0)flow_area+=(float)fabs(region_info_prim[i].uv_y*cell[j].yplane_area());
			 if(cell[j].faces[3]&&region_info_prim[i].uv_y>0.0)flow_area+=(float)fabs(region_info_prim[i].uv_y*cell[j].yplane_area());
			 
		     if(cell[j].faces[4]&&region_info_prim[i].uv_z<0.0)flow_area+=(float)fabs(region_info_prim[i].uv_z*cell[j].zplane_area());
			 if(cell[j].faces[5]&&region_info_prim[i].uv_z>0.0)flow_area+=(float)fabs(region_info_prim[i].uv_z*cell[j].zplane_area());			 
		   }
		}

		vmag=(float)region_info_prim[i].qratio*qref_prim/flow_area;

		// now load u,v,w values
		for(j=0;j<active_num;j++){
		  if(cell[j].region_id==i&&cell[j].type==primary){

			cell[j].u=(float)region_info_prim[i].uv_x*vmag;
			cell[j].v=(float)region_info_prim[i].uv_y*vmag;
			cell[j].w=(float)region_info_prim[i].uv_z*vmag;

			// check for flow into an inlet cell from a flow cell
			if(cell[j].faces[0]&&region_info_prim[i].uv_x>0.0){
 			    cell[j].u=0.0;
			    cout<<"WARNING: x vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[1]&&region_info_prim[i].uv_x<0.0){
 			    cell[j].u=0.0;
			    cout<<"WARNING: x vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[2]&&region_info_prim[i].uv_y>0.0){
 			    cell[j].v=0.0;
			    cout<<"WARNING: y vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[3]&&region_info_prim[i].uv_y<0.0){
 			    cell[j].v=0.0;
			    cout<<"WARNING: y vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[4]&&region_info_prim[i].uv_z>0.0){
 			    cell[j].w=0.0;
			    cout<<"WARNING: z vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[5]&&region_info_prim[i].uv_z<0.0){
 			    cell[j].w=0.0;
			    cout<<"WARNING: z vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}

		  }		  
	    }

	}

	// secondary regions
	for(i=1;i<region_num_sec;i++){

		flow_area=0.0;

		for(j=0;j<active_num;j++){
		   if(cell[j].region_id==i&&cell[j].type==secondary){
			 
			 if(cell[j].faces[0]&&region_info_sec[i].uv_x<0.0)flow_area+=(float)fabs(region_info_sec[i].uv_x*cell[j].xplane_area());
			 if(cell[j].faces[1]&&region_info_sec[i].uv_x>0.0)flow_area+=(float)fabs(region_info_sec[i].uv_x*cell[j].xplane_area());
			 
			 if(cell[j].faces[2]&&region_info_sec[i].uv_y<0.0)flow_area+=(float)fabs(region_info_sec[i].uv_y*cell[j].yplane_area());
			 if(cell[j].faces[3]&&region_info_sec[i].uv_y>0.0)flow_area+=(float)fabs(region_info_sec[i].uv_y*cell[j].yplane_area());
			 
		     if(cell[j].faces[4]&&region_info_sec[i].uv_z<0.0)flow_area+=(float)fabs(region_info_sec[i].uv_z*cell[j].zplane_area());
			 if(cell[j].faces[5]&&region_info_sec[i].uv_z>0.0)flow_area+=(float)fabs(region_info_sec[i].uv_z*cell[j].zplane_area());			 
		   }
		}

		vmag=(float)region_info_sec[i].qratio*qref_sec/flow_area;

		// now load u,v,w values
		for(j=0;j<active_num;j++){
		  if(cell[j].region_id==i&&cell[j].type==secondary){

			cell[j].u=(float)region_info_sec[i].uv_x*vmag;
			cell[j].v=(float)region_info_sec[i].uv_y*vmag;
			cell[j].w=(float)region_info_sec[i].uv_z*vmag;

			// check for flow into an inlet cell from a flow cell
			if(cell[j].faces[0]&&region_info_sec[i].uv_x>0.0){
 			    cell[j].u=0.0;
			    cout<<"WARNING: x vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[1]&&region_info_sec[i].uv_x<0.0){
 			    cell[j].u=0.0;
			    cout<<"WARNING: x vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[2]&&region_info_sec[i].uv_y>0.0){
 			    cell[j].v=0.0;
			    cout<<"WARNING: y vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[3]&&region_info_sec[i].uv_y<0.0){
 			    cell[j].v=0.0;
			    cout<<"WARNING: y vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[4]&&region_info_sec[i].uv_z>0.0){
 			    cell[j].w=0.0;
			    cout<<"WARNING: z vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}
			if(cell[j].faces[5]&&region_info_sec[i].uv_z<0.0){
 			    cell[j].w=0.0;
			    cout<<"WARNING: z vel component for "<<cell[j].index_i<<" "<<cell[j].index_j<<" "<<cell[j].index_k<<" set to 0.0 - (trying to have flow into an inlet cell\n";
			}

		  }		  
	    }

	}

}

/////////////////////////////////////////////////////
void inlet::calc_region_q()
{

	float q;

	// primary
	for(j=0;j<region_num_prim;j++){
	  
	  for(q=0.0,i=0;i<active_num;i++){

		if(cell[i].region_id==j&&cell[i].type==primary){
		   if(cell[i].faces[0]&&cell[i].u<0.0)q+=(float)fabs(cell[i].u*cell[i].xplane_area());
		   if(cell[i].faces[1]&&cell[i].u>0.0)q+=(float)fabs(cell[i].u*cell[i].xplane_area());

		   if(cell[i].faces[2]&&cell[i].v<0.0)q+=(float)fabs(cell[i].v*cell[i].yplane_area());
		   if(cell[i].faces[3]&&cell[i].v>0.0)q+=(float)fabs(cell[i].v*cell[i].yplane_area());
		
		   if(cell[i].faces[4]&&cell[i].w<0.0)q+=(float)fabs(cell[i].w*cell[i].zplane_area());
		   if(cell[i].faces[5]&&cell[i].w>0.0)q+=(float)fabs(cell[i].w*cell[i].zplane_area());
		}		
	  }
	  cout<<"Primary Region #"<<j<<" Q = "<<q<<endl;
	}

	// secondary
	for(j=0;j<region_num_sec;j++){

	  for(q=0.0,i=0;i<active_num;i++){

		if(cell[i].region_id==j&&cell[i].type==secondary){
		   if(cell[i].faces[0]&&cell[i].u<0.0)q+=(float)fabs(cell[i].u*cell[i].xplane_area());
		   if(cell[i].faces[1]&&cell[i].u>0.0)q+=(float)fabs(cell[i].u*cell[i].xplane_area());

		   if(cell[i].faces[2]&&cell[i].v<0.0)q+=(float)fabs(cell[i].v*cell[i].yplane_area());
		   if(cell[i].faces[3]&&cell[i].v>0.0)q+=(float)fabs(cell[i].v*cell[i].yplane_area());
		
		   if(cell[i].faces[4]&&cell[i].w<0.0)q+=(float)fabs(cell[i].w*cell[i].zplane_area());
		   if(cell[i].faces[5]&&cell[i].w>0.0)q+=(float)fabs(cell[i].w*cell[i].zplane_area());
		}
	  }
	  cout<<"Secondary Region #"<<j<<" Q = "<<q<<endl;
	}
}

/////////////////////////////////////////////////////
void inlet::load_f_eta()
{

	// primary regions
	for(i=0;i<region_num_prim;i++){		

		// now load f, eta values
		for(j=0;j<active_num;j++){
		  if(cell[j].region_id==i&&cell[j].type==primary){
			cell[j].f=(float)region_info_prim[i].mix_frac;
			cell[j].eta=(float)region_info_prim[i].eta;
		  }		  
	    }
	}

	// secondary regions
	for(i=0;i<region_num_sec;i++){

		// now load f, eta values
		for(j=0;j<active_num;j++){
		  if(cell[j].region_id==i&&cell[j].type==secondary){
			cell[j].f=(float)region_info_sec[i].mix_frac;
			cell[j].eta=(float)region_info_sec[i].eta;
		  }		  
	    }
	}

}

////////////////////////////////////////////////////
void inlet::write_region_areas()
{

	float area;

	for(i=0;i<region_num_prim;i++){

		area=0.0;

		for(j=0;j<active_num;j++){
		   if(cell[j].region_id==i&&cell[j].type==primary){
		     if(cell[j].faces[0]||cell[j].faces[1])
			     area+=cell[j].xplane_area();
		     if(cell[j].faces[2]||cell[j].faces[3])
			     area+=cell[j].yplane_area();
		     if(cell[j].faces[4]||cell[j].faces[5])
			     area+=cell[j].zplane_area();
		   }
		}
		cout<<"Area for prim region (by either face method) "<<i+1<<" "<<area<<endl;
	}

	// secondary regions
	for(i=0;i<region_num_sec;i++){

		area=0.0;

		for(j=0;j<active_num;j++){
		   if(cell[j].region_id==i&&cell[j].type==secondary){
		     if(cell[j].faces[0]||cell[j].faces[1])
			     area+=cell[j].xplane_area();
		     if(cell[j].faces[2]||cell[j].faces[3])
			     area+=cell[j].yplane_area();
		     if(cell[j].faces[4]||cell[j].faces[5])
			     area+=cell[j].zplane_area();
		   }
		}
		cout<<"Area for sec region (by either face method) "<<i+1<<" "<<area<<endl;

	}
}

/////////////////////////////////////////////////

void inlet::load_pairs()
{
  int cnt;
    int index;

    // active cells are not included since they already have
    // specified velocity components

    // allocate memory for pair_cell array (the biggest it can possibly be is 3*active_num)
    pair_cell=new grid_cell[3*active_num];

    // load the pair_cell array
    for(cnt=0,i=0;i<active_num;i++){

      if(cell[i].faces[0]&&((cell[i].index_i+1)!=ni)&&(!active_cell(cell[i].index_i+1,cell[i].index_j,cell[i].index_k))){
          if(already_included(cnt,cell[i].index_i+1,cell[i].index_j,cell[i].index_k,&index)){
              pair_cell[index].u=cell[i].u;
          }
          else{
              pair_cell[cnt]=cell[i];
              pair_cell[cnt].index_i++;
              pair_cell[cnt].v=pair_cell[cnt].w=0.0;
              cnt++;
          }          
      }
      if(cell[i].faces[2]&&((cell[i].index_j+1)!=nj)&&(!active_cell(cell[i].index_i,cell[i].index_j+1,cell[i].index_k))){
          if(already_included(cnt,cell[i].index_i,cell[i].index_j+1,cell[i].index_k,&index)){
              pair_cell[index].v=cell[i].v;
          }
          else{
              pair_cell[cnt]=cell[i];
              pair_cell[cnt].index_j++;
              pair_cell[cnt].u=pair_cell[cnt].w=0.0;
              cnt++;
          }          
      }
      if(cell[i].faces[4]&&((cell[i].index_k+1)!=nk)&&(!active_cell(cell[i].index_i,cell[i].index_j,cell[i].index_k+1))){
          if(already_included(cnt,cell[i].index_i,cell[i].index_j,cell[i].index_k+1,&index)){
              pair_cell[index].w=cell[i].w;
          }
          else{
              pair_cell[cnt]=cell[i];
              pair_cell[cnt].index_k++;
              pair_cell[cnt].u=pair_cell[cnt].v=0.0;
              cnt++;
          }          
      }

    }
    cout<<"Pair Cell Count: "<<cnt<<endl;
    pair_cell_num=cnt;
}

/////////////////////////////////////////////////

int inlet::active_cell(int ii,int jj,int kk)
{

    int logical=0;
    for(int i=0;i<active_num;i++){
        if(cell[i].index_i==ii&&cell[i].index_j==jj&&cell[i].index_k==kk){
            logical=1;
            break;
        }
    }
    return(logical);
}

/////////////////////////////////////////////////

int inlet::already_included(int cnt,int ii,int jj,int kk, int* index)
{

    int logical=0;
    for(int i=0;i<cnt;i++){
        if(pair_cell[i].index_i==ii&&pair_cell[i].index_j==jj&&pair_cell[i].index_k==kk){
            logical=1;
            *index=i;
            break;
        }
    }
    return(logical);
}

/////////////////////////////////////////////////////
