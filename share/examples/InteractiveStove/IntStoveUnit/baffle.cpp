#include "H/baffle.h"

Baffle::Baffle(){
  setStoveParams();
  baff=new int[5];
  //RandDef(baff);
  
  do{
    RandDef(baff);
  }while( intoWall() );
  alongWall();
  findFirst();
}

Baffle::Baffle(int params[],int i){
  setStoveParams();
  baff=new int[5];
  
  convertParams(params,i);
  
  intoWall();
  alongWall();
  findFirst();
}

Baffle::~Baffle(void){
	delete [] baff;
	}
/************************************************************************/  
  void Baffle::setStoveParams(){             
  /* Sets stove specific values */
//OLD STOVE Parameters
/*    x_cell_ct  = 42;
    y_cell_ct  = 42;
    z_cell_ct  = 6;
    maxLth     = 42;
    minLth     = 1;
    minHgt     = 1;
    x_move     = y_cell_ct+1;
    y_move     = 1;
    z_move     = (x_cell_ct+1) * (y_cell_ct+1);*/
//ONIL STOVE Parameters
/*    x_cell_ct  = 64;
    y_cell_ct  = 32;
    z_cell_ct  = 12;
    maxyLth    = 32;
    maxxLth    = 64;
    minLth     = 1;
    minHgt     = 1;
    x_move     = y_cell_ct+1;
    y_move     = 1;
    z_move     = (x_cell_ct+1) * (y_cell_ct+1);*/
//NEW STOVE-MODIFIED ONIL Parameters
    x_cell_ct  = 48;
    y_cell_ct  = 32;
    z_cell_ct  = 12;
    maxyLth    = 32;
    maxxLth    = 48;
    minLth     = 1;
    minHgt     = 1;
    x_move     = y_cell_ct+1;
    y_move     = 1;
    z_move     = (x_cell_ct+1) * (y_cell_ct+1);
  }  
/************************************************************************/  
  void Baffle::findFirst(){             
  /* Finds the first vertex int the x/y plane of the next cell to be defined */
    int y_start,z_start,x_start;
  
    x_start = x_move*baff[0];	      //Moves Vertex# 1 value in the x direction
    y_start = y_move*baff[1];	      //Moves Vertex# 1 value in the y direction
    z_start = z_move*(z_cell_ct-1); //Moves Vertex# to 1 layer below top of stove
    start_vertex = y_start + x_start + z_start + 1; //Sets begining start vertex
  }  
/************************************************************************/
  void Baffle::convertParams(int params[],int baffNum){
    for(int i=0;i<5;i++)
      baff[i]=params[i+baffNum*5];
  }
/************************************************************************/
  void Baffle::defineVerts(int* &baff){ //GOOD: CHECKED May,9,2002
  /* Puts the eight verticies required to define the baffle 
  cell into an vector  */

    switch ( baff[2] ) {
      case 0: vertex[0] = start_vertex;
              vertex[1] = start_vertex + x_move;
              vertex[2] = start_vertex + x_move + z_move;
              vertex[3] = start_vertex + z_move;
              break;
      case 1: vertex[0] = start_vertex;
              vertex[1] = start_vertex + y_move;
              vertex[2] = start_vertex + y_move + z_move;
              vertex[3] = start_vertex + z_move;
              break;
      case 2: vertex[0] = start_vertex;
              vertex[1] = start_vertex - x_move;
              vertex[2] = start_vertex - x_move + z_move;
              vertex[3] = start_vertex + z_move;
              break;
      case 3: vertex[0] = start_vertex;
              vertex[1] = start_vertex - y_move;
              vertex[2] = start_vertex - y_move + z_move;
              vertex[3] = start_vertex + z_move; 
              break;
    }
    
    for (int i = 4; i < 8; i++)  
      vertex[i] = 0; 
}
/************************************************************************/
  void Baffle::RandDef(int* &baff){
    /* Randomly Defines the string of baffles to be generated   */
    /* define starting location for baffle generation */
    baff[0] = Rand()%(x_cell_ct); // number of cells from y = 0 wall
    baff[1] = Rand()%(y_cell_ct); // number of cells from x = 0 wall

    /*  define baffle generation direction: 
      0:   for +x axis
      1:   for +y axis                      
      2:   for -x axis
      3:   for -y axis                      */
    baff[2] = Rand()%4;

    /* define number of baffles to be generated */
    //baff[3] = Rand()%(maxLth-minLth)+minLth;
    if ( baff[2] == 0 || baff[2] == 2 )
      baff[3] = Rand()%(maxxLth-minLth)+minLth;
    else if ( baff[2] == 1 || baff[2] == 3 )
      baff[3] = Rand()%(maxyLth-minLth)+minLth;

    /* define length of baffle from stove ceiling, cannot be zero */
    baff[4] = Rand()%(z_cell_ct-minHgt+1)+minHgt;
  }
/************************************************************************/
  bool Baffle::intoWall() {
  /* Checks to see if the creature is trying to 
    create a baffle through a wall */

    //int  chopVal=0;
     
    switch (baff[2]) {
      case 0: if ( (baff[0]+baff[3]) > (x_cell_ct) ) {// Checks RHS Stove wall
    			// chopVal = (baff[0]+baff[3])-x_cell_ct;
				// chopBaff(baff,chopVal);}
				return 1;}
    	      break;
      case 1: if ( (baff[1]+baff[3]) > (y_cell_ct) ) {// Checks Top Stove wall 
    			// chopVal = (baff[1]+baff[3])-y_cell_ct;
				// chopBaff(baff,chopVal);}
				return 1;}
    	      break;
      case 2: if ( (baff[0]-baff[3]) < 0 ) {          // Checks LHS Stove wall
    			// chopVal = (baff[0]+baff[3])-x_cell_ct;
				// chopBaff(baff,chopVal);}
				return 1;}
    	      break;
      case 3: if ( (baff[1]-baff[3]) < 0 ) {          // Checks Bottom Stove wall 
    			// chopVal = (baff[1]+baff[3])-y_cell_ct;
				// chopBaff(baff,chopVal);}
				return 1;}
    	      break;
    }
	return 0;
  }

/************************************************************************/
  void Baffle::alongWall() {
  /* Checks to see if the baffle is being created 
    along a wall, if so it should be nulled */
    
    switch (baff[2]) {
      case 0: if (baff[1]==0 || baff[1]==x_cell_ct) 
  	        makeNull(baff);
  	      break;		
      case 1: if (baff[0]==0 || baff[0]==y_cell_ct)   
  	        makeNull(baff);
  	      break;				 
      case 2: if (baff[1] == 0 || baff[1] == x_cell_ct) // Same as case 0
  	        makeNull(baff);                              // Independent of baffle 
  	      break;		                                   // generation direction 
      case 3: if (baff[0] == 0 || baff[0] == y_cell_ct) // Same as case 1
  	        makeNull(baff);
  	      break;				 
    }
  }

/************************************************************************/
  void Baffle::chopBaff(int* &baff,int chopVal) {
  /* If baffle runs through a wall or overlaps, stop generation where 
  it hits the wall. Change it's definition */

    baff[3]-=chopVal;
    
    if(baff[3]==0)
      makeNull(baff);
  }
/************************************************************************/
  void Baffle::makeNull(int* &baff){
  /* If baffle runs along a wall, make it do nothing */
    for (int i=0; i<5;i++) 
      baff[i] = 0;
  }
/************************************************************************/
  void Baffle::writeBaffle(FILE *log){
    for(int i=0;i<5;i++)
      fprintf( log, "%5i", baff[i] );
    fprintf( log, "\n" );
  }
/*******************************************/
int Baffle::Rand( void ) {
	#ifdef _WIN32
		  return rand();
	#else
		  return lrand48();
	#endif
	
}
