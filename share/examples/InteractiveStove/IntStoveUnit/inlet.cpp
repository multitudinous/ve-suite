#include "H/inlet.h"
Vertex::Vertex(int vertexNumber, float x, float y, float z): 
         vertexNumber(vertexNumber), x(x), y(y), z(z){}

Vertex::Vertex( void ){}

Vertex::~Vertex( void ){}    

//************************************************************************/  
//************************************************************************/  
Cell::Cell(int cellNumber): cellNumber(cellNumber){}

Cell::Cell( void ){}

Cell::~Cell( void ){
   neighbors.clear();
}    

//************************************************************************/  
//************************************************************************/  
InletOutlet::InletOutlet( int start,int cStart, int distance, int i, int j, 
                           int h, int w, int d, int tcell, int tvert){
   vertexStart = start;       //inlet 353, outlet 11095 NOT zero referenced starting vert location 
   cellStart   = cStart;      //inlet 345,oulet 8821, starting cell location 
   distanceMax = distance;    //inlet 19, outlet 35 NOT zero referenced, number of vertices the inlet/outlet can move
   i_offset    = i;           //MUST be zero referenced, cell number
   j_offset    = j;           //MUST be zero referenced, cell number
   height      = h;           //MUST NOT be zero referenced, 9 for inlet, 25 oulet
   width       = w;           //MUST NOT be zero referenced, 9 for both
   depth       = d;           //MUST NOT be zero referenced, 9 for both  
   topCellNumber     = tcell; //12057 outlet cell number for the first cell in the top outlet
   topVertexNumber   = tvert; //14807 outlet cell number for the first cell in the top outlet
   //OLD STOVE
/*   if( vertexStart == 353 )
		range = 3;
   else
      range = 5;*/
   //NEW STOVE
   if( vertexStart == 265 )
		range = 3;
   else
      range = 1;
   setParams();
   InitializePossiblities();
   readSurfaceFile();
   RandDef();

//      inlet  = new InletOutlet( vertexStart265, cellStart257, distanceMax33, 
//          i_offset8, j_offset0,  9, 9, 9, topCellNumber19969, topVertexNumber23047);
//      outlet = new InletOutlet( 19405, 16897, 25, 0, 0, 25, 9, 9, 19905, 22966);
}

InletOutlet::~InletOutlet(void){
	top.clear();
   bottom.clear();
	possibilities.clear();
   surface.clear();
   usable.clear();
	}

InletOutlet::InletOutlet( InletOutlet *copy){
   this->height            = copy->height;
   this->width             = copy->width;
   this->depth             = copy->depth;
   this->startPosition     = copy->startPosition;
   this->x_vert_ct         = copy->x_vert_ct;
   this->y_vert_ct         = copy->y_vert_ct;
   this->x_move            = copy->x_move;
   this->y_move            = copy->y_move;
   this->y_dist            = copy->y_dist;
   this->x_dist            = copy->x_dist;
   this->vertexStart       = copy->vertexStart;
   this->cellStart         = copy->cellStart;
   this->distanceMax       = copy->distanceMax;
   this->i_offset          = copy->i_offset;
   this->j_offset          = copy->j_offset;
   this->topCellNumber     = copy->topCellNumber;
   this->topVertexNumber   = copy->topVertexNumber;
   this->numCellsInSurface = copy->numCellsInSurface;
   this->range			      = copy->range;

   this->top           = copy->top;
   this->bottom        = copy->bottom;
   this->surface       = copy->surface;
   this->possibilities = copy->possibilities;
   this->usable        = copy->usable;
   setParams();
}

//************************************************************************/  
void InletOutlet::setParams( void ){             
   //OLD STOVE
/*   x_vert_ct = 43;
   y_vert_ct = 43;
   x_move    = y_vert_ct;
   y_move    = 1;
   x_dist = distanceMax;
   y_dist = distanceMax;*/
   //NEW STOVE
/*   x_vert_ct = 65;
   y_vert_ct = 33;
   x_move    = y_vert_ct;
   y_move    = 1;
   x_dist = distanceMax;
   y_dist = distanceMax;*/
   //NEW STOVE -MODIFIED ONIL
   x_vert_ct = 49;
   y_vert_ct = 33;
   x_move    = y_vert_ct;
   y_move    = 1;
   x_dist = distanceMax;
   y_dist = distanceMax;

   if ( vertexStart == 265 ) {
      x_dist = distanceMax;
      y_dist = 25;
   } else {
      x_dist = 1;
      y_dist = distanceMax;
   }
   //NEW STOVE-MODIFIED ONIL
   numCellsInSurface = 1536;  //Number of cells defined in the surface of the stove

   //NEW STOVE
   //numCellsInSurface = 2048;  //Number of cells defined in the surface of the stove
   //OLD STOVE
   //numCellsInSurface = 1764;  //Number of cells defined in the surface of the stove
}  
//************************************************************************/  
/*void InletOutlet::InitializePossiblities( void ){             
   int i, j, vertex, z, cell, size;
	if ( vertexStart == 232 )
      z = 0;
   else
      z = 1;
//   inlet  = new InletOutlet(   353,  345, 27, 9, 9,  9, 9, 9, 12121, 14888);
//   outlet = new InletOutlet( 11095, 8821, 35, 0, 0, 25, 9, 9, 12057, 14807);

   j = j_offset;
   vertex = vertexStart;
   cell   = cellStart;
   possibilities.push_back( Cell( cell ) );
   size = possibilities.size() - 1;
   possibilities[ size ].neighbors.push_back( Vertex(vertex, i_offset*0.5, j*0.5, z) );
    
   for ( i = 1; i < x_dist; i++ ) {
      vertex += (x_move);
      cell += ((x_move-1));
      possibilities.push_back( Cell( cell ) );
      size = possibilities.size() - 1;
      possibilities[ size ].neighbors.push_back( Vertex(vertex, (i + i_offset)*0.5, j*0.5, z) );
   }

   i = i_offset + (x_dist - 1);
   for ( j = 1; j < y_dist; j++ ) {
      vertex += (y_move);
      cell += (y_move);
      possibilities.push_back( Cell( cell ) );
      size = possibilities.size() - 1;
      possibilities[ size ].neighbors.push_back( Vertex(vertex, (i*0.5), (j + j_offset)*0.5, z) );
   }

   j = j_offset + (y_dist - 1);
   for ( i = x_dist - 2; i >= 0; i-- ) {
      vertex += (-1 * x_move);
      cell += (-1 * (x_move-1));
      possibilities.push_back( Cell( cell ) );
      size = possibilities.size() - 1;
      possibilities[ size ].neighbors.push_back( Vertex(vertex, (i + i_offset)*0.5, j*0.5, z) );
   }

   i = i_offset ;
   for ( j = y_dist - 2; j > 0; j-- ) {
      vertex += (-1 * y_move);
      cell += (-1 * y_move);
      possibilities.push_back( Cell( cell ) );
      size = possibilities.size() - 1;
      possibilities[ size ].neighbors.push_back( Vertex(vertex, i*0.5, (j + j_offset)*0.5, z) );
   }
}*/
  
//************************************************************************/  
void InletOutlet::InitializePossiblities( void ){             
   int i, j, vertex, z, cell, size;
	if ( vertexStart == 265 )
      z = 0;
   else
      z = 1;
//      inlet  = new InletOutlet( 265, 257, 33, 8, 0,  9, 9, 9, 19969, 23047);
//      outlet = new InletOutlet( 19405, 16897, 25, 0, 0, 25, 9, 9, 19905, 22966);

//   inlet  = new InletOutlet( vertexStart265, cellStart257, distanceMax33, 
//     i_offset8, j_offset0,  9, 9, 9, topCellNumber19969, topVertexNumber23047);


   j = j_offset;
   vertex = vertexStart;
   cell   = cellStart;
   possibilities.push_back( Cell( cell ) );
   size = possibilities.size() - 1;
   possibilities[ size ].neighbors.push_back( Vertex(vertex, i_offset*0.5, j*0.5, z) );
    
	if ( x_dist>1 )
      for ( i = 1; i < x_dist; i++ ) {
         vertex += (x_move);
         cell += ((x_move-1));
         possibilities.push_back( Cell( cell ) );
         size = possibilities.size() - 1;
         possibilities[ size ].neighbors.push_back( Vertex(vertex, (i + i_offset)*0.5, j*0.5, z) );
      }  

   i = i_offset + (x_dist - 1);
   for ( j = 1; j < y_dist; j++ ) {
      vertex += (y_move);
      cell += (y_move);
      possibilities.push_back( Cell( cell ) );
      size = possibilities.size() - 1;
      possibilities[ size ].neighbors.push_back( Vertex(vertex, (i*0.5), (j + j_offset)*0.5, z) );
   }

	if ( x_dist>1 ) {
      j = j_offset + (y_dist - 1);
      for ( i = x_dist - 2; i >= 0; i-- ) {
         vertex += (-1 * x_move);
         cell += (-1 * (x_move-1));
         possibilities.push_back( Cell( cell ) );
         size = possibilities.size() - 1;
         possibilities[ size ].neighbors.push_back( Vertex(vertex, (i + i_offset)*0.5, j*0.5, z) );
      }
   }
   
	if ( vertexStart != 265 ) {
      i = i_offset ;
      for ( j = y_dist - 2; j > 0; j-- ) {
         vertex += (-1 * y_move);
         cell += (-1 * y_move);
         possibilities.push_back( Cell( cell ) );
         size = possibilities.size() - 1;
         possibilities[ size ].neighbors.push_back( Vertex(vertex, i*0.5, (j + j_offset)*0.5, z) );
      }
   }
}

//************************************************************************/
void InletOutlet::findBottomCells( void ){
   int i, j, cellNumber;
   
   cellNumber = possibilities[ startPosition ].cellNumber;
   bottom.clear();
   for ( j = 0; j < depth - 1; j++)
      for ( i = 0; i < width - 1; i++) {
         bottom.push_back( Cell(cellNumber + j + (i*(x_move - 1))) );
         //cout << bottom[ bottom.size() - 1].cellNumber << endl;
      }
}

//************************************************************************/
void InletOutlet::findTopCells( void ){ 
   int i,j;   

   top.clear();
   for ( j = 0; j < width - 1; j++)
      for ( i = 0; i < depth - 1; i++)
         top.push_back( Cell(topCellNumber + i + (j*(depth-1)) ));
   findNeighbors();
}

//************************************************************************/
void InletOutlet::RandDef( void ){
   if (vertexStart == 265)
      startPosition = 71;  //Changed 2-25-04, we know where we want the inlet and outlet
   else  
      startPosition = 47;

   //startPosition = Rand()%possibilities.size();
   evolveInletOutlet();
}

//************************************************************************/
void InletOutlet::findNeighbors( void ) {
   int i, vStart;
   
   for ( i = 0; i < top.size(); i++ ){
      if( i == 0)
         vStart = topVertexNumber;
      else {
         vStart += 1;

         if ( i%int(depth - 1) == 0 )
            vStart += 1;
      }
      top[ i ].neighbors.push_back( Vertex( vStart ,0,0,0) );
      top[ i ].neighbors.push_back( Vertex( vStart + depth,0,0,0) );
      top[ i ].neighbors.push_back( Vertex( vStart + depth + 1,0,0,0) );
      top[ i ].neighbors.push_back( Vertex( vStart + 1,0,0,0) );
   }   
}

//************************************************************************/
void InletOutlet::readSurfaceFile( void ) {
   int vert1, vert2, vert3, vert4, k, i, cell, trash;
   
   if ( vertexStart != 265 ){
     FILE *fp;
     //FILE *fp = fopen( "./STAR/surface", "w" );
     //if ( fp == NULL )
      //{
         //cout << " Error writing file " << endl;
         //exit(0);
      //}
     /*fprintf( fp, "#! /bin/tcsh\n" );
     fprintf( fp, "\n" );
     fprintf( fp, "cd ./STAR/\n" );
     fprintf( fp, "source /usr/local/starcd/etc/setstar\n" );
     fprintf( fp, "proam << EOF\n" );
     fprintf( fp, "x\n" );
     fprintf( fp, "star\n" );
     fprintf( fp, "n\n" );
     fprintf( fp, "y\n\n" );
 
 
     //fprintf( fp, "cset news name topsurf\n" );
     //fprintf( fp, "cset topsurf\n" );
     //fprintf( fp, "cwrite,surface.cel,0,cset,,all,code\n" );
     fprintf( fp, "cwrite,surface.cel,0,all,,5,code\n" );
     //fprintf( fp, "cwrite\n" );
     //fprintf( fp, "surface.cel\n" );
     //fprintf( fp, "cset all\n" );
     fprintf( fp, "close surface.cel\n" );
     fprintf( fp, "quit,nosave\n" );
     fclose( fp );
     system( "cp ./STAR/star.mdl.bak ./STAR/star.mdl" );
     system( "chmod 770 ./STAR/surface" );
     system( "./STAR/surface ");//> /dev/null" );
     //system( "./STAR/surface" );
     system( "rm ./STAR/surface" );*/

     std::cout << "|--- Reading Surface File ------|"<< std::endl;
     surface.clear();
     fp = fopen( "../../../STAR/surface.cel", "r" );
     if ( fp != NULL ) {
      for ( k = 0; k < numCellsInSurface; k ++ ) {
         fscanf( fp, "%i", &cell );
         surface.push_back( Cell( cell ) );
         for( i = 0; i < 4; i++ )
            fscanf( fp, "%i", &trash );
         fscanf( fp, "%i", &vert1 );
         fscanf( fp, "%i", &vert2 );
         fscanf( fp, "%i", &vert3 );
         fscanf( fp, "%i", &vert4 );
         for( i = 0; i < 2; i++ )
            fscanf( fp, "%i", &trash );
         surface[ k ].neighbors.push_back( Vertex( vert1,0,0,0 ) );
         surface[ k ].neighbors.push_back( Vertex( vert2,0,0,0 ) );
         surface[ k ].neighbors.push_back( Vertex( vert3,0,0,0 ) );
         surface[ k ].neighbors.push_back( Vertex( vert4,0,0,0 ) );
         }
      //system( "rm ./STAR/surface.cel" );
      fclose( fp );
     } else {
         std::cout << " STARCD NOT working!!" << std::endl;
     }
      //exit(1);  
   }
}

//************************************************************************/
void InletOutlet::evolveInletOutlet( void ){
   findTopCells();
   findBottomCells();
}
/*******************************************/
int InletOutlet::Rand( void ) {
	#ifdef _WIN32
		  return rand();
	#else
		  return lrand48();
	#endif
	
}

