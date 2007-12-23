#include "create_stove.h"

Create_stove::Create_stove()
{
}

Create_stove::~Create_stove()
{
}

void Create_stove::RunNewStove( int baffnumUI, vector<double> baffle1, 
vector<double> baffle2, 
vector<double> baffle3, 
vector<double> baffle4,
vector<double> baffle5, 
vector<double> baffle6, 
vector<double> baffle7,
int runnum)
{
   numBaffInts = baffnumUI * 5;
   numBaffs = baffnumUI;
   baff = new int[numBaffInts];

   inlet = 71;
   outlet = 47;




   for ( int i=0; i<5; i++)
   {
      if ( numBaffs > 0 )
         baff[i] = baffle1[i];
      if ( numBaffs > 1 )
         baff[i+5] = baffle2[i];
      if ( numBaffs > 2 )
         baff[i+10] = baffle3[i];
      if ( numBaffs > 3 )
         baff[i+15] = baffle4[i];
      if ( numBaffs > 4 )
         baff[i+20] = baffle5[i];
      if ( numBaffs > 5 )
         baff[i+25] = baffle6[i];
      if ( numBaffs > 6 )
         baff[i+30] = baffle7[i];

   }

   /*FILE *fp;
   fp = fopen("results.dat","r");

	if ( fp != NULL ){
      printf( "Opened File\n");
      fscanf( fp, "%lf", &n_fit_new );
      fscanf( fp, "%lf", &sigma );
      fscanf( fp, "%lf", &iter );
      fscanf( fp, "%lf", &fitness );
      fscanf( fp, "%lf", &area );
      fscanf( fp, "%i",  &inlet );
      fscanf( fp, "%i",  &outlet );
      for ( j = 0; j < numBaffInts; j ++ )
  	      fscanf( fp, "%i", &baff[j] );
      fclose( fp );
      for ( j = 0; j < numBaffInts; j ++ )
  	      printf( "%i  ", baff[j] );

   } else
      printf( "Could Not Open File\n" ); */

   std::cout << "|--- Create Stove --------------|"<< std::endl;
   stove = new Stove( numBaffs, baff, inlet, outlet);
   std::cout << "|--- Writing Baffle File -------|"<< std::endl;
   stove->baffleWrite();
   std::cout << "|--- Write Boundary File -------|"<< std::endl;
   stove->CompileAndExecu( runnum );  //appears to run the model
   //system("./STAR/setup > /dev/null");

   delete stove;
   delete [] baff;
   printf("Done!");
   //return 1;
}
