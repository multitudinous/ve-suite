#include "H/stove_data.h"

///////////////////////////////////////////////////////////////////////////////
StoveData::StoveData( Control *CNTRL )
{
	int i, j, m=0, n, *params, iter, last_iter=9999, flagi=1, m_event,
		pop_member;
	char buffer[1000];
	double fit;
	FILE *fp;

	params = new int[15];
	
	//fp = fopen( "C:\\Doug\\Stoves\\ANN\\V8-12-2002.0\\INPUT\\fitness.dat", "r" );
	//fp = fopen( "C:\\Documents and Settings\\mccdo\\Desktop\\IFLO_multi_test\\INPUT\\fitness.dat", "r" );
	
	fp = fopen( "INPUT\fitness.dat", "r" );
	fgets( buffer, 1000, fp ); // read header

	// First get initial population members
	while ( 1 )
	{
		j = fscanf( fp, "%i%i%i", &m_event, &pop_member, &iter );
		for ( i = 0; i < 15; i ++ )
		{
			fscanf( fp, "%i", &params[i] );
		}
		fscanf( fp, "%lf", &fit );

		if ( iter <= last_iter )
		{
			// add new stove
			if ( m == 32 )
			{ // can't add a 33rd member
				s_me.push_back( new Stove( 3, params, CNTRL ) );
				s_me[0]->fhist.push_back( fit );
				n = 1;
				last_iter = iter;
				break;
			}
			s_initpop.push_back( new Stove( 3, params, CNTRL ) );
			m ++;
		}

		s_initpop[m-1]->fhist.push_back( fit );

		last_iter = iter;
	}


	// Get all stoves for all mating events
	while ( 1 )
	{
		j = fscanf( fp, "%i%i%i", &m_event, &pop_member, &iter );
		for ( i = 0; i < 15; i ++ )
		{
			fscanf( fp, "%i", &params[i] );
		}
		fscanf( fp, "%lf", &fit );
		if ( j != 3 || feof( fp ) )
		{
			break;
		}

		if ( iter <= last_iter )
		{ // add new stove
			s_me.push_back( new Stove( 3, params, CNTRL ) );
			n ++;
		}
		s_me[n-1]->fhist.push_back( fit );

		last_iter = iter;
	}
	fclose( fp );

	delete [] params;
}
///////////////////////////////////////////////////////////////////////////////
StoveData::~StoveData()
{
	int i;
	for ( i = 0; i < s_initpop.size(); i ++ )
	{
		delete s_initpop[i];
	}
	s_initpop.clear();
	
	for ( i = 0; i < s_me.size(); i ++ )
	{
		delete s_me[i];
	}
	s_me.clear();	
}
///////////////////////////////////////////////////////////////////////////////
void StoveData::OutputSimulationData()
{
	int i, j, k, npop, nme, ncfd;
	double fit;
	FILE *fp;
	Stove *s;

	// Output 's_initpop'
	fp = fopen( "sim_initpop.txt", "w" );
	npop = s_initpop.size();
	for ( i = 0; i < npop; i ++ )
	{
		s = s_initpop[i];
		ncfd = s->fhist.size();
		fprintf( fp, "Stove: " );
		for ( j = 0; j < 3; j ++ )
		{
			for ( k = 0; k < 5; k ++ )
			{
				fprintf( fp, " %i", s->bafflelist[j]->baff[k] );
			}
		}
		fprintf( fp, "\n" );
		for ( j = 0; j < ncfd; j ++ )
		{
			fit = s->fhist[j];
			fprintf( fp, "%i %i %g\n", i, j, fit );
		}
	}
	fclose( fp );

	// Output 's_me'
	fp = fopen( "sim_me.txt", "w" );
	nme = s_me.size();
	for ( i = 0; i < nme; i ++ )
	{
		s = s_me[i];
		ncfd = s->fhist.size();
		fprintf( fp, "Stove: " );
		for ( j = 0; j < 3; j ++ )
		{
			for ( k = 0; k < 5; k ++ )
			{
				fprintf( fp, " %i", s->bafflelist[j]->baff[k] );
			}
		}
		fprintf( fp, "\n" );
		for ( j = 0; j < ncfd; j ++ )
		{
			fit = s->fhist[j];
			fprintf( fp, "%i %i %g\n", i, j, fit );
		}
	}
	fclose( fp );
}
///////////////////////////////////////////////////////////////////////////////
Stove *StoveData::GetStoveFromInitialPopulation( int i )
{
	return s_initpop[i];
}
///////////////////////////////////////////////////////////////////////////////
Stove *StoveData::GetStoveFromMatingEventNumber( int i )
{
	return s_me[i];
}
///////////////////////////////////////////////////////////////////////////////
int StoveData::FindNearestStoveFromInitialPopulation( Stove *s2 )
{
	double dist, dist_min;
	int i, imin, ninitpop;

	ninitpop = s_initpop.size();
	for ( i = 0; i < ninitpop; i ++ )
	{
		Stove *s1 = s_initpop[i];
		// with s1 and s2, calculate Euclidean distance in design space
		dist = s2->Distance( s1 );
		if ( i == 0 || dist < dist_min )
		{
			dist_min = dist;
			imin = i;
		}
	}
	return imin;
}
///////////////////////////////////////////////////////////////////////////////
int StoveData::FindNearestStoveFromMatingEvents( Stove *s2 )
{
	double dist, dist_min;
	int i, imin, nme;

	nme = s_me.size();
	for ( i = 0; i < nme; i ++ )
	{
		Stove *s1 = s_me[i];
		// with s1 and s2, calculate Euclidean distance in design space
		dist = s2->Distance( s1 );
		if ( i == 0 || dist < dist_min )
		{
			dist_min = dist;
			imin = i;
		}
	}
	return imin;
}
///////////////////////////////////////////////////////////////////////////////
void StoveData::writeData( FILE *file )
{
	int ninitpop = s_initpop.size();
	for ( int i = 0; i < ninitpop; i ++ )
	{
		s_initpop[i]->writeStove( file );
	}
}
